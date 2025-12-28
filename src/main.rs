use std::collections::{HashSet, HashMap};
use std::fs;
use std::fs::File;
use std::io::Write;
use std::env;
use std::hash::{Hash, Hasher};

fn main() {
    let args: Vec<String> = env::args().collect();
    let arg_count = args.len();
    if arg_count < 3 {
        eprintln!("Missing arguments.\nslicer <model_path> <output_dir> <layer_height?: 0.2>");
        return
    }
    let contents = fs::read(&args[1]).expect("Failed to read model.");
    let out_dir = &args[2];

    let layer_height: f32 = if arg_count == 4 {
        match args[3].parse() {
            Ok(n) if n > 0.0 => n,
            _ => {
                eprintln!("Invalid layer height.");
                return;
            }
        }
    } else { 0.2 };

    match fs::create_dir(out_dir) {
        Ok(_) => (),
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => (),
        _ => {
            eprintln!("Failed to create output directory.");
            return
        }
    };

    let model = Model::from_stl(contents);
    let layers= ((model.max_bounds.z - model.min_bounds.z) / layer_height).floor() as usize;

    let mut z = model.min_bounds.z + layer_height;
    for i in 0..layers {
        let data = model.slice_svg(z);

        let mut file = File::create(format!("{out_dir}/{}.svg", i)).expect(format!("Failed to write file for z = {z}").as_str());
        let _ = file.write_all(data.as_bytes());

        z += layer_height;
    }
}

#[derive(Debug, Clone)]
struct Point {
    x: f32,
    y: f32,
    z: f32
}

impl Point {
    pub fn new(bytes: &[u8]) -> Self {
        Self {
            x: f32::from_le_bytes(bytes[0..4].try_into().unwrap()),
            y: f32::from_le_bytes(bytes[4..8].try_into().unwrap()),
            z: f32::from_le_bytes(bytes[8..12].try_into().unwrap()),
        }
    }
}

impl PartialEq for Point {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y && self.z == other.z
    }
}

impl Eq for Point {}

impl Hash for Point {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        state.write_u32(self.x.to_bits());
        state.write_u32(self.y.to_bits());
        state.write_u32(self.z.to_bits());
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
struct Triangle {
    normal: Point,
    vertices: [Point; 3],
    attr: [u8; 2]
}

impl Triangle {
    pub fn new(bytes: &[u8]) -> Self {
        let mut vertices = [
            Point::new(&bytes[12..24]),
            Point::new(&bytes[24..36]),
            Point::new(&bytes[36..48])
        ];

        vertices.sort_unstable_by(|a, b| {
            a.x.total_cmp(&b.x)
                .then(a.y.total_cmp(&b.y))
                .then(a.z.total_cmp(&b.z))
        });

        Self {
            normal: Point::new(&bytes[0..12]),
            vertices,
            attr: bytes[48..50].try_into().unwrap()
        }
    }

    pub fn edges(&self) -> [(Point, Point); 3] {
        let v = &self.vertices;
        [
            (v[0].clone(), v[1].clone()),
            (v[1].clone(), v[2].clone()),
            (v[0].clone(), v[2].clone()),
        ]
    }
}

impl PartialEq for Triangle {
    fn eq(&self, other: &Self) -> bool {
        self.vertices == other.vertices
    }
}

impl Eq for Triangle {}

impl Hash for Triangle {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.vertices.hash(state);
    }
}

#[allow(dead_code)]
struct Model {
    header: [u8; 80],
    triangles: Vec<Triangle>,
    min_bounds: Point,
    max_bounds: Point,
    edge_map: HashMap<(Point, Point), Vec<usize>>
}

impl Model {
    pub fn from_stl(bytes: Vec<u8>) -> Self {
        let header: [u8; 80] = bytes[0..80].try_into().unwrap();
        let mut triangles = Vec::new();
        let mut seen = HashSet::new();

        let triangle_count = (u32::from_le_bytes(bytes[80..84].try_into().unwrap()) as usize)
            .min((bytes.len() - 84) / 50);

        for i in 0..triangle_count {
            let triangle = Triangle::new(&bytes[84 + i*50..134 + i*50]);

            // Discard malformed triangles
            let [p1, p2, p3] = &triangle.vertices;
            if p1 == p2 || p2 == p3 || p1 == p3 {
                continue;
            }

            // Discard duplicates
            if seen.insert(triangle.clone()) {
                triangles.push(triangle);
            }
        }

        let (min_bounds, max_bounds) = Self::get_bounds(&triangles);

        let edge_map = Self::create_edge_map(&triangles);

        Self {
            header,
            triangles,
            min_bounds,
            max_bounds,
            edge_map
        }
    }

    pub fn get_bounds(triangles: &Vec<Triangle>) -> (Point, Point) {
        let mut min = (f32::INFINITY, f32::INFINITY, f32::INFINITY);
        let mut max = (f32::NEG_INFINITY, f32::NEG_INFINITY, f32::NEG_INFINITY);

        for triangle in triangles {
            for point in &triangle.vertices {
                min.0 = min.0.min(point.x);
                min.1 = min.1.min(point.y);
                min.2 = min.2.min(point.z);

                max.0 = max.0.max(point.x);
                max.1 = max.1.max(point.y);
                max.2 = max.2.max(point.z);
            }
        }

        (
            Point { x: min.0, y: min.1, z: min.2 },
            Point { x: max.0, y: max.1, z: max.2 }
        )
    }

    fn create_edge_map(triangles: &Vec<Triangle>) -> HashMap<(Point, Point), Vec<usize>> {
        let mut edge_map: HashMap<(Point, Point), Vec<usize>> = HashMap::with_capacity(triangles.len() * 3);
        for (index, triangle) in triangles.iter().enumerate() {
            let edges = triangle.edges();

            for edge in edges {
                edge_map.entry(edge).or_insert(Vec::new()).push(index);
            }
        }

        edge_map
    }

    fn slice_svg(&self, z: f32) -> String {
        let paths = Self::find_paths(z, &self.triangles, &self.edge_map);

        let padding = 10.0;

        let width = self.max_bounds.x - self.min_bounds.x + padding;
        let height = self.max_bounds.y - self.min_bounds.y + padding;
        let mut data = format!("<svg width=\"{width}\" height=\"{height}\" viewBox=\"{} {} {width} {height}\" xmlns=\"http://www.w3.org/2000/svg\"><path d=\"", self.min_bounds.x - padding/2.0, self.min_bounds.y - padding/2.0);

        for path in paths {
            if path.len() == 0 { continue }
            data += format!("M{} {}", path[0].x, path[0].y).as_str();

            for point in &path[1..] {
                data += format!("L{} {}", point.x, point.y).as_str();
            }
            data += "Z";
        }
        
        data.push_str("\" fill=\"#f004\" stroke=\"red\" stroke-width=\".1\" fill-rule=\"evenodd\"/></svg>");
        data
    }

    fn is_edge_cut(edge: &(Point, Point), z: f32) -> bool {
        let (a, b) = &edge;
        !((a.z < z && b.z < z) || (a.z > z && b.z > z)) && a.z != b.z
    }

    fn intersect_edge(edge: &(Point, Point), z: f32) -> Point {
        let (a, b) = edge;
        let t = (z - a.z) / (b.z - a.z);

        Point {
            x: a.x + t * (b.x - a.x),
            y: a.y + t * (b.y - a.y),
            z
        }
    }

    fn next_cut_edge(triangle: &Triangle, last_edge: &(Point, Point), z: f32) -> Option<(Point, Point)> {
        let mut cut_edges = triangle.edges()
            .into_iter()
            .filter(|edge| Self::is_edge_cut(edge, z));

        let first = match cut_edges.next() {
            Some(e) => e,
            None => return None
        };

        let second = match cut_edges.next() {
            Some(e) => e,
            None => return None
        };

        if let Some(third) = cut_edges.next() {
            let last_point = Self::intersect_edge(&last_edge, z);

            [first, second, third].into_iter()
                .find(|e| Self::intersect_edge(e, z) != last_point)
        } else {
            Some(if first == *last_edge { second } else { first })
        }
    }

    fn find_paths(z: f32, triangles: &Vec<Triangle>, edge_map: &HashMap<(Point, Point), Vec<usize>>) -> Vec<Vec<Point>> {
        let mut triangle_set = HashSet::new();

        for (index, triangle) in triangles.iter().enumerate() {
            let v = &triangle.vertices;
            
            if (v[0].z > z && v[1].z > z && v[2].z > z) ||
                (v[0].z < z && v[1].z < z && v[2].z < z) ||
                (v[0].z == v[1].z && v[0].z == v[2].z) {
                continue
            }

            triangle_set.insert(index);
        }

        if triangle_set.is_empty() {
            return Vec::new();
        }

        let mut visited = HashSet::new();
        let mut paths = Vec::new();

        for index in &triangle_set {
            if visited.contains(index) { continue; }

            let mut path: Vec<Point> = Vec::new();

            let mut last_edge = triangles[*index].edges()
                .into_iter()
                .find(|edge| Self::is_edge_cut(edge, z))
                .expect("Invalid triangle");

            let start_index = index;
            let mut index = index;

            loop {
                visited.insert(index);

                let next_edge = match Self::next_cut_edge(&triangles[*index], &last_edge, z) {
                    Some(e) => e,
                    None => {
                        eprintln!("Incomplete path at z {z}");
                        break;
                    }
                };

                path.push(Self::intersect_edge(&next_edge, z));

                let next = edge_map.get(&next_edge)
                    .and_then(|adj_triangles| {
                        adj_triangles.iter().find(|i| *i != index && triangle_set.contains(*i))
                    });

                if let Some(next_index) = next {
                    if next_index == start_index || visited.contains(next_index) {
                        break;
                    }

                    index = next_index;
                    last_edge = next_edge.clone();
                } else {
                    eprintln!("Incomplete path at z {z}");
                    break;
                }
            }

            if !path.is_empty() {
                paths.push(path);
            }
        }

        paths
    }
}