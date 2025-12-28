# Slicer
Currently, this generates an SVG for every layer in a (binary) `.stl` file.
Not every edge case is accounted for, but it should properly slice manifold or
water-tight models like [3DBenchy](https://github.com/CreativeTools/3DBenchy/).

# Usage
```
slicer <model_path> <output_dir> <layer_height?: 0.2>
```

# Build
```
cargo build --release
```
The resulting binary should be at `./target/release/slicer`.