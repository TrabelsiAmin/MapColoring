# MapColoring

A constraint-based graph coloring solver using Prolog and Python. This project solves the map coloring problem, determining the minimum number of colors needed to color a graph such that no two adjacent vertices share the same color.

## Features

- **Constraint Logic Programming**: Uses SWI-Prolog with CLP(FD) for efficient constraint solving
- **Graph Visualization**: Visualizes the colored graph using NetworkX and Matplotlib
- **Multiple Implementations**: Includes Prolog and Python implementations
- **Flexible Input Format**: Supports simple text-based graph definition

## Project Structure

```
MapColoring/
├── prolog/              # Constraint-based solver
│   ├── coloration.pl   # Main coloring algorithm
│   └── io.pl           # File I/O operations
├── prologVersion1/      # Alternative Prolog implementation
│   ├── graphes.pl
│   └── gui.pl
├── python/              # Python interface and visualization
│   ├── main.py         # Main solver script
│   └── visualiser.py   # Graph visualization
├── carte.txt           # Example graph edges
├── solution.txt        # Generated solution
└── README.md          # This file
```

## Requirements

- SWI-Prolog (7.x or higher)
- Python 3.6+
- NetworkX
- Matplotlib

## Installation

### Step 1: Install SWI-Prolog

**Windows**: Download from https://www.swi-prolog.org/download/stable
**Linux**: `sudo apt-get install swi-prolog`
**macOS**: `brew install swi-prolog`

### Step 2: Install Python Dependencies

```bash
pip install -r requirements.txt
```

## Usage

### Basic Usage

```bash
python python/main.py
```

This script:

1. Loads the graph from `carte.txt`
2. Solves the coloring problem using Prolog
3. Saves the solution to `solution.txt`
4. Displays the result

### Visualize the Solution

```bash
python python/visualiser.py
```

This displays the colored graph in an interactive window.

## Input Format

The `carte.txt` file should contain edges in the format:

```
node1 node2
node1 node3
node2 node3
...
```

Each line represents an undirected edge between two nodes.

## Output Format

The `solution.txt` file contains the coloring solution:

```
node1 color_number
node2 color_number
node3 color_number
...
```

Colors are represented as integers (1, 2, 3, 4, ...).

## Algorithm Details

The solver uses Constraint Logic Programming (CLP) with domain constraints:

- Each node is assigned a color from the domain [1..4]
- Adjacent nodes must have different colors
- The `labeling` function finds a valid assignment

## Examples

### Example Graph

```
Edges: a-b, a-c, b-d, b-e, c-e, c-f, d-g, e-g, e-h, f-h, g-i, h-i, h-j, i-j
Solution: 4 colors needed
```

## Contributing

Feel free to extend this project with:

- Additional graph solvers (backtracking, greedy algorithms)
- More sophisticated visualization options
- Support for weighted graphs
- Performance benchmarks

## License

This project is open source and available under the MIT License.

## Author

Created as an educational project in graph theory and constraint programming.
