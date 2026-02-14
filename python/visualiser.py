"""
Graph Coloring Visualizer
Displays the colored graph using NetworkX and Matplotlib.
"""

import os
import sys
import networkx as nx
import matplotlib.pyplot as plt

def visualize_coloring(carte_file="carte.txt", solution_file="solution.txt"):
    """
    Load and visualize the colored graph.
    
    Args:
        carte_file: Path to the input graph file
        solution_file: Path to the solution file
        
    Raises:
        FileNotFoundError: If required files don't exist
    """
    
    # Check if files exist
    if not os.path.exists(carte_file):
        raise FileNotFoundError(f"Graph file '{carte_file}' not found")
    
    if not os.path.exists(solution_file):
        raise FileNotFoundError(f"Solution file '{solution_file}' not found. Run main.py first.")
    
    # Load the graph
    edges = []
    try:
        with open(carte_file, 'r') as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    a, b = line.split()
                    edges.append((a, b))
                except ValueError:
                    print(f"Warning: Could not parse edge line '{line}'", file=sys.stderr)
    except IOError as e:
        raise IOError(f"Could not read graph file: {e}")
    
    # Load the solution
    colors = {}
    try:
        with open(solution_file, 'r') as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    region, color = line.split()
                    colors[region] = int(color)
                except ValueError:
                    print(f"Warning: Could not parse solution line '{line}'", file=sys.stderr)
    except IOError as e:
        raise IOError(f"Could not read solution file: {e}")
    
    if not colors:
        raise ValueError("No solution data found in solution file")
    
    # Create graph
    G = nx.Graph()
    G.add_edges_from(edges)
    
    # Prepare colors
    color_palette = ['#FF6B6B', '#4ECDC4', '#45B7D1', '#FFA07A', 
                     '#98D8C8', '#F7DC6F', '#BB8FCE', '#85C1E2']
    
    # Map node colors
    node_colors = []
    for node in G.nodes():
        if node in colors:
            color_idx = (colors[node] - 1) % len(color_palette)
            node_colors.append(color_palette[color_idx])
        else:
            print(f"Warning: No color assigned to node '{node}'", file=sys.stderr)
            node_colors.append('#CCCCCC')
    
    # Create visualization
    plt.figure(figsize=(12, 8))
    
    # Use spring layout for better visualization
    pos = nx.spring_layout(G, k=2, iterations=50, seed=42)
    
    # Draw graph
    nx.draw_networkx_nodes(G, pos, node_color=node_colors, node_size=1000, alpha=0.9)
    nx.draw_networkx_edges(G, pos, width=2, alpha=0.6)
    nx.draw_networkx_labels(G, pos, font_size=10, font_weight='bold')
    
    # Add title and info
    colors_used = len(set(colors.values()))
    plt.title(f"Graph Coloring Solution\n{colors_used} colors used", fontsize=14, fontweight='bold')
    plt.axis('off')
    plt.tight_layout()
    
    # Display info
    print(f"✓ Graph loaded: {G.number_of_nodes()} nodes, {G.number_of_edges()} edges")
    print(f"✓ Colors used: {colors_used}")
    
    plt.show()

if __name__ == "__main__":
    try:
        visualize_coloring()
    except (FileNotFoundError, IOError, ValueError) as e:
        print(f"✗ Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"✗ Unexpected error: {e}", file=sys.stderr)
        sys.exit(1)
