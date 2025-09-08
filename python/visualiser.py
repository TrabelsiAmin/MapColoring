import networkx as nx
import matplotlib.pyplot as plt

# Charger la carte
edges = []
with open("carte.txt") as f:
    for line in f:
        a, b = line.strip().split()
        edges.append((a, b))

# Charger la solution
colors = {}
with open("solution.txt") as f:
    for line in f:
        region, color = line.strip().split()
        colors[region] = int(color)

G = nx.Graph()
G.add_edges_from(edges)
color_map = ['red', 'green', 'blue', 'yellow']
node_colors = [color_map[colors[n] - 1] for n in G.nodes()]

nx.draw(G, with_labels=True, node_color=node_colors, node_size=800)
plt.title("Coloration de la carte")
plt.show()
