import matplotlib.pyplot as plt
import numpy as np
import sys

def plot_chord_ring(nodes, M):
    ring_size = 2 ** M
    fig, ax = plt.subplots(figsize=(10, 10))  # Adjust figure size if needed

    # Draw the ring
    circle = plt.Circle((0, 0), 1, color='lightgray', fill=False)
    ax.add_artist(circle)

    # Plot each node and adjust label positions
    for node, id in nodes.items():
        angle = 2 * np.pi * id / ring_size
        x, y = np.cos(angle), np.sin(angle)
        ax.plot(x, y, 'o', markersize=10)  # Adjust marker size if needed

        # Calculate label offset
        label_x = x * 1.15  # Adjust multiplier for greater offset
        label_y = y * 1.15  # Adjust multiplier for greater offset

        ax.text(label_x, label_y, f' {node} ({id})', fontsize=9,  # Adjust fontsize if needed
                ha='right' if x < 0 else 'left', 
                va='top' if y < 0 else 'bottom')

    # Set equal aspect and limits
    ax.set_aspect('equal', adjustable='datalim')
    ax.plot()
    plt.title('Chord Ring Visualization')
    plt.show()

def parse_node_file(filename):
    nodes = {}
    with open(filename, 'r') as file:
        for line in file:
            parts = line.strip().split(':')
            if len(parts) == 2:
                node, id = parts[0].strip(), int(parts[1].strip())
                nodes[node] = id
    return nodes

def main():
    if len(sys.argv) != 3:
        print("Usage: python script.py <M> <input_file>")
        sys.exit(1)

    M = int(sys.argv[1])
    input_file = sys.argv[2]
    nodes = parse_node_file(input_file)

    plot_chord_ring(nodes, M)

if __name__ == "__main__":
    main()
