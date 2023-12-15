import re
import sys

def distance(a, b, ring_size):
    """Calculates the clockwise distance between two IDs in a Chord ring."""
    return (b - a + ring_size) % ring_size

def ft_start(node_id, i, ring_size):
    return (node_id + 2 ** (i - 1)) % ring_size

def calculate_finger_entry(node_id, i, ring_size, nodes):
    start = ft_start(node_id, i, ring_size)
    # Create a list of (node_id, distance) tuples
    distances = [(n, distance(start, n, ring_size)) for n in nodes]

    # Find the node with the smallest distance to the start value
    closest_node = min(distances, key=lambda x: x[1])[0]
    return closest_node

# Rest of the script remains the same


def parse_input(input_data):
    nodes = {}
    node_id_map = {}  # Map node names to their IDs
    current_node_id = None
    for line in input_data.split('\n'):
        if line.startswith('State:'):
            match = re.search(r'id = (\d+), ref = (node\d+)', line)
            current_node_id = int(match.group(1))
            current_node_name = match.group(2)
            node_id_map[current_node_name] = current_node_id
            nodes[current_node_id] = []
        else:
            match = re.match(r'(\d+), \d+, "\[\d+, \d+\)", (node\d+)', line)
            if match:
                index = int(match.group(1))
                node_ref = match.group(2)
                nodes[current_node_id].append((index, node_ref))
            else:
                print(f"Warning: No match found for line: {line}")
    return nodes, node_id_map

def check_finger_tables(input_data, M, ring_size):
    nodes_data, node_id_map = parse_input(input_data)
    nodes = list(nodes_data.keys())
    errors = []

    # Create an inverse mapping from ID to name for error messages
    node_name_map = {v: k for k, v in node_id_map.items()}

    for node_id, fingers in nodes_data.items():
        for index, actual_successor_name in fingers:
            actual_successor_id = node_id_map.get(actual_successor_name, None)
            expected_successor_id = calculate_finger_entry(node_id, index, ring_size, nodes)
            if expected_successor_id != actual_successor_id:
                expected_successor_name = node_name_map.get(expected_successor_id, "Unknown")
                errors.append(f"Node {{'{node_name_map[node_id]}', {node_id}}}, Index {index}: "
                              f"Start = {ft_start(node_id, index, ring_size)} "
                              f"Expected Node {{'{expected_successor_name}', {expected_successor_id}}}, "
                              f"but found Node {{'{actual_successor_name}', {actual_successor_id}}}")

    return errors

def main():
    if len(sys.argv) != 3:
        print("Usage: python script.py <M> <input_file>")
        sys.exit(1)

    M = int(sys.argv[1])  # Size of the identifier space
    input_file = sys.argv[2]

    with open(input_file, 'r') as file:
        input_data = file.read()

    ring_size = 2 ** M
    errors = check_finger_tables(input_data, M, ring_size)

    if errors:
        print("Errors found in the finger tables:")
        for error in errors:
            print(error)
    else:
        print("All finger tables are correct.")

if __name__ == "__main__":
    main()
