import sys
import re
from statistics import mean, stdev

def process_file_data_per_process(file_path):
    with open(file_path, 'r') as file:
        data = {}
        current_process = None

        for line in file:
            line = line.strip()

            # Identify the process
            if line.startswith("****** Process"):
                current_process = line.split()[2]
                if current_process not in data:
                    data[current_process] = {"iterate": [], "hashing": [], "gen_server": []}

            # Check for hashing and gen_server lines
            if "crypto:hash" in line or "pure_hash" in line:
                time_spent = float(line.split()[3])
                data[current_process]["hashing"].append(time_spent)

            if "iterate_binary" in line:
                time_spent = float(line.split()[3])
                data[current_process]["iterate"].append(time_spent)

            if "gen_server:" in line:
                time_spent = float(line.split()[3])
                data[current_process]["gen_server"].append(time_spent)

        # Calculate mean and std for each process
        results = {}
        for process, times in data.items():
            hashing_times = times["hashing"]
            gen_server_times = times["gen_server"]
            iterate_times = times["iterate"]

            if hashing_times:
                hashing_mean = mean(hashing_times)
                hashing_std = stdev(hashing_times)
            else:
                hashing_mean = hashing_std = 0

            if gen_server_times:
                gen_server_mean = mean(gen_server_times)
                gen_server_std = stdev(gen_server_times)
            else:
                gen_server_mean = gen_server_std = 0

            if iterate_times:
                iterate_times_mean = mean(iterate_times)
                iterate_times_std = stdev(iterate_times) if len(iterate_times) > 1 else 0
            else:
                iterate_times_mean = iterate_times_std = 0

            results[process] = {
                "iterate": {"mean": iterate_times_mean, "std": iterate_times_std},
                "hashing": {"mean": hashing_mean, "std": hashing_std},
                "gen_server": {"mean": gen_server_mean, "std": gen_server_std}}

        return results

def main():
    if len(sys.argv) != 2:
        print("Usage: python script.py <input_file>")
        sys.exit(1)

    file_path = sys.argv[1]

    data = process_file_data_per_process(file_path)

    total_hashing_time = sum(process['hashing']['mean'] for process in data.values())

    total_gen_server_time = sum(process['gen_server']['mean'] for process in data.values())

    total_iterate_time = sum(process['iterate']['mean'] for process in data.values())

    mean_hashing_time = total_hashing_time / len(data)

    mean_gen_server_time = total_gen_server_time / len(data)

    mean_iterate_time = total_iterate_time / len(data)

    print(f"Mean hashing time (uS): {mean_hashing_time}")
    print(f"Mean gen_server time (uS): {mean_gen_server_time}")
    print(f"Mean iterate time (uS): {mean_iterate_time}")



    # print(data)

if __name__ == "__main__":
    main()
