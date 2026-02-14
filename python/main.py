"""
Map Coloring Solver
Solves the graph coloring problem using Prolog constraint logic programming.
"""

import subprocess
import sys
import os

def solve_coloring(carte_file="carte.txt", solution_file="solution.txt", prolog_file="prolog/coloration.pl"):
    """
    Solve the graph coloring problem using Prolog.
    
    Args:
        carte_file: Path to the input graph file
        solution_file: Path to save the solution
        prolog_file: Path to the Prolog solver
        
    Returns:
        dict: A dictionary mapping nodes to their color assignments
    """
    
    # Check if input file exists
    if not os.path.exists(carte_file):
        raise FileNotFoundError(f"Graph file '{carte_file}' not found")
    
    # Prepare the Prolog command
    prolog_command = [
        "swipl", "-q", "-s", prolog_file, "-g",
        f"charger_carte('{carte_file}', Sommets), colorier(Sommets, Couleurs), sauvegarder_solution('{solution_file}', Sommets, Couleurs), halt."
    ]
    
    try:
        # Execute Prolog solver
        result = subprocess.run(prolog_command, capture_output=True, text=True, timeout=30)
        
        if result.returncode != 0:
            raise RuntimeError(f"Prolog solver failed: {result.stderr}")
            
    except FileNotFoundError:
        raise RuntimeError("SWI-Prolog is not installed or not in PATH. Please install it from https://www.swi-prolog.org/download/stable")
    except subprocess.TimeoutExpired:
        raise RuntimeError("Prolog solver timed out (>30s). Graph might be too complex.")
    
    # Read the solution
    if not os.path.exists(solution_file):
        raise RuntimeError(f"Solution file '{solution_file}' was not generated")
    
    solution = {}
    try:
        with open(solution_file, 'r') as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    region, couleur = line.split()
                    solution[region] = int(couleur)
                except ValueError:
                    print(f"Warning: Could not parse line '{line}'", file=sys.stderr)
    except IOError as e:
        raise RuntimeError(f"Could not read solution file: {e}")
    
    return solution

if __name__ == "__main__":
    try:
        solution = solve_coloring()
        print("✓ Map coloring solution:")
        print("-" * 40)
        
        # Sort and display solution
        for region in sorted(solution.keys()):
            color = solution[region]
            color_names = ["Red", "Green", "Blue", "Yellow"]
            color_name = color_names[color - 1] if color <= len(color_names) else f"Color {color}"
            print(f"  {region:10} → {color_name}")
        
        print("-" * 40)
        colors_used = len(set(solution.values()))
        print(f"✓ Colors required: {colors_used}")
        
    except (FileNotFoundError, RuntimeError) as e:
        print(f"✗ Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"✗ Unexpected error: {e}", file=sys.stderr)
        sys.exit(1)
