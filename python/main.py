import subprocess

# Appelle le code Prolog
subprocess.run(["swipl", "-q", "-s", "prolog/coloration.pl", "-g",
                "charger_carte('carte.txt', Sommets), colorier(Sommets, Couleurs), sauvegarder_solution('solution.txt', Sommets, Couleurs), halt."])

# Lit la solution
solution = {}
with open("solution.txt") as f:
    for line in f:
        region, couleur = line.strip().split()
        solution[region] = int(couleur)

print("Solution obtenue :")
print(solution)
