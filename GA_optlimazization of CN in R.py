import numpy as np
import pandas as pd
from deap import base, creator, tools, algorithms
import random

# Load your data
data_combinedCN = pd.read_csv("d:/2_granty_projekty/2_Bezici/0_DS/datbaze_data/K_S_optimalization.csv")

# Define NSE calculation
def nse(observed, predicted):
    mean_observed = np.mean(observed)
    numerator = np.sum((observed - predicted) ** 2)
    denominator = np.sum((observed - mean_observed) ** 2)
    return 1 - (numerator / denominator)

# Define the SCS-CN model
def CN_model(params, Hrain_mm):
    CN, rIa = params
    A = 25.4 * (1000 / CN - 10)
    Ia = A * rIa
    He_mm = np.where(Ia > Hrain_mm, 0, (Hrain_mm - Ia) ** 2 / (Hrain_mm - Ia + A))
    return He_mm

# Objective function for GA
def objective_function(params, subset_data):
    Hrain_mm = subset_data['CC_Rain_m3'] / subset_data['area'] * 1000
    predicted_runoff = CN_model(params, Hrain_mm)
    observed_runoff = subset_data['CC_Runoff_m3'] / subset_data['area'] * 1000
    return nse(observed_runoff, predicted_runoff),

# Bounds for dry and wet conditions
CNlower_bounds_dry = [20, 0.2]
CNupper_bounds_dry = [99, 0.35]
CNlower_bounds_wet = [40, 0.1]
CNupper_bounds_wet = [99, 0.3]

# Define decorators to enforce bounds after mutation and crossover
def checkBounds(min_bound, max_bound):
    def decorator(func):
        def wrapped(*args, **kargs):
            offspring = func(*args, **kargs)
            for child in offspring:
                for i in range(len(child)):
                    if child[i] < min_bound[i]:
                        child[i] = min_bound[i]
                    elif child[i] > max_bound[i]:
                        child[i] = max_bound[i]
            return offspring
        return wrapped
    return decorator

# Configure GA environment
creator.create("FitnessMax", base.Fitness, weights=(1.0,))
creator.create("Individual", list, fitness=creator.FitnessMax)

toolbox = base.Toolbox()
toolbox.register("attr_float", random.uniform, CNlower_bounds_dry[0], CNupper_bounds_dry[1])
toolbox.register("individual", tools.initRepeat, creator.Individual, toolbox.attr_float, n=2)
toolbox.register("population", tools.initRepeat, list, toolbox.individual)

# Loop through each unique run.ID
best_solutions = []

for CNxID in data_combinedCN['run.ID'].unique():
    subset_data = data_combinedCN[data_combinedCN['run.ID'] == CNxID]
    print(f"Starting GA for combination: {CNxID}")

    # Set bounds based on initial condition
    if subset_data.iloc[0]['initial.cond.'] == 'dry':
        lower_bounds = CNlower_bounds_dry
        upper_bounds = CNupper_bounds_dry
    else:
        lower_bounds = CNlower_bounds_wet
        upper_bounds = CNupper_bounds_wet

    # Update toolbox bounds based on condition
    toolbox.register("attr_float", random.uniform, lower_bounds[0], upper_bounds[1])
    toolbox.register("individual", tools.initRepeat, creator.Individual, toolbox.attr_float, n=2)
    toolbox.register("population", tools.initRepeat, list, toolbox.individual)

    # GA operators
    toolbox.register("evaluate", objective_function, subset_data=subset_data)
    toolbox.register("mate", tools.cxBlend, alpha=0.5)
    toolbox.register("mutate", tools.mutGaussian, mu=0, sigma=0.05, indpb=0.2)  # Adjust sigma as needed
    toolbox.register("select", tools.selTournament, tournsize=3)

    # Decorate to enforce bounds after mutation and crossover
    toolbox.decorate("mate", checkBounds(lower_bounds, upper_bounds))
    toolbox.decorate("mutate", checkBounds(lower_bounds, upper_bounds))

    # Initialize population
    population = toolbox.population(n=200)

    # Run the genetic algorithm
    result, log = algorithms.eaSimple(population, toolbox, cxpb=0.8, mutpb=0.2, ngen=100, verbose=False)

    # Extract the best solution for this ID
    best_individual = tools.selBest(population, k=1)[0]
    best_CN, best_Ia = best_individual[0], best_individual[1]
    best_nse = objective_function(best_individual, subset_data=subset_data)[0]

    best_solutions.append({
        'run.ID': CNxID,
        'best_CN': best_CN,
        'best_Ia': best_Ia,
        'nse_CN': best_nse
    })
    print(f"Best CN: {best_CN}, Best Ia: {best_Ia}, NSE: {best_nse}")

# Convert results to a DataFrame
best_solutions_df = pd.DataFrame(best_solutions)
data_combinedCN = data_combinedCN.merge(best_solutions_df, on='run.ID', how='left')
data_combinedCN.to_csv("d:/2_granty_projekty/2_Bezici/0_DS/datbaze_data/optiamalized_K_S_CN_Ia.csv", index=False)
#print(best_solutions_df)
