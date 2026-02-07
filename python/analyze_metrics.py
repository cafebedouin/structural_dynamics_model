import json
import os
from collections import defaultdict
import statistics
import re

def analyze_corpus_metrics():
    """
    Analyzes the corpus data to calculate metric statistics for each constraint type.
    """
    corpus_path = os.path.join(os.path.dirname(__file__), '../outputs/corpus_data.json')
    
    try:
        with open(corpus_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
    except FileNotFoundError:
        print(f"Error: Corpus data file not found at {corpus_path}")
        return
    except json.JSONDecodeError:
        print(f"Error: Could not decode JSON from {corpus_path}")
        return

    metrics_by_computed_type = defaultdict(lambda: {'extractiveness': [], 'suppression': []})
    valid_types = {'mountain', 'rope', 'snare', 'tangled_rope', 'scaffold', 'piton'}

    for constraint_id, constraint_data in data.get('constraints', {}).items():
        if not constraint_data or not isinstance(constraint_data, dict):
            continue

        metrics = constraint_data.get('metrics', {})
        extractiveness = metrics.get('extractiveness')
        suppression = metrics.get('suppression')

        if extractiveness is None or suppression is None:
            continue

        classifications = constraint_data.get('classifications', [])
        if not classifications:
            continue

        for classification in classifications:
            computed_type = classification.get('type')
            if computed_type and computed_type in valid_types:
                # We are interested in the *computed* type for this analysis
                metrics_by_computed_type[computed_type]['extractiveness'].append(extractiveness)
                metrics_by_computed_type[computed_type]['suppression'].append(suppression)


    print("Metric Analysis by Computed Constraint Type")
    print("=" * 80)
    print(f"{'Type':<15} {'Metric':<20} {'Count':<10} {'Min':<10} {'Max':<10} {'Mean':<10} {'StdDev':<10}")
    print("-" * 80)

    for constraint_type, values in sorted(metrics_by_computed_type.items()):
        for metric_name, data_points in values.items():
            if not data_points:
                continue
            
            count = len(data_points)
            min_val = min(data_points)
            max_val = max(data_points)
            mean_val = statistics.mean(data_points)
            stdev_val = statistics.stdev(data_points) if count > 1 else 0.0

            print(f"{constraint_type:<15} {metric_name:<20} {count:<10} {min_val:<10.2f} {max_val:<10.2f} {mean_val:<10.2f} {stdev_val:<10.2f}")
    print("=" * 80)

if __name__ == "__main__":
    analyze_corpus_metrics()
