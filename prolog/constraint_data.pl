:- module(constraint_data, [
    base_extractiveness/2,
    suppression_score/2
]).

:- use_module(config).
:- use_module(narrative_ontology).

:- multifile base_extractiveness/2, suppression_score/2.

base_extractiveness(C, V) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(C, ExtMetricName, V).

suppression_score(C, V) :-
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(C, SuppMetricName, V).
