% Axiom contradictions for kernel: adverse_effect_guarantee_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% instrument_dependent_reading↔textualist_severability_reading: Instrument-dependent holds that a right's practical unenforceability constitutes its functional suspension; textualist-severability holds that the right's validity is wholly independent of any instrument's adequacy. A single framework cannot simultaneously treat evidentiary provability as constitutive of the right and as merely incidental to it — accepting one axiom requires rejecting the other.
% instrument_dependent_reading↔channel_conversion_reading: Instrument-dependent locates the rights-defeating mechanism in the measurement gap itself (the guarantee becomes unprovable). Channel-conversion locates it in the mobility/exit structure of the visa, treating the measurement gap as a secondary symptom. A framework that holds the wage-measurement gap as THE constitutive failure cannot simultaneously hold that the wage-measurement gap is merely downstream of a separate, more fundamental mobility failure — one claim subordinates the other's primary mechanism to secondary status, which is mutually exclusive as a claim about where the causal weight sits.

narrative_ontology:cs_axiom_contradiction(guarantee_is_coextensive_with_measurement_instrument, guarantee_severable_from_implementing_instrument).
narrative_ontology:cs_axiom_contradiction(guarantee_severable_from_implementing_instrument, guarantee_is_coextensive_with_measurement_instrument).
narrative_ontology:cs_axiom_contradiction(guarantee_is_coextensive_with_measurement_instrument, mobility_structure_is_load_bearing_mechanism).
narrative_ontology:cs_axiom_contradiction(mobility_structure_is_load_bearing_mechanism, guarantee_is_coextensive_with_measurement_instrument).
