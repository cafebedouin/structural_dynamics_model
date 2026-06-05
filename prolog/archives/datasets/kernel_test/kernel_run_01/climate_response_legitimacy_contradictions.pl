% Axiom contradictions for kernel: climate_response_legitimacy
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% mitigation_priority↔degrowth_transformation: Mitigation-priority axiom holds that economic growth is compatible with climate stability via technological decoupling; degrowth axiom holds that growth imperative itself is incompatible with climate stability. No single framework can hold both: accepting degrowth's axiom requires rejecting the decoupling premise that grounds mitigation-priority legitimacy.

narrative_ontology:cs_axiom_contradiction(technological_decoupling_feasible, growth_decoupling_insufficient).
narrative_ontology:cs_axiom_contradiction(growth_decoupling_insufficient, technological_decoupling_feasible).
narrative_ontology:cs_story_uid(climate_response_legitimacy_contradictions, '26645708-de4e-4dbd-bc8e-4da80a2628a9').
narrative_ontology:cs_created_at('26645708-de4e-4dbd-bc8e-4da80a2628a9', '').
