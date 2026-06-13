% Axiom contradictions for kernel: performance_legitimacy
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% quantitative_growth_reading↔qualitative_development_reading: Quantitative reading treats growth rate as non-negotiable floor (legitimacy fails below threshold); qualitative reading treats growth rate as negotiable outcome of structural optimization (legitimacy survives lower growth if quality improves). No single framework can hold both 'growth rate is legitimacy floor' and 'growth rate is subordinate to quality' simultaneously.
% techno_nationalist_reading↔livelihood_security_reading: Techno-nationalist reading subordinates consumption and welfare spending to strategic industrial investment (legitimacy through geopolitical capability); livelihood reading subordinates industrial investment to immediate service delivery (legitimacy through daily experience). No single framework can hold both 'strategic industries are legitimacy foundation' and 'daily welfare is legitimacy foundation' as primary simultaneously.

narrative_ontology:cs_axiom_contradiction(gdp_growth_as_legitimacy_signal, innovation_primacy_over_volume).
narrative_ontology:cs_axiom_contradiction(innovation_primacy_over_volume, gdp_growth_as_legitimacy_signal).
narrative_ontology:cs_axiom_contradiction(strategic_autonomy_primacy, legitimacy_via_felt_experience).
narrative_ontology:cs_axiom_contradiction(legitimacy_via_felt_experience, strategic_autonomy_primacy).
