% Axiom contradictions for kernel: hebrew_living_language
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% liturgical_reading↔native_daily_reading: No single coherent framework can hold both 'symbolic preservation constitutes linguistic life' and 'only generative vernacular use constitutes linguistic life' — these are mutually exclusive definitions of what 'living language' means
% continuity_narrative_reading↔native_daily_reading: No single coherent framework can hold both 'revival was restoration of pre-existing natural state' and 'Hebrew was dormant/dead until reconstruction' — one requires continuity, the other requires rupture and reconstruction

narrative_ontology:cs_axiom_contradiction(ritual_domain_sufficiency, vernacular_primacy).
narrative_ontology:cs_axiom_contradiction(vernacular_primacy, ritual_domain_sufficiency).
narrative_ontology:cs_axiom_contradiction(restoration_not_construction, vernacular_primacy).
narrative_ontology:cs_axiom_contradiction(vernacular_primacy, restoration_not_construction).
