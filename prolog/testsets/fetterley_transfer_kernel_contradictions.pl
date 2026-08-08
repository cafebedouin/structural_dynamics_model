% Axiom contradictions for kernel: fetterley_transfer_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% deflationary_reading↔installed_authorship_reading: installed_authorship_reading's axiom is that reading positions are authored and can be re-authored through deliberately addressed literature (the manual is writable). deflationary_reading's axiom is that there is no position to author in the first place, only proximity in a distribution — so 're-authoring a position' is a category error, not a hard-but-real intervention.

narrative_ontology:cs_axiom_contradiction(no_installed_position_exists, reading_position_is_authored_not_excavated).
narrative_ontology:cs_axiom_contradiction(reading_position_is_authored_not_excavated, no_installed_position_exists).
