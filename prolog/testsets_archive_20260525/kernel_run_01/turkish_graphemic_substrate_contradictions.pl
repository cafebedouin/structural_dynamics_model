% Axiom contradictions for kernel: turkish_graphemic_substrate
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% ottoman_continuity_reading↔secular_nationalist_reading: No single framework can hold both 'Turkish identity is continuous with Ottoman-Islamic civilization' and 'Turkish identity must rupture from Ottoman-Islamic past.' Accepting one axiom requires rejecting the other as the foundation of legitimate graphemic authority.

narrative_ontology:cs_axiom_contradiction(ottoman_islamic_continuity_essential, secularism_requires_european_alignment).
narrative_ontology:cs_axiom_contradiction(secularism_requires_european_alignment, ottoman_islamic_continuity_essential).
