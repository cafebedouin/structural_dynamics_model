% Axiom contradictions for kernel: latin_correctness
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% continuity_reading↔rupture_reading: Continuity reading treats linguistic change as legitimate evolution; rupture reading treats post-classical change as corruption requiring reversal. No single framework can hold both 'organic change is legitimate' and 'organic change is corruption' simultaneously.

narrative_ontology:cs_axiom_contradiction(medieval_change_is_natural_evolution, classical_purity_as_sole_standard).
narrative_ontology:cs_axiom_contradiction(classical_purity_as_sole_standard, medieval_change_is_natural_evolution).
