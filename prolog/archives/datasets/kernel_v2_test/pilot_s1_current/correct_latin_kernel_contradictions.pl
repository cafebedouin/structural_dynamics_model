% Axiom contradictions for kernel: correct_latin_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% continuity_reading↔discontinuity_reading: Continuity reading holds that living practice legitimates linguistic change; discontinuity reading holds that only textual fidelity legitimates. No single framework can hold both: either drift is legitimate evolution or it is corruption requiring symbolic recovery.

narrative_ontology:cs_axiom_contradiction(medieval_development_continuity, classical_forms_primacy_recoverable).
narrative_ontology:cs_axiom_contradiction(classical_forms_primacy_recoverable, medieval_development_continuity).
