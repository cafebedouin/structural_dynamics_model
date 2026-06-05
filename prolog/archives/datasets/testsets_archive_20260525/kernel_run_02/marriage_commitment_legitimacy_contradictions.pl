% Axiom contradictions for kernel: marriage_commitment_legitimacy
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% exogenous_override_reading↔endogenous_reinterpretation_reading: No single coherent framework can hold both 'federal coercion forced capitulation with unchanged doctrine' and 'divine revelation commanded theological reversal.' If the doctrine is unchanged (exogenous), God did not command the reversal (endogenous). If God commanded it (endogenous), the doctrine changed (not merely suspended under duress).

narrative_ontology:cs_axiom_contradiction(federal_coercion_determines_practice, manifestation_as_genuine_prophecy).
narrative_ontology:cs_axiom_contradiction(manifestation_as_genuine_prophecy, federal_coercion_determines_practice).
