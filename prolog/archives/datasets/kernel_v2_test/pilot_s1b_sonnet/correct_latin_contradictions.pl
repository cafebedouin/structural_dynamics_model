% Axiom contradictions for kernel: correct_latin
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% continuity_reading↔rupture_reading: Continuity assumes medieval Latin is a degraded but continuous evolution of Classical (same kernel, drifted). Rupture assumes medieval Latin is a separate linguistic system (different kernel). No single framework can hold both: either the medieval form is reachable from Classical through correction (continuity) or it is not (rupture). These are mutually exclusive claims about linguistic identity.

narrative_ontology:cs_axiom_contradiction(linguistic_continuity_across_vulgarization, textual_recovery_primacy).
narrative_ontology:cs_axiom_contradiction(textual_recovery_primacy, linguistic_continuity_across_vulgarization).
