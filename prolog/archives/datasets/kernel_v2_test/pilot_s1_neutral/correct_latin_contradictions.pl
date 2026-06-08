% Axiom contradictions for kernel: correct_latin
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% continuity_reading↔discontinuity_reading: Continuity reading's axiom (living practice grounds linguistic legitimacy) and discontinuity reading's axiom (textual fidelity grounds linguistic legitimacy) are mutually exclusive authority criteria. No single coherent framework can hold both: either practice or text is the ultimate arbiter of correctness, not both simultaneously. Hybrid_reading attempts to hold both but does so by subordinating practice to textual correction, effectively collapsing to discontinuity's axiom with a continuity concession.

narrative_ontology:cs_axiom_contradiction(unbroken_community_entails_linguistic_continuity, classical_form_is_pure_norm).
narrative_ontology:cs_axiom_contradiction(classical_form_is_pure_norm, unbroken_community_entails_linguistic_continuity).
