% Axiom contradictions for kernel: usul_al_fiqh_method
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% hanafi_reading↔hanbali_reading: Hanafi epistemology holds that reason can independently generate legal rulings through qiyas/istihsan; Hanbali epistemology holds that only transmitted text can authorize rulings. No single framework can hold both: accepting reason as independent source requires rejecting text-only constraint, and vice versa.

narrative_ontology:cs_axiom_contradiction(qiyas_validity_foundational, text_literalism_methodological_necessity).
narrative_ontology:cs_axiom_contradiction(text_literalism_methodological_necessity, qiyas_validity_foundational).
