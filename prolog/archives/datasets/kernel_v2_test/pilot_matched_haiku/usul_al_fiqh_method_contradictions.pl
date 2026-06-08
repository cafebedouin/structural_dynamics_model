% Axiom contradictions for kernel: usul_al_fiqh_method
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% hanafi_reading↔hanbali_reading: Hanafi epistemology holds that reason can independently generate legal rulings through qiyas/istihsan; Hanbali epistemology holds that only transmitted text can authorize rulings. No single framework can hold both: accepting reason as independent source requires rejecting text-only constraint, and vice versa.
% maliki_reading↔shafii_reading: Maliki epistemology holds that Medinan customary practice ('amal) has independent authority even against isolated hadith; Shafi'i epistemology holds that only authenticated hadith can override other authenticated hadith. No single framework can hold both: accepting 'amal as authoritative requires rejecting hadith-only hierarchy, and vice versa.
% hanafi_reading↔shafii_reading: Hanafi epistemology accepts istihsan (juristic preference) as a valid source; Shafi'i epistemology categorically rejects istihsan as arbitrary. No single framework can hold both: accepting istihsan as valid requires rejecting its categorical prohibition, and vice versa.

narrative_ontology:cs_axiom_contradiction(juristic_reason_as_legitimate_source, text_literalism_necessity).
narrative_ontology:cs_axiom_contradiction(text_literalism_necessity, juristic_reason_as_legitimate_source).
narrative_ontology:cs_axiom_contradiction(medina_practice_authoritative_source, hadith_hierarchy_requirement).
narrative_ontology:cs_axiom_contradiction(hadith_hierarchy_requirement, medina_practice_authoritative_source).
narrative_ontology:cs_axiom_contradiction(juristic_reason_as_legitimate_source, hadith_hierarchy_requirement).
narrative_ontology:cs_axiom_contradiction(hadith_hierarchy_requirement, juristic_reason_as_legitimate_source).
