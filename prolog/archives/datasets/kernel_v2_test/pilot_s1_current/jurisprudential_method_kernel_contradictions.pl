% Axiom contradictions for kernel: jurisprudential_method_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% hanafi_reading↔hanbali_reading: Hanafi elevates reason (qiyas/istihsan) to co-equal source status; Hanbali restricts reason to servant of text. No single framework holds both: accepting reason as co-equal source requires rejecting textualist restriction as foundational axiom.
% maliki_reading↔shafi_i_reading: Maliki grounds authority in living communal practice (amal) as independent source; Shafi'i subordinates practice to systematic textual hierarchy. No single framework holds both: accepting practice as source-level authority requires rejecting Shafi'i's hierarchical systematization as complete.

narrative_ontology:cs_axiom_contradiction(reason_as_coequal_authority, textual_primacy_with_restricted_extension).
narrative_ontology:cs_axiom_contradiction(textual_primacy_with_restricted_extension, reason_as_coequal_authority).
narrative_ontology:cs_axiom_contradiction(medinan_practice_prophetic_authenticity, formal_hierarchy_prevents_arbitrariness).
narrative_ontology:cs_axiom_contradiction(formal_hierarchy_prevents_arbitrariness, medinan_practice_prophetic_authenticity).
