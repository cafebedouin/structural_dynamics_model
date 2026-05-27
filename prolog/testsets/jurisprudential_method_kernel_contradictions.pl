% Axiom contradictions for kernel: jurisprudential_method_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% hanafi_reading↔hanbali_reading: Hanafi axiom (reason is a legitimate tool for extending divine law) and Hanbali axiom (human reasoning beyond text is bid'ah) cannot coexist in a single framework. Accepting Hanbali's axiom requires rejecting analogical reasoning as a valid source; accepting Hanafi's axiom requires treating reason as divinely sanctioned.

narrative_ontology:cs_axiom_contradiction(reason_as_legitimate_extension_tool, qiyas_is_bid_ah).
narrative_ontology:cs_axiom_contradiction(qiyas_is_bid_ah, reason_as_legitimate_extension_tool).
narrative_ontology:cs_story_uid(jurisprudential_method_kernel_contradictions, '2da85e40-666c-413a-9f16-7f81e2f51068').
narrative_ontology:cs_created_at('2da85e40-666c-413a-9f16-7f81e2f51068', '').
