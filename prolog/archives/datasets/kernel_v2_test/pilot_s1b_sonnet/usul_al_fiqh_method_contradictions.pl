% Axiom contradictions for kernel: usul_al_fiqh_method
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% hanafi_reading↔hanbali_reading: Hanafi axiom (analogical reasoning is expansively valid where text is silent) and Hanbali axiom (weak hadith preferred over analogy; analogy minimized) cannot coexist in a single framework—accepting Hanbali's textualist restriction requires rejecting Hanafi's rationalist expansion as epistemically invalid
% maliki_reading↔shafii_reading: Maliki axiom (Medinan practice carries independent evidentiary weight) and Shafi'i axiom (only authenticated hadith, not regional practice, is valid Sunnah evidence) are mutually exclusive—accepting Shafi'i's authentication requirement rejects Maliki's elevation of unauthenticated practice to source status

narrative_ontology:cs_axiom_contradiction(reason_supplements_revelation_expansively, textual_sufficiency_primacy).
narrative_ontology:cs_axiom_contradiction(textual_sufficiency_primacy, reason_supplements_revelation_expansively).
narrative_ontology:cs_axiom_contradiction(medinan_practice_independent_authority, authenticated_hadith_supersedes_qiyas).
narrative_ontology:cs_axiom_contradiction(authenticated_hadith_supersedes_qiyas, medinan_practice_independent_authority).
