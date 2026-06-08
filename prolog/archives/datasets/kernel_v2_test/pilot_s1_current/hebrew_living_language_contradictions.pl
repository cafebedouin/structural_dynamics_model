% Axiom contradictions for kernel: hebrew_living_language
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% liturgical_reading↔native_daily_reading: Liturgical reading holds that preserved recitation constitutes occupancy; native-daily reading holds that only generative vernacular use constitutes occupancy. No single linguistic framework can hold both 'recitation = living' and 'recitation ≠ living' simultaneously.

narrative_ontology:cs_axiom_contradiction(textual_recitation_preserves_liveness, native_speaker_authenticity_requirement).
narrative_ontology:cs_axiom_contradiction(native_speaker_authenticity_requirement, textual_recitation_preserves_liveness).
