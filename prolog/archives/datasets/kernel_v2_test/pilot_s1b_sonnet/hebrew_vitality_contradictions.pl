% Axiom contradictions for kernel: hebrew_vitality
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% liturgical_reading↔native_daily_reading: Liturgical reading holds that symbolic preservation constitutes occupying the kernel (vitality = continuity of ritual use). Native-daily reading holds that only native generation constitutes vitality (ritual use = preservation, not life). No single coherent framework can hold both: either ritual recitation counts as vitality or it does not.

narrative_ontology:cs_axiom_contradiction(ritual_transmission_suffices_for_vitality, vernacular_use_constitutes_linguistic_life).
narrative_ontology:cs_axiom_contradiction(vernacular_use_constitutes_linguistic_life, ritual_transmission_suffices_for_vitality).
