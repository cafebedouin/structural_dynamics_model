% Axiom contradictions for kernel: personhood_boundary_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% autonomy_reading↔personhood_continuity_reading: Autonomy_reading's axiom (the embryo is not yet a rights-holder distinct from the mother, so bodily control governs) and personhood_continuity_reading's axiom (the embryo is already a full rights-holder whose innocent life cannot be taken) cannot both be true simultaneously — accepting one's characterization of the embryo's moral status as true requires rejecting the other's.

narrative_ontology:cs_axiom_contradiction(bodily_autonomy_is_prior_to_third_party_claims, developmental_continuity_entails_status_equivalence).
narrative_ontology:cs_axiom_contradiction(developmental_continuity_entails_status_equivalence, bodily_autonomy_is_prior_to_third_party_claims).
