% Axiom contradictions for kernel: marriage_authority_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% parsi_community_reading↔secular_contractual_reading: Parsi reading grounds authority in ethno-religious community membership (endogamy enforcement, loss of status for out-marriage); secular reading grounds authority in individual choice independent of community. No framework holds both — accepting community-membership-as-authority requires rejecting individual-choice-as-sole-basis.

narrative_ontology:cs_axiom_contradiction(zoroastrian_identity_through_endogamy, marriage_as_individual_consent).
narrative_ontology:cs_axiom_contradiction(marriage_as_individual_consent, zoroastrian_identity_through_endogamy).
