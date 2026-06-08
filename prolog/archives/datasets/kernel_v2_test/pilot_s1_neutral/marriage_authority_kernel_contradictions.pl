% Axiom contradictions for kernel: marriage_authority_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% muslim_shariat_reading↔secular_contractual_reading: Shariat reading holds divine law as non-negotiable and superior to state law; secular reading holds individual consent as sole legitimate basis for marriage authority. No single framework can hold both axioms — accepting divine law supremacy requires rejecting consent-based legitimacy, and vice versa.
% hindu_codified_reading↔muslim_shariat_reading: Hindu reading accepts parliamentary sovereignty to reform religious law via statute (1955 Act overrode polygamy, reformed divorce); Muslim reading rejects state authority to codify or reform Shariat. No framework holds both — accepting legislative reform power requires rejecting divine law immunity from state revision.
% parsi_community_reading↔secular_contractual_reading: Parsi reading grounds authority in ethno-religious community membership (endogamy enforcement, loss of status for out-marriage); secular reading grounds authority in individual choice independent of community. No framework holds both — accepting community-membership-as-authority requires rejecting individual-choice-as-sole-basis.

narrative_ontology:cs_axiom_contradiction(divine_revelation_binding_authority, individual_consent_sufficient_for_binding).
narrative_ontology:cs_axiom_contradiction(individual_consent_sufficient_for_binding, divine_revelation_binding_authority).
narrative_ontology:cs_axiom_contradiction(dharmashastra_continuous_religious_tradition, divine_revelation_binding_authority).
narrative_ontology:cs_axiom_contradiction(divine_revelation_binding_authority, dharmashastra_continuous_religious_tradition).
narrative_ontology:cs_axiom_contradiction(community_authority_legitimacy_via_self_determination, individual_consent_sufficient_for_binding).
narrative_ontology:cs_axiom_contradiction(individual_consent_sufficient_for_binding, community_authority_legitimacy_via_self_determination).
