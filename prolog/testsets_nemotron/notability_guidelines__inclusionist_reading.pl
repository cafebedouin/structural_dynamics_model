% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__inclusionist_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: WP:N as Structural Gatekeeping Apparatus Excluding Marginalized Knowledge (Inclusionist Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   The English Wikipedia's notability guideline (WP:N) requires topics to
 *   have received 'significant coverage' in 'reliable sources' independent of
 *   the subject. The inclusionist reading identifies this as a structural
 *   gatekeeping apparatus: the 'reliable sources' criterion systematically
 *   privileges knowledge produced by Western academic and media institutions
 *   while excluding oral traditions, indigenous knowledge systems, grassroots
 *   movements, and Global South epistemic communities whose knowledge
 *   circulates through community validation rather than institutional
 *   publication. The constraint extracts epistemic legitimacy from
 *   marginalized communities and concentrates it in established knowledge
 *   producers. The guideline's persistence depends on active enforcement
 *   through AfD (Articles for Deletion) processes and the citation supply
 *   chain that makes 'reliable sources' a self-reinforcing loop. The claimed
 *   type is snare — pure extraction with coordination story as cover.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.78).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.72).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "WP:N as Structural Gatekeeping Apparatus Excluding Marginalized Knowledge (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, '68ed2718-16d6-4928-ad74-dc43eb80b727').
narrative_ontology:cs_kernel_codification('68ed2718-16d6-4928-ad74-dc43eb80b727', formalized).
narrative_ontology:cs_authority_grounding('68ed2718-16d6-4928-ad74-dc43eb80b727', extraction).
narrative_ontology:cs_interpretation_layer_present('68ed2718-16d6-4928-ad74-dc43eb80b727').
narrative_ontology:cs_reading_relation('68ed2718-16d6-4928-ad74-dc43eb80b727', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('68ed2718-16d6-4928-ad74-dc43eb80b727', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('68ed2718-16d6-4928-ad74-dc43eb80b727', foundational, epistemic_pluralism_required).
narrative_ontology:cs_axiom_status(epistemic_pluralism_required, holdable).
narrative_ontology:cs_axiom_grounding('68ed2718-16d6-4928-ad74-dc43eb80b727', epistemic_pluralism_required, deontological).
narrative_ontology:cs_axiom('68ed2718-16d6-4928-ad74-dc43eb80b727', foundational, institutional_citation_is_not_neutral).
narrative_ontology:cs_axiom_status(institutional_citation_is_not_neutral, holdable).
narrative_ontology:cs_axiom_grounding('68ed2718-16d6-4928-ad74-dc43eb80b727', institutional_citation_is_not_neutral, empirically_contingent).
narrative_ontology:cs_reference_frame('68ed2718-16d6-4928-ad74-dc43eb80b727', inclusionist_epistemic_justice_frame).
narrative_ontology:cs_drift_state('68ed2718-16d6-4928-ad74-dc43eb80b727', contemporary_systemic_bias_recognition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('68ed2718-16d6-4928-ad74-dc43eb80b727', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, academic_citation_gatekeepers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, established_media_organizations).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_communities_oral_traditions).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, global_south_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, grassroots_activists).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, non_english_language_knowledge_holders).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, western_epistemic_superiority).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, reliable_sources_hegemony).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, verifiability_as_exclusion_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities, academic journals, and research institutions whose publications define the 'reliable sources' corpus. They control the citation supply chain and collect epistemic rent: knowledge must pass through their validation infrastructure to achieve Wikipedia notability. They can shift validation standards to favor their own outputs.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    institutional, generational, arbitrage, global).

% Journal editors, peer reviewers, and tenure committees who determine what counts as 'reliable' academic output. They set the standards that Wikipedia's 'reliable sources' criterion inherits. They benefit from Wikipedia's amplification of their validation decisions.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, academic_citation_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, academic_citation_gatekeepers, beneficiary).

% Major newspapers, broadcasters, and wire services whose reporting constitutes 'reliable sources' for contemporary topics. They benefit from Wikipedia's citation of their work as authority, driving traffic and institutional legitimacy. They can exit by withdrawing cooperation but have no incentive to.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, established_media_organizations, beneficiary,
    powerful, biographical, mobile, global).

% Communities whose knowledge is transmitted orally, through community practice, or via non-institutional channels. Their knowledge is systematically excluded because it lacks 'reliable sources' as defined. Exit means abandoning their epistemic practices — identity_locked because the knowledge is constitutive of community identity.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_communities_oral_traditions, payer,
    powerless, generational, identity_locked, global).

% Indigenous communities whose knowledge systems rely on oral transmission, ceremonial practice, and relational validation. The 'reliable sources' criterion treats these as 'unreliable' by definition. Their exclusion is not a bug but a structural feature of the criterion's epistemic assumptions.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, indigenous_knowledge_holders, payer,
    powerless, generational, identity_locked, global).

% Communities in the Global South whose knowledge production occurs outside Western academic and media institutions. They face language barriers, citation infrastructure gaps, and epistemic marginalization. Exit options are constrained — they can build alternative platforms but face discoverability and legitimacy deficits.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, global_south_communities, payer,
    moderate, biographical, constrained, global).

% Social movements and community organizers whose documentation of their own struggles lacks institutional citation. They are excluded from the encyclopedia of record unless mediated through mainstream media or academic study. Exit is constrained by the need for public visibility.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, grassroots_activists, payer,
    moderate, biographical, constrained, global).

% Communities producing knowledge in languages other than English. The 'reliable sources' criterion heavily weights English-language sources. Translation and citation infrastructure gaps create structural exclusion. Exit options exist but are costly.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, non_english_language_knowledge_holders, payer,
    moderate, biographical, constrained, global).

% Wikipedia editors who actively enforce WP:N through AfD nominations and citation demands. They administer the constraint's enforcement machinery. They benefit from status within the editor community but are not the primary extractive beneficiaries — they are the constraint's enforcement arm.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, deletionist_editors, agenda_setter,
    organized, biographical, mobile, global).

% Wikipedia editors who argue for broader notability standards and alternative validation mechanisms. They observe the constraint's operation from a reformist position but lack institutional power to change the criterion. They represent the internal resistance to the extraction.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, inclusionist_editors, observer,
    organized, biographical, mobile, global).

% The institutional steward of Wikipedia who could change policy but has consistently declined to reform WP:N's 'reliable sources' criterion despite documented exclusionary effects. They occupy a dual position: agenda-setters who could alter the constraint, but observers who treat the constraint as community consensus.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikimedia_foundation_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, wikimedia_foundation_leadership, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents vanity articles, hoaxes, and promotional content by requiring independent verification through sources with editorial oversight.
% TRANSFER_FUNCTION: Moves epistemic authority and visibility from marginalized communities (who lack institutional citation infrastructure) to institutional knowledge producers (who control the 'reliable sources' supply chain). The transfer is legitimacy — the right to appear in the encyclopedia of record.
% ABSENT_VOICES: Indigenous elders, oral tradition keepers, Global South community historians, and grassroots organizers who would argue their knowledge is validated through community practice not institutional citation. They are structurally excluded from AfD discussions because their validation methods are not recognized as 'reliable sources'.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, marginalized communities' knowledge would become includable through alternative validation (community attestation, oral history protocols, non-Western epistemic standards). Institutional knowledge producers would lose their gatekeeper monopoly. The encyclopedia's content would fundamentally reorganize around a plurality of epistemic standards.
% FOUNDING_PROBLEM: Early Wikipedia faced rampant vanity articles, promotional content, and hoaxes. The notability guideline was created to prevent the encyclopedia from becoming a directory of self-promotion.
% FOUNDING_PROBLEM_CORROBORATION: Deletionist editors and Wikimedia Foundation leadership attest the vanity/hoax problem remains live and WP:N is necessary. Inclusionist editors, marginalized community advocates, and independent researchers (e.g., studies on Wikipedia's systemic bias) attest the founding problem is substantially solved and the criterion now functions as extraction. No consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers epistemic authority from communities lacking institutional citation infrastructure to those who possess it, without reciprocity. Suppression (0.72) is substantial because the constraint's persistence depends on actively preventing alternative validation mechanisms (oral history, community attestation, non-Western epistemic standards) from gaining standing. Theater ratio (0.38) reflects that the 'quality filter' framing increasingly masks rent-seeking by institutional knowledge producers. Accessibility collapse (0.45) is moderate because alternative knowledge infrastructures exist but are systematically marginalized. Resistance (0.55) is present but fragmented across communities with different power levels.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional knowledge producer seat, WP:N appears as a coordination mechanism (rope) preventing epistemic chaos. From the marginalized community seat, the same structure operates as a snare extracting their epistemic labor while denying them standing. The engine computes this seat divergence from the structural data — the claimed type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers (universities, journals, established media) are structural beneficiaries: they control the citation supply chain that defines 'reliable sources' and collect epistemic rent from being the gatekeepers. Marginalized communities are structural victims: their knowledge is excluded by criterion design, not quality deficiency. They are identity_locked — their epistemic practices are constitutive of community identity, making exit from the exclusion structurally impossible without cultural loss. Academic citation gatekeepers and established media organizations occupy agenda_setter and beneficiary roles respectively. Deletionist editors operate as agenda_setters enforcing the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing vanity articles and hoaxes) is contested as live or dead. The mandate has been captured by institutional knowledge producers who benefit from the exclusionary criterion. Mandatrophy is unresolved — the constraint's original coordination function has been subordinated to its extraction function, but the coordination story remains the public justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_violence_vs_quality_control,
    'Is the exclusionary effect of ''reliable sources'' a necessary quality-control tradeoff or an irreducible epistemic violence against non-institutional knowledge systems?',
    'Comparative analysis of knowledge validation outcomes in communities that use community-attestation vs. institutional-citation models; longitudinal tracking of which excluded knowledge eventually gains institutional recognition.',
    'If epistemic violence, the constraint''s extractiveness is structural and irreducible — no reform of ''reliable sources'' can fix it. If quality-control tradeoff, targeted criterion adjustments could reduce extraction while preserving coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_violence_vs_quality_control, conceptual, 'Whether the exclusion mechanism is structurally necessary or contingently extractive.').

omega_variable(
    citation_supply_chain_monopoly,
    'Does the ''reliable sources'' criterion create a self-reinforcing monopoly for Western academic publishing, or does it reflect an independent epistemic standard?',
    'Network analysis of citation flows between Wikipedia, academic journals, and media outlets; counterfactual modeling of knowledge inclusion under alternative validation criteria.',
    'If monopoly, the constraint''s beneficiaries are not passive recipients but active architects of the exclusion. If independent standard, the extraction is a byproduct of a genuine coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(citation_supply_chain_monopoly, empirical, 'Whether the citation supply chain is endogenous to the constraint or exogenous.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (policy enforcement, citation requirements) or internalized (marginalized communities self-excluding because they believe their knowledge ''isn''t notable'')?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., communities that build alternative wikis but still self-censor), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal and communal contexts.').

omega_variable(
    kernel_reading_framing,
    'Does the inclusionist reading''s framing of WP:N as ''gatekeeping apparatus'' foreclose the deletionist reading''s ''quality filter'' framing within any single commitment framework, or do they coexist as competing interpretive positions?',
    'Analysis of whether any single editor or community can simultaneously hold both readings as operative without logical contradiction, or whether adopting one commits them to rejecting the other''s core premise.',
    'If forecloses, the readings cannot coexist in one framework — the kernel has a genuine logical fault line. If coexists_with, the dispute is political not logical. If influences, the inclusionist reading creates downstream pressure on deletionist legitimacy without resolving the dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Structural relationship between inclusionist and deletionist readings of the notability_guidelines kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 2001, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t2001, notability_guidelines__inclusionist_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(nota_tr_t2006, notability_guidelines__inclusionist_reading, theater_ratio, 2006, 0.26).
narrative_ontology:measurement(nota_tr_t2011, notability_guidelines__inclusionist_reading, theater_ratio, 2011, 0.3).
narrative_ontology:measurement(nota_tr_t2016, notability_guidelines__inclusionist_reading, theater_ratio, 2016, 0.34).
narrative_ontology:measurement(nota_tr_t2021, notability_guidelines__inclusionist_reading, theater_ratio, 2021, 0.37).
narrative_ontology:measurement(nota_tr_t2025, notability_guidelines__inclusionist_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(nota_be_t2001, notability_guidelines__inclusionist_reading, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(nota_be_t2006, notability_guidelines__inclusionist_reading, base_extractiveness, 2006, 0.58).
narrative_ontology:measurement(nota_be_t2011, notability_guidelines__inclusionist_reading, base_extractiveness, 2011, 0.64).
narrative_ontology:measurement(nota_be_t2016, notability_guidelines__inclusionist_reading, base_extractiveness, 2016, 0.71).
narrative_ontology:measurement(nota_be_t2021, notability_guidelines__inclusionist_reading, base_extractiveness, 2021, 0.75).
narrative_ontology:measurement(nota_be_t2025, notability_guidelines__inclusionist_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t2001, notability_guidelines__inclusionist_reading, suppression_requirement, 2001, 0.45).
narrative_ontology:measurement(nota_su_t2006, notability_guidelines__inclusionist_reading, suppression_requirement, 2006, 0.52).
narrative_ontology:measurement(nota_su_t2011, notability_guidelines__inclusionist_reading, suppression_requirement, 2011, 0.58).
narrative_ontology:measurement(nota_su_t2016, notability_guidelines__inclusionist_reading, suppression_requirement, 2016, 0.65).
narrative_ontology:measurement(nota_su_t2021, notability_guidelines__inclusionist_reading, suppression_requirement, 2021, 0.69).
narrative_ontology:measurement(nota_su_t2025, notability_guidelines__inclusionist_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(notability_guidelines__inclusionist_reading, 0.08).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, reliable_sources_guideline).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, verifiability_policy).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, wikipedia_admin_corps_governance).

% DUAL FORMULATION NOTE:
% Part of the notability_guidelines kernel family. This reading (inclusionist_reading) identifies the constraint as snare with institutional beneficiaries and marginalized victims. The deletionist_reading identifies it as rope with quality-control coordination. The deliberative_reading identifies it as scaffold with evolving boundaries. The three readings decompose the single natural-language concept 'WP:N' into structurally distinct constraints per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(notability_guidelines__inclusionist_reading, organized, 0.75).
constraint_indexing:directionality_override(notability_guidelines__inclusionist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
