% ============================================================================
% CONSTRAINT STORY: institutional_validation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_validation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_validation_reading
 *   human_readable: Institutional Validation as Knowledge Legitimacy Gate
 *   domain: epistemology/institutional
 *
 * SUMMARY:
 *   This constraint is the institutional validation reading of the
 *   knowledge_legitimacy_biomedicine kernel. It holds that knowledge claims
 *   become legitimate through validation by institutional mechanisms: peer
 *   review, publication in high-impact journals, academic credentials, and
 *   replication within recognized institutional structures. This reading
 *   treats institutional markers as necessary and sufficient conditions for
 *   epistemic legitimacy. The constraint coordinates genuine quality
 *   filtering while simultaneously extracting rents through barrier-to-entry
 *   effects and systematic exclusion of valid non-institutional knowledge.
 *   The claim/metric gap is deliberate: claimed as tangled_rope
 *   (acknowledging both coordination and extraction) while metrics show
 *   substantial and rising extraction over time as institutional gatekeeping
 *   has intensified.
 *
 * KEY AGENTS:
 *   - established_research_institutions: Primary agenda-setters (institutional/arbitrage) — control funding, infrastructure, and legitimacy criteria
 *   - high_impact_journal_publishers: Agenda-setters and beneficiaries (institutional/mobile) — operate peer review infrastructure and extract revenue
 *   - credentialed_academic_gatekeepers: Beneficiaries (powerful/mobile) — hold positions as reviewers and editors, benefit from system that validates their authority
 *   - independent_researchers: Payers (moderate/constrained) — produce knowledge outside institutions, face systematic legitimacy barriers
 *   - cross_domain_synthesizers: Payers (moderate/constrained) — penalized by disciplinary specialization requirements
 *   - early_career_investigators: Payers and eventual beneficiaries (moderate/identity_locked) — must navigate validation mechanisms to establish careers
 *   - non_institutional_knowledge_producers: Excluded (powerless/trapped) — systematically excluded regardless of knowledge validity
 *   - epistemology_researchers: Observers (analytical/analytical) — study the structure and see both coordination and extraction functions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_validation_reading, 0.68).
domain_priors:suppression_score(institutional_validation_reading, 0.72).
domain_priors:theater_ratio(institutional_validation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_validation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(institutional_validation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(institutional_validation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(institutional_validation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(institutional_validation_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_validation_reading, tangled_rope).
narrative_ontology:human_readable(institutional_validation_reading, "Institutional Validation as Knowledge Legitimacy Gate").
narrative_ontology:topic_domain(institutional_validation_reading, "epistemology/institutional").

domain_priors:requires_active_enforcement(institutional_validation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(institutional_validation_reading, 'a30fcf17-77ad-4a89-9d2d-25d752704bfc').
narrative_ontology:cs_kernel_codification('a30fcf17-77ad-4a89-9d2d-25d752704bfc', distributed).
narrative_ontology:cs_authority_grounding('a30fcf17-77ad-4a89-9d2d-25d752704bfc', expertise).
narrative_ontology:cs_interpretation_layer_present('a30fcf17-77ad-4a89-9d2d-25d752704bfc').
narrative_ontology:cs_reading_relation('a30fcf17-77ad-4a89-9d2d-25d752704bfc', knowledge_legitimacy_biomedicine__synthesis_hypothesis_reading, coexists_with).
narrative_ontology:cs_reading_relation('a30fcf17-77ad-4a89-9d2d-25d752704bfc', knowledge_legitimacy_biomedicine__pragmatic_action_reading, coexists_with).
narrative_ontology:cs_axiom('a30fcf17-77ad-4a89-9d2d-25d752704bfc', foundational, institutional_validation_necessity).
narrative_ontology:cs_axiom_status(institutional_validation_necessity, holdable).
narrative_ontology:cs_axiom_grounding('a30fcf17-77ad-4a89-9d2d-25d752704bfc', institutional_validation_necessity, conventional).
narrative_ontology:cs_axiom('a30fcf17-77ad-4a89-9d2d-25d752704bfc', foundational, peer_review_sufficiency).
narrative_ontology:cs_axiom_status(peer_review_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('a30fcf17-77ad-4a89-9d2d-25d752704bfc', peer_review_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('a30fcf17-77ad-4a89-9d2d-25d752704bfc', institutional_peer_review_primacy).
narrative_ontology:cs_drift_state('a30fcf17-77ad-4a89-9d2d-25d752704bfc', contemporary_replication_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a30fcf17-77ad-4a89-9d2d-25d752704bfc', '').
narrative_ontology:cs_kernel_id(institutional_validation_reading, knowledge_legitimacy_biomedicine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_validation_reading, established_research_institutions).
narrative_ontology:constraint_beneficiary(institutional_validation_reading, high_impact_journal_publishers).
narrative_ontology:constraint_beneficiary(institutional_validation_reading, credentialed_academic_gatekeepers).
narrative_ontology:constraint_victim(institutional_validation_reading, independent_researchers).
narrative_ontology:constraint_victim(institutional_validation_reading, cross_domain_synthesizers).
narrative_ontology:constraint_victim(institutional_validation_reading, early_career_investigators).
narrative_ontology:constraint_victim(institutional_validation_reading, non_institutional_knowledge_producers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(institutional_validation_reading, early_career_investigators).
narrative_ontology:constraint_beneficiary(institutional_validation_reading, funding_bodies).
narrative_ontology:constraint_vindicates(institutional_validation_reading, peer_review_as_quality_guarantee).
narrative_ontology:constraint_vindicates(institutional_validation_reading, institutional_affiliation_as_competence_signal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control access to research funding, laboratory infrastructure, and institutional credibility. Set standards for what counts as legitimate knowledge production through hiring committees, grant review panels, and editorial boards. Benefit from concentration of resources and prestige within existing institutional structures.
narrative_ontology:constraint_stakeholder(institutional_validation_reading, established_research_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate the peer review infrastructure and control publication in venues that confer legitimacy. Extract subscription revenue and article processing charges while claiming to provide quality filtering. Their gatekeeping function is what makes institutional validation necessary for knowledge claims to be taken seriously.
narrative_ontology:constraint_stakeholder(institutional_validation_reading, high_impact_journal_publishers, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(institutional_validation_reading, high_impact_journal_publishers, beneficiary).

% Hold positions as peer reviewers, grant reviewers, and editorial board members. Their credentials grant them authority to adjudicate knowledge claims. Benefit from the system that validates their own past work and maintains the value of their institutional positions.
narrative_ontology:constraint_stakeholder(institutional_validation_reading, credentialed_academic_gatekeepers, beneficiary,
    powerful, biographical, mobile, global).

% Produce knowledge outside institutional structures but face systematic barriers to legitimacy. Their work is dismissed regardless of quality if it lacks institutional affiliation, peer review in recognized venues, or proper credentialing. Must either gain institutional access or accept marginalization.
narrative_ontology:constraint_stakeholder(institutional_validation_reading, independent_researchers, payer,
    moderate, biographical, constrained, global).

% Attempt to integrate knowledge across disciplinary boundaries but are penalized by institutional structures that reward narrow specialization. Peer reviewers from single disciplines reject cross-domain work as insufficiently rigorous in their specific domain, creating a systematic barrier to synthesis.
narrative_ontology:constraint_stakeholder(institutional_validation_reading, cross_domain_synthesizers, payer,
    moderate, biographical, constrained, global).

% Must navigate institutional validation mechanisms to establish careers. Benefit from eventual legitimacy if successful, but bear high costs during the credentialing process. Identity-locked because leaving means abandoning years of specialized training and the only career path their education prepared them for.
narrative_ontology:constraint_stakeholder(institutional_validation_reading, early_career_investigators, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(institutional_validation_reading, early_career_investigators, beneficiary).

% Practitioners, citizen scientists, traditional knowledge holders, and others who produce valid knowledge outside academic structures. Their contributions are systematically excluded from legitimacy regardless of empirical validity because they lack the institutional markers the validation system requires.
narrative_ontology:constraint_stakeholder(institutional_validation_reading, non_institutional_knowledge_producers, excluded,
    powerless, biographical, trapped, global).

% Use institutional validation as a risk-reduction mechanism for grant allocation. Benefit from being able to defer epistemic judgment to peer review and institutional prestige rather than evaluating knowledge claims directly. The system reduces their decision-making burden while concentrating resources in established institutions.
narrative_ontology:constraint_stakeholder(institutional_validation_reading, funding_bodies, beneficiary,
    institutional, generational, mobile, national).

% Study the structure of knowledge validation systems and document how institutional mechanisms shape what counts as legitimate knowledge. Can see both the coordination function (quality filtering, fraud prevention) and the extraction function (barrier to entry, rent collection, exclusion of valid non-institutional knowledge).
narrative_ontology:constraint_stakeholder(institutional_validation_reading, epistemology_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides quality filtering, fraud prevention, and replication verification in knowledge production. Solves the collective problem of distinguishing reliable from unreliable claims when direct evaluation by every consumer is impossible.
% TRANSFER_FUNCTION: Moves epistemic authority and resource access from knowledge producers to institutional gatekeepers. Transfers career opportunities, funding, and legitimacy from those without institutional markers to those with proper credentials, affiliations, and publication records.
% ABSENT_VOICES: Independent researchers, cross-domain synthesizers, traditional knowledge holders, and practitioners whose knowledge is valid but lacks institutional validation. They would argue for epistemic pluralism and direct evaluation of claims rather than proxy markers, but are structurally excluded from the venues where legitimacy criteria are set.
% DISAPPEARANCE_RATIONALE: If institutional validation requirements vanished overnight, knowledge production would reorganize around direct evaluation of claims, alternative credentialing mechanisms would emerge, cross-domain synthesis would become viable, and the concentration of resources in established institutions would dissolve. The entire structure of academic careers and research funding would need reconstruction.
% FOUNDING_PROBLEM: Early scientific knowledge production lacked quality control mechanisms, leading to widespread fraud, irreproducible results, and difficulty distinguishing valid from invalid claims at scale.
% FOUNDING_PROBLEM_CORROBORATION: Established institutions attest the problem remains live, citing ongoing replication crises and fraud cases. Critics including metascience researchers, independent scholars, and cross-domain practitioners attest that institutional validation has become decoupled from actual quality control, pointing to systematic failures of peer review, reproducibility crises within institutionally validated work, and exclusion of valid knowledge from non-institutional sources. Historical analysis from science studies scholars documents the shift from quality filtering to credentialing gatekeeping.
narrative_ontology:disappearance_verdict(institutional_validation_reading, world_rearranges).
narrative_ontology:founding_problem_status(institutional_validation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(institutional_validation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-18',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(institutional_validation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_validation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_validation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(institutional_validation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because institutional validation requirements create barriers to entry that are decoupled from actual knowledge quality, concentrating resources and authority in established institutions. Suppression is high (0.72) because the constraint actively excludes valid knowledge from non-institutional sources and penalizes cross-domain synthesis. Theater ratio is moderate (0.42) because peer review and replication requirements perform real quality-filtering functions, but a growing share of institutional validation activity serves gatekeeping and credentialing rather than quality control. The measurement series shows rising extraction and suppression over the interval as institutional gatekeeping has intensified and become more decoupled from quality filtering. Accessibility collapse is moderate (0.58) because alternative validation mechanisms exist but lack legitimacy. Resistance is moderate (0.54) because excluded knowledge producers and critics of institutional gatekeeping mount ongoing challenges, but institutional power maintains the system.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (institutions, publishers, gatekeepers) should compute this as coordination they provide and maintain. The payer and excluded seats should compute it as enforced extraction that blocks valid knowledge production. The engine derives this divergence from the structural data — the authored claim does not adjudicate between these framings.
 *
 * DIRECTIONALITY LOGIC:
 *   Established institutions and journal publishers are structural beneficiaries (set the rules, collect resources and prestige, d near beneficiary end). Independent researchers, cross-domain synthesizers, and non-institutional knowledge producers are targets (bear exclusion costs, constrained or trapped exit, d near target end). Early career investigators are mixed (identity-locked, must pay costs to eventually benefit). Credentialed gatekeepers benefit from the system that validates their authority. Funding bodies benefit from reduced decision-making burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (quality control in knowledge production) remains live, but the institutional validation mechanism has accumulated substantial extraction as gatekeeping functions have intensified beyond quality filtering. The tangled_rope classification captures this: genuine coordination (peer review does catch some fraud and error) layered with asymmetric extraction (systematic exclusion of valid non-institutional knowledge, barrier-to-entry effects, concentration of resources). The rising theater_ratio indicates drift toward performative validation (checking institutional markers) rather than substantive quality evaluation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_filtering_vs_gatekeeping,
    'What fraction of institutional validation activity serves genuine quality filtering versus credentialing gatekeeping?',
    'Systematic analysis of peer review outcomes comparing rejection rates for methodologically sound work from non-institutional versus institutional sources, controlling for actual quality. Natural experiments from open review systems that separate quality evaluation from institutional markers.',
    'A high gatekeeping fraction would support reclassification toward snare (extraction masquerading as coordination). A high quality-filtering fraction would support the coordination claim and justify the barrier costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_filtering_vs_gatekeeping, empirical, 'Whether institutional validation primarily filters quality or primarily enforces credentialing barriers.').

omega_variable(
    institutional_markers_vs_knowledge_validity,
    'Do institutional validation markers (credentials, peer review, high-impact publication) reliably correlate with knowledge validity, or have they become decoupled?',
    'Replication studies comparing success rates for institutionally validated versus non-institutionally validated claims. Analysis of major scientific errors and frauds that passed institutional validation. Systematic review of excluded knowledge that was later validated.',
    'Strong decoupling would indicate the constraint persists through enforcement rather than functional necessity, supporting higher extraction classification. Strong correlation would support the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_markers_vs_knowledge_validity, empirical, 'Whether institutional markers track actual knowledge quality or have become proxy credentials.').

omega_variable(
    cross_domain_synthesis_exclusion,
    'Is the systematic exclusion of cross-domain synthesis a necessary cost of disciplinary rigor, or an extractive barrier that blocks valid integrative knowledge?',
    'Historical analysis of major scientific advances that required cross-domain synthesis. Comparison of knowledge production rates and breakthrough frequency in systems with versus without strong disciplinary boundaries. Natural experiments from interdisciplinary research institutes.',
    'If synthesis exclusion blocks valid knowledge production, the constraint''s extraction is higher than coordination function justifies. If it protects against invalid integration, the barrier is a necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_domain_synthesis_exclusion, conceptual, 'Whether disciplinary specialization requirements serve quality control or block valid synthesis.').

omega_variable(
    alternative_validation_viability,
    'Could alternative validation mechanisms (open review, replication markets, direct claim evaluation) provide equivalent quality filtering without the barrier-to-entry costs?',
    'Pilot programs testing alternative validation systems. Comparison of fraud detection rates, replication success, and knowledge production speed across validation mechanisms. Analysis of why alternative systems have not displaced institutional validation despite lower barriers.',
    'Viable alternatives would indicate institutional validation persists through enforcement rather than necessity, supporting higher extraction. Non-viable alternatives would justify the coordination costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_validation_viability, empirical, 'Whether institutional validation is structurally necessary or maintained through suppression of alternatives.').

omega_variable(
    reading_frame_under_determination,
    'Is this constraint best understood as institutional validation (this reading), synthesis hypothesis (sibling reading emphasizing cross-domain integration), or pragmatic action (sibling reading emphasizing practical effectiveness)?',
    'The three readings are alternative framings of the same kernel (knowledge legitimacy in biomedicine). This reading emphasizes institutional mechanisms; synthesis_hypothesis_reading emphasizes integrative explanatory power; pragmatic_action_reading emphasizes real-world outcomes. Each produces different beneficiary/victim structures and different extraction profiles.',
    'The choice of reading determines which aspects of knowledge legitimacy are treated as central versus peripheral, which validation mechanisms are seen as necessary versus extractive, and which knowledge producers are seen as legitimate versus excluded. Cross-reading comparison reveals how framing shapes classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_frame_under_determination, conceptual, 'Committer-frame ambiguity: which reading of the knowledge legitimacy kernel best captures the constraint''s structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_validation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, institutional_validation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(inst_tr_t8, institutional_validation_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(inst_tr_t16, institutional_validation_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(inst_tr_t24, institutional_validation_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(inst_tr_t32, institutional_validation_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(inst_tr_t40, institutional_validation_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, institutional_validation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(inst_be_t8, institutional_validation_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(inst_be_t16, institutional_validation_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(inst_be_t24, institutional_validation_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(inst_be_t32, institutional_validation_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(inst_be_t40, institutional_validation_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inst_su_t0, institutional_validation_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(inst_su_t8, institutional_validation_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(inst_su_t16, institutional_validation_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(inst_su_t24, institutional_validation_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(inst_su_t32, institutional_validation_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(inst_su_t40, institutional_validation_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_validation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(institutional_validation_reading, 0.12).
narrative_ontology:affects_constraint(institutional_validation_reading, synthesis_hypothesis_reading).
narrative_ontology:affects_constraint(institutional_validation_reading, pragmatic_action_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the knowledge_legitimacy_biomedicine kernel. The institutional_validation_reading (this constraint) treats institutional mechanisms as necessary and sufficient for legitimacy. The synthesis_hypothesis_reading treats cross-domain integrative power as the legitimacy criterion. The pragmatic_action_reading treats practical effectiveness as the legitimacy criterion. Each reading produces different ε values because they identify different beneficiaries, different victims, and different coordination/extraction boundaries. They are linked as a constraint family because they are alternative framings of the same underlying question about knowledge legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
