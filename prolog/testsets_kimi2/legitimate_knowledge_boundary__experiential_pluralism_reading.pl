% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__experiential_pluralism_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism Reading of the Legitimate Knowledge Boundary
 *   domain: epistemology/social_theory
 *
 * SUMMARY:
 *   This constraint story models the experiential pluralism reading of the
 *   legitimate_knowledge_boundary kernel: the arrangement by which knowledge
 *   claims gain legitimacy primarily through lived experience and community
 *   validation, with methodological standards treated as one tool among many
 *   rather than a necessary gate. The reading has been institutionalized in
 *   participatory research norms, policy consultation frameworks, and
 *   disciplinary peer-review reforms. It coordinates the inclusion of
 *   previously excluded epistemic communities while extracting authority from
 *   credentialed experts and methodological institutions. The claimed type is
 *   tangled_rope; the metrics are authored independently to describe the
 *   constraint's actual operation.
 *
 * KEY AGENTS:
 *   - epistemic_justice_advocates: agenda_setter (organized/global) â set boundary rules requiring community validation
 *   - marginalized_knowledge_communities: beneficiary (moderate/global) â gain epistemic standing through lived experience
 *   - credentialed_experts: payer (powerful/global) â face devaluation of methodological capital
 *   - methodological_institutions: payer (institutional/national) â partially displaced by community validation requirements
 *   - excluded_universalists: excluded (moderate/global) â methodological universalists barred from participatory spaces
 *   - interdisciplinary_observers: observer (analytical/global) â study knowledge quality outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.62).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.55).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism Reading of the Legitimate Knowledge Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/social_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__experiential_pluralism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '56694151-92fe-4971-b91d-8a3851d5fe29').
narrative_ontology:cs_kernel_codification('56694151-92fe-4971-b91d-8a3851d5fe29', distributed).
narrative_ontology:cs_authority_grounding('56694151-92fe-4971-b91d-8a3851d5fe29', distributed).
narrative_ontology:cs_reading_relation('56694151-92fe-4971-b91d-8a3851d5fe29', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('56694151-92fe-4971-b91d-8a3851d5fe29', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('56694151-92fe-4971-b91d-8a3851d5fe29', foundational, lived_experience_primacy).
narrative_ontology:cs_axiom_status(lived_experience_primacy, holdable).
narrative_ontology:cs_axiom_grounding('56694151-92fe-4971-b91d-8a3851d5fe29', lived_experience_primacy, deontological).
narrative_ontology:cs_axiom('56694151-92fe-4971-b91d-8a3851d5fe29', foundational, community_validation_authoritative).
narrative_ontology:cs_axiom_status(community_validation_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('56694151-92fe-4971-b91d-8a3851d5fe29', community_validation_authoritative, conventional).
narrative_ontology:cs_reference_frame('56694151-92fe-4971-b91d-8a3851d5fe29', distributed_community_validation).
narrative_ontology:cs_drift_state('56694151-92fe-4971-b91d-8a3851d5fe29', contemporary_academic_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56694151-92fe-4971-b91d-8a3851d5fe29', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_knowledge_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, methodological_institutions).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, standpoint_epistemology).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and enforce norms requiring community validation and lived experience as primary epistemic credentials; organize peer review reform, policy consultation mandates, and disciplinary boundary policing. They set the agenda for what counts as legitimate knowledge without necessarily capturing the extracted authority themselves.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, epistemic_justice_advocates, agenda_setter,
    organized, generational, mobile, global).

% Gain standing in policy and research deliberation through community-validated lived experience; their knowledge claims acquire legitimacy previously reserved for credentialed inquiry. Exit is identity-locked because their epistemic authority is tied to group membership and community endorsement.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_knowledge_communities, beneficiary,
    moderate, generational, identity_locked, global).

% Face devaluation of methodological training and peer-reviewed findings when these conflict with community-validated experiential claims; must recast work in experiential terms or accept reduced influence in participatory spaces. Their exit is constrained by sunk career investment and institutional affiliation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts, payer,
    powerful, biographical, constrained, global).

% Peer review bodies, credentialing boards, and funding agencies whose authority is partially displaced by community validation requirements; must incorporate participatory mechanisms or lose policy relevance. Exit is constrained because their legitimacy is tied to the credentialing function being displaced.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, methodological_institutions, payer,
    institutional, generational, constrained, national).

% Methodological universalists and positivist researchers who would argue for context-independent standards but are not admitted to participatory spaces where their framing is labeled epistemically violent or extractive.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, excluded_universalists, excluded,
    moderate, biographical, constrained, global).

% Study whether community validation regimes improve knowledge quality or simply shift authority; they are outside the beneficiary-payer structure and track outcomes across institutional contexts.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, interdisciplinary_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__experiential_pluralism_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__experiential_pluralism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Includes previously excluded knowledge holders in legitimate discourse; democratizes knowledge production by lowering formal credential barriers; prevents elite capture of truth-claims in policy and research.
% TRANSFER_FUNCTION: Moves epistemic authority, policy influence, and research funding access from credentialed experts and methodological institutions to community-validated lived experience; transfers discursive power from universalist methodology to situated, identity-bound knowledge claims.
% ABSENT_VOICES: Methodological universalists and positivist researchers who would argue for context-independent standards are structurally excluded from participatory and co-produced knowledge spaces; they are present in adjacent scientific institutions but not in the deliberative forums governed by this boundary.
% DISAPPEARANCE_RATIONALE: If this boundary vanished overnight, marginalized communities would lose recently gained epistemic standing in policy and research institutions; participatory deliberation would revert to credential-only input; peer review and funding allocation would reorganize around methodological universalism.
% FOUNDING_PROBLEM: Elite capture of knowledge production; historical exclusion of subaltern, indigenous, and marginalized voices from science and policy; over-reliance on detached methodological abstraction that silenced situated knowers.
% FOUNDING_PROBLEM_CORROBORATION: Marginalized communities and critical race/STS scholars attest to historical exclusion. Independent philosophers of science and methodologists outside the beneficiary set contest that the founding problem remains unaddressed, arguing the correction has overshot into methodological devaluation; no neutral corroborating consensus exists.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderately high because the constraint systematically reallocates epistemic authority from credentialed inquiry to community-validated experience, imposing costs on experts whose training and findings are subordinated. Suppression (0.55) reflects the active delegitimization of methodological universalism as epistemic violence or privilege within spaces operating under this reading. Theater ratio (0.55) is substantial because community validation has become partly performative â consultation rituals that do not shift actual decision power. Accessibility collapse (0.45) is moderate: pure methodological review remains in adjacent scientific institutions. Resistance (0.58) is significant and growing, driven by methodological disciplines. The measurement series run on a single shared time grid showing extraction and theater rising as the reading institutionalizes.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat experiences the constraint as liberating coordination â a genuine lowering of barriers and redistribution of epistemic authority. The payer seats experience the same structure as extractive â accumulated human and institutional capital is devalued, and they must adopt community-validation framings to retain influence. The agenda-setter seat experiences it as rightful correction. The engine computes this divergence from structural data; the authored claim does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (marginalized knowledge communities) with identity-locked exit sit near the full-beneficiary end, so the engine damps their effective extraction into net subsidy. Payers (credentialed experts, methodological institutions) with constrained exit sit near the full-target end, amplifying effective extraction. The agenda-setter with mobile exit sits low d. Excluded universalists are outside the derivation chain but their exclusion is part of the enforcement object.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â exclusion of marginalized voices â is contested as to whether it remains live. The temporal measurements show rising extractiveness and theater over the interval, consistent with accumulation on top of a coordination function. The Tangled Rope classification prevents misreading the arrangement as pure extraction (ignoring genuine inclusion) or pure coordination (ignoring methodological devaluation). If the founding problem is dead but the arrangement persists, the drift data would support a future piton or mandatrophy reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the legitimate knowledge boundary best modeled as a single constraint with observer-dependent classification, or as a family of structurally distinct constraints (readings) with independent epsilon values?',
    'Compare computed classifications across sibling readings; if they diverge into different types, the kernel is genuinely contested and requires decomposition.',
    'If siblings converge to the same type despite different content, the contest is superficial; if they diverge, the kernel is a site of genuine structural pluralism requiring separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the kernel hosts genuine structural pluralism').

omega_variable(
    community_validation_gatekeeping,
    'Does community validation operate as a genuine low-barrier inclusion mechanism, or as an identity-based gatekeeping system that excludes non-community members?',
    'Empirical comparison of knowledge production outcomes and participant demographics under community validation regimes versus credential-only regimes.',
    'If gatekeeping, extractiveness is higher than modeled; if inclusion, coordination function dominates and the constraint may compute closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_validation_gatekeeping, empirical, 'Inclusion versus identity-gatekeeping function').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of methodological universalism structural (institutional policies exclude universalists) or internalized (credentialed experts self-censor to avoid being labeled epistemically violent)?',
    'Track whether methodological critiques reappear when institutional enforcement weakens or when critics acquire protective coalition power.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after leaving specific institutional contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of methodological dissent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(legi_tr_t32, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement(legi_tr_t40, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legi_be_t8, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(legi_be_t16, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(legi_be_t32, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(legi_be_t40, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(legi_su_t16, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(legi_su_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(legi_su_t32, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(legi_su_t40, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legitimate_knowledge_boundary kernel. The kernel conflates three structurally distinct claims about what makes knowledge legitimate. Each reading has its own epsilon, stakeholders, and classification. This reading subordinates methodological credentialing to lived experience and community validation; its siblings treat credentialing as primary or as an equal co-production partner.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
