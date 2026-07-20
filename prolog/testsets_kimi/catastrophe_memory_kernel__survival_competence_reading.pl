% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Ritual Encoding of Persecution-Survival Competence
 *   domain: religious studies/collective memory/ritual practice
 *
 * SUMMARY:
 *   This constraint story instantiates the survival_competence_reading of the
 *   catastrophe_memory_kernel. It treats communal ritual practice â
 *   particularly mourning and commemorative ritual â as a mechanism that
 *   encodes and transmits practical competence for surviving persecution. The
 *   ritual rehearses resource pooling, social cohesion, and threat-response
 *   patterns. The community benefits from intergenerational resilience, but
 *   members who would otherwise assimilate bear boundary-maintenance costs
 *   (restricted interaction, social sanctions for non-compliance, and forgone
 *   integration opportunities). Ritual specialists administer the encoding.
 *   The constraint is claimed as tangled_rope because it possesses both a
 *   genuine coordination function and asymmetric extraction.
 *
 * KEY AGENTS:
 *   - persecuted_community (moderate/identity_locked): primary beneficiary â receives encoded survival competence through ritual rehearsal; exit is identity-locked.
 *   - assimilation_prone_members (powerless/constrained): primary victim â bears boundary-maintenance costs and social sanctions; blocked from full integration.
 *   - ritual_specialists (organized/constrained): agenda setter â encodes, transmits, and enforces ritual patterns; derives authority from practice.
 *   - host_society_institutions (institutional/analytical): excluded â exert assimilation pressure but are outside the ritual's meaning-making.
 *   - memory_scholars (analytical/analytical): observer â analyze whether the ritual transmits competence or enforces boundaries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.55).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.45).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Ritual Encoding of Persecution-Survival Competence").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious studies/collective memory/ritual practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, '6326d964-f4fd-4f7d-a088-6760e80e6e59').
narrative_ontology:cs_kernel_codification('6326d964-f4fd-4f7d-a088-6760e80e6e59', distributed).
narrative_ontology:cs_authority_grounding('6326d964-f4fd-4f7d-a088-6760e80e6e59', practice).
narrative_ontology:cs_interpretation_layer_present('6326d964-f4fd-4f7d-a088-6760e80e6e59').
narrative_ontology:cs_reading_relation('6326d964-f4fd-4f7d-a088-6760e80e6e59', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_reading_relation('6326d964-f4fd-4f7d-a088-6760e80e6e59', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6326d964-f4fd-4f7d-a088-6760e80e6e59', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('6326d964-f4fd-4f7d-a088-6760e80e6e59', foundational, mourning_as_operational_rehearsal).
narrative_ontology:cs_axiom_status(mourning_as_operational_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('6326d964-f4fd-4f7d-a088-6760e80e6e59', mourning_as_operational_rehearsal, instrumental).
narrative_ontology:cs_axiom('6326d964-f4fd-4f7d-a088-6760e80e6e59', foundational, boundary_maintenance_as_survival_cost).
narrative_ontology:cs_axiom_status(boundary_maintenance_as_survival_cost, holdable).
narrative_ontology:cs_axiom_grounding('6326d964-f4fd-4f7d-a088-6760e80e6e59', boundary_maintenance_as_survival_cost, instrumental).
narrative_ontology:cs_reference_frame('6326d964-f4fd-4f7d-a088-6760e80e6e59', ritual_survival_competence).
narrative_ontology:cs_drift_state('6326d964-f4fd-4f7d-a088-6760e80e6e59', contemporary_secular_diaspora, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6326d964-f4fd-4f7d-a088-6760e80e6e59', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, persecuted_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilation_prone_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, functionalist_ritual_theory).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, collective_resilience_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A dispersed minority community under historical or ongoing persecution threat. Members participate in mourning and commemorative rituals that rehearse resource pooling, social cohesion, and threat-response patterns. The ritual is experienced as preserving collective survival competence across generations, but exit is identity-locked: leaving the ritual framework means leaving the community's protective social fabric and often the community itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, persecuted_community, beneficiary,
    moderate, generational, identity_locked, regional).

% Community members who face strong incentives to integrate with the host society for economic or social advancement. They bear the costs of boundary maintenance: ritual obligations restrict interaction, diet, and schedule; social sanctions for non-compliance or assimilation are real. Their exit is constrained by family pressure and the threat of ostracism, but they do not control the ritual's content.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilation_prone_members, payer,
    powerless, biographical, constrained, regional).

% Elders, clergy, or knowledge-keepers who design, lead, and judge correct ritual performance. They link mourning narratives to survival lessons, correct deviations, and enforce participation norms. Their authority derives from their position in the transmission chain, not from extracting material surplus, but they control the interpretive frame.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, ritual_specialists, agenda_setter,
    organized, generational, constrained, regional).

% State and civic institutions of the surrounding society that exert assimilation pressure through education, employment, and legal frameworks. They are excluded from the ritual's internal logic: they see the practices as antiquated or separatist and do not corroborate the survival-competence reading, but they are not in the room when the ritual's meaning is adjudicated.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, host_society_institutions, excluded,
    institutional, generational, analytical, national).

% Academic observers who study whether the ritual genuinely transmits survival competence or primarily enforces boundaries. They attest to functional outcomes in some historical contexts and to boundary-maintenance costs in others, but do not participate in the ritual's authority structure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, memory_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transmission of practical competence for surviving persecution: mourning and commemorative rituals rehearse resource pooling, social cohesion, rapid mobilization, and threat-response patterns.
% TRANSFER_FUNCTION: Moves encoded survival knowledge and behavioral scripts from elder generations and ritual specialists to the broader community, while moving boundary-maintenance costs onto members who would otherwise assimilate.
% ABSENT_VOICES: Secularized former community members who have assimilated and no longer credit the ritual with survival value; host society educators who see the ritual as separatist; younger members who would prefer non-ritual survival strategies.
% DISAPPEARANCE_RATIONALE: If the ritual constraint vanished, some argue the community would lose a critical rehearsal structure for persecution-response and coherence would fragment; others argue that assimilation pathways would open, economic and social integration would advance, and the community would reorganize around civic rather than ritual resilience. The parties dispute whether the constraint is load-bearing.
% FOUNDING_PROBLEM: How does a threatened, dispersed community preserve practical competence for surviving persecution across generations without centralized state protection or written archives?
% FOUNDING_PROBLEM_CORROBORATION: Community historians and ritual specialists attest the problem remains live, citing historical persecution cycles. Sociologists and assimilation advocates attest the problem has shifted: modern threats differ, and the ritual now preserves boundary distinctiveness more than operational survival competence. No fully independent corroboration exists outside the disputing parties; the evidence is interpreted through competing readings.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate: the ritual genuinely transmits coordination-relevant knowledge, but it also enforces boundary distinctiveness that extracts compliance from assimilation-prone members. Suppression (0.45) reflects social enforcement of participation and sanctions for exit, though not typically violent. Theater ratio (0.28) is low-moderate: most ritual activity is functional rehearsal, but some performative maintenance exists to signal group identity to outsiders. Accessibility collapse (0.60) is moderate-high: once a member is identity-locked, alternatives to the ritual framework are cognitively and socially closed off. Resistance (0.30) is moderate: younger and assimilation-prone members exhibit passive and occasionally active resistance.
 *
 * PERSPECTIVAL GAP:
 *   The persecuted_community seat experiences the constraint as protective coordination that preserves life and identity across generations. The assimilation_prone_members seat experiences the same rituals as enforced extraction that blocks economic and social advancement. The ritual_specialists seat experiences it as a sacred duty and functional necessity. The engine computes this divergence from the structural data: shared identity_locked exit for the community versus constrained exit for peripheral members, with directionality deriving from beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The persecuted_community is the declared beneficiary (low directionality): the constraint subsidizes their survival competence. Assimilation_prone_members are the declared victims (high directionality): the constraint extracts compliance and opportunity costs from them. Ritual_specialists sit near symmetric or moderate beneficiary: they accrue authority but also bear the maintenance burden of the ritual system. Host society institutions are excluded and receive no directional relationship. Memory_scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk here is mislabeling the constraint as pure coordination (rope) because the survival-competence function is genuine. The boundary-maintenance costs and the presence of identifiable victims (assimilation_prone_members) prevent that classification. Conversely, mislabeling it as pure extraction (snare) would ignore the historical evidence that ritual rehearsal has genuinely preserved coordination capacity during persecution events. The tangled_rope classification captures the hybrid structure: survival competence is transmitted, but the same mechanism enforces boundaries that victimize a subgroup.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_boundary_ambiguity,
    'Does the ritual''s survival-competence function predominate over its boundary-maintenance effect, or has the boundary effect become the primary function while survival competence serves as legitimation?',
    'Comparative ethnographic analysis of ritual communities with varying threat levels: if survival-competence correlates with threat exposure, functionalism is supported; if boundary rigidity persists independent of threat, boundary-maintenance predominates.',
    'If boundary-maintenance predominates, effective extraction is higher than the survival-competence framing suggests, shifting classification toward snare. If survival competence is genuine and primary, the constraint remains tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_boundary_ambiguity, empirical, 'Whether the ritual''s primary output is survival competence or boundary enforcement').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of exit structural (social sanctions, ostracism) or internalized (identity fusion making exit unthinkable)?',
    'Post-exit trajectory study: if suppression persists after physical separation from the community, the mechanism is partially internalized.',
    'Internalized suppression raises effective extraction beyond the structural measure; reclassification may shift toward snare if internalization is near-total.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in ritual compliance').

omega_variable(
    cost_bearer_concentration,
    'Are the boundary-maintenance costs borne by a distinct subgroup (assimilation-prone members) or diffusely by the entire community?',
    'Socioeconomic stratification analysis within the community: if costs concentrate on peripheral or younger members, extraction is asymmetric; if costs are evenly distributed, the constraint is closer to symmetric coordination.',
    'Asymmetric cost concentration supports the tangled_rope classification; diffuse costs would support rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_bearer_concentration, empirical, 'Whether boundary costs are asymmetric or community-wide').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(survival_competence_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(survival_competence_tr_t10, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(survival_competence_tr_t20, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(survival_competence_tr_t30, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(survival_competence_tr_t40, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(survival_competence_tr_t50, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(survival_competence_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(survival_competence_be_t10, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(survival_competence_be_t20, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(survival_competence_be_t30, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(survival_competence_be_t40, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(survival_competence_be_t50, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(survival_competence_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(survival_competence_su_t10, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(survival_competence_su_t20, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(survival_competence_su_t30, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(survival_competence_su_t40, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(survival_competence_su_t50, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel decomposes into four structurally distinct constraints (readings) because the natural-language label 'catastrophe ritual' conflates multiple claims with different epsilon values, beneficiary structures, and empirical status. The survival_competence_reading has moderate extractiveness tied to boundary-maintenance costs; the boundary_maintenance_reading likely has higher extractiveness and different victim structure; the symbol_continuity_reading likely has lower extraction; and the trauma_encoding_reading has a different coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
