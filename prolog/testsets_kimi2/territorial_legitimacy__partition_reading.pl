% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via International Partition (UN 181 / 1948 Borders Reading)
 *   domain: political/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the partition reading of the
 *   territorial legitimacy kernel: legitimacy in the Israel-Palestine context
 *   derives from UN Resolution 181 (1947) and the subsequent international
 *   recognition of states within defined borders. Under this reading, both an
 *   Israeli and a Palestinian state are legitimate within recognized
 *   boundaries; settlements beyond the 1967 lines are illegitimate; and the
 *   two-state solution remains structurally possible. The constraint is a
 *   constructed international legal framework, not a natural law, that
 *   coordinates recognition while asymmetrically extracting from populations
 *   whose claims fall outside its partition logic. This is one reading of a
 *   contested kernel; sibling readings include indigenous continuity and
 *   security necessity.
 *
 * KEY AGENTS:
 *   - UN partition institutions (agenda_setter / institutional / analytical): Administer the framework through Resolution 181 and subsequent resolutions.
 *   - Israeli state (beneficiary + payer / powerful / constrained): Gains legitimacy within borders but is constrained beyond them.
 *   - Palestinian national movement (beneficiary + payer / moderate / constrained): Promised statehood but bears costs of non-implementation and occupation.
 *   - Palestinian refugees (payer / powerless / trapped): Displaced and denied return by the partition demarcation.
 *   - Israeli settler movement (payer / organized / constrained): Claims beyond Green Line delegitimized by the framework.
 *   - International Court and tribunals (observer / institutional / analytical): Interpret the gap between law and facts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.72).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via International Partition (UN 181 / 1948 Borders Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '9f5beb80-be26-48c1-b32d-f9db3e7320ce').
narrative_ontology:cs_kernel_codification('9f5beb80-be26-48c1-b32d-f9db3e7320ce', formalized).
narrative_ontology:cs_authority_grounding('9f5beb80-be26-48c1-b32d-f9db3e7320ce', lineage).
narrative_ontology:cs_interpretation_layer_present('9f5beb80-be26-48c1-b32d-f9db3e7320ce').
narrative_ontology:cs_reading_relation('9f5beb80-be26-48c1-b32d-f9db3e7320ce', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f5beb80-be26-48c1-b32d-f9db3e7320ce', territorial_legitimacy__security_necessity_reading, influences).
narrative_ontology:cs_axiom('9f5beb80-be26-48c1-b32d-f9db3e7320ce', foundational, statehood_derives_from_international_recognition).
narrative_ontology:cs_axiom_status(statehood_derives_from_international_recognition, holdable).
narrative_ontology:cs_axiom_grounding('9f5beb80-be26-48c1-b32d-f9db3e7320ce', statehood_derives_from_international_recognition, conventional).
narrative_ontology:cs_axiom('9f5beb80-be26-48c1-b32d-f9db3e7320ce', foundational, territorial_integrity_within_mandated_borders).
narrative_ontology:cs_axiom_status(territorial_integrity_within_mandated_borders, holdable).
narrative_ontology:cs_axiom_grounding('9f5beb80-be26-48c1-b32d-f9db3e7320ce', territorial_integrity_within_mandated_borders, conventional).
narrative_ontology:cs_reference_frame('9f5beb80-be26-48c1-b32d-f9db3e7320ce', un_charter_partition_framework).
narrative_ontology:cs_drift_state('9f5beb80-be26-48c1-b32d-f9db3e7320ce', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9f5beb80-be26-48c1-b32d-f9db3e7320ce', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, palestinian_national_movement).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_settler_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_state).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_national_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the international legal framework of partition and state recognition through the General Assembly, Security Council, and UN agencies. Sets the boundaries of legitimate statehood via Resolution 181 and subsequent resolutions. Does not directly bear costs or collect rents but maintains authority through the framework's persistence and interpretive function.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, un_partition_institutions, agenda_setter,
    institutional, civilizational, analytical, global).

% Derives international legitimacy and UN membership from the partition framework and recognition within 1948 and 1967 borders. Bears costs through constraints on territorial expansion beyond recognized boundaries; settlements beyond the Green Line are delegitimized under this reading. Exit from the framework means abandoning the legal basis of recognized statehood.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_state, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israeli_state, payer).

% Is the designated beneficiary of the partition framework's promise of statehood, with recognition growing in international institutions. Bears costs through decades of statelessness, occupation, and displacement as the framework remains unimplemented. Cannot exit without abandoning the territorial claim and refugee rights framework tied to partition.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_national_movement, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, palestinian_national_movement, payer).

% Bear the extraction of the partition framework most directly: displaced by the 1948 demarcation, excluded from the territory designated for the Israeli state, and denied return under the framework's logic of two separate states. Trapped in camps and host countries with no viable exit to their original localities and minimal representation in negotiations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Claims territorial and biblical rights beyond the 1967 lines that the partition reading delegitimizes as illegal occupation under international law. Faces international condemnation, legal prohibition, and potential evacuation under the framework. Constrained because abandoning the settlement project would mean surrendering ideological and material investments, though individual exit is physically possible.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_settler_movement, payer,
    organized, biographical, constrained, regional).

% Interprets the partition framework and its boundaries through advisory opinions and rulings, notably on settlements and occupation. Observes the gap between the framework's legal requirements and territorial facts on the ground. Can alter the framework's juridical operation but does not control the political or security agenda.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_court_and_tribunals, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a mutually recognized framework for statehood in a contested territory, providing a non-violent, legal mechanism for determining borders and international personality without requiring war of annihilation or unilateral conquest.
% TRANSFER_FUNCTION: Moves territorial legitimacy from historical, indigenous, and security-based claims to internationally recognized partition boundaries; transfers sovereignty and population movements across demarcated lines; channels diplomatic recognition through UN membership.
% ABSENT_VOICES: Indigenous continuity advocates who reject the legitimacy of partition entirely, and security necessity advocates who reject fixed borders in favor of strategic depth, are structurally marginalized in UN framework negotiations and General Assembly debates.
% DISAPPEARANCE_RATIONALE: Without the partition framework, the legal basis for Israeli statehood and Palestinian statehood claims dissolves into competing historical and force-based claims; the entire Middle Eastern diplomatic architecture, Oslo framework, and two-state solution would collapse, and territorial legitimacy would revert to alternative grounds.
% FOUNDING_PROBLEM: How to resolve competing nationalist claims to the same territory after the British Mandate without war of extermination, and how to establish legitimate statehood in a post-mandate territory with a mixed population.
% FOUNDING_PROBLEM_CORROBORATION: The UN Special Committee on Palestine (UNSCOP) documented the problem in 1947 from outside the beneficiary states. However, Palestinian indigenous continuity advocates dispute that partition was the correct or legitimate solution, and Zionist security advocates dispute that the borders were adequate or sustainable. Corroboration exists but is contested by excluded parties.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the framework channels legitimacy through partition while permanently displacing refugees and delegitimizing settler claims, without delivering the promised Palestinian state. Suppression is high (0.72) because alternatives to partition (indigenous continuity, security expansion, binationalism) are structurally suppressed in international legal forums even as they persist on the ground. Theater ratio is substantial (0.65): the two-state solution is increasingly performative as diplomatic language while territorial facts shift through settlement expansion and entrenched occupation, yet genuine coordination persists through continued state recognition and diplomatic relations. Resistance is high (0.75) because both stateless populations and expansionist movements actively contest the framework. The metric series share one time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (UN institutions) computes the constraint as coordination with manageable extraction costs necessary for international order. The beneficiary seats (Israeli state, Palestinian movement) experience a mix of subsidy and constraint. The payer seats (refugees, settlers) experience the constraint as enforced exclusion from territory and legitimacy. The engine computes this divergence from the same structural data; the reading does not adjudicate a single true type.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and Palestinian national movement are structural beneficiaries of the partition framework (low d, subsidized by recognition and statehood claims), though both also bear costs through border constraints and non-implementation. Palestinian refugees and the settler movement are structural targets (high d): refugees are excluded from territory by the partition logic, and settlers are excluded from legitimacy beyond the lines. The UN institutions sit near analytical with minimal extraction. The directionality derivation from beneficiary/victim declarations captures the asymmetry without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition framework was built to solve the problem of competing nationalist claims in a post-mandate territory. The founding problem is contested: UNSCOP attested it in 1947, but excluded parties dispute both the diagnosis and the remedy. The framework has not atrophied into pure theater because state recognition and diplomatic relations still genuinely coordinate international behavior; however, the growing gap between legal borders and facts on the ground (practice drift) creates mandatrophy pressure. The classification as tangled rope captures both the persistent coordination function and the asymmetric extraction, preventing mislabeling as either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'Does the partition reading of territorial legitimacy foreclose indigenous continuity claims, or can both coexist within a single international legal framework?',
    'Analysis of UN resolutions and international court opinions that simultaneously reference partition boundaries and indigenous rights to determine whether the framework is structurally committed to rejecting indigenous continuity as a basis of legitimacy.',
    'If foreclosed, the partition reading extracts more heavily from indigenous-continuity holders by rendering their claims legally void; if coexistent, the extraction is diffused and the framework is more pluralistic than its partition logic suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Whether partition and indigenous continuity readings are mutually exclusive or coexistent within international law.').

omega_variable(
    practice_drift_vs_framework,
    'Has the partition framework''s practice drift (settlements, occupation, non-implementation) become so severe that the framework is now primarily performative?',
    'Comparative assessment of territorial control facts versus recognized borders over time, measuring the gap between the legal map and the demographic-military reality.',
    'If drift is severe, the constraint approaches piton status where maintenance is theatrical; if moderate, it remains a tangled rope with genuine but degraded coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_drift_vs_framework, empirical, 'Whether operational reality has decoupled from the partition framework to the point of performative maintenance.').

omega_variable(
    refugee_representation_gap,
    'Are Palestinian refugees structurally excluded from the partition framework''s beneficiary structure, or are they deferred beneficiaries of a not-yet-implemented state?',
    'Legal and political analysis of refugee representation in peace negotiations, UN framework institutions, and citizenship rights under the two-state model.',
    'If excluded, the framework''s extraction from refugees is direct and unambiguous; if deferred, the extraction is contingent on implementation failure and may be reversed by statehood realization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_representation_gap, conceptual, 'Whether refugees are victims of partition or prospective beneficiaries of its deferred implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tlp_tr_t0, territorial_legitimacy__partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tlp_tr_t15, territorial_legitimacy__partition_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(tlp_tr_t30, territorial_legitimacy__partition_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(tlp_tr_t45, territorial_legitimacy__partition_reading, theater_ratio, 45, 0.42).
narrative_ontology:measurement(tlp_tr_t60, territorial_legitimacy__partition_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(tlp_tr_t75, territorial_legitimacy__partition_reading, theater_ratio, 75, 0.65).

% Extraction over time
narrative_ontology:measurement(tlp_be_t0, territorial_legitimacy__partition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tlp_be_t15, territorial_legitimacy__partition_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(tlp_be_t30, territorial_legitimacy__partition_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(tlp_be_t45, territorial_legitimacy__partition_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(tlp_be_t60, territorial_legitimacy__partition_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(tlp_be_t75, territorial_legitimacy__partition_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tlp_su_t0, territorial_legitimacy__partition_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tlp_su_t15, territorial_legitimacy__partition_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(tlp_su_t30, territorial_legitimacy__partition_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(tlp_su_t45, territorial_legitimacy__partition_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(tlp_su_t60, territorial_legitimacy__partition_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(tlp_su_t75, territorial_legitimacy__partition_reading, suppression_requirement, 75, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one component of the territorial_legitimacy kernel family. The kernel decomposes into three structurally distinct readings (partition, indigenous continuity, security necessity) because the referent 'territorial legitimacy in Israel-Palestine' covers multiple incompatible normative bases with different epsilon values, beneficiary structures, and victim sets. Each reading carries its own epsilon and its own classification; they are linked as a constraint family rather than collapsed into one ambiguous story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
