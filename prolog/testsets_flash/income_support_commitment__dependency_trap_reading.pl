% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story represents the 'dependency trap' reading of
 *   unconditional income support. In this view, while intended to provide a
 *   safety net, the system inadvertently creates disincentives to work,
 *   leading to skill atrophy and increased reliance on state provisions. It
 *   is seen as a Tangled Rope because it offers a coordination function
 *   (basic income floor) but simultaneously extracts from working taxpayers
 *   and, over time, from the human capital of its long-term recipients. The
 *   claimed type (Tangled Rope) reflects this reading's structural
 *   assessment, independent of the metrics, which describe the observed
 *   effects.
 *
 * KEY AGENTS:
 *   - income_support_recipients_exiting_labor: Primary beneficiary (powerless/identity_locked) — benefits from income, but bears cost of skill atrophy.
 *   - working_taxpayers: Primary payer (moderate/constrained) — funds the system, bears cost of perceived reduced productivity.
 *   - poor_individuals_with_atrophied_skills: Secondary victim (powerless/trapped) — bears long-term costs of dependency.
 *   - social_policy_makers: Agenda setter (institutional/constrained) — administers the system, balances competing interests.
 *   - economic_productivity_advocates: Excluded voice (organized/mobile) — would object to work disincentives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.65).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.45).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, 'a564ac4d-ebb3-4d0f-9b02-c534218e7b70').
narrative_ontology:cs_kernel_codification('a564ac4d-ebb3-4d0f-9b02-c534218e7b70', formalized).
narrative_ontology:cs_authority_grounding('a564ac4d-ebb3-4d0f-9b02-c534218e7b70', lineage).
narrative_ontology:cs_interpretation_layer_present('a564ac4d-ebb3-4d0f-9b02-c534218e7b70').
narrative_ontology:cs_reading_relation('a564ac4d-ebb3-4d0f-9b02-c534218e7b70', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('a564ac4d-ebb3-4d0f-9b02-c534218e7b70', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('a564ac4d-ebb3-4d0f-9b02-c534218e7b70', foundational, work_as_moral_imperative).
narrative_ontology:cs_axiom_status(work_as_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('a564ac4d-ebb3-4d0f-9b02-c534218e7b70', work_as_moral_imperative, deontological).
narrative_ontology:cs_axiom('a564ac4d-ebb3-4d0f-9b02-c534218e7b70', foundational, skill_atrophy_as_societal_cost).
narrative_ontology:cs_axiom_status(skill_atrophy_as_societal_cost, holdable).
narrative_ontology:cs_axiom_grounding('a564ac4d-ebb3-4d0f-9b02-c534218e7b70', skill_atrophy_as_societal_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('a564ac4d-ebb3-4d0f-9b02-c534218e7b70', productive_citizen_welfare_state).
narrative_ontology:cs_drift_state('a564ac4d-ebb3-4d0f-9b02-c534218e7b70', contemporary_ubi_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a564ac4d-ebb3-4d0f-9b02-c534218e7b70', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, income_support_recipients_exiting_labor).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, poor_individuals_with_atrophied_skills).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who receive unconditional income support and, as a result, reduce or cease their participation in the formal labor market. They benefit from the income floor but risk skill atrophy and increased reliance on state provision, making re-entry difficult.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, income_support_recipients_exiting_labor, beneficiary,
    powerless, biographical, identity_locked, national).

% Individuals who contribute to the tax base that funds unconditional income support. They bear the financial burden of the program, perceiving it as subsidizing non-participation and reducing the overall productive capacity of the economy.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    moderate, biographical, constrained, national).

% A subset of income support recipients who, over time, experience a degradation of their marketable skills due to prolonged absence from the labor force. This makes them more dependent on the state and less able to achieve self-sufficiency, effectively trapping them in a cycle of dependence.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, poor_individuals_with_atrophied_skills, payer,
    powerless, generational, trapped, local).

% Government bodies and legislators responsible for designing, implementing, and funding income support programs. They balance social welfare goals with economic productivity concerns, often facing political pressure from both beneficiaries and taxpayers.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, social_policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Think tanks, business associations, and economists who prioritize labor force participation and economic growth. They argue against unconditional income support due to its perceived negative impact on work incentives and overall productivity, advocating for work-conditional welfare programs.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, economic_productivity_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a basic income floor to ensure social stability and prevent extreme poverty, aiming to coordinate a minimum standard of living for all citizens.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base (primarily working taxpayers) to individuals receiving unconditional income support.
% ABSENT_VOICES: Advocates for stricter work requirements or targeted welfare programs are often marginalized in discussions about universal basic income, as their concerns about work incentives and dependency are framed as lacking compassion or understanding of structural unemployment.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, a significant portion of the population would face immediate financial hardship, leading to widespread social unrest, increased poverty, and a collapse of the social safety net. The labor market would not immediately absorb these individuals, and the state would be forced to implement emergency measures.
% FOUNDING_PROBLEM: The problem of poverty, economic insecurity, and the administrative complexity of means-tested welfare programs, aiming to provide a simple, dignified safety net.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of unconditional income support attest that the founding problem of poverty and insecurity remains live, citing ongoing economic precarity. Critics, including economic productivity advocates and some policymakers, argue that while poverty persists, the specific problem of administrative complexity has been overemphasized, and the program's negative externalities (dependency, skill atrophy) have become the more pressing issue, shifting the original problem's status to 'contested' or 'dead' in its original framing.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because a significant portion of the tax base is transferred to non-working individuals, and the long-term cost of skill atrophy is borne by the recipients themselves and the broader economy. Suppression (0.45) is present in the form of reduced incentives to seek work and the structural barriers to re-entry for those with atrophied skills, rather than overt coercion. Theater ratio is low (0.1) as the system largely functions as intended by this reading, with minimal performative maintenance masking a different function. The measurements show a gradual increase in extractiveness and suppression over time, reflecting the deepening of the 'dependency trap' as individuals remain out of the labor force longer.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of working taxpayers and economic productivity advocates, the constraint is highly extractive, as they bear the costs of funding non-participation and the perceived loss of economic output. For the long-term recipients, it is a complex mix of benefit (income) and extraction (loss of skills, reduced autonomy). Social policy makers face the challenge of reconciling these divergent views.
 *
 * DIRECTIONALITY LOGIC:
 *   Income support recipients who exit the labor market are beneficiaries (d near 0.0) in terms of direct financial gain, but also targets (d near 1.0) in terms of long-term skill atrophy and reduced autonomy. Working taxpayers are clear targets (d near 1.0) as they fund the system. Poor individuals with atrophied skills are also targets, experiencing the most severe form of extraction through lost human capital. Social policy makers are agenda setters, balancing the system's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the constraint's mandate (alleviating poverty) has been partially subverted by its unintended consequences (dependency). The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the initial coordination function). The 'contested' status of the founding problem further highlights this mandatrophy, as the original problem's solution has created new problems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    work_disincentive_magnitude,
    'What is the actual magnitude of the work disincentive effect of unconditional income support, and how does it vary by demographic group and economic conditions?',
    'Large-scale randomized controlled trials (RCTs) of unconditional basic income programs across diverse economic contexts, measuring labor market participation, hours worked, and skill development over several years.',
    'If the work disincentive is found to be negligible, this reading''s core premise would be weakened, shifting the constraint closer to a Rope. If the disincentive is substantial and widespread, it would strengthen the Tangled Rope classification and potentially push it towards a Snare for long-term recipients.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(work_disincentive_magnitude, empirical, 'Empirical uncertainty regarding the extent of work disincentives.').

omega_variable(
    skill_atrophy_causality,
    'Is the observed skill atrophy among long-term recipients primarily caused by the unconditional income support itself, or by other confounding factors such as pre-existing health conditions, lack of job opportunities, or structural unemployment?',
    'Longitudinal studies tracking skill development and labor market outcomes of income support recipients, controlling for various socio-economic and health variables, and comparing with control groups not receiving unconditional support.',
    'If skill atrophy is largely attributable to the income support, it reinforces the ''dependency trap'' aspect and the extractive nature for recipients. If other factors are dominant, the ''dependency trap'' argument is weakened, and the constraint''s negative effects on recipients might be re-attributed to external systemic issues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_atrophy_causality, empirical, 'Causal ambiguity regarding skill atrophy and state dependence.').

omega_variable(
    reading_framing_legitimacy,
    'Is the ''dependency trap'' framing a legitimate analysis of the income support system''s effects, or is it a politically motivated narrative designed to undermine social welfare programs?',
    'Analysis of the historical and political context of the ''dependency trap'' discourse, examining its proponents, funding sources, and rhetorical strategies, alongside a rigorous comparison with alternative framings (e.g., ''freedom floor'').',
    'If the framing is found to be primarily ideological and lacking strong empirical grounding, its legitimacy as a structural description of the constraint would be reduced, potentially shifting the classification towards a more benign type (e.g., Rope) or highlighting the political contestation over its nature. If it is robustly supported, it strengthens this reading''s validity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'Conceptual ambiguity regarding the political legitimacy of the ''dependency trap'' framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__dependency_trap_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__dependency_trap_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__dependency_trap_reading, theater_ratio, 40, 0.095).
narrative_ontology:measurement(inco_tr_t50, income_support_commitment__dependency_trap_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__dependency_trap_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__dependency_trap_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(inco_be_t50, income_support_commitment__dependency_trap_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__dependency_trap_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__dependency_trap_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(inco_su_t50, income_support_commitment__dependency_trap_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'income_support_commitment' kernel. It focuses on the potential for unconditional income support to create dependency and disincentivize work, contrasting with the 'freedom_floor_reading' (autonomy and dignity) and 'targeting_efficiency_reading' (focused allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
