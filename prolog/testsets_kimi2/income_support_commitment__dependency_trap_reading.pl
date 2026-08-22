% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint is the dependency_trap_reading of the
 *   income_support_commitment kernel. It treats unconditional income
 *   transfers not as a benign autonomy floor but as a structurally extractive
 *   arrangement: a coalition of non-working recipients and state
 *   administrators extracts resources from working taxpayers while trapping
 *   the poor in skill atrophy and state dependency. The constraint family
 *   includes the freedom_floor_reading (dignity/autonomy enabler) and the
 *   targeting_efficiency_reading (means-tested concentration). Per
 *   Îµ-invariance and Rule 1, this file models only the dependency trap
 *   reading with a single stable Îµ and beneficiary/victim structure.
 *
 * KEY AGENTS:
 *   - non_working_recipients: Primary beneficiary (moderate/constrained) â receives transfers, defends the arrangement politically.
 *   - working_taxpayers: Primary target (organized/constrained) â funds transfers through coerced taxation.
 *   - skill_atrophy_population: Secondary target (powerless/trapped) â loses human capital through prolonged non-participation.
 *   - state_administrators: Agenda setter (institutional/arbitrage) â administers transfers, benefits from constituency stability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.62).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.55).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, 'f9a54d8a-f13c-40ef-ade3-46706c4418f1').
narrative_ontology:cs_kernel_codification('f9a54d8a-f13c-40ef-ade3-46706c4418f1', formalized).
narrative_ontology:cs_authority_grounding('f9a54d8a-f13c-40ef-ade3-46706c4418f1', extraction).
narrative_ontology:cs_interpretation_layer_present('f9a54d8a-f13c-40ef-ade3-46706c4418f1').
narrative_ontology:cs_reading_relation('f9a54d8a-f13c-40ef-ade3-46706c4418f1', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9a54d8a-f13c-40ef-ade3-46706c4418f1', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('f9a54d8a-f13c-40ef-ade3-46706c4418f1', foundational, unconditional_support_erodes_human_capital).
narrative_ontology:cs_axiom_status(unconditional_support_erodes_human_capital, holdable).
narrative_ontology:cs_axiom_grounding('f9a54d8a-f13c-40ef-ade3-46706c4418f1', unconditional_support_erodes_human_capital, empirically_contingent).
narrative_ontology:cs_reference_frame('f9a54d8a-f13c-40ef-ade3-46706c4418f1', temporary_social_insurance).
narrative_ontology:cs_drift_state('f9a54d8a-f13c-40ef-ade3-46706c4418f1', post_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9a54d8a-f13c-40ef-ade3-46706c4418f1', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, non_working_recipients).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, skill_atrophy_population).
narrative_ontology:constraint_vindicates(income_support_commitment__dependency_trap_reading, welfare_trap_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income transfers without employment requirements. Over time, detachment from the labor market becomes normalized, and the gap between their consumption and what market wages could replace widens, making political defense of the transfer a rational strategy.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, non_working_recipients, beneficiary,
    moderate, biographical, constrained, national).

% Fund the transfer system through compulsory taxation. Face a net resource outflow with no direct benefit. Exit options are limited to emigration or grey-market labor, both carrying legal and financial penalties.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    organized, biographical, constrained, national).

% Poor individuals who enter the support system and lose employable skills during prolonged non-participation. Become structurally unable to command wages high enough to offset benefit withdrawal, locking them into continued receipt even if they prefer work.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, skill_atrophy_population, payer,
    powerless, biographical, trapped, national).

% Design benefit levels, tax schedules, and eligibility rules. Justify the system as poverty reduction while managing fiscal sustainability. Political careers and bureaucratic budgets depend on maintaining a stable beneficiary coalition.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, state_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, non_working_recipients).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees a subsistence floor without means-testing overhead, eliminating administrative complexity and immediate destitution risk for individuals outside the labor market.
% TRANSFER_FUNCTION: Moves financial resources from working taxpayers to non-working recipients through state taxation and disbursement; simultaneously transfers human capital from the poor to a state of dependency by removing labor-market participation incentives and allowing skills to decay.
% ABSENT_VOICES: Future selves of current recipients who would prefer skill retention and earned income; working poor just above the benefit threshold who face negative effective marginal tax rates; employers who would absorb low-skill labor if subsidies were wage-linked rather than unconditional.
% DISAPPEARANCE_RATIONALE: If unconditional transfers vanished, non-working recipients would face immediate subsistence pressure, labor supply at the bottom of the wage distribution would rise, working taxpayers would retain more income, and the political coalition sustaining the arrangement would collapse; the social compact around work and welfare would reorganize around market participation or conditional support.
% FOUNDING_PROBLEM: Mass industrial poverty and cyclical unemployment in which large populations could not earn subsistence wages, requiring a social safety net to prevent destitution and social unrest.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and pre-welfare social reformers attest the founding destitution problem. Contemporary labor economists and taxpayer advocacy groups outside the beneficiary coalition attest that general prosperity and labor-market institutions have substantially solved acute destitution, while social-policy expansion advocates contest this.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.62) because the financial transfer is real and substantial, but the deeper extraction is the human-capital destruction and tax burden shifted to workers. Suppression is moderate (0.55) because the constraint depends on tax enforcement and benefit-eligibility maintenance, not purely on voluntary participation. Theater ratio rises to 0.42 as poverty-reduction rhetoric increasingly masks the persistence of a politically self-sustaining transfer coalition. Accessibility collapse is 0.45 because alternatives (private charity, local mutual aid, wage labor) remain structurally available but are disadvantaged by the unconditional state alternative. Resistance is 0.50 because taxpayer backlash and labor-market concerns produce persistent political contestation.
 *
 * PERSPECTIVAL GAP:
 *   The non-working recipient seat experiences the constraint as subsistence security and political representation; the working taxpayer seat experiences it as expropriation; the skill-atrophy seat experiences it as a trap with no exit. The state administrator seat experiences it as a governance tool. The engine computes this divergence from the structural data â the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-working recipients are beneficiaries (constrained exit, low d) because the transfer subsidizes their non-participation. Working taxpayers are targets (constrained exit by tax coercion, high d). The skill-atrophy population are targets (trapped exit, high d) because the constraint destroys their market alternatives. State administrators are agenda-setters with arbitrage-grade exit options (low d) because they can alter policy or move to other institutional roles without bearing the constraint's costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was acute industrial destitution. Economic development and labor-market institutions have substantially resolved that problem, yet the arrangement persists because it now serves a concentrated beneficiary coalition and state administrators. This is mandatrophy: a coordination mechanism (temporary safety net) has atrophied into an extraction mechanism (permanent dependency trap). The T17 accumulation signal is present in the rising extractiveness series. The classification prevents mislabeling by requiring both beneficiaries and victims for tangled rope, capturing the dual coordination-extraction nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_elasticity,
    'Does unconditional income support substantially reduce labor supply and erode skills, or do recipients reallocate effort to unpaid caregiving and education without net human-capital loss?',
    'Randomized controlled trials and natural experiments comparing unconditional cash transfers to conditional or null treatments, measuring long-term earnings trajectories and skill assessments.',
    'If labor-supply effects are negligible, the dependency-trap reading loses its empirical foundation and reverts toward the freedom-floor reading; if effects are large, the reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity, empirical, 'Empirical ambiguity about work-disincentive magnitude and skill atrophy.').

omega_variable(
    founding_problem_obsolescence,
    'Has the original problem of mass destitution been solved by general prosperity, or does it persist in structurally unemployed populations that unconditional support is correctly addressing?',
    'Comparative historical analysis of poverty rates, living standards, and labor-market absorption capacity in welfare-state economies versus pre-welfare baselines.',
    'If the founding problem is dead, the arrangement is likely mandatrophied; if live, the dependency-trap reading overstates extraction and the freedom-floor or targeting-efficiency readings may better fit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding safety-net problem remains live or is obsolete.').

omega_variable(
    recipient_beneficiary_victim_ambiguity,
    'Are non-working recipients net beneficiaries of the transfer system, or are they also victims of a structure that destroys their human capital and political autonomy?',
    'Longitudinal studies of recipient well-being, self-reported agency, and earnings potential before and after exit from unconditional programs.',
    'If recipients are simultaneously victims, the gain-flow becomes ambiguous and the constraint may shift toward a piton or snare classification depending on who captures the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recipient_beneficiary_victim_ambiguity, conceptual, 'Conceptual ambiguity about whether recipients are beneficiaries, victims, or both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(income_support_dep_trap_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(income_support_dep_trap_tr_t8, income_support_commitment__dependency_trap_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(income_support_dep_trap_tr_t16, income_support_commitment__dependency_trap_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(income_support_dep_trap_tr_t24, income_support_commitment__dependency_trap_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(income_support_dep_trap_tr_t32, income_support_commitment__dependency_trap_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(income_support_dep_trap_tr_t40, income_support_commitment__dependency_trap_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(income_support_dep_trap_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(income_support_dep_trap_be_t8, income_support_commitment__dependency_trap_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(income_support_dep_trap_be_t16, income_support_commitment__dependency_trap_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(income_support_dep_trap_be_t24, income_support_commitment__dependency_trap_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(income_support_dep_trap_be_t32, income_support_commitment__dependency_trap_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(income_support_dep_trap_be_t40, income_support_commitment__dependency_trap_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(income_support_dep_trap_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(income_support_dep_trap_su_t8, income_support_commitment__dependency_trap_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(income_support_dep_trap_su_t16, income_support_commitment__dependency_trap_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(income_support_dep_trap_su_t24, income_support_commitment__dependency_trap_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(income_support_dep_trap_su_t32, income_support_commitment__dependency_trap_reading, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(income_support_dep_trap_su_t40, income_support_commitment__dependency_trap_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% The income_support_commitment kernel decomposes into three structurally distinct constraints: the dependency_trap_reading (extractive, contested, moderate Îµ), the freedom_floor_reading (coordination-focused, low Îµ), and the targeting_efficiency_reading (allocation-optimizing, low-to-moderate Îµ). Each has a distinct beneficiary/victim structure and classification. They share the same policy domain but instantiate different normative commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
