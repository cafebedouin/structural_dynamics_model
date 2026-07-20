% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency-Trap Snare
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency_trap_reading of the
 *   unconditional_income_support kernel. It treats unconditional cash
 *   transfers not as a poverty-alleviation mechanism but as a snare that
 *   extracts from the working poor (by displacing targeted benefits worth
 *   more than the cash amount) and from taxpayers (by financing universal
 *   payments to the non-needy). The structural claim is that universality
 *   functions as a laundering device: the same policy that appears as an
 *   autonomy-enabling floor in sibling readings operates here as upward
 *   redistribution maintained by suppressing the political viability of
 *   targeted aid.
 *
 * KEY AGENTS:
 *   - Middle/upper-class recipients (moderate/mobile) â net beneficiaries receiving transfers despite not needing them
 *   - UBI advocates (organized/mobile) â collect political capital from the universality principle
 *   - Working poor (powerless/trapped) â lose targeted programs worth more than the UBI amount
 *   - Taxpayers (moderate/constrained) â bear net fiscal cost without commensurate benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.78).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.65).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Dependency-Trap Snare").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, '628cfbf6-16cd-404b-bfce-0dd99f47d2ca').
narrative_ontology:cs_kernel_codification('628cfbf6-16cd-404b-bfce-0dd99f47d2ca', formalized).
narrative_ontology:cs_authority_grounding('628cfbf6-16cd-404b-bfce-0dd99f47d2ca', expertise).
narrative_ontology:cs_interpretation_layer_present('628cfbf6-16cd-404b-bfce-0dd99f47d2ca').
narrative_ontology:cs_reading_relation('628cfbf6-16cd-404b-bfce-0dd99f47d2ca', unconditional_income_support__freedom_floor_reading, influences).
narrative_ontology:cs_reading_relation('628cfbf6-16cd-404b-bfce-0dd99f47d2ca', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('628cfbf6-16cd-404b-bfce-0dd99f47d2ca', foundational, labor_supply_neutrality_unattainable).
narrative_ontology:cs_axiom_status(labor_supply_neutrality_unattainable, holdable).
narrative_ontology:cs_axiom_grounding('628cfbf6-16cd-404b-bfce-0dd99f47d2ca', labor_supply_neutrality_unattainable, empirically_contingent).
narrative_ontology:cs_axiom('628cfbf6-16cd-404b-bfce-0dd99f47d2ca', foundational, targeted_aid_superiority).
narrative_ontology:cs_axiom_status(targeted_aid_superiority, holdable).
narrative_ontology:cs_axiom_grounding('628cfbf6-16cd-404b-bfce-0dd99f47d2ca', targeted_aid_superiority, empirically_contingent).
narrative_ontology:cs_reference_frame('628cfbf6-16cd-404b-bfce-0dd99f47d2ca', productivist_social_policy).
narrative_ontology:cs_drift_state('628cfbf6-16cd-404b-bfce-0dd99f47d2ca', post_pilot_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('628cfbf6-16cd-404b-bfce-0dd99f47d2ca', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocates).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional cash transfers regardless of financial need. Because they are not dependent on the support for survival, the payment functions as pure disposable-income augmentation. They face no work requirements, means tests, or clawbacks, and would lose this windfall only if the policy were repealed.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients, beneficiary,
    moderate, biographical, mobile, national).

% Derive political capital, institutional funding, and movement prestige from advancing universalist policy frameworks. The universality principle is their organizing identity; means-testing would fragment their coalition and reduce their agenda-setting leverage in legislative bargaining.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocates, beneficiary,
    organized, generational, mobile, national).

% Receive the cash transfer but simultaneously lose access to targeted in-kind and means-tested benefitsâhousing vouchers, Medicaid, SNAPâthat previously delivered higher net value than the unconditional amount. Cannot opt out of the policy substitution without leaving the welfare system entirely, which is not viable at low income levels.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    powerless, immediate, trapped, national).

% Finance the unconditional transfer through broad-based taxation. They do not receive commensurate benefits if their income is above the threshold where the transfer is clawed back, and they cannot individually opt out of the tax obligation that funds the scheme.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates administrative overhead, eligibility determination, and stigma associated with means-tested welfare by replacing fragmented programs with a single uniform cash payment to all residents.
% TRANSFER_FUNCTION: Moves fungible cash from taxpayers to all residents regardless of need, while displacing targeted in-kind and means-tested benefits that previously delivered higher net value to low-income households; net extraction falls on working poor and taxpayers while windfall gains accrue to non-needy households and political capital accrues to universalist advocates.
% ABSENT_VOICES: Recipients of targeted in-kind services and their caseworkers, who would object that the cash transfer is worth less than the bundled benefits they lose; also low-wage employers in sectors dependent on labor-force attachment, who are rarely at the policy-design table.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared, middle-class recipients would lose a windfall, the working poor would attempt to re-enroll in targeted programs (where administrative capacity remains), taxpayers would see fiscal relief, and the political coalition organized around universality would lose its flagship policy; the social-policy landscape would revert toward means-testing.
% FOUNDING_PROBLEM: Administrative complexity, non-take-up, and stigma in targeted welfare programs; patchwork safety nets leaving gaps; bureaucratic poverty traps created by benefit cliffs.
% FOUNDING_PROBLEM_CORROBORATION: UBI advocates and some social-policy researchers attest the problem is live. Conservative welfare economists, targeted-program administrators, and AEI fiscal analyses from outside the benefiting parties attest the problem is solved or soluble without universal cash, and that the unconditional arrangement creates larger distortions than it resolves.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because empirical pilot evidence shows labor-supply reductions and because targeted-aid displacement creates net losses for the working poor. Suppression (0.65) reflects the political and fiscal crowding-out that makes returning to means-testing difficult once the universal program is entrenched. Theater ratio (0.45) captures the growing gap between anti-poverty rhetoric and the actual incidence of benefits: as evidence accumulates that the transfer is not well-targeted, the poverty-reduction justification becomes increasingly performative. Accessibility collapse (0.60) models the erosion of administrative capacity and political will for targeted programs. Resistance (0.55) is moderate because taxpayers and displaced benefit recipients push back, but they are diffuse relative to the concentrated universalist coalition.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (middle-class recipients, advocates) and payer seats (working poor, taxpayers) should compute to different constraint types. From the beneficiary side, the arrangement resembles a rope or scaffoldâa simple, universal benefit. From the payer side, it computes as a snare: the same structure extracts net value because the financing and displacement mechanisms are regressive. The engine derives this divergence from the structural asymmetry in exit options (mobile vs trapped) and the directionality derived from beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Middle_upper_class_recipients and ubi_advocates are declared beneficiaries, which drives their directionality toward the beneficiary pole (low d, damped or negative effective extraction). Working_poor and taxpayers are declared victims, driving directionality toward the target pole (high d, amplified effective extraction). The working poor are further toward the target end because their exit is trapped (cannot recover lost targeted aid), while taxpayers are constrained but not fully trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâadministrative complexity and stigma in targeted welfareâmay have been live historically. Under this reading, however, the arrangement does not solve that problem; it replaces it with a larger distortion. The mandate is not so much outlived as misaligned from inception: the policy persists because the coalition of middle-class recipients and universalist advocates benefits from its steady-state operation, not because the original administrative problem required this specific solution. Classification as snare prevents mislabeling this as a scaffold (transitional) or rope (mutually beneficial coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_elasticity_ambiguity,
    'Does unconditional income support reduce labor supply by more than 3% net, and does that reduction represent welfare loss or autonomous choice?',
    'Long-run randomized controlled trials with comprehensive welfare accounting, tracking hours, earnings, and subjective well-being.',
    'If employment effects are negligible, the extraction claim weakens toward a pure redistribution story; if large and concentrated among the working poor, snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_ambiguity, empirical, 'Empirical ambiguity around labor-supply elasticity and its welfare interpretation.').

omega_variable(
    kernel_polysemy,
    'Is the unconditional_income_support kernel a commitment system with distinct structurally stable readings, or a single policy mechanism with observer-dependent framing?',
    'Comparative structural analysis across readings; if sibling readings produce mutually exclusive beneficiary/victim sets with stable epsilon values, the kernel is genuinely polysemic.',
    'If polysemic, this reading''s snare classification is locally valid but does not foreclose sibling readings; if univocal, one reading is structurally mistaken.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_polysemy, conceptual, 'Whether the kernel is structurally polysemic or univocal.').

omega_variable(
    fiscal_replacement_vs_supplement,
    'Does unconditional income support fiscally crowd out targeted programs on a one-to-one basis, or does it supplement them?',
    'Budgetary analysis of jurisdictions implementing unconditional transfers versus counterfactual maintenance of means-tested aid.',
    'If strict fiscal replacement occurs, working poor are net victims and extraction is high; if supplement, the victim claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_replacement_vs_supplement, empirical, 'Whether the policy replaces or supplements targeted aid.').

omega_variable(
    upward_redistribution_incidence,
    'What share of unconditional transfer payments flows to households above the median income, and what share of financing falls on households below it?',
    'Microsimulation and distributional national accounts tracing gross receipts and tax incidence.',
    'A regressive net incidence would confirm the snare structure; a progressive net incidence would refute this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(upward_redistribution_incidence, empirical, 'Distributional incidence of benefits and financing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__dependency_trap_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__dependency_trap_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__dependency_trap_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__dependency_trap_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__dependency_trap_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__dependency_trap_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.73).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__dependency_trap_reading, base_extractiveness, 16, 0.76).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__dependency_trap_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(unco_su_t4, unconditional_income_support__dependency_trap_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(unco_su_t8, unconditional_income_support__dependency_trap_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(unco_su_t16, unconditional_income_support__dependency_trap_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__dependency_trap_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is the dependency_trap_reading of the unconditional_income_support kernel, decomposed per the epsilon-invariance principle because the kernel's colloquial label conflates structurally distinct claims: autonomy-enabling floor, dependency trap, and political paradox.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
