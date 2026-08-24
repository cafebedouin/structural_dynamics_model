% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_floor_reading of the
 *   unconditional_income_support kernel. It reads universal basic income as a
 *   coordination mechanism that solves the problem of labor market coercion
 *   by providing an unconditional exit option. The reading claims Pareto
 *   improvement: beneficiaries gain autonomy without creating victims (net
 *   fiscal incidence is progressive, labor supply effects are minimal per
 *   empirical evidence). The constraint operates as a rope — a genuine
 *   coordination function with minimal extraction — because it replaces the
 *   complex, coercive machinery of conditional welfare with a simple
 *   universal payment. The kernel is contested: sibling readings frame the
 *   same policy as incentive-distorting subsidy (dependency_trap_reading) or
 *   politically ambiguous Trojan horse (universality_paradox_reading).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.35).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.15).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '2a874bab-ae49-49ff-9116-78edc3ef4591').
narrative_ontology:cs_kernel_codification('2a874bab-ae49-49ff-9116-78edc3ef4591', distributed).
narrative_ontology:cs_authority_grounding('2a874bab-ae49-49ff-9116-78edc3ef4591', distributed).
narrative_ontology:cs_reading_relation('2a874bab-ae49-49ff-9116-78edc3ef4591', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a874bab-ae49-49ff-9116-78edc3ef4591', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('2a874bab-ae49-49ff-9116-78edc3ef4591', foundational, income_floor_enables_autonomy).
narrative_ontology:cs_axiom_status(income_floor_enables_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('2a874bab-ae49-49ff-9116-78edc3ef4591', income_floor_enables_autonomy, deontological).
narrative_ontology:cs_axiom('2a874bab-ae49-49ff-9116-78edc3ef4591', foundational, universalism_eliminates_stigma).
narrative_ontology:cs_axiom_status(universalism_eliminates_stigma, holdable).
narrative_ontology:cs_axiom_grounding('2a874bab-ae49-49ff-9116-78edc3ef4591', universalism_eliminates_stigma, instrumental).
narrative_ontology:cs_reference_frame('2a874bab-ae49-49ff-9116-78edc3ef4591', autonomy_enabling_social_floor).
narrative_ontology:cs_drift_state('2a874bab-ae49-49ff-9116-78edc3ef4591', post_covid_basic_income_trials, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('2a874bab-ae49-49ff-9116-78edc3ef4591', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_victims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, taxpayers).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, taxpayers).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, traditional_welfare_administrators).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, autonomy_as_freedom_from_coercion).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, universalism_eliminates_stigma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work in insecure, low-wage, or gig employment with little bargaining power. The income floor gives them genuine exit power from exploitative shifts, unstable schedules, and dangerous conditions — they can refuse work that harms them without facing destitution.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, constrained, national).

% Provide unpaid care for children, elders, or disabled relatives. Current welfare systems either ignore this work or penalize it through means-testing. The unconditional floor recognizes care as socially necessary labor and removes the poverty trap that forces caregivers into paid work before they're ready.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, national).

% Produce cultural value that markets under-reward. The income floor lets them sustain creative practice without commercial compromise or precarious day jobs that drain creative capacity. It functions as a public investment in cultural infrastructure.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists, beneficiary,
    moderate, biographical, constrained, national).

% Experience economic coercion that binds them to abusive relationships or households. The unconditional, individual, non-means-tested payment is often the only resource they can access without abuser interference — it is a literal exit door.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_victims, beneficiary,
    powerless, immediate, trapped, local).

% Fund the transfer through general taxation. They also receive the payment themselves (universality), so the net position varies by income. High earners are net payers; low earners are net beneficiaries. The universality means no stigma or administrative burden in claiming.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, taxpayers, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, taxpayers, beneficiary).

% Design and administer the universal payment infrastructure. Their role shifts from gatekeeping eligibility (means-testing, conditionality enforcement) to simple universal disbursement — a dramatic reduction in administrative coercion and discretion.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, policy_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate the existing conditional welfare bureaucracy. Their institutional mission, staffing, and budget are threatened by a system that replaces casework with universal payment. They experience the constraint as institutional displacement.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, traditional_welfare_administrators, payer,
    institutional, biographical, constrained, national).

% Argue that unconditional income destroys work incentives and creates dependency. They are excluded from the freedom_floor reading's beneficiary set because that reading treats their core premise (idleness induction) as empirically falsified by Alaska/Kenya/Finland data.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, dependency_trap_advocates, excluded,
    organized, biographical, mobile, national).

% Oppose universal transfers on cost grounds, favoring targeted aid. They are excluded because the freedom_floor reading treats universality as the stigma-eliminating mechanism — targeting reintroduces the coercion the floor removes.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, fiscal_hawks, excluded,
    powerful, biographical, mobile, national).

% Study labor supply effects, fiscal impacts, and wellbeing outcomes across pilots (Alaska PFD, Kenya UBI, Finland, Canada, Spain, US city pilots). They provide the empirical ground for the reading's moderate-extraction claim.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a universal income floor that enables voluntary participation in care, creative, civic, and entrepreneurial sectors by removing existential labor market coercion — the 'gun to the head' that forces people into exploitative arrangements.
% TRANSFER_FUNCTION: Moves resources from general taxation to all residents unconditionally. The transfer is universal (no means-test), individual (not household-based), and unconditional (no work requirement). Net flow is progressive: high earners pay more in tax than they receive; low earners receive more than they pay.
% ABSENT_VOICES: Fiscal conservatives who see universal transfers as wasteful leakage to the non-needy; targeted-welfare advocates who argue universality dilutes resources for the most vulnerable; employers who benefit from a coercive labor market that disciplines wage demands. These voices are structurally excluded from the freedom_floor reading because the reading's core premise is that their objections rest on empirical claims (work disincentive, fiscal unsustainability) that the reading treats as falsified.
% DISAPPEARANCE_RATIONALE: Without the floor, precarious workers lose exit power from exploitative jobs; caregivers lose recognition of unpaid work and face renewed poverty traps; artists lose creative autonomy to commercial pressures; abuse victims lose the only economic exit option that doesn't require abuser cooperation. The coercive structure of conditional welfare and desperate labor markets reasserts itself.
% FOUNDING_PROBLEM: The coercive nature of conditional welfare (means-testing, work requirements, stigma, administrative discretion) and the labor market desperation that forces people into exploitative, dangerous, or degrading work because the alternative is destitution.
% FOUNDING_PROBLEM_CORROBORATION: Alaska Permanent Fund Dividend studies (no labor supply reduction, poverty reduction); Kenya UBI trial (GiveDirectly, 12-year RCT showing increased enterprise, wellbeing, female empowerment); Finnish basic income experiment (wellbeing gains, no employment reduction); Ontario pilot cancellation (participants reported immediate return to coercion and stigma). All from independent researchers outside beneficiary groups.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because the transfer is real but net incidence is progressive and labor supply effects are empirically small. Suppression is low (0.15) because the constraint *removes* coercion (means-tests, work requirements, stigma) rather than imposing it. Theater ratio is low (0.20) because the mechanism is straightforward disbursement — little performative bureaucracy. Accessibility collapse is moderate (0.40) because alternatives (targeted welfare, labor market alone) persist but are structurally inferior for the named beneficiaries. Resistance is moderate (0.45) from institutional incumbents (welfare bureaucracies) and ideological opponents, not from the constraint's operation.
 *
 * PERSPECTIVAL GAP:
 *   From the freedom_floor seat, the constraint is pure coordination (rope) — it enables voluntary association by removing the gun to the head. From the dependency_trap seat, the same policy is extraction (snare) — it taxes workers to subsidize idleness. From the universality_paradox seat, it's a tangled_rope — the coordination story is real but the political coalition required to implement it smuggles in fiscal commitments that distort the design. The engine computes these per-seat divergences from the structural data; this commentary declares the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (precarious_workers, caregivers, artists, abuse_victims) are structural targets of labor market coercion *before* the constraint; the constraint flips their directionality toward beneficiary (d near 0.0). Taxpayers are dual-role: net payers at high income (d toward 0.5-0.6), net beneficiaries at low income (d toward 0.0). Policy_administrators are agenda_setters with arbitrage exit (they design the system). Traditional_welfare_administrators are payers (institutional displacement). Excluded seats (dependency_trap_advocates, fiscal_hawks) are excluded because the reading treats their core premises as empirically falsified — their exclusion is the reading's epistemic boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint has no mandatrophy risk in this reading because its founding problem (labor market coercion, welfare stigma) is live and the mechanism directly addresses it. The dependency_trap reading *claims* mandatrophy (the problem was poverty, UBI solves it inefficiently), but this reading treats that claim as empirically falsified. The universality_paradox reading claims a different mandatrophy: the political coalition that passes UBI will hollow it out (means-test it, phase it out) — that is a structural risk this reading acknowledges via the theater_ratio trajectory (slow rise as implementation nears).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the freedom_floor_reading a structurally distinct constraint from its sibling readings, or a framing choice over the same policy object?',
    'Test ε-invariance: do the sibling readings author materially different extractiveness, suppression, and beneficiary/victim structures for the *same* policy design? If yes, they are distinct constraints (per DP-001). If they only differ in evaluation of the same structural facts, they are observer-axis variants.',
    'If distinct constraints, each gets its own classification and the kernel is a family. If observer variants, the kernel is a single constraint with multiple readings — the engine''s per-seat computation already captures this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the kernel''s readings instantiate distinct constraints or observer perspectives on one constraint.').

omega_variable(
    labor_supply_effect_magnitude,
    'What is the true labor supply elasticity of an unconditional income floor at national scale?',
    'Longitudinal data from national-scale implementations (none yet exist). Current evidence: Alaska PFD (no reduction), Kenya 12-year RCT (increased enterprise), Finland 2-year trial (no reduction, wellbeing up), negative income tax experiments 1970s (small reductions, concentrated in secondary earners). Extrapolation to permanent national floor is the gap.',
    'If elasticity is near zero, extractiveness stays moderate (rope). If elasticity is high (significant work reduction), extractiveness rises and the constraint shifts toward tangled_rope (coordination + asymmetric extraction from workers to non-workers).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_supply_effect_magnitude, empirical, 'Whether labor supply effects remain minimal at national permanent scale.').

omega_variable(
    fiscal_sustainability_vs_autonomy,
    'Can a universal income floor at autonomy-enabling level be fiscally sustained without regressive financing or inflationary pressure?',
    'Macroeconomic modeling of funding paths (VAT, wealth tax, carbon dividend, sovereign money, deficit financing) at 15-25% GDP cost. Political economy of each path: who bears incidence, what secondary distortions emerge.',
    'If financing requires regressive taxation or generates inflation that erodes the floor''s real value, the constraint develops victims (low-income net payers) and shifts toward tangled_rope or snare. If progressive financing holds, the Pareto claim stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_sustainability_vs_autonomy, conceptual, 'Whether the autonomy floor''s financing structure preserves its Pareto character.').

omega_variable(
    suppression_mechanism_internalized,
    'For abuse_victims, is the suppression they face under the status quo structural (legal/financial barriers) or internalized (psychological entrapment), and does the income floor address both?',
    'Post-exit trajectory studies: do abuse victims who receive unconditional income leave abusive situations at higher rates, and does the effect persist? Compare to conditional welfare recipients.',
    'If suppression is largely internalized, the income floor''s low suppression metric (0.15) understates the constraint''s actual liberatory effect — the floor removes structural barriers but psychological entrapment persists. This would mean the constraint''s effective suppression for this seat is higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether the suppression removed by the income floor is structural or internalized for the most trapped beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__freedom_floor_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(unco_tr_t30, unconditional_income_support__freedom_floor_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(unco_tr_t40, unconditional_income_support__freedom_floor_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(unco_tr_t50, unconditional_income_support__freedom_floor_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__freedom_floor_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(unco_be_t30, unconditional_income_support__freedom_floor_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(unco_be_t40, unconditional_income_support__freedom_floor_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(unco_be_t50, unconditional_income_support__freedom_floor_reading, base_extractiveness, 50, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unconditional_income_support__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__freedom_floor_reading, 0.15).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint family (unconditional_income_support kernel) decomposes the single policy label 'UBI' into three structurally distinct readings with different ε, beneficiary/victim structures, and types. The freedom_floor_reading claims rope (coordination, Pareto); dependency_trap_reading claims snare (extraction, workers pay for idleness); universality_paradox_reading claims tangled_rope (coordination story masks political extraction). They share the kernel but instantiate different constraints per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, powerless, 0.05).
constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
