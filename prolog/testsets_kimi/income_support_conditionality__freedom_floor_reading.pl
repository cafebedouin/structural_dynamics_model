% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   Unconditional income support guarantees a subsistence floor without work
 *   requirements or behavioral conditions. In the freedom_floor_reading, this
 *   constraint decommodifies labor power by ensuring that workers can refuse
 *   exploitative or coercive employment without facing destitution. Employers
 *   lose the structural power that comes from the threat of poverty, while
 *   low-wage workers gain a genuine exit option from the labor market. The
 *   constraint reclassifies the labor market from a coercive snare to a
 *   coordinative rope by providing the outside option that makes voluntary
 *   coordination possible. This is the freedom_floor_reading of the
 *   income_support_conditionality kernel.
 *
 * KEY AGENTS:
 *   - welfare_state: agenda_setter (institutional/constrained) â designs, taxes, and disburses the floor
 *   - low_wage_workers: primary beneficiary (powerless/mobile) â gain freedom to refuse coercive work
 *   - employers: primary victim (powerful/mobile) â lose coercive disciplinary power derived from destitution threat
 *   - taxpayers: payer (organized/constrained) â fund the transfer through general taxation
 *   - labor_economists: observer (analytical) â evaluate employment and autonomy effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.42).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.35).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '818329a5-6d1d-4e01-b537-1f25419e9989').
narrative_ontology:cs_kernel_codification('818329a5-6d1d-4e01-b537-1f25419e9989', formalized).
narrative_ontology:cs_authority_grounding('818329a5-6d1d-4e01-b537-1f25419e9989', lineage).
narrative_ontology:cs_interpretation_layer_present('818329a5-6d1d-4e01-b537-1f25419e9989').
narrative_ontology:cs_reading_relation('818329a5-6d1d-4e01-b537-1f25419e9989', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('818329a5-6d1d-4e01-b537-1f25419e9989', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('818329a5-6d1d-4e01-b537-1f25419e9989', foundational, labor_power_decommodification).
narrative_ontology:cs_axiom_status(labor_power_decommodification, holdable).
narrative_ontology:cs_axiom_grounding('818329a5-6d1d-4e01-b537-1f25419e9989', labor_power_decommodification, deontological).
narrative_ontology:cs_axiom('818329a5-6d1d-4e01-b537-1f25419e9989', foundational, positive_freedom_to_refuse).
narrative_ontology:cs_axiom_status(positive_freedom_to_refuse, holdable).
narrative_ontology:cs_axiom_grounding('818329a5-6d1d-4e01-b537-1f25419e9989', positive_freedom_to_refuse, deontological).
narrative_ontology:cs_reference_frame('818329a5-6d1d-4e01-b537-1f25419e9989', decommodified_labor_market).
narrative_ontology:cs_drift_state('818329a5-6d1d-4e01-b537-1f25419e9989', neoliberal_workfare_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('818329a5-6d1d-4e01-b537-1f25419e9989', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the unconditional income support scheme through taxation and disbursement. Sets the policy parameters for universality and maintains the fiscal infrastructure. Cannot easily abolish the scheme without major political rupture, though it can modify benefit levels and tax schedules.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, welfare_state, agenda_setter,
    institutional, generational, constrained, national).

% Receive an unconditional income floor regardless of employment status. This removes the threat of destitution that previously compelled acceptance of coercive or underpaid work, granting a material option to refuse exploitative labor contracts.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    powerless, biographical, mobile, national).

% Lose the structural power derived from workers' fear of zero income. Must offer wages and conditions sufficient to attract labor when workers possess an unconditional outside option. This erodes their coercive disciplinary leverage in wage bargaining.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers, payer,
    powerful, biographical, mobile, national).

% Fund the unconditional transfer through general taxation. They cannot individually opt out of the fiscal mechanism that sustains the floor, and they bear a net fiscal cost relative to a counterfactual without the scheme.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Evaluate the employment, wage, and welfare effects of the income floor. Provide empirical analysis on whether the constraint reduces coercion or creates disincentives, without directly receiving the transfer or paying its costs.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, labor_economists, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a society-wide guarantee of subsistence that removes the threat of destitution from employment bargaining, thereby enabling voluntary labor contracts that would otherwise be structurally coerced.
% TRANSFER_FUNCTION: Moves financial resources from general taxation to all eligible residents unconditionally, and reallocates bargaining power from employers to workers by providing a credible exit option from the labor market.
% ABSENT_VOICES: Employer associations advocating for workfare and conditionality; libertarian economists opposing redistribution entirely. They participate in broader political debate but are structurally excluded from the beneficiary framing of this reading.
% DISAPPEARANCE_RATIONALE: If the unconditional floor vanished overnight, low-wage workers would lose their outside option, the labor market would revert to a dynamic in which destitution enforces compliance, and wage bargaining would shift dramatically toward employers.
% FOUNDING_PROBLEM: Industrial and post-industrial labor markets historically compelled workers to accept any available employment under threat of starvation or destitution, preventing genuine consent to labor contracts.
% FOUNDING_PROBLEM_CORROBORATION: Labor historians and heterodox political economists attest to the historical coercion of labor markets under capitalism. Neoclassical economists and employer associations dispute that state-funded unconditional floors are the appropriate remedy, corroborating the contested status from outside the beneficiary set.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the taxation required to fund the floor and the redistribution of bargaining power away from employers; it is moderated by the genuine coordination function of providing a universal exit option. Suppression (0.35) captures tax enforcement and legal compulsion to fund the scheme, not interpersonal coercion. Theater ratio (0.22) is low because the floor operates as a direct transfer with minimal performative overlay, though pilot-program theater rises modestly over the interval. Accessibility collapse (0.30) acknowledges that private alternatives (charity, familial support, savings) still exist. Resistance (0.55) is moderate-to-high because employer lobbies and taxpayer associations actively oppose expansion. The metrics are authored independently of the rope claim; if the engine computes a tangled-rope or snare profile for certain seats, that divergence is the intended measurement.
 *
 * PERSPECTIVAL GAP:
 *   The employer seat experiences the constraint as an extraction of managerial authority and bargaining power; the low-wage worker seat experiences it as liberation from coercion. The engine should compute diametrically opposed directionalities for these two seats despite their operating within the same national policy framework. Taxpayers experience a third distinct seat: fiscal cost without direct labor-market benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers are declared beneficiaries (structural derivation pushes d toward the beneficiary end, damping or inverting effective extraction). Employers are declared victims (structural derivation pushes d toward the target end, amplifying effective extraction). Taxpayers are not declared in either base array; their directionality falls back to the canonical default for organized power. The state administrator is agenda_setter but not beneficiary; its directionality is structurally neutral to mildly beneficiary (legitimacy gain from social stability).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy by maintaining a clear, ongoing coordination function â the subsistence floor â that is not merely transitional. Its founding problem (structural coercion of labor through destitution threat) is treated as still live in this reading, so there is no resolved mandate persisting by inertia. It is not a piton because its functional role is active and substantive, not theatrical maintenance of an atrophied structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    income_support_conditionality_reading_contest,
    'This constraint is the freedom_floor_reading of the income_support_conditionality kernel; sibling readings (dependency_trap, wage_subsidy) assign opposite beneficiary/victim polarities. Does the true structure of the policy instrument favor one reading, or is the instrument itself underdetermined?',
    'Empirical measurement of labor market outcomes under unconditional income support: work hours, wage levels, employer profits, and worker subjective autonomy.',
    'If outcomes match the freedom floor reading (worker autonomy rises, wages rise, no employer subsidy), the rope classification is supported; if outcomes match the wage subsidy or dependency readings, this constraint may be a misclassified tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(income_support_conditionality_reading_contest, empirical, 'Kernel reading contest for income support conditionality.').

omega_variable(
    employer_victim_status,
    'Does the loss of employer coercive power constitute genuine extraction, or is it a correction of a pre-existing externality?',
    'Historical analysis of labor market power before and after the floor''s implementation; measurement of wage share versus profit share.',
    'If the power loss is a correction rather than extraction, the victim classification for employers is weakened and the rope classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_victim_status, empirical, 'Whether employer power loss is extraction or rebalancing.').

omega_variable(
    taxation_as_extraction,
    'Does the taxation funding the floor introduce a distinct extractive layer that undermines the rope classification?',
    'Disaggregate the constraint into its funding mechanism (taxation) and its disbursement mechanism (unconditional transfer); evaluate each as separate constraints.',
    'If taxation is independently extractive, the income support floor may decompose into a rope (disbursement) coupled with a snare or tangled rope (funding), altering the network topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taxation_as_extraction, conceptual, 'Whether the funding mechanism contaminates the coordination classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__freedom_floor_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__freedom_floor_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(inco_tr_t50, income_support_conditionality__freedom_floor_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__freedom_floor_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__freedom_floor_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(inco_be_t50, income_support_conditionality__freedom_floor_reading, base_extractiveness, 50, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_conditionality__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% The income_support_conditionality kernel decomposes into three structurally distinct constraints because the empirical and normative claims about conditionality, dependency, and employer subsidy yield different epsilon values and agent polarities. Each reading carries a separate constraint_id and is linked via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
