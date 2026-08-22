% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__wage_subsidy_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Unconditional Income Support as Employer Wage-Subsidy
 *   domain: political_economy/labor
 *
 * SUMMARY:
 *   This story instantiates the wage-subsidy reading of a contested kernel
 *   about unconditional income support. Under this reading, the transfer is
 *   not primarily read as a freedom-enabling floor or a dependency-inducing
 *   trap, but as a de facto subsidy to low-wage employers: because workers'
 *   subsistence is now partly guaranteed by the state, employers can post or
 *   hold wages lower than they otherwise would need to, capturing part of the
 *   transfer's fiscal value as reduced labor cost. The coordination function
 *   (poverty reduction, consumption smoothing) is real, but this reading
 *   holds that it operates alongside a structural extraction channel running
 *   through the labor market's wage-setting mechanism — hence tangled_rope
 *   rather than a clean rope. This is a genealogically distinct claim from
 *   the dependency_trap_reading (which locates the harm in work-incentive
 *   erosion) and the freedom_floor_reading (which reads the same transfer as
 *   decommodifying labor power); all three share the same underlying transfer
 *   but diverge in which downstream mechanism they hold dominant, and
 *   therefore each carries its own epsilon.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.68).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.52).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage-Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/labor").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, 'c83a4824-698e-432d-972f-b3ab6394eaa8').
narrative_ontology:cs_kernel_codification('c83a4824-698e-432d-972f-b3ab6394eaa8', distributed).
narrative_ontology:cs_authority_grounding('c83a4824-698e-432d-972f-b3ab6394eaa8', distributed).
narrative_ontology:cs_reading_relation('c83a4824-698e-432d-972f-b3ab6394eaa8', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('c83a4824-698e-432d-972f-b3ab6394eaa8', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('c83a4824-698e-432d-972f-b3ab6394eaa8', foundational, transfer_value_is_market_capturable).
narrative_ontology:cs_axiom_status(transfer_value_is_market_capturable, holdable).
narrative_ontology:cs_axiom_grounding('c83a4824-698e-432d-972f-b3ab6394eaa8', transfer_value_is_market_capturable, empirically_contingent).
narrative_ontology:cs_axiom('c83a4824-698e-432d-972f-b3ab6394eaa8', secondary, wage_setting_power_asymmetry_persists_under_floor).
narrative_ontology:cs_axiom_status(wage_setting_power_asymmetry_persists_under_floor, holdable).
narrative_ontology:cs_axiom_grounding('c83a4824-698e-432d-972f-b3ab6394eaa8', wage_setting_power_asymmetry_persists_under_floor, empirically_contingent).
narrative_ontology:cs_created_at('c83a4824-698e-432d-972f-b3ab6394eaa8', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, gig_platform_operators).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, general_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set wages for positions in retail, agriculture, hospitality, and warehousing. Once a public income floor exists, they can hold or lower posted wages because workers' subsistence no longer depends solely on the wage — the public transfer closes the gap. They did not design the transfer program but restructure compensation around its existence, capturing part of its value as reduced labor cost.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    organized, biographical, arbitrage, national).

% Classify workers as independent contractors and set per-task rates below what would be needed to cover subsistence absent public support. The unconditional transfer functions as an implicit floor that lets per-task pricing stay compressed while still attracting labor supply, since workers are not solely dependent on task income to survive.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, gig_platform_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Receive the unconditional transfer but see it substantially absorbed by employers holding wages flat or letting them fall in real terms; nominal income rises less than the transfer amount because employers adjust pay to the new floor. Exit to a different employer does not escape the dynamic, since the same wage-setting logic operates across the low-wage labor market — only exit from wage labor entirely would, and the transfer is not large enough to fund that exit.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, biographical, constrained, national).

% Fund the unconditional transfer through general taxation. Under this reading, a portion of what taxpayers fund as income support is redirected, through the labor market's price-setting mechanism, into employer margin rather than into net worker income — taxpayers are financing a de facto wage subsidy without that transfer showing up as a line item.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, general_taxpayers, payer,
    moderate, generational, trapped, national).

% Designs and administers the unconditional transfer, sets eligibility and payment levels, and could in principle pair the transfer with wage floors, sectoral bargaining requirements, or employer levies to prevent capture — but under current design does not, either from a genuine floor-not-ceiling philosophy or from employer-lobbying pressure on program design.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, income_support_administering_agency, agenda_setter,
    institutional, generational, analytical, national).

% Would argue that the transfer should be paired with binding wage floors or collective bargaining coverage to prevent employer capture, but are not systematically consulted in the design of unconditional transfer programs, which are typically designed as tax-and-transfer policy separate from labor market regulation.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_unions, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees a subsistence income floor independent of employment status, reducing extreme deprivation and smoothing consumption across employment gaps.
% TRANSFER_FUNCTION: Moves tax revenue from general taxpayers to individuals as unconditional payments; under this reading, a portion of that transfer is then re-captured from workers by employers, who adjust posted or effective wages downward in response to the floor — redirecting part of the fiscal transfer into reduced labor cost rather than net worker income.
% ABSENT_VOICES: Labor unions and worker advocacy organizations, who would push for the transfer to be paired with wage floors or bargaining coverage to block employer capture, are largely absent from the design conversation, which is dominated by fiscal and welfare-policy technocrats and, on the employer side, business lobbies opposing complementary labor standards.
% DISAPPEARANCE_RATIONALE: If the unconditional transfer vanished overnight, low-wage workers under this reading would lose the residual net income it still delivers even after capture, which is a real (if reduced) harm to them — but employers would lose the ability to hold wages down against a subsidized floor, and would face pressure to raise wages to retain labor supply. Whether the world 'rearranges' or stays the same depends on which effect dominates, which is exactly the contested empirical question this reading stakes a claim on.
% FOUNDING_PROBLEM: Poverty and precarity among people without stable employment, and the administrative failure of conditional welfare systems that create high effective marginal tax rates and exclude the informally employed.
% FOUNDING_PROBLEM_CORROBORATION: Program administrators and anti-poverty advocates attest the founding problem (deprivation, welfare-trap disincentives) remains live and the transfer addresses it directly. Labor economists studying incidence, and some low-wage worker organizing campaigns, attest from outside the administering agency and outside employer groups that a substantial share of the transfer's value is being captured through wage adjustment rather than reaching workers net — this is the corroboration for the wage-subsidy reading specifically, though it is disputed by program administrators themselves.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, contested).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__wage_subsidy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__wage_subsidy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.42 to 0.68) reflecting an assumed adjustment lag: employers do not immediately reprice labor when a transfer program launches, but wage-setting behavior converges toward capturing the subsidy's value as the program matures and becomes a stable, anticipated feature of the labor market rather than a one-off windfall. Suppression is moderate (0.52) and rises more slowly — the mechanism does not require coercive enforcement against workers so much as ordinary employer wage-setting discretion, but some suppression is present insofar as workers lack bargaining power or alternative-employer options to resist the adjustment. Theater ratio is modest (0.31): the poverty-reduction function is real and delivers measurable results, so this is not primarily a performative constraint, but a growing share of its public justification (as a work-enabling floor) obscures the capture dynamic this reading identifies.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage employers and gig platform operators are beneficiaries: they gain a cost-reduction opportunity they did not have to build or fund, with arbitrage-grade exit (able to restructure pay scales, relocate operations, or reclassify labor). Low-wage workers are victims: powerless, constrained exit (moving employers does not escape a market-wide wage-setting adjustment), bearing the gap between the transfer's face value and its captured value. General taxpayers are also payers under this reading — they fund a transfer whose stated purpose (worker income support) is partly redirected into employer margin, a fiscal fact distinct from any complaint about program cost per se.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents this reading from either over-crediting the program as pure coordination (which would ignore the wage-capture channel) or dismissing it as pure extraction (which would ignore the genuine subsistence-floor function it performs, especially for those outside employment entirely). Both the coordination function and the asymmetric extraction must be present simultaneously for this reading to hold; if wage capture were shown to be negligible, this reading would collapse toward the freedom_floor_reading rather than persisting as a mislabeled tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_pass_through_magnitude,
    'What fraction of an unconditional income transfer''s fiscal value is actually captured by employers through wage adjustment, versus retained by workers as net income gain?',
    'Empirical incidence studies comparing wage trajectories in labor markets with and without unconditional transfer rollout, controlling for labor demand shocks and minimum wage policy — natural experiments from staggered UBI/basic-income pilots are the strongest available evidence source.',
    'If pass-through is low, this reading''s extraction claim weakens substantially and the constraint drifts toward rope; if pass-through is high, the tangled_rope classification with substantial extraction is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_pass_through_magnitude, empirical, 'Empirical magnitude of employer wage-capture of the transfer''s value.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the wage-subsidy mechanism the dominant downstream effect of unconditional income support, or does it operate alongside (and get outweighed by) the decommodification effect the freedom_floor_reading identifies?',
    'Comparative study of worker bargaining behavior and quit rates post-transfer: rising quit rates and wage demands would support freedom_floor dominance; flat or falling real wages with stable employment would support wage_subsidy dominance.',
    'Determines which sibling reading the empirical record ultimately vindicates as the primary structural characterization of unconditional income support — the three readings are not mutually exclusive in principle but this story claims wage-subsidy as descriptively dominant for THIS constraint''s referent arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether wage-subsidy capture or labor decommodification is the dominant real-world mechanism.').

omega_variable(
    complementary_policy_counterfactual,
    'Would pairing the unconditional transfer with a binding wage floor or sectoral bargaining requirement eliminate the capture mechanism this reading identifies, without eliminating the transfer''s coordination benefits?',
    'Policy comparison across jurisdictions that pair basic income with strong wage floors versus those that do not.',
    'If complementary labor standards eliminate capture, this reading identifies a fixable design flaw rather than an intrinsic property of unconditional transfers, which would reclassify the constraint as closer to scaffold (transitional, fixable via co-design) than tangled_rope (requiring ongoing enforcement of the status quo).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementary_policy_counterfactual, preference, 'Whether capture is intrinsic to unconditional transfers or an artifact of missing complementary policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(inco_tr_t4, income_support_conditionality__wage_subsidy_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__wage_subsidy_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__wage_subsidy_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__wage_subsidy_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__wage_subsidy_reading, theater_ratio, 24, 0.31).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(inco_be_t4, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(inco_su_t4, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(inco_su_t8, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(inco_su_t12, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(inco_su_t16, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(inco_su_t24, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the income_support_conditionality kernel. freedom_floor_reading and dependency_trap_reading are separate constraint stories sharing the same underlying policy instrument (unconditional income transfer) but authoring distinct beneficiary/victim structures and distinct epsilon values, per the epsilon-invariance decomposition principle. This story (wage_subsidy_reading) is linked to both siblings via affects_constraints because empirical findings on wage pass-through would bear directly on which reading's claim the corpus should weight more heavily.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
