% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__formalist_employment_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary — Contract-and-Supervision Test Excludes Platform Workers
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the formalist reading of the employment boundary
 *   kernel: employment status is determined by formal contract terms plus a
 *   common-law test emphasizing direct, personal supervision, and platform
 *   workers who sign independent-contractor agreements and are directed by
 *   algorithms rather than named supervisors fall outside employment
 *   protections by construction. Under this reading, platform workers are not
 *   victims of employment precarity because they are understood to have
 *   chosen contractor status and its attendant flexibility; the story's
 *   beneficiary/victim structure reflects that the actual cost
 *   externalization runs to workers and state insurance systems, even though
 *   the reading's own doctrinal premise would deny that workers are owed
 *   employment protections in the first place. As the rising extractiveness
 *   series shows, the test has increasingly become the load-bearing mechanism
 *   by which a growing share of the labor market avoids employment costs,
 *   which is the empirical fact this reading's own framework has no doctrinal
 *   room to register as a problem.
 *
 * KEY AGENTS:
 *   - platform_operators: agenda_setter/beneficiary (institutional/arbitrage) — designs classification and algorithmic control, collects the cost-avoidance
 *   - platform_workers: payer (powerless/constrained) — bears the uninsured cost of income volatility and injury risk
 *   - state_unemployment_insurance_funds: payer (institutional/trapped) — absorbs fiscal externality with no contribution base
 *   - traditional_employers_in_competing_sectors: payer (powerful/constrained) — competitively disadvantaged by the classification asymmetry
 *   - labor_regulators_and_courts: observer (institutional/analytical) — administers the contested doctrinal test case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.79).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.68).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary — Contract-and-Supervision Test Excludes Platform Workers").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, '222923d5-b83d-4e6a-aaba-d83d8eaa93f3').
narrative_ontology:cs_kernel_codification('222923d5-b83d-4e6a-aaba-d83d8eaa93f3', distributed).
narrative_ontology:cs_authority_grounding('222923d5-b83d-4e6a-aaba-d83d8eaa93f3', distributed).
narrative_ontology:cs_reading_relation('222923d5-b83d-4e6a-aaba-d83d8eaa93f3', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('222923d5-b83d-4e6a-aaba-d83d8eaa93f3', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('222923d5-b83d-4e6a-aaba-d83d8eaa93f3', foundational, formal_control_test_determines_status).
narrative_ontology:cs_axiom_status(formal_control_test_determines_status, holdable).
narrative_ontology:cs_axiom_grounding('222923d5-b83d-4e6a-aaba-d83d8eaa93f3', formal_control_test_determines_status, conventional).
narrative_ontology:cs_axiom('222923d5-b83d-4e6a-aaba-d83d8eaa93f3', secondary, contractor_designation_reflects_genuine_choice).
narrative_ontology:cs_axiom_status(contractor_designation_reflects_genuine_choice, holdable).
narrative_ontology:cs_axiom_grounding('222923d5-b83d-4e6a-aaba-d83d8eaa93f3', contractor_designation_reflects_genuine_choice, empirically_contingent).
narrative_ontology:cs_reference_frame('222923d5-b83d-4e6a-aaba-d83d8eaa93f3', industrial_era_control_test).
narrative_ontology:cs_drift_state('222923d5-b83d-4e6a-aaba-d83d8eaa93f3', platform_economy_maturity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('222923d5-b83d-4e6a-aaba-d83d8eaa93f3', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, venture_capital_investors).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, consumers_of_platform_services).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, state_unemployment_insurance_funds).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, traditional_employers_in_competing_sectors).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, freedom_of_contract_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, worker_choice_of_flexibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the app-based dispatch and rating systems that direct worker behavior in granular detail, then structure the relationship through onboarding contracts that classify workers as independent contractors. This classification removes obligations to pay minimum wage, overtime, unemployment insurance contributions, workers' compensation premiums, and payroll tax matching. Litigates aggressively and lobbies for statutory carve-outs (ballot initiatives, model legislation) whenever the classification is challenged in court or by regulators.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__formalist_employment_reading, platform_operators, beneficiary).

% Perform the labor that generates platform revenue, subject to algorithmic direction over pricing, routing, deactivation, and acceptance-rate thresholds functionally indistinguishable from supervision, but carry no employment status. Bear the full cost of vehicle depreciation, fuel, health insurance, and periods without any unemployment safety net if deactivated. Exit theoretically exists (work for a competing platform, leave the sector) but multi-apping and switching costs are real, and the alternative is frequently another platform with the same classification, not a different labor-market structure.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, biographical, constrained, national).

% Absorb the fiscal externality when misclassified workers experience income loss without qualifying for benefits they never had payroll contributions made toward, and absorb further costs when courts or agencies occasionally reclassify workers after the fact and funds must retroactively administer claims. Cannot exit the arrangement — the fund is a passive recipient of whatever classification outcome the legal system settles on.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_unemployment_insurance_funds, payer,
    institutional, generational, trapped, national).

% Compete for labor and market share against platform operators who do not bear the payroll-tax, insurance, and benefits costs that traditional employers in taxi, courier, and retail sectors must bear under the same formal test. Some can reclassify their own workforces toward contractor status to compete, but doing so exposes them to the same litigation risk platforms already absorb through scale; smaller competitors without platform-scale legal budgets cannot make that move safely.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_employers_in_competing_sectors, payer,
    powerful, biographical, constrained, national).

% Receive lower per-transaction prices than a fully-costed employment model would produce, subsidized in part by the labor-cost externalization the classification permits. Face no direct cost from the classification question and have not organized around it either way; their exit options with respect to the underlying labor dispute are irrelevant since they are not a party to it, though they are a beneficiary of its outcome.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, consumers_of_platform_services, beneficiary,
    organized, immediate, mobile, national).

% Apply multi-factor common-law tests (control, investment, opportunity for profit/loss, integration into business) developed for an earlier industrial economy to platform work, producing inconsistent rulings across jurisdictions. Can reclassify the relationship case by case but each ruling is contested, appealed, or legislatively overridden by the operators who have the resources to shape the next round of statutory definition.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_regulators_and_courts, observer,
    institutional, generational, analytical, national).

% Fund platform operators on valuation models that assume labor is a variable cost rather than a fixed employment obligation; the contractor classification is priced directly into enterprise value. Can exit any single platform bet without consequence and reallocate capital to whichever jurisdiction preserves the classification most durably.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, venture_capital_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__formalist_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, administrable test — formal contract terms plus direct supervision — that lets courts, regulators, and businesses determine employment status without a fact-intensive inquiry into every working relationship. This reduces litigation costs and gives businesses predictability about their statutory obligations.
% TRANSFER_FUNCTION: Moves the costs of income volatility, lack of benefits, uninsured injury risk, and retirement absence from platform operators (who would bear them under an employment classification) onto individual workers and, secondarily, onto state insurance systems that must either deny claims or absorb them without corresponding payroll contributions.
% ABSENT_VOICES: Platform workers as a class are formally represented in litigation by named plaintiffs and worker organizations, but the vast diffuse majority who never join a suit or ballot campaign have no seat in the classification decisions made in legislatures and appellate courts; state insurance fund administrators are rarely parties to the classification disputes that determine their fiscal exposure even though they bear direct costs from the outcome.
% DISAPPEARANCE_RATIONALE: If the formalist contract-and-supervision test disappeared and were replaced by economic-dependence tests overnight, platform operators would face immediate reclassification exposure, benefits and payroll-tax obligations would attach retroactively in many jurisdictions, unit economics for on-demand labor platforms would shift substantially, and some platform business models would become unviable at current pricing. The rearrangement would be large and immediate — this is precisely why platform operators litigate and lobby to preserve the formalist test rather than accepting reclassification.
% FOUNDING_PROBLEM: The formal contract-and-supervision test was built for an industrial-era economy to distinguish genuinely autonomous tradespeople and small business owners (who negotiate their own terms and bear their own business risk) from employees embedded in a single firm's direction and dependent on that firm's wages — the test solves a real classification problem for relationships that actually look like independent contracting.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and allied economists attest the test still functions correctly, characterizing app-mediated work as genuinely autonomous, gig-economy flexibility. Labor economists outside the platform industry, several state attorneys general in reclassification litigation, and international labor bodies (ILO commentary on platform work) attest that algorithmic direction over pricing, routing, and deactivation constitutes a form of control the formal test was never designed to detect, and that the test's persistence in this domain serves cost-avoidance rather than accurate classification.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__formalist_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__formalist_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.79) because, under this reading's own operative facts, the formal test permits platforms to receive the economic value of directed labor while shedding the statutory costs that would attach if the same direction were called supervision — this is a real transfer regardless of how the reading characterizes worker consent. Suppression (0.68) reflects the litigation and lobbying apparatus actively maintained to prevent reclassification, not passive doctrinal stability. Accessibility_collapse is moderate (0.4) because, unlike a genuine natural-law boundary, alternative classification frameworks (economic-dependence tests, third-category statutes) are visibly live in courts and legislatures — the formalist reading has not foreclosed them, it is contesting them. Resistance (0.6) reflects active worker organizing, state AG litigation, and competing-sector complaints, all of which the formalist reading's own doctrine treats as noise rather than signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators sit at the beneficiary end: they set the contract terms, administer the supervision test's application to their own relationships, and capture the avoided cost directly. Platform workers sit at the target end despite formally being 'independent' — their exit options are constrained by multi-apping economics and the fact that competing platforms replicate the same classification, so mobility does not functionally escape the constraint. State insurance funds are trapped: they have no exit at all, only downstream fiscal absorption. Consumers and VC investors are beneficiaries who are not parties to the classification dispute but structurally profit from its outcome, which the schema records as beneficiary role without requiring their active participation in enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing genuine independent tradespeople from firm-embedded employees — was real and remains partially live for some contractor relationships. Under the formalist reading, the test's status is treated as still-live and functioning; the mismatch this story is built to expose is that the same doctrinal test, applied to algorithmically-directed platform labor, produces a classification outcome that increasingly serves cost-avoidance rather than accurate sorting, which is exactly the founding_problem_status='contested' + disappearance_verdict='world_rearranges' combination the mandatrophy check reads for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_direction_as_supervision,
    'Does algorithmic control over pricing, routing, and deactivation constitute ''direct supervision'' under the formal test''s own terms, or is it structurally distinct from personal supervision because no human agent gives ongoing case-by-case direction?',
    'Appellate rulings squarely addressing whether algorithmic management satisfies the control prong of the common-law multi-factor test; comparative analysis of how courts have historically treated automated/systemic direction versus discretionary human supervision in prior technology transitions (e.g. assembly-line pacing).',
    'If algorithmic direction is found to satisfy the control prong, the formalist reading''s own doctrine would reclassify platform workers as employees without needing to adopt the substantive reading''s premise at all — collapsing this reading from within. If found distinct, the formalist reading''s exclusion of platform workers survives on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_direction_as_supervision, conceptual, 'Whether algorithmic control satisfies the formal test''s own supervision element.').

omega_variable(
    worker_choice_authenticity,
    'Is platform workers'' acceptance of contractor status a genuine preference for flexibility, or a constrained choice made under conditions (lack of alternative income, absence of a real employment option in the same task category) that make ''choice'' language misleading?',
    'Worker surveys distinguishing stated preference for flexibility from revealed behavior under counterfactual employment offers; natural experiments where jurisdictions mandated employee status and measured worker retention/exit versus predicted outcomes under a genuine-preference model.',
    'If choice is substantially constrained, the vindicated proposition ''worker_choice_of_flexibility_thesis'' this reading relies on is empirically weaker than the formalist reading assumes, strengthening the case that the excluded victim set (platform workers) belongs in it after all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_choice_authenticity, empirical, 'Whether worker acceptance of contractor status reflects authentic preference or constrained necessity.').

omega_variable(
    test_administrability_versus_accuracy_tradeoff,
    'Is the formal contract-and-supervision test''s coordination value (administrability, predictability) sufficient to justify its classification errors at platform scale, or has the error rate grown large enough that the coordination benefit no longer offsets the extraction it enables?',
    'Comparative fiscal and welfare-outcome analysis between jurisdictions retaining the formal test and those that have adopted economic-dependence or third-category tests for platform work, tracking state insurance fund solvency, worker income volatility, and litigation cost trends over a multi-year window.',
    'If administrability gains are shown to be small relative to the fiscal externality generated, the tangled_rope classification''s coordination-function requirement becomes harder to sustain and the constraint drifts toward a pure-extraction (snare) reading even under formalist doctrine''s own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(test_administrability_versus_accuracy_tradeoff, empirical, 'Whether the test''s coordination benefit still outweighs its extraction cost at current platform scale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(empl_tr_t4, employment_boundary__formalist_employment_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__formalist_employment_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__formalist_employment_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__formalist_employment_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__formalist_employment_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(empl_be_t4, employment_boundary__formalist_employment_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(empl_be_t8, employment_boundary__formalist_employment_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(empl_be_t12, employment_boundary__formalist_employment_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(empl_be_t16, employment_boundary__formalist_employment_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(empl_be_t20, employment_boundary__formalist_employment_reading, base_extractiveness, 20, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(empl_su_t4, employment_boundary__formalist_employment_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(empl_su_t8, employment_boundary__formalist_employment_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(empl_su_t12, employment_boundary__formalist_employment_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(empl_su_t16, employment_boundary__formalist_employment_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(empl_su_t20, employment_boundary__formalist_employment_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the employment_boundary kernel. formalist_employment_reading (this file) authors low accessibility_collapse and high suppression, reflecting an actively contested doctrinal boundary rather than settled law; substantive_employment_reading authors the same underlying labor relationships with platform workers included in the victim set and platforms carrying employer beneficiary obligations, producing a different ε and ideally a different computed type; hybrid_security_reading authors a third-category coordination structure with its own beneficiary/victim map. All three share the same real-world labor relationships but are structurally distinct constraints under the ε-invariance principle — do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
