% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Platform 'Flexible Employment' as Structural Precarity / Surplus Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the precarity-extraction reading of the contested
 *   'flexible employment legitimacy' kernel: platform labor arrangements
 *   marketed as worker-empowering flexibility are structurally a mechanism
 *   for shifting the costs of demand volatility, insurance, and equipment
 *   onto individual workers while platform operators and their institutional
 *   clients capture the algorithmic efficiency surplus. The coordination
 *   function (real-time matching of intermittent supply and demand) is
 *   genuine, which is why this reads as tangled_rope rather than pure snare —
 *   but the classification-as-contractor is maintained by active legal and
 *   lobbying enforcement, and the costs externalized onto workers are
 *   asymmetric and structurally locked in by algorithmic deactivation power
 *   with no due process. Sibling readings of the same kernel
 *   (market_efficiency_reading: flexible employment as legitimate
 *   market-clearing; developmental_state_reading: flexible employment as a
 *   transitional form requiring state-managed formalization) are NOT part of
 *   this constraint — they are separate stories with their own ε values,
 *   beneficiary structures, and classifications, linked only via network
 *   edges and the shared kernel_id.
 *
 * KEY AGENTS:
 *   - platform_operators: agenda_setter/beneficiary (institutional/arbitrage) — designs classification and algorithmic control, captures surplus
 *   - platform_investors: beneficiary (institutional/arbitrage) — underwrites growth priced on excluded labor costs
 *   - gig_workers: payer (powerless/trapped) — bears externalized risk and cost, algorithmically disciplined without employment protection
 *   - rideshare_drivers: payer (moderate/constrained) — has some collective leverage but remains outside full employment protection
 *   - labor_regulators: observer (institutional/analytical) — investigates and can reclassify but is outpaced by platform legal spending
 *   - worker_advocacy_organizations: excluded (organized/constrained) — documents harms but is absent from the legislative drafting table
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.78).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.71).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Platform 'Flexible Employment' as Structural Precarity / Surplus Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '144d4987-18e4-48c8-b9b2-216e13b4dc11').
narrative_ontology:cs_kernel_codification('144d4987-18e4-48c8-b9b2-216e13b4dc11', distributed).
narrative_ontology:cs_authority_grounding('144d4987-18e4-48c8-b9b2-216e13b4dc11', distributed).
narrative_ontology:cs_reading_relation('144d4987-18e4-48c8-b9b2-216e13b4dc11', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('144d4987-18e4-48c8-b9b2-216e13b4dc11', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('144d4987-18e4-48c8-b9b2-216e13b4dc11', foundational, coordination_function_separable_from_status_exclusion).
narrative_ontology:cs_axiom_status(coordination_function_separable_from_status_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('144d4987-18e4-48c8-b9b2-216e13b4dc11', coordination_function_separable_from_status_exclusion, empirically_contingent).
narrative_ontology:cs_axiom('144d4987-18e4-48c8-b9b2-216e13b4dc11', foundational, risk_externalization_constitutes_uncompensated_transfer).
narrative_ontology:cs_axiom_status(risk_externalization_constitutes_uncompensated_transfer, holdable).
narrative_ontology:cs_axiom_grounding('144d4987-18e4-48c8-b9b2-216e13b4dc11', risk_externalization_constitutes_uncompensated_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('144d4987-18e4-48c8-b9b2-216e13b4dc11', standard_employment_relationship_baseline).
narrative_ontology:cs_drift_state('144d4987-18e4-48c8-b9b2-216e13b4dc11', platform_economy_maturity_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('144d4987-18e4-48c8-b9b2-216e13b4dc11', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_investors).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, institutional_clients_of_platforms).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, delivery_couriers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, rideshare_drivers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, labor_flexibility_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the classification of workers as independent contractors, write the algorithmic dispatch and rating systems that discipline worker behavior without an employment relationship, and set the terms of platform access unilaterally. Capture the difference between what clients pay and what workers receive after platform take-rate, insurance externalization, and equipment costs are excluded from their books. Can relocate incorporation, exit jurisdictions, or reclassify service lines faster than any regulatory response.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, beneficiary).

% Underwrite platform growth premised on labor cost structures that exclude benefits, minimum wage guarantees, and unemployment insurance contributions. Realize returns partly through valuations that price in the absence of employer-side social insurance costs. Not party to any single worker's situation and can diversify away from any one platform's regulatory exposure.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Restaurants, retailers, and logistics firms that contract platform labor for delivery and fulfillment without carrying employer obligations themselves. Benefit from labor supply that expands and contracts with demand at no fixed cost, and can switch between competing platforms if terms shift.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, institutional_clients_of_platforms, beneficiary,
    organized, biographical, mobile, national).

% Perform the labor the platform brokers, absorb vehicle depreciation, fuel, insurance, and downtime costs the platform does not carry, and have no employer-provided unemployment insurance, sick leave, or pension contribution. Algorithmic deactivation functions as termination without recourse. Formally free to log off, but income precarity and sunk equipment costs make exit from the sector, not just the shift, the only real alternative — and even that requires forgoing income with no transition support.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers, payer,
    powerless, immediate, trapped, local).

% Paid per-delivery under algorithmically set rates that can change without notice or negotiation; bear traffic-accident and theft risk personally; often work multiple platforms simultaneously to approximate a living wage, which is itself evidence the single-platform rate is set below subsistence. Reclassification to employee status is fought by platforms in courts and legislatures they can outspend.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, delivery_couriers, payer,
    powerless, immediate, trapped, local).

% Have organized in several jurisdictions to bargain collectively despite independent-contractor status, winning minimum per-trip guarantees in some cities. Retain more leverage than isolated couriers through numbers and public visibility, but remain excluded from the full employment protections that would follow reclassification, and face platform lobbying to preserve contractor status via ballot measures and statutory carve-outs.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, rideshare_drivers, payer,
    moderate, biographical, constrained, national).

% Investigate worker classification, issue rulings on misclassification, and can impose reclassification, back-pay, or benefits mandates. Their jurisdiction is fragmented by locality and jurisdiction-shopping by platforms, and their enforcement capacity lags platform legal spending.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% Document algorithmic wage theft, deactivation without appeal, and social-insurance gaps, and push for reclassification and portable benefits. Rarely seated at the table when platforms negotiate exemptions with legislators; their evidence enters mainly through litigation and public pressure rather than direct policy co-authorship.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, worker_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Platforms genuinely solve a real matching problem — connecting fragmented, intermittent labor supply with fragmented, intermittent demand for rides, delivery, and tasks — faster and at lower search cost than prior arrangements.
% TRANSFER_FUNCTION: Moves the costs of demand volatility, equipment, insurance, and downtime from the platform and its institutional clients onto individual workers, while the surplus generated by algorithmic efficiency (reduced idle time, dynamic pricing, dispatch optimization) accrues to platform operators and investors as take-rate and valuation.
% ABSENT_VOICES: Individual gig workers and worker advocacy organizations are structurally absent from the rulemaking and ballot-measure processes that set classification law; platforms fund the campaigns and draft the model legislation that regulators and legislators then vote on.
% DISAPPEARANCE_RATIONALE: If the contractor-classification arrangement and its algorithmic enforcement vanished overnight, platforms would face immediate reclassification exposure, workers would gain access to minimum wage floors, unemployment insurance, and collective bargaining rights, and platform unit economics — which are built around excluded employer-side costs — would require substantial repricing or business-model restructuring.
% FOUNDING_PROBLEM: Matching intermittent, geographically dispersed labor supply to intermittent demand (rides, deliveries, tasks) without the fixed costs of scheduling and maintaining a large employed workforce for peak-only capacity.
% FOUNDING_PROBLEM_CORROBORATION: Platforms and their investors attest the flexibility need remains live and central to the business model. Labor regulators in multiple jurisdictions (UK Supreme Court Uber ruling, California AB5 litigation record, EU Platform Work Directive proceedings) and independent labor economists outside the platforms' employ have found the matching function does not require excluding workers from employment protections — the classification choice is separable from the coordination function it is said to require.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.78 at interval end) reflects the gap between the algorithmic efficiency surplus generated by real-time matching and what workers receive after uncompensated risk, equipment, and insurance costs are netted out. Suppression (0.71) reflects the active legal, lobbying, and algorithmic-deactivation apparatus required to maintain contractor classification against a rising tide of reclassification rulings and worker organizing. Theater ratio (0.42, rising) reflects growing platform investment in 'worker choice' messaging, driver advisory councils, and portable-benefit pilot programs that function more as public relations and litigation defense than as functional equivalents of employment protection. All three series share one time grid (0-12, monthly-scale intervals representing a multi-year enforcement-hardening arc) as required.
 *
 * PERSPECTIVAL GAP:
 *   From the platform operator's seat, the arrangement is coordination: it solves a genuine matching problem, and the contractor classification is presented as preserving worker autonomy and flexibility. From the gig worker's seat, the same structure is enforced extraction: the 'flexibility' is nominal (algorithmic scheduling and deactivation constrain behavior as tightly as employment supervision would) while the protections of employment are withheld. The engine computes this divergence from the structural power/exit data; this story does not resolve it — it is the mandatrophy-relevant divergence the classification exists to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators and investors sit at the beneficiary end: they set the classification terms, can exit jurisdictions or reclassify service lines at will (arbitrage exit), and capture the surplus. Gig workers and delivery couriers sit at the full-target end: trapped exit (leaving the platform sacrifices immediate income with no transition support, and leaving the sector entirely means forgoing sunk equipment investment), powerless bargaining position, and no institutional recourse against algorithmic deactivation. Rideshare drivers are differentiated from couriers by moderate power and constrained (rather than trapped) exit — collective organizing in several jurisdictions has won minimum per-trip guarantees, illustrating that identical nominal contractor status produces different effective directionality depending on organizing capacity, not formal classification alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (matching intermittent labor supply and demand) has not disappeared and remains genuinely valuable — this prevents mislabeling the entire platform labor model as pure snare. But the founding justification for excluding workers from employment protections (that the matching function requires contractor status) is contested by regulatory rulings finding the two are separable. The tangled_rope classification holds both truths: real coordination value AND asymmetric extraction riding on the same structure, requiring active enforcement (legal defense of classification, algorithmic control substituting for supervisory relationship, lobbying against reclassification) to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_precarity,
    'Is the precarity-extraction reading the structurally correct lens for evaluating platform flexible employment, or do the market_efficiency_reading and developmental_state_reading readings better capture the arrangement in jurisdictions with stronger labor protections or earlier-stage platform markets?',
    'Cross-jurisdictional comparison of platform unit economics, worker income volatility, and reclassification litigation outcomes: where courts and regulators consistently find the coordination function separable from contractor exclusion (as in this reading''s premise), the precarity-extraction reading is corroborated; where flexibility genuinely requires excluded status to function economically, market_efficiency_reading gains support.',
    'If the developmental_state_reading is empirically dominant in a given market (i.e., formalization is actively underway and effective), this constraint''s high extractiveness and suppression values would overstate the arrangement''s entrenchment there; the sibling stories carry that structural difference rather than this one being revised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_precarity, conceptual, 'Which kernel reading structurally fits a given platform labor market').

omega_variable(
    surplus_offset_magnitude,
    'Do the wage gains platforms advertise (higher per-hour rates than comparable formal employment in some markets) genuinely offset the risk-externalization and social-insurance costs shifted to workers, or is the offset illusory once uncompensated hours, equipment depreciation, and insurance gaps are priced in?',
    'Independent (non-platform-funded) full-cost-accounting studies comparing effective hourly earnings net of all worker-borne costs against comparable formal-sector wages plus benefits, across multiple platforms and jurisdictions.',
    'If net earnings genuinely exceed formal-sector equivalents even after full cost accounting, the extraction measure here would be overstated and the arrangement would sit closer to a rope with acceptable risk-sharing; existing independent studies (UK, California, EU) generally find net earnings below formal-sector equivalents, supporting the extraction reading, but the evidence base is uneven across platforms and geographies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surplus_offset_magnitude, empirical, 'Whether platform wage premiums offset externalized costs').

omega_variable(
    algorithmic_control_as_employment_relationship,
    'Does algorithmic dispatch, rating-based deactivation, and dynamic pricing constitute the functional equivalent of an employment relationship (direction and control), independent of contractual labeling?',
    'Legal and labor-economics analysis of the degree of behavioral control algorithms exert compared to traditional supervisory control; several courts (UK Supreme Court in Uber v Aslam) have already ruled in the affirmative for specific platforms.',
    'A strong affirmative finding across jurisdictions would support reclassifying most platform gig work as employment by function, sharply raising the case for suppression being structural (legal fiction maintained against functional reality) rather than merely contractual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_control_as_employment_relationship, empirical, 'Whether algorithmic control substitutes for legal employment control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(flex_tr_t2, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 2, 0.24).
narrative_ontology:measurement(flex_tr_t4, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 4, 0.29).
narrative_ontology:measurement(flex_tr_t6, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(flex_tr_t8, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 12, 0.42).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(flex_be_t2, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(flex_be_t4, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 4, 0.64).
narrative_ontology:measurement(flex_be_t6, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 6, 0.69).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 8, 0.73).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 12, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(flex_su_t2, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 2, 0.54).
narrative_ontology:measurement(flex_su_t4, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(flex_su_t6, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement(flex_su_t8, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(flex_su_t12, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 12, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__precarity_extraction_reading, 0.12).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language concept 'flexible employment legitimacy' per the ε-invariance principle: precarity_extraction_reading (this story, tangled_rope, ε=0.78), market_efficiency_reading (separate story, expected lower ε, rope-leaning), and developmental_state_reading (separate story, expected scaffold-leaning with a transition/formalization justification). Each carries its own ε, beneficiary/victim structure, and claimed_type; they are linked here rather than merged because measuring the same underlying platform-labor phenomenon through different structural lenses (market-clearing efficiency vs. cost-externalization vs. transitional-formalization) yields genuinely different ε values, which under the ε-invariance principle means they are different constraints, not one constraint with an ambiguous measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
