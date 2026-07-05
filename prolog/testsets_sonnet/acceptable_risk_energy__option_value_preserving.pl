% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Option-Value-Preserving Reading of Acceptable Energy Risk
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   Grid planning authorities and energy regulators increasingly justify
 *   keeping both nuclear and fossil generation capacity licensed and
 *   subsidized — alongside renewable buildout — on the grounds that under
 *   deep uncertainty about technology costs, climate trajectories, and demand
 *   growth, foreclosing any major pathway too early risks catastrophic
 *   misallocation. This option-value framing has genuine decision-theoretic
 *   grounding (real-options value is a legitimate concept under
 *   irreversibility and uncertainty) but it also structurally favors
 *   incumbents: fossil and nuclear operators who would otherwise face closure
 *   on economic or environmental merit are instead protected as 'preserved
 *   options,' while renewable-only developers absorb the opportunity cost and
 *   diffuse populations absorb the delayed climate and health costs.
 *
 * KEY AGENTS:
 *   - grid_planning_authorities: agenda_setter (institutional/analytical) — administers licensing that operationalizes the option-preserving standard
 *   - incumbent_fossil_operators and incumbent_nuclear_operators: beneficiaries (powerful/arbitrage,constrained) — protected from closure by the optionality framing
 *   - renewable_only_developers: payer (organized/constrained) — bears opportunity cost of capital and grid capacity diverted to incumbent preservation
 *   - communities_near_legacy_plants and climate_exposed_populations: payers (powerless/trapped) — bear localized and diffuse costs of delayed pathway closure
 *   - future_decision_makers: beneficiary (analytical/analytical) — inherits flexibility without bearing its present cost
 *   - risk_assessment_analysts: observer — evaluates the reading against its siblings using decision theory and empirical energy data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.42).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.48).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Option-Value-Preserving Reading of Acceptable Energy Risk").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, '4e07c3bd-dee9-4191-a1d7-758fc981fedd').
narrative_ontology:cs_kernel_codification('4e07c3bd-dee9-4191-a1d7-758fc981fedd', distributed).
narrative_ontology:cs_authority_grounding('4e07c3bd-dee9-4191-a1d7-758fc981fedd', distributed).
narrative_ontology:cs_reading_relation('4e07c3bd-dee9-4191-a1d7-758fc981fedd', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('4e07c3bd-dee9-4191-a1d7-758fc981fedd', acceptable_risk_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('4e07c3bd-dee9-4191-a1d7-758fc981fedd', foundational, irreversibility_under_deep_uncertainty_warrants_hedging).
narrative_ontology:cs_axiom_status(irreversibility_under_deep_uncertainty_warrants_hedging, holdable).
narrative_ontology:cs_axiom_grounding('4e07c3bd-dee9-4191-a1d7-758fc981fedd', irreversibility_under_deep_uncertainty_warrants_hedging, instrumental).
narrative_ontology:cs_axiom('4e07c3bd-dee9-4191-a1d7-758fc981fedd', secondary, premature_pathway_closure_is_itself_a_harm).
narrative_ontology:cs_axiom_status(premature_pathway_closure_is_itself_a_harm, holdable).
narrative_ontology:cs_axiom_grounding('4e07c3bd-dee9-4191-a1d7-758fc981fedd', premature_pathway_closure_is_itself_a_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('4e07c3bd-dee9-4191-a1d7-758fc981fedd', multi_pathway_hedged_portfolio).
narrative_ontology:cs_drift_state('4e07c3bd-dee9-4191-a1d7-758fc981fedd', post_cost_curve_resolution_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4e07c3bd-dee9-4191-a1d7-758fc981fedd', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, incumbent_fossil_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, incumbent_nuclear_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, grid_planning_authorities).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, future_decision_makers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, renewable_only_developers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, communities_near_legacy_plants).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, climate_exposed_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, future_taxpayers_funding_stranded_assets).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, deep_uncertainty_precludes_premature_lock_in).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, portfolio_diversification_under_ignorance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets integrated resource plans that formally require multiple pathways be kept 'available' — licensing extensions for nuclear, permitting continuations for gas, alongside renewable buildout. Justifies this as preserving flexibility against demand and technology uncertainty. Administers the licensing and permitting apparatus that keeps incumbent pathways alive rather than closing them.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, grid_planning_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Continue operating gas and coal plants under 'bridge fuel' and 'reliability reserve' justifications that ride directly on the option-value framing. Collect capacity payments and avoid stranded-asset write-downs because the option-preserving standard treats their closure as a foreclosed-optionality risk rather than a completed transition.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, incumbent_fossil_operators, beneficiary,
    powerful, biographical, arbitrage, national).

% Receive extended licenses and subsidy support justified by the same option-preserving logic — nuclear capacity is kept 'on the table' as a hedge against renewable intermittency, even where near-term economics disfavor new build. Benefits from being named a preserved pathway rather than judged on standalone merit.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, incumbent_nuclear_operators, beneficiary,
    powerful, generational, constrained, national).

% Compete for grid interconnection queue slots and capacity-market revenue against incumbent pathways that the acceptable-risk standard protects from full closure. Bear the opportunity cost: capital, land, and interconnection capacity that could accelerate renewable buildout is instead allocated to keeping fossil and nuclear options viable. Cannot exit the market they compete in; can only lobby to change the standard.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, renewable_only_developers, payer,
    organized, biographical, constrained, national).

% Continue to bear the localized health, pollution, and accident-risk burden of fossil and nuclear plants kept operating past the point their coordination case was strongest, because those plants are formally classified as preserved options rather than retired capacity. Geographically fixed near the facilities; cannot relocate the risk exposure.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, communities_near_legacy_plants, payer,
    powerless, biographical, trapped, local).

% Bear the diffuse, delayed cost of slower fossil phase-out that the option-preserving standard permits by treating rapid fossil closure as itself a risk (loss of flexibility) rather than a benefit. Have no seat in the domestic planning process that sets the standard and no capacity to exit the climate system.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, climate_exposed_populations, payer,
    powerless, civilizational, trapped, global).

% Inherit a wider menu of technically viable pathways than they would under an early-committal standard, and can revise the energy mix as new information (cost curves, failure data, climate trajectories) arrives. Benefit from optionality without bearing its present carrying cost.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, future_decision_makers, beneficiary,
    analytical, generational, analytical, national).

% Will ultimately fund decommissioning, remediation, and stranded-asset write-downs for fossil and nuclear capacity kept alive under the option-preserving standard longer than its economic case justified. Have no vote in current licensing decisions that create this future liability.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, future_taxpayers_funding_stranded_assets, payer,
    powerless, generational, trapped, national).

% Evaluate competing acceptable-risk frameworks (this option-preserving reading versus catastrophic-tail-dominant and expected-value-dominant readings) using real-options theory, decision analysis under deep uncertainty, and comparative energy-mortality datasets. Can document the tradeoffs but do not set the standard.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, risk_assessment_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__option_value_preserving, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__option_value_preserving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents irreversible, premature foreclosure of energy pathways under conditions where the true relative costs and risks of nuclear, fossil, and renewable systems are not yet fully knowable — genuinely valuable when technology trajectories, demand growth, and climate sensitivity are all uncertain and reversal costs are asymmetric.
% TRANSFER_FUNCTION: Moves capital, grid-interconnection capacity, and regulatory attention away from renewable-only developers and toward maintaining incumbent fossil and nuclear capacity; moves localized health and pollution costs onto communities near legacy plants; moves climate risk forward in time onto future and globally distributed populations; moves flexibility benefits to future decision-makers who did not pay for the optionality.
% ABSENT_VOICES: Climate-exposed populations in other jurisdictions and future taxpayers have no representation in the domestic grid-planning process that decides which pathways count as worth preserving. Renewable-only developers are present but structurally outweighed by incumbents with existing capacity and regulatory relationships.
% DISAPPEARANCE_RATIONALE: Incumbent operators and grid planners argue the world would rearrange catastrophically — losing reserve capacity and technology hedges before replacements are proven at scale. Renewable developers and climate-exposed populations argue the standard mainly rearranges the world in incumbents' favor and that removing it would accelerate an already-viable transition; the dispute over which is true IS the kernel contest this story is one reading of.
% FOUNDING_PROBLEM: Energy infrastructure decisions are irreversible and capital-intensive (decades-long asset lives), made under genuine uncertainty about future technology costs, climate sensitivity, and demand — early full commitment to any single pathway risks catastrophic misallocation if the underlying assumptions prove wrong.
% FOUNDING_PROBLEM_CORROBORATION: Decision-theory researchers studying deep uncertainty and real-options value attest the founding problem remains genuinely live for some technology classes. Independent grid economists and stranded-asset researchers outside the incumbent industries attest that for fossil generation specifically the uncertainty has substantially resolved (cost curves, climate impacts, and renewable maturity are now well-characterized), and that continued invocation of 'preserving optionality' functions increasingly as cover for delaying committed closure rather than genuine hedging.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, contested).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).
:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the coordination function is real — asymmetric irreversibility does genuinely justify some hedging — but the standard's actual operation increasingly protects incumbent revenue streams beyond what the uncertainty argument supports, particularly for fossil generation where cost and climate trajectories have substantially resolved. Suppression is moderate (0.48): both extreme readings (immediate fossil elimination on catastrophic-tail grounds, or pure least-cost selection on expected-value grounds) are structurally disadvantaged in the planning process, which is itself a form of suppression of alternative risk framings, not merely of physical exit options. Theater ratio rises modestly (0.30) as 'preserving optionality' becomes cited even where evidentiary uncertainty has narrowed and the real driver is protecting existing licensed capacity from write-down.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent fossil and nuclear operators sit near the beneficiary end: the standard converts what would otherwise be economically or environmentally forced closure into protected status, and their exit options (arbitrage for fossil operators who can redeploy capital, constrained for nuclear operators locked into long asset lives) reflect that asymmetric protection. Grid planning authorities are agenda-setters who administer but do not personally collect the extraction, so they sit closer to a coordination seat than a beneficiary seat despite formally implementing the standard. Renewable developers and the two powerless victim groups sit near the target end: they bear real costs (foregone market share, localized pollution, diffuse climate exposure) with no comparable capacity to redirect the standard. Future decision-makers are unusual: they benefit from the preserved optionality without paying its carrying costs, which the analytical power/exit combination captures without needing an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine decision-theoretic uncertainty justifying hedged commitment) was more clearly live decades ago when technology cost curves and climate sensitivity were far less characterized. The founding_problem_status is authored as contested rather than dead because the uncertainty argument remains partially live for genuinely novel technology classes (e.g., advanced nuclear, long-duration storage) even as it has become largely obsolete for continued fossil operation specifically. Classifying this as tangled_rope rather than snare or mountain prevents two mislabelings: treating the entire option-preserving standard as pure extraction (which would ignore the real hedging value for genuinely uncertain technologies) and treating it as natural/inevitable risk policy (which would ignore the asymmetric incumbent protection layered onto the genuine coordination core).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is option-value preservation the structurally correct reading of ''acceptable risk'' for energy policy, or does its dominance in current planning practice reflect incumbent capture of the risk-framing process rather than superior decision theory?',
    'Compare planning-body composition and public comment records across jurisdictions that have adopted option-preserving standards against those using expected-value or catastrophic-tail framings; look for correlation between incumbent industry representation on planning bodies and adoption of the option-preserving standard specifically.',
    'If adoption correlates strongly with incumbent representation rather than with genuine technological uncertainty in the affected sector, the reading functions substantially as regulatory capture wearing decision-theory language; if adoption correlates with objectively high-uncertainty technology classes regardless of incumbent presence, the reading is closer to genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the dominance of this reading reflects sound decision theory or incumbent capture of the risk-framing process.').

omega_variable(
    uncertainty_decay_by_pathway,
    'Has the genuine deep uncertainty that justifies option preservation actually decayed asymmetrically across pathways — resolved for fossil (well-characterized cost and climate trajectories) but still substantially live for advanced nuclear and long-duration storage?',
    'Track confidence intervals in independent (non-industry-funded) energy cost and climate-impact projections over time, per technology class; compare width of intervals for fossil versus emerging nuclear/storage technologies.',
    'If uncertainty has resolved for fossil specifically, continued fossil preservation under this standard is a mandatrophy case — the founding problem is dead for that pathway even if live for others, which would argue for splitting this constraint further by technology rather than treating ''energy pathways'' as one undifferentiated set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncertainty_decay_by_pathway, empirical, 'Whether founding uncertainty has decayed unevenly across the preserved pathways.').

omega_variable(
    sibling_reading_selection_is_the_dispute,
    'Which of the three kernel readings (option-value-preserving, catastrophic-tail-dominant, expected-value-dominant) a given regulator or advocate adopts is itself the primary site of political contest — is this selection driven by evidence about which framework best fits energy-specific uncertainty, or by prior commitments to incumbent or challenger interests?',
    'Not resolvable by data internal to this constraint; requires tracing argument genealogies and funding sources behind advocacy for each reading across multiple jurisdictions.',
    'Documents that the kernel contest itself is downstream of interest-driven framework selection, which affects how much independent weight the option-preserving reading''s decision-theoretic justification should carry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_selection_is_the_dispute, preference, 'The kernel-level contest over which acceptable-risk reading applies is itself an object of dispute among differently-interested parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.14).
narrative_ontology:measurement(acce_tr_t4, acceptable_risk_energy__option_value_preserving, theater_ratio, 4, 0.17).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__option_value_preserving, theater_ratio, 8, 0.2).
narrative_ontology:measurement(acce_tr_t12, acceptable_risk_energy__option_value_preserving, theater_ratio, 12, 0.23).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_energy__option_value_preserving, theater_ratio, 16, 0.26).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__option_value_preserving, theater_ratio, 20, 0.28).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_energy__option_value_preserving, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(acce_be_t4, acceptable_risk_energy__option_value_preserving, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__option_value_preserving, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(acce_be_t12, acceptable_risk_energy__option_value_preserving, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_energy__option_value_preserving, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__option_value_preserving, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_energy__option_value_preserving, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(acce_su_t4, acceptable_risk_energy__option_value_preserving, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__option_value_preserving, suppression_requirement, 8, 0.39).
narrative_ontology:measurement(acce_su_t12, acceptable_risk_energy__option_value_preserving, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_energy__option_value_preserving, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__option_value_preserving, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_energy__option_value_preserving, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the acceptable_risk_energy kernel, decomposed per the ε-invariance principle because 'acceptable risk' in energy policy conflates structurally distinct decision rules with different beneficiary/victim structures and different ε values. option_value_preserving (this story) protects incumbent nuclear and fossil capacity as hedges and produces moderate, rising extraction (tangled_rope). catastrophic_tail_dominant would suppress high-tail-risk pathways (likely nuclear) regardless of expected-value cost. expected_value_dominant would select purely on mortality-per-TWh and would tend to favor whichever pathway has the best aggregate safety record, producing a different victim set (likely nuclear-adjacent communities or fossil-adjacent communities depending on current data) than this reading's diffuse opportunity-cost victims. All three are linked via affects_constraints; none averages or measures the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
