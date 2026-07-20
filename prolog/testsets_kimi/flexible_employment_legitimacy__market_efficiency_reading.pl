% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__market_efficiency_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Legitimate Market-Clearing Mechanism (Market Efficiency Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the market-efficiency reading of the
 *   flexible-employment legitimacy kernel. It treats gig-work and zero-hour
 *   arrangements as natural market-clearing mechanisms that match labor
 *   supply to demand, treats wage convergence as a scarcity signal, and
 *   treats platform algorithms as neutral coordination technology. The
 *   constraint is the institutionalized legitimacy framework â legal
 *   classifications, policy discourse, and platform narratives â that
 *   prevents formalization and vindicates the model. As a kernel reading, it
 *   is generated clean: no sibling reading is folded inside it; the contest
 *   is routed to omega variables and the cs_structure block.
 *
 * KEY AGENTS:
 *   - platform_operators: Primary agenda-setter (institutional/global/arbitrage) â designs rules and captures surplus.
 *   - corporate_users_of_flexible_labor: Primary beneficiary (powerful/mobile) â gains cost flexibility without employment overhead.
 *   - flexible_workers: Primary target (powerless/constrained) â bears risk, volatility, and absence of protections.
 *   - displaced_formal_workers: Excluded victim (moderate/constrained) â lost formal pathways, absent from policy conversation.
 *   - labor_organizers: Excluded challenger (organized/constrained) â blocked from platform workplaces by algorithmic and legal barriers.
 *   - platform_economy_researchers: Analytical observer (analytical) â provides independent empirical assessment of the market-clearing claim.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.72).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.68).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Legitimate Market-Clearing Mechanism (Market Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__market_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, '5f4c10c9-87d4-4f7e-942e-cbee5fb984f8').
narrative_ontology:cs_kernel_codification('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8', formalized).
narrative_ontology:cs_authority_grounding('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8', expertise).
narrative_ontology:cs_interpretation_layer_present('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8').
narrative_ontology:cs_reading_relation('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8', foundational, wage_convergence_scarcity_signal).
narrative_ontology:cs_axiom_status(wage_convergence_scarcity_signal, holdable).
narrative_ontology:cs_axiom_grounding('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8', wage_convergence_scarcity_signal, empirically_contingent).
narrative_ontology:cs_axiom('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8', foundational, platform_neutrality_coordination).
narrative_ontology:cs_axiom_status(platform_neutrality_coordination, holdable).
narrative_ontology:cs_axiom_grounding('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8', platform_neutrality_coordination, instrumental).
narrative_ontology:cs_reference_frame('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8', market_clearing_equilibrium).
narrative_ontology:cs_drift_state('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8', post_platform_contestation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5f4c10c9-87d4-4f7e-942e-cbee5fb984f8', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, corporate_users_of_flexible_labor).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, displaced_formal_workers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, neoclassical_labor_market_theory).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, algorithmic_neutrality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design algorithmic labor-allocation systems, set commission structures, and classify workers as independent contractors. Lobby for regulatory frameworks that preserve flexible labor models. Capture data rents and service fees from every matched transaction.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Access on-demand staffing without employment overhead, converting fixed labor costs into variable ones. Shift regulatory and insurance liabilities to workers and platforms. Can revert to formal employment if flexible labor costs rise.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, corporate_users_of_flexible_labor, beneficiary,
    powerful, biographical, mobile, national).

% Accept algorithmically dispatched tasks, bearing income volatility, equipment costs, and absence of social protections. Algorithmic ratings and need for immediate income constrain exit. Frequently compelled to work below minimum wage after costs.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers, payer,
    powerless, immediate, constrained, local).

% Formerly employed in formal wage positions that were converted to or replaced by flexible gig models. Would object to the reclassification but are structurally absent from policy conversations dominated by platform and efficiency narratives.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, displaced_formal_workers, excluded,
    moderate, biographical, constrained, national).

% Seek to organize flexible workers and reclassify them as employees. Excluded from platform workplaces by algorithmic management and legal contractor classifications. Their resistance is the primary challenge to the constraint's persistence.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_organizers, excluded,
    organized, generational, constrained, national).

% Conduct empirical studies on wages, working conditions, and algorithmic management. Provide independent analysis of whether the market-clearing narrative matches observed outcomes. Their findings feed regulatory and judicial review.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_economy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__market_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces search and matching frictions between episodic labor demand and available workers, allowing rapid scaling without long-term employment overhead.
% TRANSFER_FUNCTION: Moves income risk, equipment cost, and social-protection obligations from employers to individual workers, while moving surplus value and data rents to platforms and cost savings to corporate users and consumers.
% ABSENT_VOICES: Workers who would prefer formal employment but are channeled into flexible arrangements by absence of alternatives; organized labor representatives blocked by algorithmic management and contractor classifications; local regulators preempted by national platform-friendly legislation.
% DISAPPEARANCE_RATIONALE: If the legitimacy constraint vanished, platforms would face reclassification suits, labor costs would rise as employment obligations attached, corporate users would shift to formal staffing, and consumer prices would rise â the labor market would reorganize away from spot-contracting.
% FOUNDING_PROBLEM: High fixed costs and legal rigidity of formal employment creating mismatches with variable labor demand; frictional unemployment from inefficient job search and hiring gates.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and mainstream labor economists attest the problem is still live. Labor sociologists and critical economists attest the problem is largely solved by existing employment law and that the constraint functions to legitimize regulatory arbitrage; empirical studies on worker earnings and conditions from outside the beneficiary set support the shifted-function reading.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the arrangement systematically externalizes income risk and social-protection costs onto workers while platforms capture data rents and commissions. Suppression is substantial (0.68) because the constraint's persistence requires active legal suppression of employee-classification claims, preemption of local labor standards, and algorithmic management that prevents collective bargaining. Theater_ratio (0.52 at interval end) reflects the growing share of discourse devoted to 'entrepreneurship' and 'flexibility' that masks a stable extraction structure. Accessibility_collapse (0.60) captures the erosion of formal-entry pathways as platforms dominate local labor markets. Resistance (0.55) tracks mounting worker organizing and regulatory pushback that has not yet reversed the constraint. The claim is tangled_rope because a genuine coordination function (matching, reduced search frictions) is inseparable from asymmetric extraction in the actual institutional form.
 *
 * PERSPECTIVAL GAP:
 *   The platform and corporate-user seats compute toward rope or benign coordination; the worker seat computes toward snare. The engine derives this from the same structural data: low exit and high power for beneficiaries, constrained exit and powerlessness for targets. The perspectival gap is wide because the constraint's coordination story is structurally available only to the power-advantaged seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform_operators are structural beneficiaries and agenda-setters (low d); they design the rules and capture surplus. Corporate_users are beneficiaries (low d) who gain cost flexibility. Flexible_workers are targets (high d) who bear risk and cost. The divergence is sharp: the same arrangement reads as efficient coordination from the platform seat and as extraction from the worker seat. Displaced_formal_workers and labor_organizers sit at high d due to exclusion and resistance costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by separating the coordination function (genuine matching efficiency) from the extraction layer (risk externalization, surplus capture). A rope reading would require no victims and low suppression; the presence of flexible_workers as payers and the active enforcement requirement block that. A snare reading would deny the coordination function; but the matching technology does reduce search frictions, so pure snare is also blocked. Tangled_rope captures the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_clearing_naturalness,
    'Is flexible employment a spontaneous market-clearing equilibrium, or does it depend on contingent legal classifications and active regulatory suppression of alternatives?',
    'Comparative institutional analysis of jurisdictions with different classification rules; if flexible employment collapses under stricter employment tests, it is constructed rather than natural.',
    'If constructed, the constraint''s legitimacy as natural coordination dissolves and it reclassifies toward snare or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_clearing_naturalness, conceptual, 'Whether the arrangement is a natural market outcome or a legally constructed category.').

omega_variable(
    autonomy_vs_constrained_choice,
    'Does the observed worker ''autonomy'' reflect genuine preference for flexibility, or does it mask constrained choice under algorithmic management and absence of formal alternatives?',
    'Panel studies tracking worker welfare across transitions between flexible and formal employment; revealed preference under alternative availability.',
    'If constrained choice, the coordination story loses empirical foundation and the constraint''s beneficiary structure concentrates extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_constrained_choice, empirical, 'Whether worker autonomy is genuine preference or rationalized constraint.').

omega_variable(
    reading_family_contamination,
    'Does the empirical challenge to the market efficiency reading (rising extraction, worker protests) contaminate the sibling precarity extraction reading by confirming its premises, or does it leave the kernel contested?',
    'Track whether evidence against market efficiency is absorbed by the precarity reading or generates a fourth reading.',
    'Determines whether the kernel resolves into a dominant reading or persists as a family of linked constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_family_contamination, conceptual, 'Whether empirical challenges to this reading strengthen a sibling reading or sustain kernel contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_emp_mkt_eff_tr_t0, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(flex_emp_mkt_eff_tr_t5, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(flex_emp_mkt_eff_tr_t10, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(flex_emp_mkt_eff_tr_t15, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(flex_emp_mkt_eff_tr_t20, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(flex_emp_mkt_eff_be_t0, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(flex_emp_mkt_eff_be_t5, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(flex_emp_mkt_eff_be_t10, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(flex_emp_mkt_eff_be_t15, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(flex_emp_mkt_eff_be_t20, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(flex_emp_mkt_eff_su_t0, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(flex_emp_mkt_eff_su_t5, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(flex_emp_mkt_eff_su_t10, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(flex_emp_mkt_eff_su_t15, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(flex_emp_mkt_eff_su_t20, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, developmental_state_reading).

% DUAL FORMULATION NOTE:
% The flexible_employment_legitimacy kernel decomposes into three structurally distinct constraints. The market_efficiency_reading claims natural coordination with low extraction; the precarity_extraction_reading claims asymmetric surplus extraction; the developmental_state_reading claims transitional coordination requiring sunset management. Each has a distinct epsilon, beneficiary structure, and type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
