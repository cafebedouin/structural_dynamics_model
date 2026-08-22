% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Performance Legitimacy: Quantitative Growth Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint describes a specific reading of 'performance legitimacy'
 *   in a state-capitalist context, where the state's right to rule is
 *   primarily justified by its ability to deliver high rates of GDP growth
 *   and job creation. This reading prioritizes quantitative economic
 *   expansion, often at the expense of environmental sustainability, social
 *   equity, or qualitative development. The constraint is actively enforced
 *   through top-down policy directives and cadre evaluation systems, creating
 *   a powerful incentive structure for local officials and benefiting large
 *   industrial complexes.
 *
 * KEY AGENTS:
 *   - state_leadership: Primary agenda-setter (institutional/identity_locked) — defines and enforces growth targets.
 *   - industrial_export_complex: Primary beneficiary (organized/constrained) — receives preferential policies for contributing to GDP.
 *   - local_government_officials: Secondary beneficiary (powerful/identity_locked) — career tied to local GDP growth.
 *   - general_populace: Primary payer (powerless/constrained) — bears environmental and social costs.
 *   - environmental_advocates: Payer (moderate/constrained) — marginalized in favor of growth.
 *   - small_and_medium_enterprises: Payer (moderate/constrained) — struggle against large state-backed entities.
 *   - international_investors: Observer (organized/arbitrage) — monitor growth for investment opportunities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.78).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.85).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy: Quantitative Growth Reading").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '7eb0019c-5033-4b3f-901d-0b8d6600392d').
narrative_ontology:cs_kernel_codification('7eb0019c-5033-4b3f-901d-0b8d6600392d', formalized).
narrative_ontology:cs_authority_grounding('7eb0019c-5033-4b3f-901d-0b8d6600392d', extraction).
narrative_ontology:cs_interpretation_layer_present('7eb0019c-5033-4b3f-901d-0b8d6600392d').
narrative_ontology:cs_reading_relation('7eb0019c-5033-4b3f-901d-0b8d6600392d', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_reading_relation('7eb0019c-5033-4b3f-901d-0b8d6600392d', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('7eb0019c-5033-4b3f-901d-0b8d6600392d', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('7eb0019c-5033-4b3f-901d-0b8d6600392d', foundational, gdp_growth_as_primary_legitimacy_metric).
narrative_ontology:cs_axiom_status(gdp_growth_as_primary_legitimacy_metric, holdable).
narrative_ontology:cs_axiom_grounding('7eb0019c-5033-4b3f-901d-0b8d6600392d', gdp_growth_as_primary_legitimacy_metric, conventional).
narrative_ontology:cs_axiom('7eb0019c-5033-4b3f-901d-0b8d6600392d', foundational, investment_driven_development_is_optimal).
narrative_ontology:cs_axiom_status(investment_driven_development_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('7eb0019c-5033-4b3f-901d-0b8d6600392d', investment_driven_development_is_optimal, instrumental).
narrative_ontology:cs_reference_frame('7eb0019c-5033-4b3f-901d-0b8d6600392d', uninterrupted_high_gdp_growth).
narrative_ontology:cs_drift_state('7eb0019c-5033-4b3f-901d-0b8d6600392d', contemporary_era_of_sustainability_concerns, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7eb0019c-5033-4b3f-901d-0b8d6600392d', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, general_populace).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_advocates).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, small_and_medium_enterprises).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that defines and enforces the growth targets. Their legitimacy is directly tied to achieving these targets, leading to policies that prioritize GDP expansion above other concerns. They benefit from the stability and perceived success that high growth rates provide.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_leadership, agenda_setter,
    institutional, generational, identity_locked, national).

% Large state-owned enterprises and private companies heavily involved in export-oriented manufacturing and infrastructure. They receive preferential policies, subsidies, and access to capital, directly benefiting from the investment-driven growth model. Their success is measured by their contribution to GDP and exports.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    organized, biographical, constrained, global).

% Their career progression and performance evaluations are primarily based on the GDP growth rates achieved in their jurisdictions. This incentivizes them to pursue large-scale, often environmentally costly, infrastructure and industrial projects, sometimes leading to overcapacity and debt.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary,
    powerful, immediate, identity_locked, regional).

% While benefiting from job creation and some improvements in living standards, they bear the costs of environmental degradation, social inequality, and potential economic instability from an over-reliance on quantitative growth. Their consent is assumed through the promise of future prosperity.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, general_populace, payer,
    powerless, biographical, constrained, national).

% Bear the direct costs of pollution and resource depletion resulting from unchecked industrial expansion. Their efforts to push for sustainable practices are often suppressed or marginalized in favor of growth targets, making them targets of the constraint.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmental_advocates, payer,
    moderate, generational, constrained, local).

% Often struggle to compete with large state-backed entities for resources and market share. They face higher regulatory burdens and less access to capital, making them pay a disproportionate cost for an economic model that favors large-scale industrial players.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, small_and_medium_enterprises, payer,
    moderate, biographical, constrained, local).

% Monitor the growth rates and economic policies, seeking opportunities for investment. While not directly subject to the constraint's enforcement, their confidence in the growth narrative influences capital flows and external perceptions of legitimacy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, international_investors, observer,
    organized, immediate, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national economic policy and local government actions towards a singular, measurable goal of GDP growth, ensuring a unified direction for development and resource allocation.
% TRANSFER_FUNCTION: Transfers economic resources, policy support, and political capital from environmental protection, social welfare, and smaller enterprises to large-scale industrial projects and export-oriented sectors, in exchange for high GDP figures and job creation.
% ABSENT_VOICES: Genuine civil society organizations focused on environmental protection, labor rights, and equitable distribution of wealth are largely excluded from policy-making. They would advocate for a more balanced development model, but their concerns are often sidelined in favor of growth imperatives.
% DISAPPEARANCE_RATIONALE: If the legitimacy derived from quantitative GDP growth vanished overnight, the entire state-capitalist development model would collapse. Local officials would lose their primary performance metric, industrial complexes would lose their policy justification, and the state would need to rapidly redefine its social contract, leading to a fundamental reorganization of economic and political priorities.
% FOUNDING_PROBLEM: To rapidly industrialize and lift a large population out of poverty, requiring a clear, measurable, and enforceable metric for economic progress and governmental performance.
% FOUNDING_PROBLEM_CORROBORATION: State leadership and beneficiaries attest the problem is still live, citing ongoing development needs and global competition. Environmental advocates and some economists, from outside the benefiting parties, attest that while poverty reduction was achieved, the singular focus on GDP growth has created new, severe problems (environmental, social, debt) that the original framework cannot address, rendering the founding problem 'dead' in its original form.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a vast economic system towards a common goal (rapid development and job creation), but does so with significant asymmetric extraction. Extractiveness is high (0.78) due to the costs borne by the environment and segments of the populace, and the suppression of alternative development paths. Suppression is very high (0.85) because the state actively enforces this growth model and limits dissent. Theater ratio is moderate (0.45), reflecting that while some growth figures are genuine, there's also significant performative reporting and 'face-saving' projects that prioritize optics over genuine utility. The metrics show a trend of increasing extractiveness and suppression as the growth model matured and its costs became more apparent, with a slight recent dip reflecting attempts to rebalance.
 *
 * PERSPECTIVAL GAP:
 *   State leadership and the industrial-export complex perceive this as a necessary and effective Rope, delivering prosperity and stability. The general populace, environmental advocates, and SMEs experience it as a Snare, where the benefits are unevenly distributed and the costs are substantial and unavoidable. The engine's per-seat classification will reflect these divergent experiences based on their declared power, exit options, and roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The state leadership and local government officials are beneficiaries, as their legitimacy and careers are directly tied to the growth figures. The industrial-export complex also benefits from the policies. The general populace, environmental advocates, and SMEs are payers, bearing the social and environmental costs. The 'identity_locked' exit option for state and local officials reflects that their professional and political identities are fused with the success of this growth model, making exit from this paradigm unthinkable without a fundamental shift in the state's self-conception.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope, rather than a pure Snare, acknowledges the genuine coordination function in the early stages of development. However, the high and increasing extractiveness and suppression, coupled with the 'contested' status of the founding problem, suggest a strong drift towards a Snare. The system's persistence is increasingly due to the beneficiaries' capture of the growth narrative and the suppression of alternatives, rather than the universal benefit of the coordination. The high theater ratio indicates that the performance of growth is becoming more important than its actual quality or sustainability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_quality_vs_quantity,
    'Is the measured GDP growth genuinely improving overall societal well-being, or is it primarily driven by unsustainable investment and resource depletion?',
    'Longitudinal studies tracking genuine progress indicators (GPI), environmental impact assessments, and Gini coefficient trends, independent of official GDP reporting.',
    'If growth is found to be low-quality and unsustainable, the constraint''s effective extractiveness would be re-evaluated upwards, and its coordination function would be seen as a cover for resource depletion, pushing it closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(growth_quality_vs_quantity, empirical, 'Distinguishing between quantitative growth and qualitative development.').

omega_variable(
    legitimacy_source_shift,
    'To what extent has the state''s legitimacy shifted from genuine performance (solving poverty) to the mere maintenance of growth figures, regardless of their social or environmental cost?',
    'Analysis of public discourse, policy documents, and official rhetoric over time, comparing emphasis on ''growth'' versus ''quality of life'' or ''sustainability''.',
    'If the shift is substantial, the constraint''s ''coordination'' aspect becomes increasingly theatrical, and its classification would drift towards Piton or Snare, as the original mandate atrophies into a performance for power maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_shift, conceptual, 'Assessing the evolution of the state''s legitimacy grounding.').

omega_variable(
    overcapacity_and_debt_sustainability,
    'Are the investment-driven growth strategies leading to unsustainable levels of industrial overcapacity and local government debt, threatening future economic stability?',
    'Independent audits of local government balance sheets, analysis of industrial capacity utilization rates, and assessment of non-performing loans in the financial system.',
    'If overcapacity and debt are found to be severe and systemic, the long-term costs to the general populace would be re-evaluated upwards, increasing the constraint''s extractiveness and highlighting its potential for future collapse, reinforcing a Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(overcapacity_and_debt_sustainability, empirical, 'Evaluating the sustainability of the investment-driven growth model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t1980, performance_legitimacy__quantitative_growth_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(perf_tr_t1990, performance_legitimacy__quantitative_growth_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(perf_tr_t2000, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(perf_tr_t2010, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(perf_tr_t2020, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2020, 0.48).
narrative_ontology:measurement(perf_tr_t2024, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(perf_be_t1980, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(perf_be_t1990, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(perf_be_t2000, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(perf_be_t2010, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(perf_be_t2020, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2020, 0.79).
narrative_ontology:measurement(perf_be_t2024, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t1980, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(perf_su_t1990, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(perf_su_t2000, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(perf_su_t2010, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(perf_su_t2020, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2020, 0.87).
narrative_ontology:measurement(perf_su_t2024, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__quantitative_growth_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel. Its focus on quantitative growth influences, and is influenced by, other readings of state legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
