% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__expected_value_dominant, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Acceptable Risk Metric for Energy Pathways
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   The expected-value-dominant reading of acceptable risk in energy policy
 *   uses mortality-per-TWh as a universal comparator. It claims to be a
 *   neutral coordination tool (rope) but structurally extracts from fossil
 *   pathways by counting all attributable deaths at full weight while
 *   probabilistically discounting low-probability nuclear catastrophes. The
 *   metric is enforced through international financing criteria (World Bank,
 *   EIB), national regulation, and climate finance conditionality.
 *   Fossil-dependent workers, communities, and nations bear concentrated
 *   costs; nuclear and renewable sectors gain concentrated benefits. The
 *   coordination function (common metric) is real but the asymmetric
 *   extraction and active suppression of fossil pathways make this a tangled
 *   rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.72).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.78).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value-Dominant Acceptable Risk Metric for Energy Pathways").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '60ddfa41-2991-41e4-a3d1-400f6dd5bac1').
narrative_ontology:cs_kernel_codification('60ddfa41-2991-41e4-a3d1-400f6dd5bac1', formalized).
narrative_ontology:cs_authority_grounding('60ddfa41-2991-41e4-a3d1-400f6dd5bac1', expertise).
narrative_ontology:cs_interpretation_layer_present('60ddfa41-2991-41e4-a3d1-400f6dd5bac1').
narrative_ontology:cs_reading_relation('60ddfa41-2991-41e4-a3d1-400f6dd5bac1', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('60ddfa41-2991-41e4-a3d1-400f6dd5bac1', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('60ddfa41-2991-41e4-a3d1-400f6dd5bac1', foundational, expected_value_optimality_for_energy_risk).
narrative_ontology:cs_axiom_status(expected_value_optimality_for_energy_risk, holdable).
narrative_ontology:cs_axiom_grounding('60ddfa41-2991-41e4-a3d1-400f6dd5bac1', expected_value_optimality_for_energy_risk, instrumental).
narrative_ontology:cs_axiom('60ddfa41-2991-41e4-a3d1-400f6dd5bac1', foundational, probabilistic_discounting_of_low_probability_high_consequence_events).
narrative_ontology:cs_axiom_status(probabilistic_discounting_of_low_probability_high_consequence_events, holdable).
narrative_ontology:cs_axiom_grounding('60ddfa41-2991-41e4-a3d1-400f6dd5bac1', probabilistic_discounting_of_low_probability_high_consequence_events, empirically_contingent).
narrative_ontology:cs_reference_frame('60ddfa41-2991-41e4-a3d1-400f6dd5bac1', quantitative_comparative_risk_assessment).
narrative_ontology:cs_drift_state('60ddfa41-2991-41e4-a3d1-400f6dd5bac1', post_paris_agenda_net_zero_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('60ddfa41-2991-41e4-a3d1-400f6dd5bac1', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, renewable_energy_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, energy_policy_makers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_workers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, energy_intensive_industries).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, developing_nations_relying_on_fossil).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, expected_value_decision_rule).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, probabilistic_risk_assessment).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, comparative_energy_mortality_metrics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains policy legitimacy and investment from mortality-per-TWh metrics that show nuclear as low-risk; probabilistic discounting of accidents makes nuclear appear safer than fossil alternatives. Exit constrained by high capital lock-in and regulatory dependence.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_industry, beneficiary,
    powerful, biographical, constrained, national).

% Uses the metric to argue for rapid decarbonization; the framework validates renewables as low-mortality. Can shift advocacy strategies but remains committed to the metric as a policy lever.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, renewable_energy_advocates, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, renewable_energy_advocates, agenda_setter).

% Bear concentrated job losses and community decline as policy shifts away from fossil pathways justified by the metric. Skills and geographic ties trap them; retraining is partial and uncertain.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_workers, payer,
    organized, biographical, trapped, regional).

% Experience fiscal collapse, population loss, and health degradation as extraction winds down. No meaningful exit; the metric renders their pathway illegitimate in policy discourse.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_communities, payer,
    powerless, generational, trapped, local).

% Face rising energy costs and regulatory burden as fossil baseload is suppressed; can relocate or switch fuels but at high capital cost and competitiveness risk.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_intensive_industries, payer,
    powerful, biographical, constrained, national).

% Depend on cheap fossil energy for development; the metric legitimizes international pressure to forego fossil pathways before affordable alternatives exist. Exit constrained by finance and technology access.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, developing_nations_relying_on_fossil, payer,
    moderate, generational, constrained, national).

% Adopt and enforce the mortality-per-TWh framework as the standard for energy decisions; the metric simplifies complex trade-offs into a single comparable number. Can revise the framework but face institutional inertia.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% Provide the underlying mortality data and climate projections that feed the metric; observe its policy uptake and note omissions (land use, justice, systemic risk).
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, climate_scientists, observer,
    institutional, civilizational, analytical, global).

% Argue that low-probability high-impact events (nuclear meltdown, runaway climate) dominate risk calculus; their decision rule is structurally excluded by the expected-value framing.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, catastrophic_tail_advocates, excluded,
    moderate, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, quantitative, cross-pathway metric (mortality per TWh) that enables governments, financiers, and international bodies to compare energy options on a common basis and coordinate policy, investment, and regulation.
% TRANSFER_FUNCTION: Moves policy support, public investment, regulatory leniency, and social license from fossil pathways (where all air pollution and mining deaths count at full weight) to nuclear and renewable pathways (where nuclear accidents are probability-discounted and renewables show near-zero mortality), concentrating transition costs on fossil-dependent workers, communities, and nations.
% ABSENT_VOICES: Fossil fuel dependent communities and workers, nations without nuclear infrastructure or capital for rapid renewable deployment, and catastrophic-tail-risk advocates are excluded from the metric's design and its policy adoption. They would object to the probabilistic discounting of nuclear catastrophe and the full-weight counting of chronic fossil harms without parallel accounting for climate tail risk.
% DISAPPEARANCE_RATIONALE: If the mortality-per-TWh framework vanished overnight, energy policy would lose its dominant comparative scaffold. Investment would revert to national criteria (cost, security, employment), likely increasing fossil use in developing nations, altering nuclear new-build calculus, and fragmenting international climate finance criteria.
% FOUNDING_PROBLEM: Post-1970s energy crises and early climate awareness created demand for a unified, quantitative basis to compare health impacts across energy systems — replacing ad hoc national assessments with a common metric that could guide policy, investment, and regulation.
% FOUNDING_PROBLEM_CORROBORATION: IAEA and WHO joint studies (e.g., 2006 Chernobyl Forum, comparative risk assessments) corroborate the metric's utility for cross-technology comparison. Fossil-dependent nations (OPEC, major coal producers), labor unions (industrial, mining), and catastrophic-tail-risk scholars contest its completeness — noting omitted climate tail risk, land-use mortality, energy poverty deaths, and distributive justice.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the metric systematically shifts policy burden onto fossil pathways while crediting nuclear/renewables with probabilistic discounts. Suppression (0.78) is higher because the constraint's persistence depends on active exclusion of fossil options from finance and policy — not merely participant preference. Theater ratio (0.38) is moderate: the comparative metric genuinely solves a coordination problem (pre-1980s energy risk assessment was fragmented), but a growing share of enforcement activity defends the asymmetric weighting rather than the coordination function itself. Accessibility collapse (0.68) reflects that fossil pathways are increasingly illegitimate in formal policy space. Resistance (0.45) is moderate — fossil interests resist but the metric is embedded in international institutions.
 *
 * PERSPECTIVAL GAP:
 *   From the policy-maker/analyst seat, the metric is a genuine coordination advance — a single number replacing incommensurable national assessments. From the fossil-worker/community seat, the same structure operates as enforced extraction: their livelihoods are discounted by a probabilistic framing they had no role in designing. From the catastrophic-tail advocate seat, the metric is a category error: it optimizes the wrong objective (expected value vs. ruin avoidance). The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry and renewable advocates are structural beneficiaries (d near 0.1-0.2): they collect policy rents, investment flows, and regulatory preference. Fossil workers and communities are full targets (d near 0.9-1.0): trapped, identity-locked to carbon geographies, bearing concentrated costs. Energy-intensive industries and developing nations are constrained targets (d ~0.6-0.7): powerful enough to negotiate but structurally disadvantaged by the metric. Policy makers sit near symmetric (d ~0.4-0.5): they administer the framework and gain coordination capacity but face political blowback. Catastrophic-tail advocates are excluded (d undefined): their decision rule is structurally incompatible with the metric's premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unified quantitative risk comparison) remains live but the metric's scope has expanded beyond its original justification. What began as a tool for comparing electricity generation risks now governs economy-wide energy transition finance, suppressing fossil pathways in contexts where alternatives are not yet viable. The mandate has atrophied into a general prohibition tool — the coordination function is real but the extraction has accumulated. This is not a pure snare (coordination exists) nor a pure rope (extraction is asymmetric and enforced). Tangled rope captures the dual structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the expected_value_dominant reading''s structural relationship to the acceptable_risk_energy kernel affect its classification, given sibling readings catastrophic_tail_dominant and option_value_preserving?',
    'Compare the three readings'' beneficiary/victim structures, suppression mechanisms, and metric referents. If they share the same referent but author different ε, they are distinct constraints per ε-invariance — document the structural delta for each.',
    'If the kernel is treated as a single constraint, the ε-invariance principle is violated — different readings produce different ε for the same label. The decomposition into three constraint stories with network links is the correct modeling choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment: this is one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    suppression_mechanism_fossil_pathway,
    'Is the suppression of fossil pathways structural (finance exclusion, regulatory prohibition) or internalized (industry self-censorship, worker identity fusion with carbon economies)?',
    'Track fossil project cancellations: if projects die from external capital withdrawal, suppression is structural; if firms voluntarily redirect capital anticipating policy, internalized component exists. Post-exit trajectory of fossil-dependent communities: persistent decline suggests internalized suppression.',
    'If internalized, effective suppression is higher than structural measure — targets carry the constraint with them. This would increase χ for trapped payer seats and strengthen tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_fossil_pathway, empirical, 'Structural vs. internalized suppression of fossil energy pathways.').

omega_variable(
    metric_completeness_mortality_only,
    'Does mortality-per-TWh capture the decision-relevant harms, or does its omission of climate tail risk, land-use mortality, energy poverty deaths, and distributive justice make it a partial metric masquerading as complete?',
    'Systematic review of excluded harm categories: quantify climate mortality (tail and chronic), land-use displacement deaths, energy access mortality, and compare magnitude to included air-pollution mortality. If excluded categories are comparable or larger, the metric is structurally incomplete.',
    'If metric is incomplete, the coordination function is defective — the constraint coordinates on a partial basis, increasing its extractive character. This would raise extractiveness and support tangled_rope over rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_completeness_mortality_only, empirical, 'Whether mortality-per-TWh is a complete or partial risk metric for energy decisions.').

omega_variable(
    nuclear_accident_probability_discount,
    'Is the probabilistic discounting of nuclear accidents (e.g., 0.0001/year core damage frequency) empirically warranted, or does it reflect industry-sourced probabilistic risk assessment with known blind spots (common-cause failures, human factors, beyond-design-basis events)?',
    'Compare PRA predictions against historical accident frequency (Three Mile Island, Chernobyl, Fukushima, near-misses). If observed frequency exceeds PRA by orders of magnitude, the discount is structurally unjustified.',
    'If discount is unwarranted, nuclear''s beneficiary status is artifact of a flawed metric — the constraint extracts from fossil while gifting nuclear an unearned advantage. This would increase extractiveness and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_accident_probability_discount, empirical, 'Empirical validity of nuclear accident probability estimates used in mortality-per-TWh.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arevd_tr_t1980, acceptable_risk_energy__expected_value_dominant, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(arevd_tr_t1990, acceptable_risk_energy__expected_value_dominant, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(arevd_tr_t2000, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(arevd_tr_t2010, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(arevd_tr_t2015, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(arevd_tr_t2020, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2020, 0.37).
narrative_ontology:measurement(arevd_tr_t2025, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(arevd_be_t1980, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(arevd_be_t1990, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(arevd_be_t2000, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(arevd_be_t2010, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement(arevd_be_t2015, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(arevd_be_t2020, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(arevd_be_t2025, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(arevd_su_t1980, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(arevd_su_t1990, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(arevd_su_t2000, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(arevd_su_t2010, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(arevd_su_t2015, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(arevd_su_t2020, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2020, 0.76).
narrative_ontology:measurement(arevd_su_t2025, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, information_standard).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__expected_value_dominant, 0.03).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint, catastrophic_tail_dominant, and option_value_preserving form the acceptable_risk_energy constraint family — three structurally distinct readings of the same kernel. Each has its own ε, beneficiaries, victims, and classification. They are linked via affects_constraints because the expected-value reading is often cited as the 'standard' against which tail-risk and option-value readings position themselves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, organized, 0.85).
constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
