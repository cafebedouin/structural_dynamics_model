% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation as Legitimate Climate Response
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   This constraint instantiates the degrowth_transformation reading of the
 *   contested climate_response_legitimacy kernel. It posits that wealthy
 *   nations must dismantle the growth imperative through universal basic
 *   services, working-time reduction, and democratic firm ownership in order
 *   to achieve legitimate climate policy. The current generation in developed
 *   economies enters the cost-bearer set through structural economic change,
 *   while future generations and vulnerable Global South populations benefit
 *   from reduced warming without relying on uncertain technological
 *   decoupling. The arrangement faces severe political feasibility barriers
 *   and high resistance from growth-dependent publics and fossil capital.
 *
 * KEY AGENTS:
 *   - developed_economy_citizens: Primary target (organized / constrained exit) â bears income reduction, ownership restructuring, and working-time reallocation
 *   - future_generations: Primary beneficiary (powerless / trapped) â receives climate stability and technological independence but has no present voice
 *   - global_south_vulnerable: Secondary beneficiary (powerless / trapped) â benefits from reduced warming and basic-services transfers without exit from the climate system
 *   - fossil_fuel_incumbents: Concentrated payer (powerful / arbitrage exit) â faces stranded assets and loss of ownership control but can shift capital globally
 *   - transformation_policy_elites: Agenda setter (institutional / mobile) â designs and administers the structural transformation
 *   - climate_research_community: Analytical observer (analytical / analytical exit) â provides the empirical and economic framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.75).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.72).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.75).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation as Legitimate Climate Response").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, '0e3af71e-6f64-4c91-879c-40a9884fce8e').
narrative_ontology:cs_kernel_codification('0e3af71e-6f64-4c91-879c-40a9884fce8e', formalized).
narrative_ontology:cs_authority_grounding('0e3af71e-6f64-4c91-879c-40a9884fce8e', expertise).
narrative_ontology:cs_interpretation_layer_present('0e3af71e-6f64-4c91-879c-40a9884fce8e').
narrative_ontology:cs_reading_relation('0e3af71e-6f64-4c91-879c-40a9884fce8e', climate_response_legitimacy__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('0e3af71e-6f64-4c91-879c-40a9884fce8e', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('0e3af71e-6f64-4c91-879c-40a9884fce8e', foundational, growth_imperative_incompatible_with_planetary_boundaries).
narrative_ontology:cs_axiom_status(growth_imperative_incompatible_with_planetary_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('0e3af71e-6f64-4c91-879c-40a9884fce8e', growth_imperative_incompatible_with_planetary_boundaries, empirically_contingent).
narrative_ontology:cs_axiom('0e3af71e-6f64-4c91-879c-40a9884fce8e', foundational, present_generation_obligation_to_divest_growth).
narrative_ontology:cs_axiom_status(present_generation_obligation_to_divest_growth, holdable).
narrative_ontology:cs_axiom_grounding('0e3af71e-6f64-4c91-879c-40a9884fce8e', present_generation_obligation_to_divest_growth, deontological).
narrative_ontology:cs_reference_frame('0e3af71e-6f64-4c91-879c-40a9884fce8e', post_growth_social_contract).
narrative_ontology:cs_drift_state('0e3af71e-6f64-4c91-879c-40a9884fce8e', contemporary_growth_dependency, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0e3af71e-6f64-4c91-879c-40a9884fce8e', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, global_south_vulnerable).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, developed_economy_citizens).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, fossil_fuel_incumbents).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, degrowth_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, intergenerational_justice).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, distributive_climate_justice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit the climate outcome of present decisions. They benefit from reduced warming and reduced dependency on uncertain future technology, but have no present political standing and cannot exit the temporal order.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Live in wealthy nations where the constraint demands reduced working hours, income compression, and restructuring of firm ownership. They experience this as a direct reduction in consumption possibilities and economic autonomy. Exit is constrained by national policy reach and by cultural fusion with the growth imperative.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, developed_economy_citizens, payer,
    organized, biographical, constrained, continental).

% Reside in climate-vulnerable regions that benefit from reduced global emissions and from basic-services transfers enabled by wealthy-nation transformation. They lack direct voice in wealthy-nation policy and cannot exit the global climate system.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, global_south_vulnerable, beneficiary,
    powerless, generational, trapped, global).

% Design and advocate for universal basic services, working-time reduction, and democratic ownership mandates. They occupy institutional positions in climate ministries, research institutes, and advocacy organizations. Their exit is mobile across policy domains and jurisdictions.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, transformation_policy_elites, agenda_setter,
    institutional, generational, mobile, global).

% Control carbon-intensive capital stocks and political networks in wealthy nations. They bear concentrated losses from stranded assets and ownership restructuring. Their exit options include capital flight and lobbying, but policy barriers are rising.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, fossil_fuel_incumbents, payer,
    powerful, biographical, arbitrage, global).

% Provides empirical climate projections and ecological-economics analysis that underpin the degrowth framing. Serves as an analytical seat without direct extraction or payment.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_research_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__degrowth_transformation, diffuse).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents catastrophic climate change and delivers intergenerational justice by reducing emissions through structural economic transformation in wealthy nations, replacing technological dependency with direct throughput reduction.
% TRANSFER_FUNCTION: Moves economic surplus, working time, and productive capacity from current wealthy-nation consumption and carbon-intensive capital accumulation to global climate stability and vulnerable-population resilience, while reducing aggregate throughput.
% ABSENT_VOICES: Future generations cannot speak in present policy forums; fossil fuel workers and developing-country industrializers are underrepresented in degrowth policy design; technological optimists and green-growth advocates are excluded from legitimacy in this framing.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, wealthy nations would revert to growth-dependent emissions trajectories, intergenerational climate debt would accumulate, and the policy architecture for universal basic services and democratic ownership would dissolve. The global climate response would reorganize around technological mitigation or adaptation.
% FOUNDING_PROBLEM: Uncontrolled greenhouse gas emissions driven by growth-dependent economies in wealthy nations, creating intergenerational injustice and existential climate risk.
% FOUNDING_PROBLEM_CORROBORATION: Climate science (IPCC) attests the emissions problem from outside the beneficiary set. However, the degrowth-specific solution is primarily attested by degrowth scholars and climate-justice movements. Green-growth economists and technological optimists dispute that dismantling growth is necessary, and no neutral third party corroborates the specific structural remedy.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75 at interval end) because the constraint demands substantial current consumption and capital accumulation be foregone in wealthy nations. Suppression is high (0.72) because implementation requires overcoming the deeply embedded growth imperative through sustained policy enforcement against powerful incumbents and resistant publics. Theater ratio is low (0.25): the reading is relatively transparent about costs and does not disguise extraction behind a fake coordination story. Accessibility collapse is moderate (0.48): this reading delegitimizes green-growth and techno-optimist alternatives by framing them as insufficient, but they remain intellectually available. Resistance is high (0.70) due to the political feasibility barrier and organized opposition from fossil capital and growth-dependent publics. The measurement series run on a single shared grid to prevent misaligned dating of transitions.
 *
 * PERSPECTIVAL GAP:
 *   The developed-economy citizen seat experiences the constraint as heavy extraction demanding present sacrifice for diffuse future benefit. The future-generations seat experiences it as necessary protection. The policy-elite seat experiences it as legitimate coordination justified by planetary boundaries. The engine computes these divergent seat classifications from the same structural data: beneficiary versus victim status, trapped versus constrained versus arbitrage exit, and power asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed economy citizens are declared victims with constrained exit and moderate organized power, placing them near the full-target end (high d, high effective extraction). Fossil fuel incumbents are declared victims but with arbitrage-grade exit, which damps their d toward the middle relative to trapped citizens. Future generations and Global South vulnerable populations are declared beneficiaries with trapped exit, placing them near the full-beneficiary end (low d, effective extraction damped or inverted into subsidy). Transformation policy elites are agenda setters with mobile exit; their structural position is administrative rather than extractive, and they are neither beneficiaries nor victims in the receipt sense.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the arrangement as a pure snare: there is a genuine coordination function (climate stability, intergenerational justice) that is not merely cover for extraction. It also prevents mislabeling as a pure rope: the cost-bearing is sharply asymmetric, concentrated on present wealthy-nation publics and capital, and the constraint cannot persist without active enforcement against powerful resistance. If the founding problem (uncontrolled emissions) were dead but the arrangement persisted, it would drift toward piton; here the founding problem remains live, though contested in its specific remedy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_transient_or_structural,
    'Is the political infeasibility of degrowth transformation a transient democratic bargaining problem or a structural feature of growth-dependent political economies?',
    'Comparative historical analysis of societies that successfully reduced throughput versus those that failed; measurement of institutional veto-point density in wealthy democracies.',
    'If structural, the constraint can only be implemented through escalating suppression, shifting it toward snare. If transient, the constraint may mature into a rope as political coalitions stabilize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_feasibility_transient_or_structural, empirical, 'Whether implementation barriers are temporary or endemic.').

omega_variable(
    growth_imperative_naturality,
    'Is the growth imperative a removable social construct or an emergent structural feature of complex economies?',
    'Empirical study of steady-state and degrowth pilot economies; analysis of whether zero-growth societies sustain institutional complexity without collapse.',
    'If the growth imperative is mountain-like, the constraint''s extraction fights a natural law and suppression must rise indefinitely. If it is constructed, the constraint is a tangled rope or scaffold with finite enforcement needs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(growth_imperative_naturality, conceptual, 'Natural vs constructed status of the growth imperative.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state enforcement against protesters and capital flight) or internalized (citizens believe growth is necessary for welfare)?',
    'Post-implementation suppression trajectory: if resistance collapses after policy adoption, suppression was primarily internalized; if resistance intensifies, structural.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest â the target population carries the constraint even after exit routes open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__degrowth_transformation, theater_ratio, 10, 0.19).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__degrowth_transformation, theater_ratio, 20, 0.21).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__degrowth_transformation, theater_ratio, 30, 0.23).
narrative_ontology:measurement(clim_tr_t40, climate_response_legitimacy__degrowth_transformation, theater_ratio, 40, 0.24).
narrative_ontology:measurement(clim_tr_t50, climate_response_legitimacy__degrowth_transformation, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(clim_be_t40, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(clim_be_t50, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 50, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(clim_su_t40, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(clim_su_t50, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_legitimacy kernel, which decomposes into three structurally distinct claims: mitigation_priority ( techno-decoupling, growth-preserving), adaptation_priority (warming acceptance, resilience), and degrowth_transformation (growth dismantling, structural redistribution). Each reading has a different beneficiary/victim structure and extractiveness profile. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
