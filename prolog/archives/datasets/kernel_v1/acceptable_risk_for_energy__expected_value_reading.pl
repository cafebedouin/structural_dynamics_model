% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: acceptable_risk_for_energy__expected_value_reading
 *   human_readable: Expected Value Minimization in Collective Risk Decisions (Energy Policy Reading)
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   The expected-value reading of acceptable risk in energy policy
 *   instantiates one logical possibility within a contested kernel:
 *   collective decisions about infrastructure risk should minimize the
 *   probabilistically weighted sum of harm across all possible outcomes,
 *   weighted by their estimated probability. This reading dominates
 *   contemporary risk regulation (nuclear power, industrial facilities,
 *   transportation infrastructure) through Probabilistic Safety Assessment
 *   (PSA) methodologies. The constraint exhibits the defining signature of a
 *   reading in a kernel dispute: it appears natural and rational from
 *   institutional perspectives (energy developers, regulators, climate
 *   coalitions) but structurally imposes extraction on populations exposed to
 *   tail-event catastrophes estimated below regulatory thresholds. The
 *   reading forecloses precautionary logic (which demands accounting for
 *   irreducible uncertainty in probability estimation) and influences but
 *   does not entirely foreclose catastrophic-tail logic (which demands
 *   prevention of maximum credible disasters). The core axiom distinguishing
 *   this reading is that low-probability events should be weighted by their
 *   probability in optimization — a simple claim that becomes a contested
 *   commitment when populations differ in their exposure to tail-event risk
 *   and their ability to exit the risk-bearing zones.
 *
 * KEY AGENTS:
 *   - Low-Probability Disaster Populations: Primary victim (powerless/trapped) — forced exposure to tail-event risk estimated below regulatory thresholds; bear extraction with zero agency in the optimization
 *   - Energy Infrastructure Developers: Primary beneficiary (institutional/arbitrage) — can justify high-capacity projects (renewables, nuclear, industrial facilities) by expected-value logic that catastrophic-tail reading would block
 *   - Regulatory Risk Assessors: Institutional secondary actor (moderate/constrained) — coordinating PSA reviews while facing industry influence and political pressure to approve infrastructure
 *   - Climate Advocacy Coalition: Organized secondary actor (organized/constrained) — benefit from expected-value logic permitting climate-critical infrastructure; suppressed in voicing precautionary concerns
 *   - PSA Methodological System: Institutional process (institutional/arbitrage) — provides legitimacy cover for decisions constrained by political/economic factors; maintains theatrical rigor through mathematical complexity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional reading as a law of rational decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_reading, 0.58).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_reading, 0.68).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_reading, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_reading, "Expected Value Minimization in Collective Risk Decisions (Energy Policy Reading)").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_reading, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_reading, '283cf16e-c6ab-46d3-a9a8-69aae76d4d2b').
narrative_ontology:cs_kernel_codification('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b', formalized).
narrative_ontology:cs_authority_grounding('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b', expertise).
narrative_ontology:cs_interpretation_layer_present('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b').
narrative_ontology:cs_reading_relation('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b', acceptable_risk_for_energy__catastrophic_tail_reading, coexists_with).
narrative_ontology:cs_reading_relation('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b', acceptable_risk_for_energy__precautionary_reading, forecloses).
narrative_ontology:cs_axiom('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b', foundational, probabilistic_aggregation_of_outcomes).
narrative_ontology:cs_axiom_status(probabilistic_aggregation_of_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b', probabilistic_aggregation_of_outcomes, instrumental).
narrative_ontology:cs_axiom('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b', foundational, tail_event_probability_estimation_sufficiency).
narrative_ontology:cs_axiom_status(tail_event_probability_estimation_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b', tail_event_probability_estimation_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b', probabilistic_safety_assessment_framework).
narrative_ontology:cs_drift_state('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b', contemporary_climate_urgency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('283cf16e-c6ab-46d3-a9a8-69aae76d4d2b', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_reading, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_reading, energy_infrastructure_developers).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_reading, aggregate_climate_benefits_recipients).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_reading, low_probability_disaster_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_reading, risk_asymmetry_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATISTICALLY INVISIBLE VICTIM (SNARE) — Residents in zones designated 'acceptable risk' under expected-value calculus face forced exposure to tail-event catastrophe. Their exit options are trapped (relocation costs prohibitive, property legally encumbered by zoning, local employment tied to industrial presence). The expected-value framework explicitly excludes their voice from the decision calculus — they are not represented in the optimization. Maximum extraction with no agency.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__expected_value_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGULATORY RISK ASSESSOR (TANGLED ROPE) — Risk analysts coordinating PSA (Probabilistic Safety Assessment) reviews face genuine coordination function: aggregating disparate failure-mode data into a coherent risk picture enables engineering improvements and resource allocation. Simultaneously, they bear asymmetric extraction: their career and reputation depend on not blocking projects (industry influence, political pressure to approve infrastructure). Constrained by methodological commitments to expected-value logic and regulatory timelines. Mixed function — genuine technical coordination with embedded extraction pressure.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__expected_value_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENERGY INFRASTRUCTURE DEVELOPER (ROPE) — Benefits from expected-value framework: can justify projects by showing net climate benefit exceeds acceptable risk thresholds. Experiences the constraint as coordination: PSA protocols enable transparent negotiation with regulators, insurers, and investors. The framework allows projects that catastrophic-tail logic would block. Net beneficiary with arbitrage options (can shift projects between jurisdictions with different acceptable-risk thresholds).
constraint_indexing:constraint_classification(acceptable_risk_for_energy__expected_value_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE ADVOCACY COALITION (TANGLED ROPE) — Organized agents (climate groups, public health bodies) benefit from expected-value logic when it permits climate-critical infrastructure (renewables, grid modernization) that catastrophic-tail reading would block. Simultaneously, they bear extraction: the framework suppresses their voice on behalf of statistically invisible populations and forecloses discussion of irreducible uncertainty in probability estimates (precautionary reading). Constrained by political coalitions that demand infrastructure progress. Genuine climate coordination benefit with embedded suppression of alternative risk frameworks.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__expected_value_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PSA METHODOLOGICAL RITUAL (PITON) — Probabilistic Safety Assessment procedures are substantially performative: uncertainty quantification in low-probability tail events is irreducibly subjective (expert elicitation, historical data extrapolation, model selection all embed hidden assumptions). The ritual of PSA review persists because it provides legitimacy to decisions already constrained by political/economic factors. Reviewers know their uncertainty bounds don't capture black-swan events but the process maintains the theatrical appearance of quantitative rigor. Theater ratio elevated by the ritual's mathematical legitimacy cover.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__expected_value_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EXPECTED VALUE AS NATURAL LAW (MOUNTAIN) — From a universal perspective, expected-value logic appears as a law of rational decision-making: any alternative framework is supposedly irrational, incoherent, or unprincipled. The constraint naturalizes a specific axiom (probabilistic aggregation) as a mathematical truth. However, the structural data contradicts this — the framework systematically excludes certain populations (low-probability victims) and suppresses epistemic alternatives (precautionary framework). The engine's false-summit detector will identify this as naturalization of a contingent institutional choice, not a law of decision theory.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__expected_value_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acceptable_risk_for_energy__expected_value_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acceptable_risk_for_energy__expected_value_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_reading, TR),
    TR >= 0.70.

:- end_tests(acceptable_risk_for_energy__expected_value_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The expected-value framework creates genuine coordination benefits (PSA enables transparent engineering-focused negotiation) but systematically benefits developers and climate coalitions while imposing uncompensated tail-event risk on statistically invisible populations. The extraction is neither minimal (rope-level) nor maximal (pure snare) because some agents (regulators, risk assessors) achieve mixed coordination-extraction and some benefit from climate outcomes. The trajectory shows extractiveness rising over time (0.42 → 0.58) as the framework normalizes through institutional adoption and as climate urgency increases pressure for project approval. Suppression (0.68): Moderate-high and rising. The framework suppresses alternative risk logics (precautionary, catastrophic-tail) by treating expected-value as the only rational framework. It suppresses voice of low-probability victim populations by defining them outside the victim-set for regulatory purposes. It suppresses epistemic uncertainty about tail-event probability estimation by embedding it in black-box expert elicitation. Theater ratio (0.62): Moderate-high. PSA methodology maintains appearance of quantitative rigor while embedding substantial subjective judgment (expert elicitation for low-probability estimates, model selection, probability distribution choice). The ritual persists because it provides legitimacy to decisions already constrained by political-economic factors. The rising trajectory (0.48 → 0.62) reflects increasing mathematical sophistication that obscures rather than resolves fundamental uncertainty in tail-event probability estimation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the kernel dispute perfectly across its six perspectives. The energy developer sees coordination (Rope) — PSA enables transparent infrastructure negotiation. The regulatory assessor sees mixed coordination-extraction (Tangled Rope) — genuine technical coordination with embedded career/reputational pressure. The statistically invisible victim sees pure extraction (Snare) — forced risk exposure with no agency. The climate coalition sees mixed coordination-extraction (Tangled Rope) — climate benefit with suppressed precautionary voice. The methodological system sees its own degraded ritual (Piton) — mathematical legitimacy cover for decisions made elsewhere. The civilizational analytical observer risks seeing rationality as natural law (Mountain) — expected-value optimization presented as the only coherent framework. The perspectival gaps reveal that 'acceptable risk' is not an objective threshold but an institutional choice that benefits some and imposes costs on others.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness depends on their structural relationship to the expected-value optimization. Beneficiaries (developers with arbitrage options across jurisdictions) experience low or negative chi despite moderate extractiveness. Victims (trapped populations) experience maximum chi — high base extractiveness with high suppression and no exit. Moderate agents (regulators) experience middle chi — some coordination benefit, some extraction pressure. Organized agents with constrained exits (climate coalitions) experience moderate-high chi — genuine climate benefit offset by suppression of alternative frameworks. The directionality derivation shows how institutional power + beneficiary status → low d → negative f(d) conversely, powerless + victim + trapped → high d → high f(d). The analytical perspective risks d=0.72 (observer is statistically outside the extraction flow, yet systemically complicit in naturalizing the framework).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probability_estimation_irreducibility,
    'Are low-probability tail-event probabilities meaningfully estimable or is the quantification itself an artefact of methodological choice?',
    'Historical comparison of PSA estimates vs actual outcomes for events in the 10^-6 to 10^-7 per reactor-year range; analysis of expert-elicitation calibration and model-selection sensitivity',
    'If probabilities are meaningfully estimable: expected-value logic is coherent (mountain-adjacent). If estimation is irreducibly uncertain: precautionary reading becomes structurally necessary (forecloses expected-value reading''s exclusion of uncertainty).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_estimation_irreducibility, empirical, 'Whether low-probability tail-event probabilities are meaningfully estimable').

omega_variable(
    victim_set_externality_scope,
    'Who is a victim in the expected-value framework? Does it include populations exposed to tail-event catastrophe whose probability was estimated below regulatory thresholds?',
    'Regulatory review of PSA victim-set definitions; comparison with catastrophic-tail reading''s victim-set; historical analysis of actual disaster populations vs pre-disaster regulatory victim categorization',
    'If victim set includes low-probability exposure populations: framework becomes snare for them (high suppression). If victim set excludes them (standard practice): they are invisible in the optimization, creating the perspectival gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_externality_scope, conceptual, 'Whether PSA victim-set includes low-probability disaster populations').

omega_variable(
    alternative_risk_axioms_foreclosure,
    'Does expected-value optimization logically foreclose precautionary and catastrophic-tail readings, or do they coexist as alternative coherent frameworks?',
    'Logical analysis of axiom compatibility; institutional history of risk frameworks in energy regulation; whether regulators can hold multiple frameworks simultaneously (expected-value for day-to-day, precautionary for governance of catastrophic risk)',
    'If foreclosed: expected-value reading is the only coherent framework (mountain adjacent). If coexistent: the reading_relations are coexists_with (both readings live simultaneously in different parties'' commitments). If influences: expected-value creates downstream pressure on alternatives without eliminating them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_risk_axioms_foreclosure, conceptual, 'Whether alternative risk axioms are foreclosed by expected-value reading or coexistent').

omega_variable(
    climate_benefit_asymmetry_justification,
    'Does the magnitude of climate benefits justify the distribution of risk costs? Are climate beneficiaries (global future populations) the same as risk bearers (local present populations)?',
    'Cost-benefit analysis with explicit beneficiary/victim mapping; analysis of whether climate benefits flow to same populations as risk exposure; historical institutional decisions on cross-group risk-benefit tradeoffs',
    'If asymmetry is unjustified: the expected-value reading is revealed as extraction mechanism (snare/tangled-rope confirmation). If justified: constraint is genuinely coordination (rope-adjacent).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_benefit_asymmetry_justification, preference, 'Whether climate benefits justify asymmetric risk distribution').

omega_variable(
    regulatory_threshold_stability,
    'Are ''acceptable risk'' thresholds (e.g., 10^-6 individual risk per year for nuclear, 10^-5 for industrial facilities) scientifically derived or institutionally determined?',
    'Historical institutional choice analysis; international comparative regulatory thresholds; correlation between thresholds and industrial advocacy vs public health pressure',
    'If scientifically derived: framework legitimacy is higher (mountain-adjacent). If institutionally determined: thresholds are contingent extraction surfaces (snare properties confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_threshold_stability, empirical, 'Whether acceptable-risk thresholds are scientifically or institutionally determined').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acceptable_risk_ev_tr_t0, acceptable_risk_for_energy__expected_value_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(acceptable_risk_ev_tr_t3, acceptable_risk_for_energy__expected_value_reading, theater_ratio, 3, 0.55).
narrative_ontology:measurement(acceptable_risk_ev_tr_t6, acceptable_risk_for_energy__expected_value_reading, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(acceptable_risk_ev_be_t0, acceptable_risk_for_energy__expected_value_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(acceptable_risk_ev_be_t3, acceptable_risk_for_energy__expected_value_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(acceptable_risk_ev_be_t6, acceptable_risk_for_energy__expected_value_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(acceptable_risk_ev_su_t0, acceptable_risk_for_energy__expected_value_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(acceptable_risk_ev_su_t3, acceptable_risk_for_energy__expected_value_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(acceptable_risk_ev_su_t6, acceptable_risk_for_energy__expected_value_reading, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_reading, acceptable_risk_for_energy__catastrophic_tail_reading).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_reading, acceptable_risk_for_energy__precautionary_reading).

% DUAL FORMULATION NOTE:
% The acceptable_risk_for_energy kernel generates three constraint stories, one for each reading of how collective risk decisions should be made. The expected_value_reading (this story) has extractiveness 0.58 and forecloses the precautionary reading while coexisting with the catastrophic-tail reading. The three stories are linked by network.affects_constraints to enable contamination propagation analysis across readings. Each reading has its own ε, beneficiary/victim structure, and classification landscape. The kernel dispute is not resolvable within expected-value logic (mountain framing) — it requires cross-reading analysis (precautionary framework's irreducibility claim, catastrophic-tail framework's maximum-credible-event logic).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
