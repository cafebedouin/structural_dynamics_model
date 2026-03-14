% ============================================================================
% CONSTRAINT STORY: us_military_spending_justification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_military_spending_justification, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_military_spending_justification
 *   human_readable: US Military Spending Justification Framework
 *   domain: geopolitical/economic/security
 *
 * SUMMARY:
 *   The US military spending justification framework represents a structural
 *   constraint that coordinates genuine national security functions
 *   (deterrence, force projection, alliance commitments) while simultaneously
 *   enabling substantial extraction through budget lock-in, institutional
 *   momentum, and asymmetric information. The constraint exhibits tangled
 *   rope characteristics: real coordination benefits coexist with extractive
 *   asymmetry. Beneficiaries (defense contractors, military leadership)
 *   experience the constraint as pure coordination; victims (fiscal
 *   sustainability, displaced domestic programs) experience it as pure
 *   extraction. The framework's theater ratio (0.68) reflects high
 *   performative content: threat narratives are continuously regenerated and
 *   updated post-hoc to justify existing spending levels rather than
 *   preceding spending decisions. The empirical puzzle is whether military
 *   spending reflects objective geopolitical necessity or institutional
 *   preference amplified through information asymmetry and organized
 *   beneficiary pressure. The cold war institutional legacy perspective
 *   suggests spending levels persist through inertia despite the primary
 *   rationale (Soviet containment) having evaporated; the natural law
 *   perspective risks naturalizing as immutable security requirement what may
 *   be a contingent institutional arrangement.
 *
 * KEY AGENTS:
 *   - Defense Industrial Complex: Primary beneficiary (institutional/arbitrage) — sole-source contracts, cost-plus pricing, recurring revenue streams create arbitrage opportunities; experiences constraint as pure coordination
 *   - Military Leadership: Institutional beneficiary (institutional/constrained) — genuine coordination function (deterrence, readiness) but also benefits from budget maximization and institutional autonomy
 *   - Geopolitical Dominance States: Secondary beneficiary (institutional/arbitrage) — benefits from US military spending enabling global power projection and alliance dominance
 *   - Fiscal Sustainability: Primary victim (powerless/trapped) — abstract constraint that cannot organize; bears full opportunity cost through budget crowding
 *   - Domestic Social Programs: Primary victim (powerless/trapped) — infrastructure, education, healthcare programs face chronic underfunding relative to military spending; no exit or reallocation mechanism
 *   - Non-military Technological Innovation: Secondary victim (moderate/constrained) — loses talent pool and investment capital to defense sector; faces barriers to redirecting research funding
 *   - Taxpayer Base: Secondary victim (moderate/constrained) — bears tax burden and opportunity cost; some capacity to organize (voting, advocacy) but structural barriers prevent effective reallocation
 *   - Defense Budget Reformers: Organized advocates (organized/constrained) — perceive alternative institutional pathways (acquisition reform, competitive bidding) but face entrenched resistance
 *   - Cold War Institutional Legacy: Institutional momentum mechanism (institutional/arbitrage) — justification framework persists through inertia; generates threat narratives endogenously to sustain spending
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_military_spending_justification, 0.58).
domain_priors:suppression_score(us_military_spending_justification, 0.65).
domain_priors:theater_ratio(us_military_spending_justification, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_military_spending_justification, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_military_spending_justification, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_military_spending_justification, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_military_spending_justification, tangled_rope).
narrative_ontology:human_readable(us_military_spending_justification, "US Military Spending Justification Framework").
narrative_ontology:topic_domain(us_military_spending_justification, "geopolitical/economic/security").

domain_priors:requires_active_enforcement(us_military_spending_justification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_military_spending_justification, defense_industrial_complex).
narrative_ontology:constraint_beneficiary(us_military_spending_justification, military_leadership).
narrative_ontology:constraint_beneficiary(us_military_spending_justification, geopolitical_dominance_states).
narrative_ontology:constraint_victim(us_military_spending_justification, domestic_social_programs).
narrative_ontology:constraint_victim(us_military_spending_justification, fiscal_sustainability).
narrative_ontology:constraint_victim(us_military_spending_justification, non_military_technological_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FISCAL SUSTAINABILITY (SNARE) — Cannot exit the military spending framework; bears full opportunity cost. Every dollar allocated to military spending is unavailable for infrastructure, education, healthcare. Powerless agents (low-income citizens, future generations) have no mechanism to redirect spending. The constraint extracts through budget lock-in and political asymmetry.
constraint_indexing:constraint_classification(us_military_spending_justification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TAXPAYER BASE (TANGLED ROPE) — Faces genuine security coordination benefits (deterrence, power projection capacity) alongside asymmetric extraction (tax burden, opportunity cost). Constrained by political mobilization barriers and information asymmetry. Some capacity to organize (voting, advocacy) but structural barriers prevent effective exit or reallocation.
constraint_indexing:constraint_classification(us_military_spending_justification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENSE INDUSTRIAL COMPLEX (ROPE) — Experiences the constraint as pure coordination: government demand creates stable, predictable revenue. Sole source contracts, cost-plus pricing, and revolving-door personnel flow create arbitrage opportunities. Net beneficiary. The coordination function is real (procurement efficiency, supply chain stability) but extraction runs entirely toward this agent.
constraint_indexing:constraint_classification(us_military_spending_justification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEFENSE BUDGET REFORMERS (SCAFFOLD) — Organized advocates (think tanks, Congress members, audit bodies) see military spending as a temporary coordination failure with a sunset: acquisition reform, competitive bidding, performance auditing, and base closure rounds represent exit pathways. Theater is high (bipartisan military-support rhetoric), but organized agents perceive actionable alternatives. Sunset logic assumes reform can reduce extraction component while maintaining coordination function.
constraint_indexing:constraint_classification(us_military_spending_justification, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COLD WAR INSTITUTIONAL LEGACY (PITON) — The justification framework persists through institutional inertia despite its primary rationale (Soviet containment) having evaporated three decades ago. Military spending levels are maintained through performative threat narratives and theater (China threat, terrorism, near-peer competition) rather than through genuine strategic necessity calculation. The institutional system itself has become the primary customer of threat narratives.
constraint_indexing:constraint_classification(us_military_spending_justification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, military spending may appear as an immutable requirement: great power competition creates a security dilemma where no state can unilaterally reduce military expenditure without accepting dominance risk. This perspective naturalizes military spending as a structural law of geopolitics. However, the structural data contradicts this — US military spending (3.5% of GDP, 40% of global military expenditure) dramatically exceeds peer competitor levels and historical deterrence thresholds, suggesting the mountain classification conceals a contingent institutional arrangement rather than a natural law.
constraint_indexing:constraint_classification(us_military_spending_justification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: MILITARY LEADERSHIP (TANGLED ROPE) — Institutional actor with genuine coordination function (deterrence, readiness, force projection) alongside extraction (budget maximization, institutional autonomy, personnel incentives). Constrained exit due to civil-military norms and statutory requirements. Leadership benefits from spending growth but also genuinely coordinates national defense. Differs from defense contractor perspective through alignment with public institutions rather than profit maximization.
constraint_indexing:constraint_classification(us_military_spending_justification, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_military_spending_justification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_military_spending_justification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_military_spending_justification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_military_spending_justification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_military_spending_justification, TR),
    TR >= 0.70.

:- end_tests(us_military_spending_justification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits genuine coordination function (deterrence capability, alliance architecture) estimated at approximately 0.25-0.30 of total spending; the remaining 0.28-0.33 represents extraction through budget lock-in, institutional momentum, and information asymmetry. The US military spending level (3.5% of GDP, ~$800B annually) is 10x+ the level of major peer competitors on a per-capita basis and far exceeds historical deterrence thresholds. This gap between coordination necessity and actual spending suggests substantial extractive component. Suppression (0.65): High. Barriers to reallocation include: congressional district dependence (military bases and contractors distributed across all states), revolving-door institutional capture (military-industry personnel flows), classification barriers (threat assessment opacity), and bipartisan military support norms that suppress debate. Information asymmetry is substantial — cost-plus contracting and classified threat assessments prevent transparent counterfactual comparison. Theater ratio (0.68): High and increasing over the measurement interval. Threat narratives (Cold War containment → terrorism → China threat → near-peer competition) are continuously updated post-hoc to justify existing spending rather than preceding budget decisions. The performance of threat identification appears endogenous to institutional preference for maintaining spending levels. The trajectory (0.38 → 0.68) reflects declining fit between justification narratives and actual spending levels — higher theater required to sustain the same extraction as Soviet threat evaporated.
 *
 * PERSPECTIVAL GAP:
 *   Maximal gap between beneficiary (rope at d ≈ 0.10) and victim (snare at d ≈ 0.95). The same institutional arrangement is experienced as pure coordination by defense contractors and as pure extraction by domestic programs competing for fiscal resources. The reformer (scaffold) perspective splits the difference, perceiving both coordination and extraction but believing the extraction component is reducible through institutional reform. The piton perspective correctly identifies that much of the justification is performative (theater ratio 0.68) but remains locked in through institutional inertia. The analytical observer perspective risks false naturalization — treating contingent institutional spending levels as immutable laws of geopolitics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by structural position relative to the extraction flow. Defense contractors: beneficiaries with arbitrage exit → d ≈ 0.05-0.15, producing negative χ (they benefit). Military leadership: institutional beneficiaries with constrained exit → d ≈ 0.20-0.30 (genuine coordination mixed with benefit). Taxpayers: secondary victims with constrained exit (can advocate but cannot force reallocation) → d ≈ 0.70, producing high χ. Fiscal sustainability: powerless victim with trapped exit → d ≈ 0.95, producing maximum χ. The reformers: organized agents with constrained exit but perceiving alternative pathways → d ≈ 0.45-0.55 (mixed experience). The piton perspective: institutional actors maintaining legacy arrangements through arbitrage → d ≈ 0.10-0.20 (beneficiaries of inertia). The natural law perspective: analytical context treating spending as structural necessity → canonical d ≈ 0.73 (but this is a false summit — the structural data shows contingent institutional arrangement).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint is validly classified as tangled rope because: (1) genuine coordination function exists — US military capacity does deter aggression and enable alliance architecture, estimated at 0.25-0.30 of total spending; (2) asymmetric extraction documented — defense contractor profits, budget lock-in, opportunity cost concentration on powerless agents; (3) active enforcement required — continuous threat narratives and institutional mechanisms maintain the constraint. The mandatrophy is resolved by recognizing that the coordination and extraction components are structurally inseparable: the bureaucratic mechanisms that enable deterrence coordination (acquisition authority, force planning, classified threat assessment) are precisely the mechanisms that enable extraction (cost-plus pricing, constituency protection, information opacity). The constraint cannot be decomposed into separate coordination and extraction constraints because the institutional machinery does both simultaneously. The reform pathways (scaffold perspective) assume decomposition is possible — that acquisition reform can preserve coordination while reducing extraction — but historical evidence (PPBS, competition initiatives, base closure attempts) suggests the institutional coupling is stronger than assumed. The mandatrophy blocks mislabeling this as pure coordination (rope) while remaining honest about the genuine coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_assessment_objectivity,
    'To what degree do measured threat levels reflect independent geopolitical reality versus institutional preference for high spending?',
    'Comparative threat assessment analysis: contrast US threat estimates with allied nations'' estimates and historical threat accuracy; examine whether threat assessments correlate with spending decisions or precede them',
    'If threats are objective: military spending constraint is genuine security coordination (shifts snare perspectives toward rope). If threats are endogenous to institutional preference: constraint is primarily extractive (confirms snare/tangled rope classifications).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_assessment_objectivity, empirical, 'Whether threat assessments are objective or endogenous to institutional spending preferences').

omega_variable(
    deterrence_threshold_ambiguity,
    'What is the minimum military spending level that maintains credible deterrence and great power status?',
    'Historical analysis of deterrence failures and successes correlated with spending ratios; peer comparison (NATO, China, Russia spending as percentage of GDP and absolute terms); modeling of force sufficiency thresholds',
    'If threshold is high (current US levels): spending is necessary (rope). If threshold is much lower (2-2.5% GDP): extraction is substantial (snare/tangled rope confirmed). If threshold is ambiguous: theater ratio dominates classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_threshold_ambiguity, conceptual, 'The minimum spending level required for credible deterrence and great power status').

omega_variable(
    domestic_opportunity_cost_measurability,
    'Can the counterfactual domestic investments (healthcare, infrastructure, education) from reallocated military spending be accurately modeled, or is the opportunity cost fundamentally unmeasurable?',
    'Economic modeling of alternative spending allocation scenarios; comparison with allied nations'' spending prioritization patterns; historical analysis of periods where military spending declined',
    'If measurable: victim classification is precise, snare features are clearly documentable. If unmeasurable: extraction is difficult to quantify, enabling plausible deniability and higher theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_opportunity_cost_measurability, empirical, 'Whether domestic opportunity cost from military spending reallocation is measurable').

omega_variable(
    revolving_door_causality,
    'Do military-industry personnel flows (contractors hiring former Pentagon officials, senators protecting district bases) constitute genuine coordination or institutional capture mechanism?',
    'Network analysis of personnel flows; correlation between post-government industry positions and Pentagon policy decisions; comparison with sectors lacking this flow (education, healthcare)',
    'If coordination: defense contractor perspective is accurately rope. If capture: defense contractor perspective should be snare or piton (institution colonized). Affects directionality and f(d) computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revolving_door_causality, empirical, 'Whether military-industry revolving door is coordination or capture mechanism').

omega_variable(
    reform_sunset_feasibility,
    'Is the scaffold perspective''s sunset logic (acquisition reform, performance auditing) structurally feasible, or are institutional lock-in mechanisms sufficiently strong to prevent meaningful extraction reduction?',
    'Historical analysis of defense reform attempts (PPBS, RFP reforms, base closure commissions) and their outcomes; identification of institutional mechanisms that have reversed or reversed-pending reforms',
    'If reform is feasible: scaffold classification is correct, extraction component is reducible. If lock-in is total: scaffold is aspirational, constraint remains snare/tangled rope. Affects mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_sunset_feasibility, empirical, 'Whether defense acquisition reform can structurally reduce extraction component').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_military_spending_justification, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usmil_tr_t1991, us_military_spending_justification, theater_ratio, 1991, 0.38).
narrative_ontology:measurement(usmil_tr_t2001, us_military_spending_justification, theater_ratio, 2001, 0.52).
narrative_ontology:measurement(usmil_tr_t2011, us_military_spending_justification, theater_ratio, 2011, 0.62).
narrative_ontology:measurement(usmil_tr_t2024, us_military_spending_justification, theater_ratio, 2024, 0.68).

% Extraction over time
narrative_ontology:measurement(usmil_be_t1991, us_military_spending_justification, base_extractiveness, 1991, 0.35).
narrative_ontology:measurement(usmil_be_t2001, us_military_spending_justification, base_extractiveness, 2001, 0.45).
narrative_ontology:measurement(usmil_be_t2011, us_military_spending_justification, base_extractiveness, 2011, 0.52).
narrative_ontology:measurement(usmil_be_t2024, us_military_spending_justification, base_extractiveness, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_military_spending_justification, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_military_spending_justification, 0.25).
narrative_ontology:affects_constraint(us_military_spending_justification, nato_burden_sharing).
narrative_ontology:affects_constraint(us_military_spending_justification, defense_contractor_monopoly).
narrative_ontology:affects_constraint(us_military_spending_justification, threat_assessment_classification).
narrative_ontology:affects_constraint(us_military_spending_justification, domestic_infrastructure_underfunding).

% DUAL FORMULATION NOTE:
% The military spending justification is upstream of specific weapons procurement constraints (F-35, carrier programs, nuclear modernization) and downstream of broader geopolitical constraints (great power competition, alliance architecture). Separate constraint stories model the specific procurement bubbles and the geopolitical drivers; this story captures the justification framework that connects them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_military_spending_justification, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
