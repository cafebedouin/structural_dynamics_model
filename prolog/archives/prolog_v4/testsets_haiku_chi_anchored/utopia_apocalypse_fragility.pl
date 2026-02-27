% ============================================================================
% CONSTRAINT STORY: utopia_apocalypse_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_utopia_apocalypse_fragility, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: utopia_apocalypse_fragility
 *   human_readable: The Utopia-Apocalypse Cliff-Edge
 *   domain: social/political/philosophical
 *
 * SUMMARY:
 *   The utopia-apocalypse cliff-edge constraint describes how complex social
 *   orders present a paradox: they are simultaneously experienced as stable
 *   foundations for human flourishing (utopia) and as fragile structures
 *   vulnerable to catastrophic collapse (apocalypse). This structural
 *   fragility creates a suppression mechanism whereby ordinary citizens
 *   remain compliant with institutional arrangements despite awareness of
 *   fragility, because perceived alternatives appear worse (total collapse).
 *   The constraint exhibits six distinct classifications depending on
 *   observer position: ordinary citizens and minorities experience it as pure
 *   extraction (snare) with maximal d and no exit; stability incumbents
 *   experience mixed coordination-extraction (tangled rope); reform movements
 *   see a temporary problem with structural sunset (scaffold); academic
 *   discourse performatively reproduces the binary frame (piton); distributed
 *   resilience networks solve it as a coordination problem (rope); and
 *   civilizational analysts risk naturalizing a contingent institutional
 *   arrangement (mountain). The theater ratio (0.68) reflects substantial
 *   performative content in apocalypse rhetoric and stability maintenance:
 *   much of the constraint's power derives from dramatization of fragility
 *   and ritualistic expressions of commitment to order rather than from
 *   structural mechanisms that actually reduce fragility. Over the
 *   measurement interval (100 years), base extractiveness has increased from
 *   0.35 to 0.58, indicating that the compliance extraction based on
 *   fragility perception has accumulated. Theater ratio has simultaneously
 *   increased from 0.52 to 0.68, suggesting that the theatrical component of
 *   stability maintenance has grown faster than actual structural
 *   improvement. This pattern is consistent with the piton hypothesis: the
 *   original functional constraint (maintaining genuine structural order) has
 *   been supplemented and partially replaced by performative ritual that
 *   maintains compliance without improving resilience.
 *
 * KEY AGENTS:
 *   - General Population: Primary victim (powerless/trapped) — bears compliance costs and catastrophic downside risk with minimal agency in system design
 *   - Structural Minorities: Primary victim (powerless/trapped) — bear disproportionate compliance costs and higher vulnerability in apocalypse scenarios
 *   - Stability Incumbents: Primary beneficiary (powerful/mobile) — institutional actors (state, major corporations, military) who benefit from compliance extraction and have exit options they choose not to exercise
 *   - Reform Movements and Civil Society: Organized actors (organized/constrained) — see the fragility as solvable through graduated institutional change with sunset logic
 *   - Distributed Resilience Networks: Secondary actors (moderate/mobile) — regional and local actors building alternative systems that reduce dependence on central order fragility
 *   - Academic and Philosophical Discourse: Institutional actors (institutional/constrained) — preserve the binary stability/apocalypse framing through performative ritual despite empirical complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(utopia_apocalypse_fragility, 0.58).
domain_priors:suppression_score(utopia_apocalypse_fragility, 0.72).
domain_priors:theater_ratio(utopia_apocalypse_fragility, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, extractiveness, 0.58).
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(utopia_apocalypse_fragility, snare).
narrative_ontology:human_readable(utopia_apocalypse_fragility, "The Utopia-Apocalypse Cliff-Edge").
narrative_ontology:topic_domain(utopia_apocalypse_fragility, "social/political/philosophical").

domain_priors:requires_active_enforcement(utopia_apocalypse_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(utopia_apocalypse_fragility, stability_incumbents).
narrative_ontology:constraint_victim(utopia_apocalypse_fragility, general_population).
narrative_ontology:constraint_victim(utopia_apocalypse_fragility, structural_minorities).
narrative_ontology:constraint_victim(utopia_apocalypse_fragility, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY CITIZEN (SNARE) — Trapped in a system perceived as stable but fragile. Cannot exit without abandoning family, property, and social identity. Bears full catastrophic risk if equilibrium fails. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.99. High effective extraction through enforced compliance with stability mechanisms.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STRUCTURAL MINORITIES (SNARE) — Trapped not only by territorial exit costs but by systemic exclusion from resources and voice. Bear disproportionate risk in apocalypse scenario while receiving minimal benefits from stability. d≈0.98, f(d)≈1.41, σ=1.2 → χ≈1.01. Maximal extraction: high suppression, high vulnerability, minimal agency.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: STABILITY INCUMBENTS (TANGLED ROPE) — Institutional actors (government, large corporations, military) who benefit from the status quo equilibrium. Experience the constraint as both coordination (maintaining social order benefits them) and extraction (they extract compliance and resources through enforcing stability mechanisms). Mobile exit options (capital flight, elite networks) but choose to maintain the system. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.24. Mixed coordination-extraction; they have agency.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM MOVEMENTS (SCAFFOLD) — Organized agents (NGOs, civil rights groups, institutional reformers) who see the cliff-edge fragility and propose gradual structural changes with sunset logic: as reforms mature, the system transitions from binary (stable/apocalypse) to graduated stability with built-in resilience. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38. Sunset: as institutional adaptability increases, the extreme fragility decreases.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC DISCOURSE (PITON) — The constraint persists in philosophical framing (social contract theory, civilization-collapse narratives) despite degraded explanatory power. Theater_ratio=0.68 indicates substantial performative content: much apocalypse rhetoric is ritualistic anxiety rather than actionable structural analysis. The discourse maintains the binary frame (stability/apocalypse) through institutional inertia even as empirical evidence suggests graduated transitions and polyarchic resilience mechanisms.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DISTRIBUTED RESILIENCE NETWORKS (ROPE) — Local and regional actors (mutual aid networks, polyarchic governance experiments, decentralized resource systems) who experience the cliff-edge constraint as a coordination problem solvable through distributed alternatives. These networks have mobile exit options (geographic relocation, exit to alternative systems) and use the fragility of centralized order as motivation for resilience building. d≈0.45, f(d)≈0.45, σ=0.9 → χ≈0.18. Low extraction; primarily coordination function.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical view, the fragility of complex social orders may appear as an inherent property of systems with high interdependence and low local resilience — a near-inevitable feature of civilization itself. However, base properties (ε=0.58, suppression=0.72) reveal this as a false summit: the cliff-edge fragility is not natural law but contingent institutional architecture that can be redesigned.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(utopia_apocalypse_fragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(utopia_apocalypse_fragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(utopia_apocalypse_fragility, TR),
    TR >= 0.70.

:- end_tests(utopia_apocalypse_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts compliance and acquiescence through the mechanism of fragility perception: ordinary citizens accept hierarchical control, resource concentration, and systemic inequality because the perceived alternative (civilizational collapse) appears worse. However, this is not maximum extraction because some exit options exist (emigration, community-level alternatives) and some incumbents invest in actual resilience mechanisms that reduce fragility. The increase from 0.35 to 0.58 over the interval reflects intensifying fragility perception and compliance accumulation. Suppression (0.72): High. Multiple barriers prevent exit: geographic/resource constraints prevent migration for most; institutional systems provide no formal opt-out mechanisms; cultural narratives frame alternative social orders as utopian fantasies or apocalyptic scenarios; military and police monopolize force. Theater ratio (0.68): Moderate-high. Stability maintenance includes both functional components (law enforcement, resource distribution systems, coordination mechanisms) and performative components (state ceremonies, apocalypse rhetoric, ritual expressions of commitment to order). The increase from 0.52 to 0.68 suggests that performative content is growing relative to functional content — the constraint maintains compliance increasingly through dramatization and ritual rather than through improved structural resilience. Claimed type (snare): The base properties support snare classification from the powerless perspective. Extractiveness (0.58) ≥ 0.46, suppression (0.72) ≥ 0.60. From the global perspective of trapped populations with no exit, this is extraction with minimal coordination benefit.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The powerless citizen sees a snare: trapped in a fragile system, forced to accept extraction through compliance, facing apocalyptic downside if they defect. The stability incumbent sees a tangled rope: they experience the system as coordination (maintaining order benefits them) but also as extraction mechanism (they extract compliance). They have mobile exit options (capital flight, elite networks) but maintain the system because it works for them. The reform movement sees a scaffold: the fragility is temporary and solvable through institutional redesign; as reforms mature (graduated resilience mechanisms, distributed authority, resource decentralization), the binary cliff-edge transforms into graduated stability. The academic sees a piton: the binary frame (stability/apocalypse) persists as institutional ritual despite empirical evidence of polyarchic alternatives and graduated transitions. The resilience network sees a rope: fragility of centralized order is a coordination problem solvable through distributed alternatives; the constraint's power derives from dependence on central systems. The civilizational analyst risks seeing a mountain: social fragility may appear to be an inherent property of complex systems, a law of civilization itself — but the structural data reveals this as a false summit, a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   General population: Victim + trapped → d≈0.95, f(d)≈1.42. Maximal extraction. No exit option; full exposure to fragility risk; minimal say in system design. Structural minorities: Victim + trapped → d≈0.98, f(d)≈1.41. Maximal extraction, slightly higher than general population due to compounding exclusions. Stability incumbents: Beneficiary + mobile → d≈0.35, f(d)≈0.35. Significant beneficiary position, but not maximal benefit because genuine system collapse would harm them too. Mobile exit options distinguish them from trapped populations but they choose to stay and maintain system. Reform movements: Mixed beneficiary/victim + constrained → d≈0.50, f(d)≈0.65. Symmetric position; movements benefit from the current system's resources and institutional legitimacy but are constrained by the system's resistance to change. They are both victims of fragility and potential beneficiaries of reform. Distributed resilience networks: Victim (of central system dependence) + mobile → d≈0.45, f(d)≈0.45. Mobile because they can build parallel systems; victim classification because central order fragility constrains their growth and forces defensive posturing. Academic discourse: Institutional actor + constrained → d≈0.30, f(d)≈0.25. Discourse benefits from continued framing of civilization as binary/fragile (maintains relevance of stability-concerned intellectuals) but is constrained by institutional dependence on the order it ritualistically depicts as fragile.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY SIGNAL: The constraint's extractiveness (0.58) exceeds the snare threshold (0.46) but approaches but does not quite exceed the high-extraction threshold (0.70) requiring explicit mandatrophy resolution. However, the perspectival distribution reveals a latent mandatrophy hazard: the constraint could easily be mislabeled as 'natural law' (civilization inherently fragile) if the analyst confuses the mountain perspective with the actual structure. The false summit is particularly dangerous here because: (1) naturalizing the cliff-edge as inevitable discourages resilience work; (2) this naturalizing tendency is institutionally convenient for stability incumbents; (3) academic discourse actively propagates the naturalization through piton-like performative ritual. The remedy is not to suppress the mountain perspective but to recognize it as one legitimate indexical reading (civilizational view) while simultaneously validating the snare diagnosis from trapped populations' perspective. The constraint does not require forced mandatrophy resolution (extractiveness is not >0.70) but DOES require perspectival transparency: any public discourse about this constraint must identify which agent position is being analyzed. Discourse that shifts between 'civilization is inherently fragile' (mountain, analytical view) and 'ordinary citizens must accept this fragility' (snare, powerless view) commits a perspectival ambiguity that serves incumbent interests. Resolving the latent mandatrophy means making perspectival position explicit and treating the divergent classifications not as a classification failure but as a faithful record of structurally divergent experiences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stability_threshold_multiplicity,
    'Is the cliff-edge a single bifurcation point or a multiplicity of stability thresholds depending on which system dimensions are perturbed?',
    'Dynamical systems analysis of social order perturbations (policy changes, leadership transitions, resource shocks) across historical and contemporary cases; identification of whether collapse requires simultaneous failure across multiple subsystems or can occur from single-axis failure',
    'If single bifurcation: the snare classification is accurate — civilization is genuinely fragile and exit is genuinely constrained. If multiplicity: the constraint is partially scaffolded — partial system failure does not necessarily cascade to apocalypse, and resilience interventions can succeed at lower cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_threshold_multiplicity, empirical, 'Whether cliff-edge is a single bifurcation or multiple thresholds').

omega_variable(
    exit_option_availability,
    'For powerless agents within a social order, what real exit options exist short of geographic migration or structural collapse? Are alternative social orders genuinely accessible?',
    'Historical and contemporary case studies of exit opportunities: economic relocation, community switching, institutional opt-out, parallel economy participation, emigration feasibility. Quantify fraction of population for which each exit option is genuinely available.',
    'If exit options are near-zero: trapped exit classification is correct, snare diagnosis is structural. If exit options exist but are suppressed: suppression metric needs upward revision. If exit options are abundant: agent power classification should shift from powerless to constrained/organized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_availability, empirical, 'Real exit options available to trapped populations').

omega_variable(
    catastrophic_failure_causality,
    'When social orders have collapsed historically (Rome, Maya, Soviet Union), did collapse result from exogenous shocks exceeding system capacity, or from endogenous structural fragility that made the system vulnerable to minor perturbations?',
    'Detailed historical analysis correlating collapse timing, scale of precipitating event, and pre-collapse structural robustness. Identify whether collapses are preceded by measurable increase in fragility indicators or whether stable systems collapse from large unexpected shocks.',
    'If exogenous-dominated: the cliff-edge is less sharp than the snare framing suggests — systems are fragile to large shocks but stable to normal variation. If endogenous-dominated: the snare classification is confirmed; small structural changes can trigger cascade failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophic_failure_causality, empirical, 'Role of endogenous vs exogenous factors in historical collapse').

omega_variable(
    beneficiary_dependence_on_apocalypse_risk,
    'Do stability incumbents actually benefit from the perception of fragility and apocalypse risk, or would they be better served by genuine system resilience?',
    'Game-theoretic and institutional analysis of incumbent incentives under different stability regimes. Compare incumbent welfare in: high-fragility/low-resilience systems vs. high-resilience/graduated-stability systems. Quantify whether fragility perception generates compliance extraction that benefits incumbents more than actual resilience would.',
    'If fragility is instrumentalized: incumbents actively suppress resilience mechanisms, justifying the snare classification and the tangled_rope incumbent perspective as coordination-extraction hybrid. If fragility is accidental: incumbents would accept reform, and the scaffold sunset is credible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_dependence_on_apocalypse_risk, empirical, 'Whether incumbent incentives actively preserve fragility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(utopia_apocalypse_fragility, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(utop_tr_t0, utopia_apocalypse_fragility, theater_ratio, 0, 0.52).
narrative_ontology:measurement(utop_tr_t50, utopia_apocalypse_fragility, theater_ratio, 50, 0.61).
narrative_ontology:measurement(utop_tr_t100, utopia_apocalypse_fragility, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(utop_be_t0, utopia_apocalypse_fragility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(utop_be_t50, utopia_apocalypse_fragility, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(utop_be_t100, utopia_apocalypse_fragility, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(utopia_apocalypse_fragility, enforcement_mechanism).
narrative_ontology:affects_constraint(utopia_apocalypse_fragility, civilizational_risk_perception).
narrative_ontology:affects_constraint(utopia_apocalypse_fragility, authority_concentration).
narrative_ontology:affects_constraint(utopia_apocalypse_fragility, collective_action_escape_trap).

% DUAL FORMULATION NOTE:
% The utopia-apocalypse cliff-edge decomposes into multiple constraint stories reflecting different aspects of the binary frame: (1) structural fragility of centralized systems (this story, ε=0.58, snare at powerless level), (2) civilizational risk perception dynamics (downstream, ε=0.65, how the fragility perception is maintained), (3) authority concentration mechanisms (upstream, ε=0.45, why alternatives to centralization are suppressed). This story sits at the middle layer where the structural fragility meets its compliance-extraction mechanism. Upstream ε reflects institutional design choices; downstream ε reflects discourse and psychological propagation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(utopia_apocalypse_fragility, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
