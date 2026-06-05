% ============================================================================
% CONSTRAINT STORY: rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rational_dropout_reading, []).

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
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rational_dropout_reading
 *   human_readable: Rational Dropout: Nuclear War Structurally Possible but Strategically Inaccessible
 *   domain: strategic_studies/nuclear_deterrence/rational_choice
 *
 * SUMMARY:
 *   Nuclear weapons fundamentally altered the strategic landscape by raising
 *   costs of major-power war beyond rational justification. The
 *   rational-dropout reading frames this as a constraint where war remains
 *   technically possible and reachable but strategically inaccessible because
 *   cost-benefit analysis excludes it from serious consideration. Unlike
 *   deterrence-through-fear (threat of punishment so severe that even
 *   rationality calculations would exclude war) or technical impossibility
 *   (physical barriers prevent execution), rational dropout operates through
 *   the actor's own reasoning: leadership evaluates the costs and benefits
 *   and concludes war is not worth fighting. This constraint extracts from
 *   revisionist powers (they cannot pursue ambitions through force even when
 *   militarily capable) while benefiting status quo powers (their position is
 *   preserved without requiring active defense). The extractiveness increased
 *   from 1945 to 1985 as nuclear arsenals grew and cost estimates became more
 *   precise; it has plateaued since as strategic doctrine stabilized. The
 *   theater ratio increased as verification regimes, arms-control
 *   negotiations, and strategic communication channels proliferated—the
 *   constraint is increasingly maintained through performative institutional
 *   mechanics (NPT compliance, inspection regimes, confidence-building
 *   measures) rather than pure cost-benefit calculation, though the
 *   underlying mechanism remains rational analysis.
 *
 * KEY AGENTS:
 *   - Status Quo Powers (institutional/arbitrage): Primarily the US and post-WWII allies — benefit from preservation of existing order without needing military victory; lowest cost position
 *   - Revisionist Powers (powerless/trapped): Any power seeking to change territorial or hegemonic balance through force — locked into rational-dropout framework even when grievances are strong; highest experienced extraction
 *   - Emerging Regional Powers (moderate/constrained): Newly nuclear states (India, Pakistan, North Korea) — experience mixed coordination (avoiding inadvertent escalation) and extraction (unable to credibly threaten nuclear use)
 *   - Non-Proliferation Regime (institutional/arbitrage): International institutions (IAEA, NPT signatories, UN Security Council) — benefit from preservation of constraint but rely on rational cost-benefit rather than institutional enforcement to maintain it
 *   - Strategic Uncertainty Coalition (organized/constrained): Arms control experts, crisis communication specialists, confidence-building mechanism architects — see rational dropout as temporary and work toward alternative frameworks (transparency, graduated response)
 *   - Analytical Observer (analytical/analytical): Civilizational perspective recognizing that rational dropout may be unstable across power transitions and technological changes; benefits and costs are asymmetrically distributed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rational_dropout_reading, 0.52).
domain_priors:suppression_score(rational_dropout_reading, 0.48).
domain_priors:theater_ratio(rational_dropout_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rational_dropout_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(rational_dropout_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(rational_dropout_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(rational_dropout_reading, "Rational Dropout: Nuclear War Structurally Possible but Strategically Inaccessible").
narrative_ontology:topic_domain(rational_dropout_reading, "strategic_studies/nuclear_deterrence/rational_choice").

domain_priors:requires_active_enforcement(rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(rational_dropout_reading, formalized).
narrative_ontology:cs_authority_grounding(rational_dropout_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(rational_dropout_reading).
narrative_ontology:cs_kernel_id(rational_dropout_reading, nuclear_impossibility_kernel).
narrative_ontology:cs_reading_relation(rational_dropout_reading, structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation(rational_dropout_reading, credibility_paradox_reading, influences).
narrative_ontology:cs_axiom(rational_dropout_reading, foundational, cost_benefit_rationality_excludes_war).
narrative_ontology:cs_axiom_status(cost_benefit_rationality_excludes_war, holdable).
narrative_ontology:cs_axiom_grounding(rational_dropout_reading, cost_benefit_rationality_excludes_war, empirically_contingent).
narrative_ontology:cs_axiom(rational_dropout_reading, foundational, rationality_framework_universally_adopted).
narrative_ontology:cs_axiom_status(rationality_framework_universally_adopted, holdable).
narrative_ontology:cs_axiom_grounding(rational_dropout_reading, rationality_framework_universally_adopted, conventional).
narrative_ontology:cs_reference_frame(rational_dropout_reading, rational_cost_benefit_equilibrium).
narrative_ontology:cs_drift_state(rational_dropout_reading, contemporary_power_transition, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rational_dropout_reading, status_quo_powers).
narrative_ontology:constraint_beneficiary(rational_dropout_reading, non_proliferation_regime).
narrative_ontology:constraint_victim(rational_dropout_reading, revisionist_powers).
narrative_ontology:constraint_victim(rational_dropout_reading, strategic_escalation_planners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REVISIONIST POWER (SNARE) — Possesses nuclear capability but faces structural barrier: rational cost-benefit analysis excludes war from decision set even when territorial or hegemonic grievances are strong. No exit from the rationality trap: abandoning cost-benefit reasoning appears as strategic irrationality. Bears full extraction cost (constrained options, locked-out scenarios) while unable to organize alternative frameworks.
constraint_indexing:constraint_classification(rational_dropout_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL COMPETITOR (TANGLED ROPE) — Mid-tier nuclear power experiences mixed dynamics: genuine coordination function (avoiding inadvertent escalation, maintaining signaling channels) coexists with asymmetric extraction (constrained by fait accompli doctrine, unable to threaten use despite technical capability). Benefits from stabilization of regional order; bears costs of locked-out strategic options.
constraint_indexing:constraint_classification(rational_dropout_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: STATUS QUO HEGEMON (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: rational cost-benefit analysis preserves existing order without requiring force. Can exercise arbitrage by maintaining ambiguity about own rationality thresholds while relying on others' commitment to cost-benefit reasoning. Low extraction cost.
constraint_indexing:constraint_classification(rational_dropout_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-PROLIFERATION REGIME (PITON) — Institutional structure claiming to prevent war through limiting proliferation, but the actual mechanism maintaining restraint is rational cost-benefit analysis, not treaty compliance. Theater ratio high: NPT compliance and inspection regimes perform verification function but deterrence operates through cost calculations independent of regime mechanics. Maintenance through institutional inertia as primary restraint mechanism atrophies.
constraint_indexing:constraint_classification(rational_dropout_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STRATEGIC UNCERTAINTY COALITION (SCAFFOLD) — Organized epistemic and policy actors (arms control analysts, confidence-building mechanism architects, crisis communication specialists) see rational dropout as a temporary coordination problem with sunset conditions. Structured uncertainty reduction (transparency measures, communication protocols, graduated response doctrines) can eventually replace cost-benefit stalemate with genuine cooperative security. Suppression declining over time as alternative frameworks mature.
constraint_indexing:constraint_classification(rational_dropout_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits both genuine coordination (cost-benefit analysis aligns incentives away from war) and extractive asymmetry (structure benefits status quo, locks out revisionist options, creates long-term instability as power balances shift). Not a mountain (cost thresholds are contingent on technology, demographics, resource availability) but also not pure rope (distributional consequences are irreversible over generational timescales).
constraint_indexing:constraint_classification(rational_dropout_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rational_dropout_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rational_dropout_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rational_dropout_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rational_dropout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rational_dropout_reading, TR),
    TR >= 0.70.

:- end_tests(rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts substantially from revisionist powers—they bear costs of strategic options foreclosed by rational calculation—but the extraction is not maximal because it operates through the actor's own reasoning rather than coercive suppression. A revisionist leader could theoretically reject cost-benefit rationality and declare war rational on cultural, religious, or honor-based grounds, but doing so invokes a different rationality model and abandons the dominant strategic framework. The increase from 0.35 (1945) to 0.52 (1985-2005) reflects the maturation of nuclear arsenals and cost estimates: early nuclear weapons had uncertain effects and limited arsenals, allowing rationality to remain ambiguous; mature arsenals with destruction estimates in the billions made cost-benefit calculations crisp and extraction severe. Suppression (0.48): Moderate. The main suppressive mechanism is the internalization of rational cost-benefit reasoning as the legitimate framework for strategic decision-making—alternatives (honor codes, revolutionary commitment, religious obligation) are delegitimized rather than banned. Revisionist powers are not prevented from considering war (no physical barriers) but are suppressed by epistemic consensus that war is irrational. Theater ratio (0.61): Moderately high. Arms-control regimes, confidence-building measures, and strategic communication channels perform verification and reassurance functions, but the actual restraint mechanism is rational cost-benefit calculation independent of these institutions. The theater has grown as institutions proliferate (NPT, IAEA, hotlines, MIRV treaties) while their functional impact on restraint is indirect—they reduce uncertainty but do not drive the rational-dropout constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits wide perspectival divergence. The status quo hegemon (institutional/arbitrage) perceives rope—pure coordination mechanism that solves the collective-action problem of avoiding war. The revisionist power (powerless/trapped) perceives snare—structural locks on war despite military capability and political motivation. The regional competitor (moderate/constrained) perceives tangled rope—genuine coordination function (avoiding inadvertent escalation) mixed with extraction (constrained strategic options). The non-proliferation regime (institutional/arbitrage) perceives piton—institutional structures (treaties, inspections) that perform theater but derive actual restraint from rational cost-benefit lying outside the regime. The strategic uncertainty coalition (organized/constrained) perceives scaffold—temporary coordination failure being superseded by transparency and graduated-response mechanisms. The analytical observer perceives tangled rope at civilizational scale—durable coordination function undermined by asymmetric benefit distribution and eventual instability as power transitions. No single perspective is 'correct'—the perspectival distribution itself reveals the constraint's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation of directionality (d) from beneficiary/victim declarations follows the pipeline: status quo powers are beneficiaries with arbitrage exit options, producing low d (≈ 0.15) and negative χ; revisionist powers are victims with trapped exit (locked into rationality framework), producing high d (≈ 0.95) and maximum χ. Regional competitors are both partially (beneficiary of stability coordination, victim of locked-out options), producing moderate d (≈ 0.55). The analytical observer sees asymmetric benefits and costs distributed globally, producing d reflecting the mixed position (≈ 0.65). The scaffold perspective (organized/constrained) reflects agents working to transition from rational-dropout to cooperative-security frameworks, producing moderate d and reduced χ as alternatives mature.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine coordination (avoiding inadvertent war) from extractive asymmetry (locking out revisionist options). Status quo powers achieve their objectives through cost-benefit calculation; they do not need to enforce the constraint actively because other powers' rationality does the work. Revisionist powers have no coordination function—they cannot reduce costs or increase benefits through bargaining within the rational-dropout framework; they only experience suppression. The tangled-rope classification captures this hybrid: the constraint genuinely coordinates to prevent accidental escalation (rope component) while extracting from those with grievances against the status quo (snare component). The mandatrophy is resolved by recognizing that different agent cohorts experience different constraint types from the same structural data—the classification variance is a feature, not a bug.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationality_axiom_universality,
    'Does cost-benefit rationality remain stable as foundation of restraint across all actors, or does it degrade under conditions of existential threat, regime collapse, or shift to different rationality models?',
    'Historical analysis of decision-making in nuclear crises (Cuban Missile Crisis, Kargil, Taiwan Strait); assessment of whether actors invoked cost-benefit or alternative frameworks; longitudinal study of whether rationality assumptions persist under extreme stress conditions.',
    'If stable: snare perspective is durable (revisionist powers remain locked in rational framework). If degrades: snare becomes unstable; constraint shifts toward forcible suppression (snare with active enforcement required) or collapses entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_axiom_universality, empirical, 'Whether cost-benefit rationality remains stable across crisis conditions').

omega_variable(
    cost_threshold_parameters,
    'What specific parameters determine whether war appears rational or irrational in cost-benefit calculations for different actors, and how sensitive is the rational-dropout outcome to changes in these parameters?',
    'Game-theoretic sensitivity analysis; reconstruction of actual decision-maker cost models from strategic doctrine; systematic variation of casualty estimates, economic loss projections, and alliance credibility assumptions to identify threshold crossings.',
    'If thresholds are robust: rational dropout persists across reasonable parameter variations. If sensitive: small changes in military technology, alliance structure, or casualty estimates could flip outcome from snare to crisis decision-making.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_threshold_parameters, empirical, 'Sensitivity of rationality thresholds to parameter changes').

omega_variable(
    sibling_reading_foreclosure_status,
    'Does the rational-dropout reading logically foreclose the structural-contraction or credibility-paradox readings, or do they coexist as live interpretations of the same nuclear dynamics?',
    'Formal logical analysis of axioms: rational-dropout asserts cost-benefit analysis excludes war from active consideration; structural-contraction asserts power dynamics force war into reachable set despite rationality; credibility-paradox asserts deterrent credibility depends on irrational commitment. Can all three hold simultaneously within a single framework?',
    'If foreclosure: rational-dropout rules out siblings; framework must choose. If coexistence: all three readings describe different aspects of nuclear dynamics (micro/individual rationality vs macro/systemic dynamics); constraint family requires all three stories for complete picture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'Logical relationship between rational-dropout and sibling readings').

omega_variable(
    power_transition_dynamics,
    'As relative power between nuclear states shifts (China rising, US declining; India-Pakistan parity; European rearmament), do rational cost-benefit calculations shift such that war eventually appears rational to formerly-constrained actors?',
    'Long-term strategic modeling of power transition scenarios; assessment of whether rational-dropout constraints persist or degrade as distribution of power changes; identification of power trajectory thresholds where cost calculations flip.',
    'If persistent: rational dropout remains stable across generational power shifts. If unstable: constraint has finite lifetime; successor regime (structural contraction or credibility paradox) will dominate as power distribution changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_transition_dynamics, empirical, 'Stability of rational-dropout under power transition').

omega_variable(
    technological_cost_displacement,
    'Do emerging technologies (autonomous systems, precision weapons, cyber capabilities, hypersonic delivery) change cost-benefit calculations by lowering apparent costs or enabling limited-strike scenarios that rational-dropout logic excludes?',
    'Analysis of strategic doctrine evolution in response to new military capabilities; modeling of how precision, speed, and autonomy affect rationality calculations; assessment of whether new technologies enable ''limited nuclear war'' scenarios that rational-dropout framework treats as impossible.',
    'If costs remain prohibitive: rational-dropout persists through technological change. If costs decline: some actors may shift toward snare-with-constrained-exit (limited war scenarios appear rational) rather than trapped-powerless position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_cost_displacement, empirical, 'Effect of emerging military technologies on cost-benefit calculations').

omega_variable(
    asymmetric_rationality_models,
    'Do different strategic cultures and decision-making traditions employ fundamentally different rationality models (Western cost-benefit vs honor-based vs hierarchical vs collective-survival) such that rational-dropout applies to some actors but not others?',
    'Comparative analysis of strategic doctrine across cultural and institutional contexts; assessment of whether non-Western decision-makers explicitly reject or reframe cost-benefit rationality; case studies of past crises where rationality models diverged.',
    'If uniform rationality: rational-dropout applies globally and symmetrically. If asymmetric: constraint operates differently for different cultural contexts; some actors may remain in snare while others experience rope or scaffold; generates instability at cultural interfaces.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_rationality_models, conceptual, 'Whether rationality models are culturally universal or context-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rational_dropout_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_1945, rational_dropout_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(theater_t20_1965, rational_dropout_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(theater_t40_1985, rational_dropout_reading, theater_ratio, 40, 0.63).
narrative_ontology:measurement(theater_t60_2005, rational_dropout_reading, theater_ratio, 60, 0.61).

% Extraction over time
narrative_ontology:measurement(extract_t0_1945, rational_dropout_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extract_t20_1965, rational_dropout_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(extract_t40_1985, rational_dropout_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(extract_t60_2005, rational_dropout_reading, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rational_dropout_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rational_dropout_reading, structural_contraction_reading).
narrative_ontology:affects_constraint(rational_dropout_reading, credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% The rational-dropout reading is one reading of the contested nuclear_impossibility_kernel. The sibling readings (structural-contraction and credibility-paradox) are separate constraint stories instantiating alternative structural mechanisms for the same domain phenomenon. All three belong to the nuclear_impossibility_kernel constraint family. Rational-dropout and structural-contraction coexist as live interpretations; rational-dropout influences credibility-paradox (if cost-benefit rationality dominates, paradox-based deterrence becomes unstable).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
