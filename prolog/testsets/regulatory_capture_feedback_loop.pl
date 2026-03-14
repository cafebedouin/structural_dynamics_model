% ============================================================================
% CONSTRAINT STORY: regulatory_capture_feedback_loop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_feedback_loop, []).

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
 *   constraint_id: regulatory_capture_feedback_loop
 *   human_readable: Regulatory Capture Feedback Loop
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Regulatory capture operates as a feedback loop: regulated firms gain
 *   influence over their regulators through lobbying, campaign finance, and
 *   revolving-door employment; captured regulators write rules favoring
 *   incumbents; these rules entrench incumbent market position and raise
 *   barriers to entry; entrenched incumbents accumulate wealth and political
 *   power; increased wealth funds more effective lobbying; captured
 *   regulators become more captured. The loop is self-reinforcing and
 *   exhibits classic tangled-rope structure: genuine coordination functions
 *   (technical standards, safety regulation, contract enforcement) exist
 *   alongside asymmetric extraction benefiting incumbents and harming
 *   consumers and competitors. Over time, the theater ratio increases as
 *   regulatory bodies maintain performative public participation and
 *   cost-benefit analysis that no longer influence actual rulemaking.
 *   Extractiveness increases as barriers to entry accumulate and market
 *   concentration rises. The constraint is not a snare for the industry
 *   (which benefits coordinatively) but a snare for diffuse public and
 *   potential competitors (who cannot exit and have no voice). For the
 *   regulator, capture is experienced as constrained identity lock — career
 *   incentives and professional networks make industry-favorable regulation
 *   the natural course, not a conscious choice.
 *
 * KEY AGENTS:
 *   - Regulated Industry: Primary beneficiary (institutional/arbitrage) — captures regulatory rules to prevent competition and protect margins
 *   - Public Interest: Primary victim (powerless/trapped) — diffuse, unorganized, cannot exit regulatory framework; bears cost of reduced competition and innovation
 *   - Competitive Entrants: Secondary victim (moderate/constrained) — face asymmetric regulatory barriers; can exit market but at high cost
 *   - Captured Regulator: Institutional actor (institutional/constrained) — genuinely coordinates technical standards but also constrained by industry dependence; experiences mixed extraction and coordination
 *   - Legitimating Regulatory Theater: Institutional mechanism (institutional/arbitrage) — maintains public comment periods and impact statements that no longer influence decisions; theater persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — observes genuine coordination function alongside asymmetric extraction; identifies the constraint as tangled rope rather than pure snare or pure coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_feedback_loop, 0.58).
domain_priors:suppression_score(regulatory_capture_feedback_loop, 0.68).
domain_priors:theater_ratio(regulatory_capture_feedback_loop, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_feedback_loop, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_feedback_loop, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(regulatory_capture_feedback_loop, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_feedback_loop, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_feedback_loop, "Regulatory Capture Feedback Loop").
narrative_ontology:topic_domain(regulatory_capture_feedback_loop, "political_economy/governance").

domain_priors:requires_active_enforcement(regulatory_capture_feedback_loop).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_feedback_loop, regulated_industry).
narrative_ontology:constraint_victim(regulatory_capture_feedback_loop, public_interest).
narrative_ontology:constraint_victim(regulatory_capture_feedback_loop, competitive_entrants).
narrative_ontology:constraint_victim(regulatory_capture_feedback_loop, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC INTEREST (SNARE) — The diffuse public cannot organize, lacks resources to monitor regulatory proceedings, has no exit option from the regulatory framework. Bears full cost of regulatory capture through higher prices, reduced innovation, and foregone consumer surplus. No meaningful agency or voice in the process.
constraint_indexing:constraint_classification(regulatory_capture_feedback_loop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETITIVE ENTRANT (SNARE) — New market entrants face regulatory barriers designed by incumbent firms. Can exit the market but at high cost (sunk R&D, market timing loss). High suppression from asymmetric regulatory information and incumbent lobbying capacity. Extraction is substantial — incumbents use capture to prevent competition.
constraint_indexing:constraint_classification(regulatory_capture_feedback_loop, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGULATED INDUSTRY (ROPE) — Primary beneficiary. Experiences the regulatory framework as a coordination mechanism that stabilizes market conditions, prevents ruinous competition, and protects margins. Can arbitrage regulatory differences across jurisdictions. Net beneficiary — extraction runs toward this agent. For the incumbent, the constraint appears purely coordinative.
constraint_indexing:constraint_classification(regulatory_capture_feedback_loop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPTURED REGULATOR (TANGLED ROPE) — The regulatory agency genuinely coordinates industry activity (prevents fraud, sets technical standards, ensures safe production). But the agency is also constrained by industry funding, staff revolving doors, and dependence on industry expertise. Experiences extraction from the industry (career incentives point away from consumer protection) while also experiencing the coordination function (genuine need for technical regulation). Mixed experience — real coordination function alongside asymmetric dependence.
constraint_indexing:constraint_classification(regulatory_capture_feedback_loop, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGITIMATING REGULATORY THEATER (PITON) — The regulatory process maintains substantial performative content: public comment periods that are ignored, environmental impact statements that don't influence decisions, cost-benefit analyses designed to justify predetermined conclusions. The theater persists through institutional inertia — the legitimating function has atrophied while the performative apparatus remains. Theater ratio reflects that much regulatory activity is about legitimation rather than actual constraint on industry behavior.
constraint_indexing:constraint_classification(regulatory_capture_feedback_loop, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational analytical view, regulatory capture exhibits both genuine coordination functions (technical standards, prevention of negative externalities, reduction of information asymmetries between firms and consumers) and asymmetric extraction (incumbent protection, barrier to entry, consumer welfare reduction). The constraint is neither pure extraction nor pure coordination. Effective extraction χ reflects that the coordination function is real but captured by beneficiaries who distort it toward extraction.
constraint_indexing:constraint_classification(regulatory_capture_feedback_loop, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_feedback_loop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_feedback_loop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_feedback_loop, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_feedback_loop, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_feedback_loop, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_feedback_loop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The regulated industry captures substantial regulatory benefits through barrier creation and margin protection, but the extraction is not maximal because genuine coordination functions exist — the industry does benefit from technical standards, contract enforcement, and fraud prevention. The 0.58 value reflects the balance between real coordination (pulling down extraction) and asymmetric barrier-creation (pulling up extraction). Measurement trajectory (0.35→0.58 over 20 years) shows accumulation as entry barriers compound and market concentration rises. Suppression (0.68): High. Multiple barriers constrain alternatives: (1) structural — potential entrants face regulatory moats; (2) informational — incumbent firms have superior knowledge of regulatory process; (3) resource — lobbying capacity is asymmetric; (4) institutional — revolving door creates social cohesion between industry and regulators that makes alternatives unthinkable. Theater ratio (0.65): Moderately high and increasing. Public comment periods exist but don't change rulemaking; cost-benefit analyses are written to justify predetermined conclusions; environmental reviews proceed on schedule regardless of findings. The theater provides legitimacy while actual decisions reflect captured preferences. Theater increases over time as the gap between performative process and actual outcome widens.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between the regulated industry (Rope) and the public (Snare). The industry perceives the constraint as successful coordination that benefits everyone — technical standards, clear rules, stable markets. The public perceives extraction with no coordination benefit — they pay higher prices and face reduced choice and innovation. Both perceptions are structurally accurate from their positions. The secondary gap is between the captured regulator's public statements (Rope, coordination rhetoric) and the captured regulator's actual behavior (Snare, industry favoritism). The regulator experiences genuine cognitive dissonance here — they believe they are doing their job (coordination) while actually executing extraction for beneficiaries. The analytical observer sees through this gap and classifies it accurately as tangled rope — acknowledging both the real coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directional structure shows why this constraint is tangled rope rather than pure snare or pure rope. The regulated industry benefits from coordination (low d, negative χ). The public bears costs without benefit (high d, high f(d), high χ). The regulator is constrained by industry dependence (moderate d for institutional perspective with constrained exit). The analytical observer sees both functions simultaneously (moderate d, effective χ in the tangled rope range). No agent perceives pure extraction or pure coordination except from their own perspective. The captured regulator's d value is moderately elevated (0.45-0.55 range) despite institutional power because their exit options are constrained by career path dependence and professional network lock-in — their identity as a 'serious regulatory professional' is constituted through industry relationships, making genuine departure from industry preferences identity-threatening. This is identity_locked at the institutional level, not just constrained by material barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that regulatory capture is genuinely both coordination AND extraction simultaneously, not a mislabeling of one as the other. The coordination is real: technical standards, contract enforcement, fraud prevention, and reduction of information asymmetries are genuine coordination goods. The extraction is real: barriers to entry, margin protection, and consumer welfare reduction are genuine extraction bads. The classification as tangled rope reflects that both functions coexist and are structurally entangled. The mandatrophy is resolved by rejecting the false dichotomy — this is not 'is it coordination or extraction?' but 'what is the proportion and distribution?' The answer: coordination benefits are asymmetrically captured by incumbents, making the distribution extractive even though the function is coordinative. This is the signature of tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_capture_boundary,
    'At what point does regulatory coordination of a market transition into regulatory capture that extracts value from consumers and competitors?',
    'Empirical comparison of consumer welfare metrics, entry barriers, and firm profitability across differently-structured regulatory regimes; econometric isolation of capture-driven vs coordination-driven regulatory effects',
    'If boundary is sharp and empirically detectable: classification is snare when extraction dominates. If boundary is diffuse: constraint is genuinely tangled rope with irreducible ambiguity about which function dominates',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_capture_boundary, empirical, 'Boundary between coordination and capture in regulatory systems').

omega_variable(
    revolving_door_identity_lock,
    'To what degree is regulator capture driven by structural career incentives vs internalized identity fusion with the industry worldview?',
    'Longitudinal career trajectory analysis; interview data on regulatory staff identity and professional commitment; comparison of pre- and post-industry-employment regulatory behavior for the same person',
    'If primarily structural incentives: regulators can be re-aligned by changing incentive structures. If partially identity-locked: regulators cannot perceive alternatives even if incentives change, requiring personnel replacement or cultural reform',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revolving_door_identity_lock, empirical, 'Structural vs identity-based mechanisms in regulator capture').

omega_variable(
    feedback_loop_stability,
    'Is the regulatory capture feedback loop self-stabilizing (reaches equilibrium) or does it accumulate extraction and theater over time?',
    'Time series analysis of regulatory stringency, industry lobbying expenditure, and market concentration metrics over 20+ year periods; detection of trend acceleration vs convergence',
    'If self-stabilizing: constraint approximates steady-state tangled rope. If accumulating: constraint degrades toward snare as theater increases and coordination function atrophies',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_loop_stability, empirical, 'Whether regulatory capture is equilibrial or accumulative').

omega_variable(
    crisis_punctuated_reform,
    'Does crisis (financial collapse, public health disaster, environmental damage) temporarily break regulatory capture, or does the feedback loop persist even through shocks?',
    'Comparative analysis of pre- and post-crisis regulatory stringency, industry pushback intensity, and crisis-driven vs endogenous reform cycles across different regulatory domains and time periods',
    'If crisis breaks capture: constraint has latent sunset pathway and could be reclassified as scaffold. If feedback loop persists through crisis: constraint is a structural snare with no exogenous exit mechanism',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_punctuated_reform, empirical, 'Whether crisis interrupts or reinforces regulatory capture cycles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_feedback_loop, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture_feedback_loop, theater_ratio, 0, 0.4).
narrative_ontology:measurement(regcap_tr_t10, regulatory_capture_feedback_loop, theater_ratio, 10, 0.58).
narrative_ontology:measurement(regcap_tr_t20, regulatory_capture_feedback_loop, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture_feedback_loop, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_be_t10, regulatory_capture_feedback_loop, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(regcap_be_t20, regulatory_capture_feedback_loop, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_feedback_loop, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture_feedback_loop, barrier_to_entry_accumulation).
narrative_ontology:affects_constraint(regulatory_capture_feedback_loop, market_concentration_feedback).
narrative_ontology:affects_constraint(regulatory_capture_feedback_loop, revolving_door_professional_networks).

% DUAL FORMULATION NOTE:
% Regulatory capture has multiple structural manifestations: (1) market-level capture (this story) where rules protect incumbents; (2) agency-level capture where regulators adopt industry preferences; (3) personnel-level capture where regulators become identity-locked to industry worldviews. These are distinct stories with different ε values that should be decomposed per the ε-invariance principle. The present story focuses on the market-level feedback loop and its effects on extractiveness and theater. Agency-level and personnel-level dynamics should be separate stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_feedback_loop, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
