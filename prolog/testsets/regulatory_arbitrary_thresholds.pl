% ============================================================================
% CONSTRAINT STORY: regulatory_arbitrary_thresholds
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_arbitrary_thresholds, []).

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
 *   constraint_id: regulatory_arbitrary_thresholds
 *   human_readable: Regulatory Arbitrary Thresholds
 *   domain: regulatory_policy/economic_policy
 *
 * SUMMARY:
 *   Regulatory arbitrary thresholds — bright-line rules (e.g., 'firms with >
 *   50 employees must implement X,' 'operations > 100 units/day require Y
 *   certification') — create a structural extraction mechanism disguised as
 *   neutral administration. The thresholds serve genuine coordination
 *   functions (preventing race-to-the-bottom, establishing minimum standards)
 *   but systematize extraction from firms caught just below the threshold and
 *   from new entrants. The constraint exhibits all six types from different
 *   perspectives: incumbent firms see pure coordination (rope); marginal
 *   firms see inescapable extraction (snare); the regulatory agency sees
 *   institutional performance (piton); reform coalitions see temporary
 *   scaffolding (scaffold); analysts risk naturalizing contingent choices as
 *   administrative necessity (mountain); moderate firms see mixed
 *   coordination and extraction (tangled rope). The theater_ratio increase
 *   (0.52 to 0.68 over 20 years) indicates the threshold has become
 *   progressively more performative — its original technical justification
 *   has atrophied while the enforcement ritual persists. The extractiveness
 *   increase (0.42 to 0.58) reflects accumulated regulatory creep: new
 *   requirements layer onto the threshold without adjusting its level,
 *   creating a ratchet effect that concentrates extraction on marginal firms.
 *
 * KEY AGENTS:
 *   - Incumbent Firms: Primary beneficiary (institutional/arbitrage) — protected from new competition by threshold-based entry barriers; can arbitrage by relocating or adjusting scale but choose not to
 *   - Marginal Compliant Firms: Primary victim (powerless/trapped) — caught at suboptimal scale below threshold; cannot grow without triggering exponential compliance cost increases; cannot relocate without losing market access
 *   - Market Entrants: Secondary victim (powerless/trapped) — must clear the threshold immediately to operate; prohibitive startup costs create barriers to entry
 *   - Consumer Welfare: Abstract victim (powerless/trapped) — pays higher prices from reduced competition; bears innovation cost from reduced competitive pressure
 *   - Regulatory Agency: Institutional actor (institutional/arbitrage) — maintains threshold enforcement as core institutional identity; sees the threshold as legitimate policy instrument while losing sight of original justification
 *   - Reform Coalition: Organized actor (organized/constrained) — consumer advocates, small-business groups, progressive regulators pushing for adaptive/risk-based regulation; constrained by entrenched institutional resistance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks accepting threshold-based regulation as immutable feature of administrative systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_arbitrary_thresholds, 0.58).
domain_priors:suppression_score(regulatory_arbitrary_thresholds, 0.65).
domain_priors:theater_ratio(regulatory_arbitrary_thresholds, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_arbitrary_thresholds, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_arbitrary_thresholds, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regulatory_arbitrary_thresholds, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_arbitrary_thresholds, tangled_rope).
narrative_ontology:human_readable(regulatory_arbitrary_thresholds, "Regulatory Arbitrary Thresholds").
narrative_ontology:topic_domain(regulatory_arbitrary_thresholds, "regulatory_policy/economic_policy").

domain_priors:requires_active_enforcement(regulatory_arbitrary_thresholds).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_arbitrary_thresholds, incumbent_firms).
narrative_ontology:constraint_beneficiary(regulatory_arbitrary_thresholds, regulatory_agencies).
narrative_ontology:constraint_victim(regulatory_arbitrary_thresholds, compliant_small_firms).
narrative_ontology:constraint_victim(regulatory_arbitrary_thresholds, market_entrants).
narrative_ontology:constraint_victim(regulatory_arbitrary_thresholds, consumer_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL FIRM (SNARE) — A small compliant firm just below an arbitrary regulatory threshold experiences total extraction without exit. Cannot relocate jurisdiction without losing market access; cannot grow without triggering disproportionate compliance costs that exceed revenue gains. The threshold functions as a trap door: firms below it pay moderate compliance costs; firms above it face exponential cost increases. The firm is structurally trapped — exit means business failure.
constraint_indexing:constraint_classification(regulatory_arbitrary_thresholds, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETITOR SIZED FIRM (TANGLED ROPE) — A firm operating above the arbitrary threshold bears significant extraction (escalated compliance costs) but also benefits from genuine coordination: the threshold prevents a race-to-the-bottom on safety/environmental standards. The constraint has real function (maintains minimum standards) and real asymmetric extraction (favors incumbents already above the threshold). Exit is constrained but possible — the firm could relocate or shrink, at moderate cost.
constraint_indexing:constraint_classification(regulatory_arbitrary_thresholds, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT INDUSTRY (ROPE) — Established firms already operating above the threshold experience the constraint as pure coordination: the threshold protects them from new competition and preserves their market share. The threshold generates barriers to entry that reduce competitive pressure. For incumbents, the constraint is coordination that locks in their position. They can exit by relocating or shrinking, but do not need to — arbitrage options are abundant.
constraint_indexing:constraint_classification(regulatory_arbitrary_thresholds, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCY (PITON) — The agency views the threshold as a clear, defensible policy instrument and sees its enforcement as necessary coordination. However, the threshold's original justification (e.g., 'protect public health above 500 units per year') has become decoupled from the actual technical justification — the threshold persists through institutional inertia and is now theater. The agency maintains the performance of threshold-based compliance checking even though the underlying logic that justified the specific number has become opaque or outdated. The agency has arbitrage options (change the threshold) but does not exercise them.
constraint_indexing:constraint_classification(regulatory_arbitrary_thresholds, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Consumer advocates, small-business associations, and progressive regulators see arbitrary thresholds as a temporary coordination failure with a clear sunset: adaptive regulation (risk-based standards, performance metrics) can replace rigid thresholds. This perspective sees the constraint as temporary scaffolding around an older regulatory model. Sunset logic is explicit: as risk-based regulation matures, threshold-based logic becomes obsolete. The coalition has agency (can lobby for reform) but constrained — changing entrenched regulatory structures takes decades.
constraint_indexing:constraint_classification(regulatory_arbitrary_thresholds, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, any large-scale regulatory system must use some form of decision boundary to sort compliance from non-compliance. The threshold appears to be an immutable feature of administrative systems: you cannot regulate without drawing lines. However, the structural data reveals this as a false natural law — the specific thresholds chosen are contingent (not derived from physics or logic), and risk-based alternatives exist. The mountain classification naturalizes what is actually an institutional choice.
constraint_indexing:constraint_classification(regulatory_arbitrary_thresholds, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_arbitrary_thresholds_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_arbitrary_thresholds, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_arbitrary_thresholds, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_arbitrary_thresholds, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_arbitrary_thresholds, TR),
    TR >= 0.70.

:- end_tests(regulatory_arbitrary_thresholds_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The threshold creates asymmetric compliance cost burden — firms below the threshold operate at suboptimal scale to avoid triggering escalated requirements; firms above it have locked in their advantage. The extraction is not maximal because some genuine coordination function persists (prevents complete regulatory collapse), and some firms benefit from threshold-based predictability. Suppression (0.65): High. Significant barriers to exit: regulatory capital requirements are jurisdiction-specific (cannot easily relocate), regulatory switching costs are high, and political resistance to deregulation is entrenched. However, suppression is not total — some jurisdictions have reformed thresholds, and arbitrage (relocating operations) is possible at substantial cost. Theater ratio (0.68): High. The threshold persists not because it is technically optimal but because it provides administrative clarity. As regulators accumulate new requirements without re-calibrating the threshold level, the threshold becomes progressively detached from its original technical justification. The enforcement ritual (checking if a firm crosses the threshold) is now mostly theater — the underlying logic that justified the specific number has become opaque or outdated.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival fragmentation. Incumbents see rope (coordinated market protection). Marginal firms see snare (inescapable trap). The agency sees piton (institutional ritual). The reform coalition sees scaffold (temporary problem with solution pathway). The analyst risks seeing mountain (administrative necessity). The perspectival gap is driven by directionality: beneficiaries with exit options classify the same constraint as pure coordination; victims with no exit classify it as pure extraction. The gap reveals that the constraint's 'type' is not intrinsic but perspective-dependent, and the baseline metrics (ε=0.58, suppression=0.65) reflect an average across this fragmented landscape.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the extraction flow. Incumbent firms have low d (beneficiaries with arbitrage options) — they experience the constraint as coordination that protects their position. Marginal firms have high d (victims with no exit) — they experience maximum extraction from the threshold trap. The regulatory agency has low-moderate d (institutional beneficiary that maintains the constraint) — the agency benefits from threshold-based administrative clarity and has arbitrage options (change the threshold) but does not exercise them. The reform coalition has moderate-high d (constrained agent bearing extraction cost of reform resistance) — they are victimized by the constraint's persistence but have some agency through organizing. The analytical observer has moderate d (can see the constraint from outside but cannot easily change it). The sigmoid f(d) amplifies these differences: victims with trapped exit experience maximum f(d) ≈ 1.42; beneficiaries with arbitrage experience minimum f(d) ≈ -0.12.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by revealing that arbitrary thresholds systematize extraction while maintaining a coordination cover story. The genuine coordination function (preventing race-to-the-bottom) justifies the existence of some threshold; the arbitrary choice of the specific threshold level enables extraction. The tangled_rope classification is correct at the aggregate level: the constraint has both a coordination function and asymmetric extraction. The two are inseparable — the coordination benefit to incumbents is achieved precisely through extracting from marginal firms. No reform can fully separate them without addressing the underlying political economy: if the threshold is set at a level that does not create entry barriers, it loses its effectiveness as coordination (incumbents lose protection). The sunset logic (scaffold perspective) is real but contingent: risk-based regulation can replace arbitrary thresholds, but doing so requires political will to dismantle incumbent protection systems. The falseness of the mountain classification (analytical observer) is the key diagnostic: administrative systems do not require arbitrary thresholds — they require decision boundaries, which can be drawn via risk-based criteria, continuous functions, or adaptive mechanisms. The mountain view naturalizes the specific choice of arbitrariness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_origin_opacity,
    'Is the arbitrary threshold truly arbitrary (set by bureaucratic path-dependence) or does it reflect hidden technical justification that is no longer explicitly stated?',
    'Historical analysis of threshold-setting decisions; interviews with original regulators; comparison of stated rationale vs technical literature at time of adoption',
    'If truly arbitrary: threshold functions as pure extraction mechanism (higher snare classification). If hidden technical logic exists: threshold functions as imperfect-but-genuine coordination (higher rope classification for incumbent perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_origin_opacity, empirical, 'Whether arbitrary threshold has hidden technical justification').

omega_variable(
    risk_based_alternative_feasibility,
    'Are risk-based alternatives to arbitrary thresholds technically and administratively feasible for this regulatory domain, or would they create new coordination problems?',
    'Pilot programs implementing adaptive regulation; comparative study of risk-based systems in other jurisdictions; measurement of compliance cost variation under risk-based vs threshold-based approaches',
    'If feasible: scaffold sunset is real and extraction is temporary (10-20 year timeline). If infeasible: threshold persists indefinitely and becomes de facto mountain (immutable feature of the regulatory system).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_based_alternative_feasibility, empirical, 'Feasibility of risk-based regulatory alternatives').

omega_variable(
    compliance_cost_distribution_measurement,
    'What is the actual distribution of compliance costs across firm sizes? Does it follow the claimed threshold structure or reveal hidden step functions?',
    'Empirical survey of compliance costs by firm size; identification of actual cost discontinuities (e.g., accounting requirements, inspection frequency, documentation burdens); comparison to stated threshold effects',
    'If costs follow threshold: extraction mechanism is transparent. If hidden step functions exist (e.g., inspection frequency jumps at 50% above threshold): extraction is more severe than metrics indicate, and suppression should be upgraded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_cost_distribution_measurement, empirical, 'Actual compliance cost distribution across firm sizes').

omega_variable(
    incumbent_coordination_genuineness,
    'Does the threshold provide genuine coordination benefit (preventing race-to-the-bottom) or is it primarily a barrier-to-entry that benefits incumbents?',
    'Counterfactual analysis: remove threshold and measure whether standards would decline; study of jurisdictions without thresholds; measurement of competitive intensity before/after threshold adoption',
    'If genuine coordination: tangled_rope and rope classifications are correct, constraint has legitimate function. If primarily barrier-to-entry: rope becomes snare, and the constraint is pure extraction dressed as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_coordination_genuineness, empirical, 'Whether threshold provides genuine coordination or primarily creates barriers to entry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_arbitrary_thresholds, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regthresh_tr_t0, regulatory_arbitrary_thresholds, theater_ratio, 0, 0.52).
narrative_ontology:measurement(regthresh_tr_t10, regulatory_arbitrary_thresholds, theater_ratio, 10, 0.62).
narrative_ontology:measurement(regthresh_tr_t20, regulatory_arbitrary_thresholds, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(regthresh_be_t0, regulatory_arbitrary_thresholds, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(regthresh_be_t10, regulatory_arbitrary_thresholds, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(regthresh_be_t20, regulatory_arbitrary_thresholds, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_arbitrary_thresholds, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_arbitrary_thresholds, regulatory_capture_incumbent_protection).
narrative_ontology:affects_constraint(regulatory_arbitrary_thresholds, barrier_to_entry_market_concentration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_arbitrary_thresholds, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
