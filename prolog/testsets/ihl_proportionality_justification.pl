% ============================================================================
% CONSTRAINT STORY: ihl_proportionality_justification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_proportionality_justification, []).

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
 *   constraint_id: ihl_proportionality_justification
 *   human_readable: IHL Proportionality Justification in Armed Conflict
 *   domain: international_humanitarian_law/military_operations
 *
 * SUMMARY:
 *   The International Humanitarian Law doctrine of proportionality mandates
 *   that parties to armed conflict balance expected military advantage
 *   against anticipated civilian harm. Proportionality operates as a
 *   constraint on targeting, mandatory in principle but opaque in practice.
 *   Military commanders conduct proportionality analysis before strikes;
 *   legal advisors review targeting decisions; post-conflict accountability
 *   mechanisms assess proportionality claims. Yet the mechanism exhibits
 *   classical extraction architecture: suppression of verification (targeting
 *   decisions remain classified), theater (elaborate legal memoranda justify
 *   decisions that were likely made on operational grounds), and asymmetric
 *   distribution (civilians bear maximum cost of proportionality calculations
 *   that systematically undervalue their harm). The constraint has genuinely
 *   coordinated at times — proportionality doctrine has prevented some
 *   operations and shifted others to lower-harm alternatives. But the overall
 *   pattern across 30+ years of case law shows theater_ratio increasing (more
 *   elaborate justifications accompanying similar harm patterns) and civilian
 *   protection declining. The constraint simultaneously enables operations
 *   that would otherwise violate community norms and creates the appearance
 *   of restraint without functional verification.
 *
 * KEY AGENTS:
 *   - Civilian Population: Primary victim (powerless/trapped) — bears cost of proportionality calculations with no voice in input, no visibility into calculation, no appeal mechanism
 *   - Military Command: Primary beneficiary (institutional/arbitrage) — benefits from proportionality doctrine by legitimizing operations while maintaining operational flexibility through opacity
 *   - International Legal Apparatus: Institutional actor (institutional/constrained) — maintains proportionality framework through theater despite structural inability to verify underlying calculations
 *   - Humanitarian Monitors: Secondary agent (moderate/constrained) — observe proportionality claims but lack access to targeting data and face suppression through denial and retaliation
 *   - State Actors (Non-Combatants): Secondary beneficiary (powerful/mobile) — benefit from proportionality as soft power constraint on adversaries while maintaining exit from humanitarian accountability
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional failure (verification absence) as epistemic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_proportionality_justification, 0.58).
domain_priors:suppression_score(ihl_proportionality_justification, 0.72).
domain_priors:theater_ratio(ihl_proportionality_justification, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_proportionality_justification, extractiveness, 0.58).
narrative_ontology:constraint_metric(ihl_proportionality_justification, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ihl_proportionality_justification, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_proportionality_justification, snare).
narrative_ontology:human_readable(ihl_proportionality_justification, "IHL Proportionality Justification in Armed Conflict").
narrative_ontology:topic_domain(ihl_proportionality_justification, "international_humanitarian_law/military_operations").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_proportionality_justification, military_forces_conducting_operations).
narrative_ontology:constraint_victim(ihl_proportionality_justification, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(ihl_proportionality_justification, international_humanitarian_law_compliance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Trapped in conflict zones with no exit capacity. Bears full cost of proportionality calculations that systematically undervalue civilian harm relative to military advantage. Cannot organize, cannot appeal, cannot escape the constraint. Maximum experienced extraction through exposure to attacks justified by opaque proportionality reasoning.
constraint_indexing:constraint_classification(ihl_proportionality_justification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HUMANITARIAN MONITOR (SNARE) — Constrained by access restrictions, funding dependence, and retaliation risk. Observes proportionality calculations but has limited capacity to challenge or verify them. Faces suppression through denial of access, expulsion, or security threats. Moderate power but trapped by structural barriers to investigation and advocacy.
constraint_indexing:constraint_classification(ihl_proportionality_justification, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MILITARY COMMAND (ROPE) — Experiences proportionality doctrine as legitimate coordination mechanism: it enables operations while nominally respecting civilian protection. Possesses arbitrage exit (operational flexibility). Benefits from proportionality framework by legitimizing military action and deflecting accountability. Sees the constraint as coordination, not extraction.
constraint_indexing:constraint_classification(ihl_proportionality_justification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL APPARATUS (PITON) — Maintains proportionality doctrine through treaty language, court opinions, and legal training despite structural inability to verify or enforce the doctrine. Theater ratio high: elaborate proportionality analyses in military reports and legal opinions performed with ceremonial rigor while actual decision-making remains opaque. Institutional inertia maintains the form of IHL compliance without functional verification.
constraint_indexing:constraint_classification(ihl_proportionality_justification, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE ACTORS (NON-COMBATANTS) (TANGLED ROPE) — Powerful states not engaged in active combat benefit from proportionality doctrine as soft power mechanism (imposes constraints on adversaries) while maintaining operational flexibility. Mobile exit (can disengage from humanitarian advocacy). Genuine coordination function (international law promotes predictability) paired with asymmetric extraction (norm allows powerful militaries to conduct operations that weaker actors cannot justify). Moderate experienced extraction offset by coordination benefits.
constraint_indexing:constraint_classification(ihl_proportionality_justification, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EPISTEMIC LIMIT VIEW (MOUNTAIN) — From civilizational perspective, proportionality is an irreducible epistemic constraint: military commanders cannot perfectly calculate civilian harm ex ante, and any rule of proportionality must tolerate some opacity. This perspective risks naturalizing a contingent institutional failure (inability to verify proportionality calculations) as a law of warfare. Engine false summit detection applies: the mountain classification reveals naturalization rather than inherent limit.
constraint_indexing:constraint_classification(ihl_proportionality_justification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_proportionality_justification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ihl_proportionality_justification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ihl_proportionality_justification, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_proportionality_justification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ihl_proportionality_justification, TR),
    TR >= 0.70.

:- end_tests(ihl_proportionality_justification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Proportionality enables military operations while creating appearance of restraint. The extraction derives from systematic undervaluation of civilian harm (proportionality calculations weight military advantage heavily relative to civilian cost), opacity of calculations (military decision-making remains classified), and asymmetric burden (only civilians bear cost of calculation errors). Base value (0.38 at interval start) reflects that proportionality doctrine has prevented some operations; current value (0.58) reflects that harm prevention has declined as theater has increased. Suppression (0.72): High. Multiple mechanisms suppress verification: targeting data classified as military secret, post-conflict access to documentation restricted, humanitarian monitors denied access to operational areas, accountability proceedings rare and lengthy. Exit barriers for civilians are insurmountable; for military, proportionality doctrine is optional in practice. Theater ratio (0.68): High and increasing. Proportionality legal memoranda elaborate continuously while operational logic remains unchanged. As conflicts persist, theater increases (more sophisticated justifications for stable harm patterns) — Goodhart substitution effect. Initial theater (0.45) reflects early period when proportionality was enforced through rules; current theater (0.72) reflects period when proportionality has become ceremonial justification.
 *
 * PERSPECTIVAL GAP:
 *   The five-way split reveals that 'the' proportionality constraint is actually multiple structurally distinct constraints layered on top of each other: (1) the legal doctrine (Rope for military, Piton for legal apparatus), (2) the verification mechanism (Snare due to suppression), (3) the accountability system (Piton — ceremonial rather than functional), (4) the humanitarian protection norm (aspirational Rope, actual Snare). The perspectival gap shows that proportionality has coordination function (prevents some operations) but extraction function (enables others that opaque calculations justify). The military sees benefit (coordination); civilians see cost (extraction); observers see performance (theater).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d values derive from each agent's relationship to the extraction flow. Trapped civilians experience maximum d (~0.95) → highest experienced extractiveness. Constrained humanitarian monitors experience moderate-high d (~0.80). Institutional military with arbitrage options experience low d (~0.15) → negative or near-zero effective extraction (they are beneficiaries). Non-combatant states experience moderate d (~0.55) — they coordinate through proportionality doctrine (benefits) but also practice it (costs), producing mixed directionality. The analytical context experiences d ~0.72 (observer position trying to measure from outside the extraction). The piton classification does not derive from high experienced extraction but from high theater ratio indicating functional decay while institutional form persists.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED VIA DECOMPOSITION: The proportionality constraint conceals two structurally distinct mechanisms: (1) Genuine proportionality norm (Rope) — when verification is transparent, calculation is honest, and accountability is real, proportionality prevents excessive harm. This has coordination function. (2) Proportionality theater (Snare) — when verification is suppressed, calculations are opaque, and accountability is absent, proportionality becomes mechanism for legitimizing predetermined military decisions while appearing to constrain them. This is pure extraction. The measured constraint (extractiveness 0.58, theater 0.68) is the blended average of two distinct mechanisms at different strength levels. The increasing theater_ratio (0.45 → 0.68) reveals the degradation path: as functional verification declined, theater increased — the form persisted while the function decayed. The mandatrophy resolves by recognizing that proportionality contains both a real coordination function and a real extraction mechanism, making it structurally Tangled Rope at the blended level or Snare at the functional level (treating theater as a distinct constraint story that should be decomposed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_verification_impossibility,
    'Is proportionality verification structurally impossible or merely institutionally absent?',
    'Technology assessment: can real-time proportionality auditing (drone footage, targeting data, collateral damage estimates) be made mandatory and transparent? If yes, verification is institutionally absent; if no, verification is epistemically impossible.',
    'If verification is possible but withheld: snare classification is confirmed — the constraint is enforcement mechanism for extraction. If verification is impossible: proportionality becomes mountain-like epistemic constraint, but still subject to institutional choice (transparency vs opacity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_verification_impossibility, empirical, 'Whether proportionality verification is structurally impossible or institutionally withheld').

omega_variable(
    military_advantage_measurement_commensurability,
    'Can military advantage and civilian harm be measured on commensurable scales for genuine proportionality calculation?',
    'Analysis of proportionality decisions in military targeting: what units and metrics are used for advantage vs harm? Are they comparable or incommensurable? Review of cases where proportionality calculations were documented.',
    'If incommensurable: proportionality is theater (calculations mask preference-based decisions). If commensurable: proportionality could be objective constraint, dependent on accuracy of harm estimation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_advantage_measurement_commensurability, conceptual, 'Commensurability of military advantage and civilian harm metrics').

omega_variable(
    substitution_effects_in_constraint_architecture,
    'Does IHL proportionality constraint reduce total civilian harm or merely shift extraction timing and location?',
    'Comparative analysis: total civilian casualties in conflicts governed by IHL vs non-IHL contexts, controlling for conflict type and intensity. Does proportionality reduce harm or redistribute it across time/space/civilian groups?',
    'If harm reduction: constraint has coordination function. If harm redistribution: constraint is pure extraction mechanism with zero net protective effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_effects_in_constraint_architecture, empirical, 'Whether proportionality reduces or redistributes civilian harm').

omega_variable(
    accountability_decoupling_mechanism,
    'Does proportionality doctrine operate as a decoupling mechanism — creating appearance of restraint while enabling extraction without accountability?',
    'Analysis of proportionality justifications in post-conflict accountability: how many proportionality-based decisions are later contested or reversed? What is prosecution rate for violations? Does doctrine create safe harbor for decision-makers?',
    'If high decoupling: snare classification confirmed — proportionality is accountability theater. If low decoupling: doctrine has genuine constraining force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_decoupling_mechanism, empirical, 'Whether proportionality doctrine decouples decision-making from accountability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_proportionality_justification, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_prop_tr_t0, ihl_proportionality_justification, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ihl_prop_tr_t15, ihl_proportionality_justification, theater_ratio, 15, 0.62).
narrative_ontology:measurement(ihl_prop_tr_t30, ihl_proportionality_justification, theater_ratio, 30, 0.68).
narrative_ontology:measurement(ihl_prop_tr_t45, ihl_proportionality_justification, theater_ratio, 45, 0.72).

% Extraction over time
narrative_ontology:measurement(ihl_prop_be_t0, ihl_proportionality_justification, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ihl_prop_be_t15, ihl_proportionality_justification, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(ihl_prop_be_t30, ihl_proportionality_justification, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(ihl_prop_be_t45, ihl_proportionality_justification, base_extractiveness, 45, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_proportionality_justification, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_proportionality_justification, military_necessity_doctrine).
narrative_ontology:affects_constraint(ihl_proportionality_justification, distinction_principle_in_warfare).
narrative_ontology:affects_constraint(ihl_proportionality_justification, civilian_casualty_accountability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_proportionality_justification, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
