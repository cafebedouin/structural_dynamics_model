% ============================================================================
% CONSTRAINT STORY: iran_war_room_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_war_room_2026, []).

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
 *   constraint_id: iran_war_room_2026
 *   human_readable: The IRGC 'Active War Room' Snare
 *   domain: geopolitical/military
 *
 * SUMMARY:
 *   The IRGC Aerospace Force's declaration of an 'active war room' represents
 *   a coercive signaling mechanism embedded within Iran's strategic
 *   competition with the US, Israel, and Gulf states. The constraint operates
 *   across multiple time scales: immediate military readiness (affects state
 *   actor decision-making in days), biographical strategic deterrence (shapes
 *   long-term force posturing), and civilizational geopolitical structure
 *   (tests the viability of deterrence frameworks). The war room declaration
 *   is a Snare from the perspective of trapped regional actors and civilians
 *   — it extracts strategic concessions (restraint, military caution) through
 *   the threat of devastating strikes, while maintaining ambiguity about
 *   actual probability and timing. This ambiguity is the mechanism: clarity
 *   would allow negotiation and potential exits; opacity keeps all potential
 *   targets in a state of mandatory vigilance and precaution.
 *
 * KEY AGENTS:
 *   - IRGC Command Hierarchy: Primary beneficiary (institutional/arbitrage) — consolidates internal authority, signals resolve, extracts strategic concessions from regional competitors without formal negotiation
 *   - Revolutionary Guard Elite: Primary beneficiary (organized/arbitrage) — benefits from military budget expansion, deterrence credibility, regional prestige from threat capability
 *   - Regional State Actors (Israel, Gulf states, US forces): Primary victim (powerless/trapped) — forced into reactive posture, constrained diplomacy, mandatory military preparedness at high cost
 *   - Civilian Populations in Strike Range: Primary victim (powerless/trapped) — zero agency, bear full cost of sustained threat uncertainty, cannot exit or participate in decision-making
 *   - US/NATO Military-Diplomatic Apparatus: Secondary victim (organized/constrained) — forced to respond with force positioning and rhetoric hardening, reducing diplomatic flexibility
 *   - International Deterrence Framework: Institutional observer (institutional/arbitrage) — ritualizes threat-response script; increasingly performative as mutual threats crowd out negotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_war_room_2026, 0.68).
domain_priors:suppression_score(iran_war_room_2026, 0.72).
domain_priors:theater_ratio(iran_war_room_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_war_room_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(iran_war_room_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(iran_war_room_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_war_room_2026, snare).
narrative_ontology:human_readable(iran_war_room_2026, "The IRGC 'Active War Room' Snare").
narrative_ontology:topic_domain(iran_war_room_2026, "geopolitical/military").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_war_room_2026, irgc_command_hierarchy).
narrative_ontology:constraint_beneficiary(iran_war_room_2026, revolutionary_guard_elite).
narrative_ontology:constraint_victim(iran_war_room_2026, regional_state_actors).
narrative_ontology:constraint_victim(iran_war_room_2026, civilian_populations).
narrative_ontology:constraint_victim(iran_war_room_2026, deterrence_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL STATE ACTORS (SNARE) — Israel, Gulf states, and US regional forces have no exit from the threat landscape. War room declaration is a coercive signaling mechanism that extracts strategic concessions (restraint, diplomatic distance) without formal negotiation. Trapped in reactive posture; cannot de-escalate unilaterally without appearing weak. d≈0.92, f(d)≈1.38, σ=1.1 → χ≈0.64.
constraint_indexing:constraint_classification(iran_war_room_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CIVILIAN POPULATIONS (SNARE) — Have zero agency. War room declaration creates sustained uncertainty about attack timing and targets. Cannot exit the geographic footprint; cannot participate in decision-making; bear full cost of potential strikes. d≈0.98, f(d)≈1.44, σ=1.1 → χ≈0.68. Maximum extraction from powerless, trapped agents.
constraint_indexing:constraint_classification(iran_war_room_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: IRGC COMMAND HIERARCHY (ROPE) — Benefits from war room declaration as a pure coordination mechanism: consolidates command authority, signals resolve to allies (Syria, Hezbollah, militias), demonstrates unity to domestic audience. Experiences the constraint as coordination, not extraction. Can exit by standing down without cost to core interests. d≈0.08, f(d)≈-0.10, σ=1.1 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(iran_war_room_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: US/NATO APPARATUS (TANGLED ROPE) — Organized actors constrained by deterrence credibility requirements. War room declaration forces response (force positioning, rhetoric hardening) that both deters and escalates. Coordination benefit: clarifies threat, enables preparedness. Extraction cost: irreversible military posturing, domestic political pressure, reduced diplomatic flexibility. d≈0.62, f(d)≈0.82, σ=1.2 → χ≈0.45. Must remain engaged; exit costs prohibitive.
constraint_indexing:constraint_classification(iran_war_room_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL DETERRENCE FRAMEWORK (PITON) — The war room declaration ritualizes a script (threat → response → counter-threat) that persists through institutional inertia. The 'active war room' is a performative claim (Khamenei and IRGC commanders benefit from domestic credibility; actual attack probability remains strategically ambiguous). Theater_ratio ≈0.55 reflects that declared readiness is partly signaling bluff. The deterrence model itself is degraded: it assumes rational actor symmetry that may not hold if factional competition within Iran creates incentive structures for actual escalation despite high costs.
constraint_indexing:constraint_classification(iran_war_room_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — At the civilizational scale, one might argue that threat signaling is an immutable feature of asymmetric military structures — that war rooms and declarations of readiness are inherent to military command logic itself. However, the base properties (ε=0.68, suppression=0.72, theater=0.55, requires_active_enforcement=false) contradict mountain classification. This is a contingent institutional arrangement, not a law of nature. The false summit detector flags this: the 'inevitability' framing naturalizes what is actually a geopolitical snare.
constraint_indexing:constraint_classification(iran_war_room_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_war_room_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_war_room_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_war_room_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_war_room_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iran_war_room_2026, TR),
    TR >= 0.70.

:- end_tests(iran_war_room_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The war room declaration extracts strategic concessions from trapped actors through coercion. Regional competitors reduce military provocations, avoid certain actions (air strikes, blockades), and divert resources to defensive posturing. The extraction is not maximal (0.80+) because the threat remains partially bluffed — actual attack probability is strategically ambiguous, preserving some negotiation space. Suppression (0.72): High. Target actors have minimal alternatives: they cannot exit the region, cannot reliably predict attack timing, and cannot safely call the bluff (any provocation risks catastrophic retaliation). The lack of clarity prevents exit options from becoming actionable. Theater (0.55): Moderate. The war room declaration includes genuine signaling (IRGC has real strike capability and is demonstrably mobilized) but also performative elements (Khamenei uses the threat for domestic political consolidation; actual attack probability may be lower than rhetoric suggests). The intermediate theater ratio indicates a hybrid mechanism: partly real threat, partly signaling bluff.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (IRGC command) sees the war room as pure coordination: an effective mechanism for consolidating authority and deterring regional competitors. The victim (regional state actors, civilians) sees pure extraction: coercive threat extraction with no exit or negotiation pathway. The organized NATO apparatus sees tangled rope: both coordination (clarifies threat) and extraction (forces military posturing). The piton perspective reveals that the deterrence framework itself is degraded — the threat-response ritual persists through institutional inertia while actual strategic logic becomes increasingly strained by factional complexity and changing military technology. The false summit (mountain) perspective risks naturalizing the snare as inevitable geopolitical law — but the structural data shows it is a contingent institutional arrangement dependent on IRGC's internal cohesion and regional belief structures.
 *
 * DIRECTIONALITY LOGIC:
 *   IRGC command: Beneficiary + arbitrage exit → d≈0.08, f(d)≈-0.10. Can stand down without core cost; experiences constraint as coordination. Regional state actors: Victim + trapped → d≈0.92, f(d)≈1.38. Cannot exit; forced into reactive posture. Civilians: Victim + trapped → d≈0.98, f(d)≈1.44. Maximum extraction — zero agency. NATO apparatus: Victim + constrained → d≈0.62, f(d)≈0.82. Organized but constrained by deterrence credibility requirements; cannot fully exit without strategic consequence. Deterrence framework: Piton classification derives from theater_ratio (0.55), not from high chi. The framework experiences its own degradation — the script persists but real strategic logic has become decoupled.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves potential mandatrophy by showing that the war room declaration is structurally distinct from legitimate deterrence coordination. Legitimate deterrence is Rope: it signals resolve while preserving negotiation pathways and allowing exit through diplomatic agreement. The IRGC war room is Snare: it extracts through ambiguity and threat, deliberately preventing target actors from knowing whether negotiation is possible or whether the threat is bluff. The mandatrophy resolution: if IRGC commanders had declared specific red lines and negotiation conditions, the constraint would classify as Tangled Rope (coordination + extraction with transparency). Instead, they declared 'active readiness' without specifying conditions — this opacity moves the classification from Tangled Rope to pure Snare. The theater ratio (0.55) indicates the performative element is substantial but not dominant; if theater were >0.70, the classification would degrade to Piton (institutional inertia). The current metrics show a genuine extraction mechanism with real deterrent force, not yet a hollow ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaration_credibility_threshold,
    'At what probability threshold does a war room declaration transition from bluff/deterrence signaling to genuine attack readiness?',
    'Intelligence analysis of actual military deployments, logistics preparation, and command structure activation; comparison of declarations with equipment positioning and personnel mobilization',
    'If threshold < 30%: many declarations are performative theater (piton persists). If threshold > 70%: war room signals are reliable attack precursors (snare severity increases). Probability estimates below 30% suggest deterrence is working; above 50% suggest deterrence is failing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(declaration_credibility_threshold, empirical, 'Probability threshold for war room declaration credibility').

omega_variable(
    factional_incentive_alignment,
    'Do IRGC factions (aerospace force, ground force, elite units) have aligned incentives for escalation, or are internal power struggles creating pressure for actual attack despite leadership restraint?',
    'Analysis of command structure, personnel flows, and factional positioning after Soleimani assassination; correlation between war room declarations and internal IRGC power dynamics',
    'If aligned: war room is unified signaling (snare classification holds, extraction is deliberate). If misaligned: factional competition may create accidental escalation risk (snare transitions toward uncontrolled extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(factional_incentive_alignment, empirical, 'Whether IRGC factions have aligned escalation incentives').

omega_variable(
    deterrence_reciprocal_clarity,
    'Do regional and global actors share a common understanding of what military actions would trigger Iranian retaliation, or is the threshold deliberately ambiguous to maximize extraction?',
    'Analysis of Iranian public and private communications; comparison of stated red lines with actual response patterns to previous provocations',
    'If clear: war room creates predictable deterrence (snare with negotiation exit). If ambiguous: extraction mechanism is maximized — potential targets cannot know their own safety (snare with no exit, maximum victimization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_reciprocal_clarity, conceptual, 'Whether deterrence thresholds are clear or deliberately ambiguous').

omega_variable(
    civilian_intelligence_accessibility,
    'Do civilian populations in strike range have access to credible intelligence about actual attack probability and timing, or is information monopolized by military/intelligence services?',
    'Survey analysis of awareness and evacuation capacity; comparison of public information available to civilians vs classified threat assessments',
    'If accessible: victims can exercise constrained exit (some protective measures). If monopolized: victims remain trapped without information (maximum snare severity, ε approaches 0.80).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civilian_intelligence_accessibility, empirical, 'Whether civilians have access to attack probability intelligence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_war_room_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irgc_tr_t0, iran_war_room_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(irgc_tr_t3, iran_war_room_2026, theater_ratio, 3, 0.48).
narrative_ontology:measurement(irgc_tr_t6, iran_war_room_2026, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(irgc_be_t0, iran_war_room_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(irgc_be_t3, iran_war_room_2026, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(irgc_be_t6, iran_war_room_2026, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_war_room_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(iran_war_room_2026, gulf_state_military_posturing).
narrative_ontology:affects_constraint(iran_war_room_2026, israel_iran_deterrence_stability).
narrative_ontology:affects_constraint(iran_war_room_2026, us_regional_force_deployment).

% DUAL FORMULATION NOTE:
% The IRGC war room is upstream of specific military confrontation risks but represents a distinct structural constraint. Downstream constraints (state actor posturing, deterrence stability) inherit the ambiguity and escalation risk from this declaration. Decomposition: the actual strike capability is a separate constraint (mountain: physical/logistical limits); the declaration strategy is this constraint (snare: coercive signaling).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iran_war_room_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
