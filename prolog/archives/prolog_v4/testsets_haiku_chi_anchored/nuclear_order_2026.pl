% ============================================================================
% CONSTRAINT STORY: nuclear_order_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_order_2026, []).

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
 *   constraint_id: nuclear_order_2026
 *   human_readable: Post-New START Strategic Ambiguity
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   Following the expiration of the New Strategic Arms Reduction Treaty (New
 *   START) on February 5, 2026, the global nuclear order has transitioned
 *   from a regulated coordination mechanism (Rope) to a hybrid
 *   extraction-coordination structure (Tangled Rope). The treaty collapse
 *   eliminates ON-SITE inspection verification, replacing it with a regime of
 *   strategic ambiguity wherein the three major nuclear powers (Russia,
 *   China, United States) maintain unilateral definitions of their arsenals,
 *   deployment postures, and escalation doctrines. This constraint exhibits
 *   competing structural functions: legitimate coordination (mutual
 *   deterrence through opacity) and asymmetric extraction (unilateral
 *   benefits from verification asymmetry). Russian Federation and China
 *   benefit from undefined compliance baselines and reduced transparency
 *   obligations. The United States experiences dual pressure: gaining
 *   flexibility in doctrine but losing verification authority and bearing
 *   signaling costs to manage escalation risk. Non-aligned nations, secondary
 *   nuclear powers, and verification communities bear structural costs
 *   through increased uncertainty, destabilization risk, and erosion of the
 *   arms control epistemic commons. The theater ratio (0.58) reflects partial
 *   continuation of performative institutional frameworks (UN forums, NPT
 *   review conferences) without functional verification capacity. The
 *   extractiveness has increased from 0.28 (under New START transparency) to
 *   0.52 (post-expiration ambiguity regime) as unilateral reinterpretation of
 *   baseline numbers becomes possible.
 *
 * KEY AGENTS:
 *   - Russian Federation: Primary beneficiary (institutional/arbitrage) — gains strategic flexibility, doctrine opacity, and hedging space without verification burden. Escalate-to-de-escalate doctrine benefits from ambiguous thresholds.
 *   - People's Republic of China: Primary beneficiary (institutional/arbitrage) — modernization acceleration without transparency commitments; benefits from opacity that masks capability ceiling hedging.
 *   - United States Strategic Command: Dual-position (institutional/arbitrage on flexibility; institutional/constrained on signaling burden) — gains force structure flexibility but loses verification authority and must constantly manage escalation signaling.
 *   - Non-Aligned Nations: Primary victims (powerless/trapped) — no verification access, no negotiating power, bear full uncertainty cost. Destabilization risk imposed externally.
 *   - Secondary Nuclear Powers (France, UK, India): Mixed position (powerful/constrained) — benefit from regime opacity that obscures their own capability hedging but also experience destabilization risk.
 *   - Arms Control Verification Communities: Victims (moderate/constrained) — lose functional ON-SITE inspection authority; credibility premiums on ambiguous signals replace verification.
 *   - CTBT Organization: Organized agent (organized/constrained) — sees potential sunset pathway through alternative verification mechanisms; constrained by non-participation of major powers.
 *   - Residual Arms Control Institutions: Institutional (institutional/arbitrage) — continue performatively (NPT review conferences, UN forums) without verification teeth; maintained through inertia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_order_2026, 0.52).
domain_priors:suppression_score(nuclear_order_2026, 0.68).
domain_priors:theater_ratio(nuclear_order_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_order_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(nuclear_order_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nuclear_order_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_order_2026, tangled_rope).
narrative_ontology:human_readable(nuclear_order_2026, "Post-New START Strategic Ambiguity").
narrative_ontology:topic_domain(nuclear_order_2026, "geopolitical/technological").

domain_priors:requires_active_enforcement(nuclear_order_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_order_2026, russian_strategic_posture).
narrative_ontology:constraint_beneficiary(nuclear_order_2026, chinese_strategic_expansion).
narrative_ontology:constraint_beneficiary(nuclear_order_2026, us_military_flexibility).
narrative_ontology:constraint_victim(nuclear_order_2026, non_aligned_nations).
narrative_ontology:constraint_victim(nuclear_order_2026, arms_control_verification_regime).
narrative_ontology:constraint_victim(nuclear_order_2026, strategic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ALIGNED NATIONS (SNARE) — Cannot exit the strategic ambiguity regime; bear full cost of unpredictable nuclear doctrine shifts and destabilized deterrence. No verification access, no negotiating power. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(nuclear_order_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECONDARY NUCLEAR POWERS (TANGLED ROPE) — Constrained by P5 dominance but benefit from regime ambiguity that obscures their own capability ceilings. Strategic opacity provides cover for hedging strategies. d≈0.62, f(d)≈0.88, σ=1.2 → χ≈0.48.
constraint_indexing:constraint_classification(nuclear_order_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: RUSSIAN FEDERATION (ROPE) — Primary beneficiary of post-START ambiguity. Captures coordination through unilateral doctrine shifts (escalate-to-de-escalate, tactical nuclear threshold opacity) while maintaining plausible negotiation pathways. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(nuclear_order_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PEOPLE'S REPUBLIC OF CHINA (ROPE) — Benefits from strategic ambiguity that provides cover for modernization without transparency commitments. Opacity enables hedging and regional posturing. d≈0.12, f(d)≈-0.06, σ=1.2 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(nuclear_order_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: UNITED STATES STRATEGIC COMMAND (TANGLED ROPE) — Experiences dual extraction and coordination. Ambiguity enables flexibility in force structure (hedge against Chinese expansion, maintain technological superiority) while also imposing verification asymmetry burden and requiring constant signaling to manage escalation risks. d≈0.48, f(d)≈0.64, σ=1.2 → χ≈0.40. Moderate effective extraction due to self-imposed signaling costs.
constraint_indexing:constraint_classification(nuclear_order_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ARMS CONTROL VERIFICATION COMMUNITIES (SNARE) — Constrained by collapse of ON-SITE inspection regime; extraction mechanism is the unilateral reinterpretation of treaty-defined baseline numbers. Cannot independently verify warhead counts, deployment status, or doctrine boundaries. Credibility premiums on ambiguous signals replace verification. d≈0.88, f(d)≈1.33, σ=1.2 → χ≈0.73.
constraint_indexing:constraint_classification(nuclear_order_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL CTBT ORGANIZATION (SCAFFOLD) — Sees the New START collapse as temporary coordination failure with potential sunset through alternative mechanisms: expanded CTBT verification, Track 2 dialogues, unilateral confidence-building measures (e.g., regular doctrine transparency statements). Organized but constrained by nuclear powers' non-participation. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.26. Low effective extraction due to agency and sunset pathway.
constraint_indexing:constraint_classification(nuclear_order_2026, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: RESIDUAL ARMS CONTROL INSTITUTIONS (PITON) — UN-led forums, NPT review conferences, and treaty secretariats continue performatively, but their functional verification authority has collapsed with New START. Institutional inertia maintains the appearance of oversight without verification teeth. theater_ratio=0.58 reflects moderate theatrical maintenance. d≈0.10, f(d)≈-0.07, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(nuclear_order_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational lens, mutual strategic ambiguity is an inherent feature of nuclear deterrence: no verification regime can achieve perfect transparency in systems designed for second-strike survivability. The opacity is not extractive but structurally necessary. However, ε=0.52, suppression=0.68 contradict the mountain classification — the engine will flag this as false summit, revealing naturalization of contingent institutional collapse.
constraint_indexing:constraint_classification(nuclear_order_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_order_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_order_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_order_2026, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_order_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_order_2026, TR),
    TR >= 0.70.

:- end_tests(nuclear_order_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The post-START regime permits unilateral reinterpretation of baseline warhead counts and deployment status. Russian and Chinese benefit from undefined compliance baselines; non-aligned nations bear uncertainty cost. However, extractiveness is not as severe as pure Snare (>0.66) because mutual deterrence provides some coordination benefit — the constraint has legitimate equilibrium functions. Suppression (0.68): High. Significant barriers to independent verification include elimination of ON-SITE inspection, limited satellite imagery clarity on warhead counts, closed-source intelligence compartmentalization, and technological hedging (hypersonics, slow-flight systems that blur delivery/non-delivery platforms). Verification failure is structural. Theater ratio (0.58): Moderate. UN forums and NPT review conferences continue with rhetoric about arms control but without functional verification capacity. Performative language substitutes for inspections. However, theater is not as high as Piton (≥0.70) because some genuine signaling occurs through military exercises, doctrine statements, and indirect intelligence channels. The regime maintains partial coordination function alongside extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival disagreement. Russian and Chinese perspectives see Rope (pure coordination; opacity enables equilibrium). US perspective sees Tangled Rope (mixed flexibility gains and signaling burden). Secondary powers see Tangled Rope with constraints (benefit from opacity but destabilized). Verification communities see Snare (full extraction; no independent verification). Non-aligned nations see pure Snare (trapped, victimized). The CTBT organization sees Scaffold (temporary; alternative verification pathways possible). Residual institutions see Piton (performative continuation). The analytical observer risks seeing Mountain (deterrence opacity is inherent to nuclear strategy) — but the structural data reveals this as false summit: contingent institutional collapse, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Russian Federation: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary from post-START uncertainty. China: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.06. Net beneficiary from opacity covering modernization. US STRATCOM: Mixed beneficiary (flexibility) + victim (signaling burden) + arbitrage → d≈0.48, f(d)≈0.64. Moderate extraction from internal pressure. Secondary nuclear powers: Beneficiary (opacity cover) + victim (destabilization risk) + constrained → d≈0.62, f(d)≈0.88. Moderate extraction. Verification communities: Victim + constrained → d≈0.88, f(d)≈1.33. High extraction from verification collapse. Non-aligned nations: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction; no exit mechanism. CTBT Organization: Organized + constrained → d≈0.42, f(d)≈0.42. Low effective extraction; agency and sunset pathway visible. Residual institutions: Institutional + arbitrage → d≈0.10, f(d)≈-0.07. Piton classification from theater gate.
 *
 * MANDATROPHY ANALYSIS:
 *   The post-START ambiguity regime resolves the mandatrophy by clarifying that the constraint is genuinely hybrid (Tangled Rope), not mislabeled Rope or Snare. The coordination function exists: mutual deterrence through ambiguous thresholds works at preventing unilateral escalation (Russian/Chinese perspectives see Rope because this function is real). The extraction function exists: verification collapse enables unilateral reinterpretation of baselines and asymmetric opacity benefits (victims see Snare because the cost structure is real). The hybrid classification avoids both false naturalizations (Mountain) and false purity claims. The theater ratio (0.58) reflects this hybridity: performative institutions continue (the coordination scaffolding) while verification capacity collapses (the extraction mechanism). The mandatrophy is RESOLVED because the structural data supports genuine coordination AND genuine asymmetric extraction simultaneously. No single type captures both; Tangled Rope is the correct classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_threshold_definition,
    'What constitutes sufficient verification of compliance: ON-SITE inspection (New START standard), satellite imagery, or unilateral declaration?',
    'Comparative analysis of detection confidence levels across regimes; historical accuracy of non-inspected declarations vs. later-discovered violations',
    'If ON-SITE required: classification remains Snare/Tangled Rope (high extraction). If satellite sufficient: potential path to Rope-like coordination. If declaration accepted: shifts to pure Scaffold (trust-based interim).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_threshold_definition, empirical, 'Technical definition of verification sufficiency').

omega_variable(
    escalation_doctrine_clarity_threshold,
    'Does strategic ambiguity about nuclear escalation thresholds represent legitimate deterrence (coordination) or extractive first-mover advantage (timing manipulation)?',
    'Analysis of near-miss incidents; correlation between doctrine ambiguity and conventional escalation patterns; war gaming outcomes under clarity vs. ambiguity',
    'If coordination: classification shifts toward Rope from beneficiary perspectives. If extraction: classification remains Snare/Tangled Rope. If mixed: confirms Tangled Rope across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_doctrine_clarity_threshold, conceptual, 'Whether doctrine ambiguity serves deterrence or enables extraction').

omega_variable(
    chinese_warhead_asymmetry_structural_position,
    'Does China''s lower declared warhead count relative to US/Russia reflect strategic choice (hedging opacity as advantage) or genuine asymmetry in strategic posture?',
    'Technical intelligence assessment of deployed vs. non-deployed arsenals; economic analysis of modernization rates; signal analysis of doctrine statements',
    'If strategic choice: China experiences Rope (beneficiary). If asymmetry: China experiences Tangled Rope (constrained by verification gap). Determines whether Chinese perspective classification is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chinese_warhead_asymmetry_structural_position, empirical, 'Nature of Chinese warhead count asymmetry').

omega_variable(
    crisis_stability_extraction_mechanism,
    'Does the absence of verification regime increase or decrease likelihood of destabilizing crisis escalation?',
    'Game-theoretic analysis of first-strike incentives under ambiguity; historical comparison with Cold War crisis dynamics; Monte Carlo simulation of escalation ladders',
    'If increases: suppression is structural to mutual deterrence, classification valid. If decreases: ambiguity masks de-escalation mechanisms, classification shifts toward Rope. If neutral: theater dominates (Piton confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_stability_extraction_mechanism, empirical, 'Crisis stability implications of verification collapse').

omega_variable(
    alternative_regime_viability,
    'Can CTBT expansion, Track 2 forums, or voluntary transparency statements genuinely replace ON-SITE inspection verification at acceptable confidence levels?',
    'Technical feasibility assessment of alternatives; historical Track 2 effectiveness; analysis of voluntary compliance patterns in other regimes',
    'If viable: Scaffold sunset pathway confirmed, potential path to new equilibrium Rope. If not viable: Snare classification entrenched, no exit mechanism exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_regime_viability, empirical, 'Viability of alternative verification mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_order_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nuord_tr_t0, nuclear_order_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nuord_tr_t3, nuclear_order_2026, theater_ratio, 3, 0.45).
narrative_ontology:measurement(nuord_tr_t6, nuclear_order_2026, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(nuord_be_t0, nuclear_order_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(nuord_be_t3, nuclear_order_2026, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(nuord_be_t6, nuclear_order_2026, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_order_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_order_2026, hypersonic_weapons_verification_gap).
narrative_ontology:affects_constraint(nuclear_order_2026, tactical_nuclear_doctrine_escalation).
narrative_ontology:affects_constraint(nuclear_order_2026, chinese_capabilities_intelligence_asymmetry).

% DUAL FORMULATION NOTE:
% Post-START ambiguity is downstream of the New START treaty expiration event but represents a distinct structural constraint on the nuclear order. Upstream constraints (treaty negotiation dynamics, compliance interpretation) have their own ε values; post-START ambiguity has ε=0.52 reflecting the hybrid coordination-extraction structure of the uncertainty regime. Downstream constraints (hypersonic verification gaps, tactical doctrine ambiguity, intelligence asymmetries) are enabled by post-START ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_order_2026, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
