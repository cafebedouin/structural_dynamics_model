% ============================================================================
% CONSTRAINT STORY: regional_military_balance_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_military_balance_stability, []).

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
 *   constraint_id: regional_military_balance_stability
 *   human_readable: Regional Military Balance Stability Constraint
 *   domain: geopolitical/security/military
 *
 * SUMMARY:
 *   Regional military balance stability represents a foundational constraint
 *   in multipolar geopolitical systems. It exhibits the full taxonomy of DR
 *   classification types from different structural positions: the security
 *   dilemma appears as an immutable natural law to the civilizational
 *   observer (mountain), but reveals itself as a contingent institutional
 *   arrangement when examined through the perspectives of subordinate states
 *   (snare), competing powers (tangled rope), hegemons (rope), and regional
 *   institutions (piton). The constraint coordinates genuine security
 *   objectives — reducing the likelihood of miscalculation through
 *   transparency, deterrence, and predictability — while simultaneously
 *   extracting resources (military spending, opportunity costs), suppressing
 *   alternatives (non-aligned pathways, regional autonomy), and concentrating
 *   benefits (hegemon status premiums, arms industry profits). The
 *   theater_ratio trajectory (rising from 0.52 to 0.68) reflects degradation
 *   of regional institutions: multilateral confidence-building measures
 *   persist as performative rituals while actual military decisions remain
 *   unilateral or alliance-controlled. This is classic piton dynamics —
 *   institutional inertia maintaining mechanisms despite declining functional
 *   constraint.
 *
 * KEY AGENTS:
 *   - Dominant Regional Hegemon: Primary beneficiary (institutional/arbitrage) — maintains status quo, legitimizes military presence, captures security premiums and alliance rents
 *   - Regional Peer Competitors: Secondary beneficiary and victim (moderate/constrained) — benefit from deterrence coordination but trapped in arms race dynamics with constrained exit options
 *   - Subordinate Regional States: Primary victims (powerless/trapped) — bear fiscal and security costs of arms competition with minimal exit capacity or coordination benefit
 *   - Defense Industry Ecosystem: Beneficiary (institutional/constrained) — profits from arms race perpetuation; constrained by regulatory frameworks and technology transfer controls
 *   - Regional Multilateral Institutions: Degraded facilitators (organized/constrained) — claim coordination function but exercise minimal constraint; theater-high; participate due to legitimacy requirements not functional capacity
 *   - Arms Control Architecture: Temporary scaffolding (analytical/analytical) — provides verification and compliance mechanisms but contains sunset logic; effectiveness degrades as proliferation expands
 *   - Civilian Populations in Volatile Zones: Diffuse victims (powerless/trapped) — bear externalities of arms competition (proxy conflicts, arms smuggling, insecurity spirals) with no voice in balance mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_military_balance_stability, 0.58).
domain_priors:suppression_score(regional_military_balance_stability, 0.72).
domain_priors:theater_ratio(regional_military_balance_stability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_military_balance_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(regional_military_balance_stability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(regional_military_balance_stability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_military_balance_stability, tangled_rope).
narrative_ontology:human_readable(regional_military_balance_stability, "Regional Military Balance Stability Constraint").
narrative_ontology:topic_domain(regional_military_balance_stability, "geopolitical/security/military").

domain_priors:requires_active_enforcement(regional_military_balance_stability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_military_balance_stability, dominant_regional_power).
narrative_ontology:constraint_beneficiary(regional_military_balance_stability, status_quo_preserving_states).
narrative_ontology:constraint_beneficiary(regional_military_balance_stability, arms_manufacturers).
narrative_ontology:constraint_victim(regional_military_balance_stability, rising_regional_powers).
narrative_ontology:constraint_victim(regional_military_balance_stability, non_aligned_states).
narrative_ontology:constraint_victim(regional_military_balance_stability, civilian_populations_in_volatile_zones).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE REGIONAL STATE (SNARE) — Trapped within the balance mechanism. Cannot credibly exit the arms competition without severe security vulnerability. Bears extraction costs (fiscal burden of military spending, opportunity costs, security dilemma acceleration) with minimal exit capacity or coordination benefit. Maximum experienced extraction.
constraint_indexing:constraint_classification(regional_military_balance_stability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL PEER COMPETITOR (TANGLED ROPE) — Experiences genuine coordination function (mutual deterrence, predictability through balance) alongside extraction (arms race spirals, resource diversion, permanent security mobilization). Constrained by security interdependence but retains some agency through alliance formation and military capability development. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(regional_military_balance_stability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DOMINANT REGIONAL HEGEMON (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences the balance as coordination mechanism: maintains status quo, legitimizes military presence, enables arms sales and strategic partnerships. High exit capacity through alternative regional arrangements. Net beneficiary — extraction flows toward this agent through security premiums, alliance commitments, and technology transfer dependencies.
constraint_indexing:constraint_classification(regional_military_balance_stability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEFENSE INDUSTRY ECOSYSTEM (TANGLED ROPE) — Genuine coordination function: provides military platforms that enable deterrence and reduce likelihood of miscalculation. Simultaneously benefits from extraction mechanism: arms race dynamics generate demand for advanced systems, upgrade cycles, and inventory replenishment. Constrained by regulatory requirements and export controls but benefits from the balance's perpetuation. Active enforcement required to maintain supply agreements and technology control regimes.
constraint_indexing:constraint_classification(regional_military_balance_stability, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL MULTILATERAL INSTITUTIONS (PITON) — ASEAN, GCC, African Union, and similar bodies claim to coordinate regional stability but exercise minimal functional constraint on military buildups. Their confidence-building measures, dialogues, and transparency initiatives are largely performative — actual military decisions remain unilateral or alliance-based. Theater ratio high (diplomatic rituals masking continued arms competition). Institutional inertia maintains these mechanisms despite degraded function. Organized agents with constrained exit (regional legitimacy depends on continued participation).
constraint_indexing:constraint_classification(regional_military_balance_stability, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ARMS CONTROL ARCHITECTURE (SCAFFOLD) — International treaty frameworks (NPT, CTBT, missile technology control regimes) represent temporary scaffolding: they reduce transparency barriers and raise compliance costs for escalation, but contain explicit sunset logic. Verification requirements increase over time; inspection regimes degrade when compliance costs rise; frameworks dissolve when hegemonic state withdraws from them. Theater ratio moderate (treaties have genuine verification functions but also symbolic value for prestige). High suppression reflects enforcement through sanctions and export controls. Temporary support structure with declining effectiveness as weaponization technologies proliferate and diversify.
constraint_indexing:constraint_classification(regional_military_balance_stability, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: SECURITY DILEMMA / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational analytical perspective, the balance of power is a structural feature of anarchic international systems: without a sovereign authority, states rationally arm themselves in response to perceived threats, creating a self-perpetuating cycle. This perspective sees the constraint as an immutable consequence of structural anarchy — no agent can credibly disarm without inviting predation. However, the structural data (active enforcement required, beneficiaries/victims present, theater_ratio high) contradicts pure mountain classification and reveals this as naturalization of contingent institutional arrangements (alliance structures, technology control regimes, transparency asymmetries) rather than immutable natural law.
constraint_indexing:constraint_classification(regional_military_balance_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_military_balance_stability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_military_balance_stability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_military_balance_stability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_military_balance_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_military_balance_stability, TR),
    TR >= 0.70.

:- end_tests(regional_military_balance_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts fiscal resources (military budgets divert from civilian spending), opportunity costs (research and talent dedicated to military applications), and security costs (arms race acceleration increases miscalculation risk). The trajectory from 0.38 to 0.58 reflects increasing sophistication and cost of military systems, plus diffusion of advanced capabilities to more regional actors, raising the minimum credible deterrent floor. Suppression (0.72): High. States face severe constraints on exit: unilateral disarmament invites predation; non-participation in alliances forfeits security guarantees; civilian resistance to military spending faces nationalist backlash. Transparency agreements and confidence-building measures nominally reduce suppression but remain largely performative. Theater ratio (0.68): High. Regional multilateral institutions conduct extensive diplomatic rituals, transparency negotiations, and dialogue mechanisms that claim to coordinate stability but exercise minimal functional constraint. Traditional deterrence doctrine itself is increasingly theatrical — nuclear stability through 'mutually assured destruction' is a narrative maintained through continuous enactment (exercises, alerts, doctrine updates) with limited real functional verification. The rise from 0.52 to 0.68 reflects institutional degradation: as actual military decisions diverge from diplomatic forums, the theater required to maintain legitimacy increases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how indexical classification reveals the political economy hidden within claims of natural law. The 'security dilemma' is hegemonic framing that naturalizes a particular institutional arrangement. From the subordinate state's perspective, the constraint is clearly extractive and coercive (snare). From the hegemon's perspective, it is coordination and rule maintenance (rope). From the arms control architecture's perspective, it is a degraded and declining scaffolding (piton becoming increasingly theater as verification regimes fail to constrain proliferation). The perspectival gap itself is diagnostic: if the constraint were a genuine natural law (mountain), all perspectives would converge on the same classification. The divergence reveals that 'stability' benefits are asymmetrically distributed and actively enforced, not naturally emergent.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality distribution follows institutional power: the hegemon (d ≈ 0.05, beneficiary + arbitrage) experiences negative effective extraction — the system extracts toward them. The peer competitor (d ≈ 0.55, mixed victim/beneficiary + constrained) experiences moderate extraction. The subordinate state (d ≈ 0.92, victim + trapped) experiences maximum extraction. The defense industry (d ≈ 0.20, beneficiary + constrained) experiences low extraction, profiting from the mechanism. The regional institution (d ≈ 0.60, nominal coordinator + constrained) experiences moderate extraction through enforcement costs. This distribution is NOT natural — it reflects institutional choices: the treaty structures that advantage some suppliers, the alliance frameworks that concentrate security guarantees, the intelligence asymmetries that disadvantage rising powers, the arms control regimes that lock in technological advantages of established powers.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival decomposition: the regional military balance is simultaneously genuine coordination (subordinate states benefit from reduced miscalculation risk) and coercive extraction (they bear disproportionate costs and have no exit). The constraint is tangled rope, not pure rope and not pure snare. The key mandatrophy trap: calling it 'natural law' (mountain) obscures that the benefits flow to specific institutional actors (hegemon, arms industry) through specific institutional mechanisms (alliances, technology transfer controls, intelligence asymmetries) that could be reformed. Acknowledging the tangled rope classification (genuine coordination + asymmetric extraction + active enforcement) enables identification of which institutions are doing the extraction and thus which institutional reforms might rebalance the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transparency_commitment_credibility,
    'Do regional transparency agreements (military budgets, force deployments, exercise notifications) represent genuine coordination or performative compliance masking continued arms escalation?',
    'Cross-verification of declared vs observed military capabilities; analysis of verification regime compliance rates; correlation between transparency measures and actual de-escalation outcomes',
    'If genuine coordination: classification shifts toward Rope for more perspectives. If performative: theater_ratio increases and classification shifts toward Snare/Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_commitment_credibility, empirical, 'Whether transparency agreements reduce actual security competition').

omega_variable(
    arms_race_inherency,
    'Is the arms race dynamic inherent to regional multipolarity or contingent on specific institutional arrangements (technology transfer policies, alliance structures, intelligence asymmetries)?',
    'Comparative analysis of different regional security systems; historical cases of arms race pause/reversal; modeling of alternative institutional arrangements with same baseline capabilities',
    'If inherent: mountain classification warranted. If contingent: classification shifts to Tangled Rope/Snare; policy interventions in institutional arrangements could reduce extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arms_race_inherency, conceptual, 'Whether arms dynamics are structural or institutional').

omega_variable(
    coalition_formation_threshold,
    'At what capability ratio do subordinate states achieve sufficient coalition power to renegotiate extraction terms or exit the balance mechanism entirely?',
    'Historical analysis of successful/failed coalition reversals; game-theoretic modeling of coalition stability; longitudinal analysis of power transition dynamics',
    'If threshold is low and achievable: powerless perspective classification may upgrade to organized; snare may shift toward tangled_rope. If threshold is high/inaccessible: mountain-like immutability confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_formation_threshold, empirical, 'Capability threshold for coalition renegotiation').

omega_variable(
    substitution_technology_disruption,
    'Do emerging military technologies (hypersonics, autonomous systems, space-based systems) disrupt the balance mechanism by rendering traditional deterrence models obsolete or by creating new extraction mechanisms?',
    'Analysis of technology adoption rates; modeling of stability outcomes under alternative capability distributions; longitudinal tracking of doctrinal responses to new technologies',
    'If disruption is destabilizing: classification may shift toward Snare (increased suppression, breakdown of coordination). If new equilibrium forms: classification may shift toward new Rope or Tangled Rope with different beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_technology_disruption, empirical, 'Impact of emerging technologies on balance stability').

omega_variable(
    hegemon_withdrawal_scenario,
    'What are the structural consequences if the dominant regional power withdraws commitment to the balance architecture (shifts focus away from region, reduces military presence, exits alliance frameworks)?',
    'Scenario modeling; historical precedent analysis (US withdrawal from regions, Soviet collapse, British decline); game-theoretic equilibrium analysis without hegemon stabilizer',
    'If withdrawal triggers arms escalation: confirms that hegemon extraction is coercive (snare). If new equilibrium forms: suggests balance mechanism is more distributed (rope). If collapse occurs: reveals mountain-like structural fragility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hegemon_withdrawal_scenario, conceptual, 'Stability consequences of hegemon withdrawal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_military_balance_stability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rmbs_tr_t0, regional_military_balance_stability, theater_ratio, 0, 0.52).
narrative_ontology:measurement(rmbs_tr_t5, regional_military_balance_stability, theater_ratio, 5, 0.6).
narrative_ontology:measurement(rmbs_tr_t10, regional_military_balance_stability, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(rmbs_be_t0, regional_military_balance_stability, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rmbs_be_t5, regional_military_balance_stability, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(rmbs_be_t10, regional_military_balance_stability, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_military_balance_stability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(regional_military_balance_stability, 0.18).
narrative_ontology:affects_constraint(regional_military_balance_stability, arms_proliferation_dynamics).
narrative_ontology:affects_constraint(regional_military_balance_stability, hegemon_credibility_commitment).
narrative_ontology:affects_constraint(regional_military_balance_stability, proxy_conflict_escalation).
narrative_ontology:affects_constraint(regional_military_balance_stability, alliance_credibility_mechanics).

% DUAL FORMULATION NOTE:
% Regional military balance is downstream of several distinct constraints: the security dilemma (immutability question), arms technology proliferation (capability distribution), and hegemon credibility (alliance reliability). These interact: if hegemon credibility declines, the balance mechanism weakens and subordinate states face increased extraction (must strengthen independent deterrents). Decomposed as separate stories and linked through network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_military_balance_stability, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
