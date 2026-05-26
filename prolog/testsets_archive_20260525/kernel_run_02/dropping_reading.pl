% ============================================================================
% CONSTRAINT STORY: dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dropping_reading, []).

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
 *   constraint_id: dropping_reading
 *   human_readable: Deterrence as Coordination Equilibrium (Dropping Reading)
 *   domain: international_relations/nuclear_deterrence/strategic_stability
 *
 * SUMMARY:
 *   The 'dropping reading' of the total_war_reachability_boundary kernel
 *   interprets the observed decline in war probability as evidence that
 *   deterrence has matured into a stable coordination equilibrium. The core
 *   claim is that while the reachability of total war remains structurally
 *   intact (nuclear arsenals persist, second-strike capabilities endure), the
 *   probability of war has dropped and continues to drop due to improved
 *   signaling, institutionalized communication channels, confidence-building
 *   measures, and increasing transparency in deterrence postures. From this
 *   reading, deterrence is not an immutable natural law but a deliberately
 *   constructed and continuously refined coordination mechanism — a Rope, or
 *   in its hybrid extraction-coordination form, a Tangled Rope. The
 *   constraint exhibits genuine coordination function (preventing war between
 *   peers that cannot resolve disputes through conventional means) alongside
 *   asymmetric extraction (civilian populations bear nuclear risk while
 *   nuclear powers gain security stability, deterrence institutions maintain
 *   career and resource streams). The probability drop suggests the
 *   coordination mechanism is working; the persistent reachability suggests
 *   the threat structure remains intact but confidence in mutual deterrence
 *   has increased.
 *
 * KEY AGENTS:
 *   - Nuclear Armed State Security Apparatus: Primary beneficiary (institutional/arbitrage) — gains security credibility and maintains status through deterrence; can arbitrage to alternative postures
 *   - Deterrence Credibility Maintenance Community: Organized beneficiary (organized/constrained) — strategic theorists, military planners, arms control negotiators who maintain the equilibrium; career and institutional interests locked into deterrence framework
 *   - Civilian Populations Under Nuclear Threat: Primary victim (powerless/trapped) — bear existential risk with zero agency; cannot exit the constraint; benefit from probability decline but not from the coordination mechanism itself
 *   - Non-Nuclear States: Secondary victim (moderate/constrained) — receive security guarantees from nuclear patrons but constrained in strategic autonomy; benefit from peer-level deterrence stability but experience extraction through dependency
 *   - Cold War Deterrence Institutional Framework: Institutional actor (institutional/arbitrage) — formal structure (NPT, arms control treaties, verification protocols) persists as Piton; theater increasing over time as functional necessity may be declining
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as inevitable strategic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dropping_reading, 0.52).
domain_priors:suppression_score(dropping_reading, 0.48).
domain_priors:theater_ratio(dropping_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dropping_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(dropping_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dropping_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dropping_reading, tangled_rope).
narrative_ontology:human_readable(dropping_reading, "Deterrence as Coordination Equilibrium (Dropping Reading)").
narrative_ontology:topic_domain(dropping_reading, "international_relations/nuclear_deterrence/strategic_stability").

domain_priors:requires_active_enforcement(dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(dropping_reading, formalized).
narrative_ontology:cs_authority_grounding(dropping_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(dropping_reading).
narrative_ontology:cs_kernel_id(dropping_reading, total_war_reachability_boundary).
narrative_ontology:cs_reading_relation(dropping_reading, contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation(dropping_reading, contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom(dropping_reading, foundational, probability_declining_toward_stability).
narrative_ontology:cs_axiom_status(probability_declining_toward_stability, holdable).
narrative_ontology:cs_axiom_grounding(dropping_reading, probability_declining_toward_stability, empirically_contingent).
narrative_ontology:cs_axiom(dropping_reading, foundational, deterrence_mechanism_functionally_proven).
narrative_ontology:cs_axiom_status(deterrence_mechanism_functionally_proven, holdable).
narrative_ontology:cs_axiom_grounding(dropping_reading, deterrence_mechanism_functionally_proven, empirically_contingent).
narrative_ontology:cs_reference_frame(dropping_reading, deterrence_stability_equilibrium).
narrative_ontology:cs_drift_state(dropping_reading, contemporary_transparency_era, gap(stable, minor, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dropping_reading, nuclear_armed_state_security_apparatus).
narrative_ontology:constraint_beneficiary(dropping_reading, deterrence_credibility_maintainers).
narrative_ontology:constraint_victim(dropping_reading, civilian_populations_under_nuclear_threat).
narrative_ontology:constraint_victim(dropping_reading, non_nuclear_states).
narrative_ontology:constraint_victim(dropping_reading, constrained_nuclear_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Trapped in deterrence equilibrium with no exit option. Bear the maximum existential cost of the constraint (potential annihilation) while having zero agency in deterrence calculations. The probability has dropped, but reachability remains — they cannot leave the threat zone. Maximum experienced extraction: zero benefit, maximum risk exposure, no exit capacity.
constraint_indexing:constraint_classification(dropping_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-NUCLEAR STATES (TANGLED_ROPE) — Constrained to participate in deterrence framework through security guarantees or alliance membership. Experience genuine coordination benefit (protection from peer powers) alongside extraction (dependency on nuclear patron, constrained strategic autonomy). Moderate experienced extraction — significant benefits but also significant constraints.
constraint_indexing:constraint_classification(dropping_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NUCLEAR ARMED STATE SECURITY APPARATUS (ROPE) — Primary beneficiary with arbitrage options. Experiences deterrence as a pure coordination equilibrium: the constraint solves a genuine coordination problem (avoiding war through credible threat). Maintains deterrence credibility as the core function. Low experienced extraction because the coordination benefit flows to this actor — they have designed and benefit from the equilibrium. Can arbitrage to alternative security postures or arms control regimes if deterrence fails.
constraint_indexing:constraint_classification(dropping_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DETERRENCE CREDIBILITY MAINTENANCE COMMUNITY (TANGLED_ROPE) — Organized actors (strategic theorists, military planners, arms control negotiators) who maintain the deterrence equilibrium. Experience both genuine coordination function (preventing war through credible threats) and extraction (their career interest in perpetuating deterrence theory, resource allocation to nuclear forces, institutional preservation of the deterrence establishment). Constrained by the need to maintain strategic ambiguity and public commitment to deterrence — cannot easily exit without risking the entire equilibrium. Moderate-high experienced extraction due to institutional lock-in, but genuine coordination benefits exist.
constraint_indexing:constraint_classification(dropping_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: COLD WAR DETERRENCE INSTITUTIONAL FRAMEWORK (PITON) — The formal treaty structure (NPT, Comprehensive Nuclear Test Ban, bilateral arms control agreements) is largely performing as ritual. Theater ratio reflects that verification mechanisms, posture announcements, and treaty compliance claims maintain performative commitment to deterrence even as the underlying threat credibility has partially degraded (probability has dropped, but reachability remains — suggesting the mechanism is partially inert). The framework persists through institutional inertia: dismantling it requires more coordination cost than maintaining it, even though its primary function (preventing major power war via nuclear balance) is partially atrophied. Piton classification derives from theater_ratio and the gap between formal structure and actual strategic reality.
constraint_indexing:constraint_classification(dropping_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, mutual nuclear deterrence appears as an immutable structural fact: any two actors with second-strike capability face an inherent coordination problem that cannot be solved except through deterrence credibility. Probability of war may drop due to improved signaling, but the reachability of total war is irreducible — it remains structurally possible as long as nuclear arsenals exist. This perspective risks naturalizing what is actually a contingent institutional arrangement (the interpretation of deterrence as the ONLY viable coordination mechanism). The engine's false summit detector will identify this if beneficiaries are declared — the analytical observer's naturalization of 'deterrence as inevitable' masks the contingent institutional structure that maintains deterrence credibility.
constraint_indexing:constraint_classification(dropping_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dropping_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dropping_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dropping_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dropping_reading, TR),
    TR >= 0.70.

:- end_tests(dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits mixed coordination and extraction. The coordination component (preventing major power war) is genuine and benefits all parties; however, the asymmetry is substantial: civilian populations under nuclear threat receive zero benefit while bearing maximum existential risk. Nuclear powers gain security credibility and maintain strategic dominance. The deterrence establishment gains institutional resources and career opportunities. The 0.52 value reflects that the coordination benefit is real and significant (preventing war is a major shared good) but the distribution of this benefit is highly asymmetric — most gains flow to nuclear powers and deterrence institutions, while most risk is borne by civilian populations with no voice in the arrangement. Suppression (0.48): Moderate. Barriers to exit from the constraint include: military alliance structures create dependencies on nuclear patrons (constrain non-nuclear states); domestic political costs of nuclear disarmament (deter revisionist policy); institutional investment in deterrence establishments (structural resistance to arms control). However, suppression is not total — some actors have successfully exited (South Africa), some non-aligned movements have reduced dependence (India, Brazil), and arms control negotiations have periodically constrained deterrence mechanisms. Theater ratio (0.35): Low-moderate, and rising over time (0.22 → 0.35). During Cold War, the deterrence mechanism was functionally vital — signaling risks, deployments, and crisis communications were all serving genuine coordination purposes. Verification of treaty compliance was functionally essential. As probability has dropped and confidence in mutual deterrence has increased, an increasing proportion of the institutional activity is performative: treaty announcements, strategic doctrine declarations, and verification theater maintain the appearance of active deterrence management when the underlying threat credibility has partially stabilized. The rising theater ratio suggests institutional inertia is increasing — the apparatus is performing its own legitimacy more than solving the coordination problem.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps in this constraint reveal the core insight of the dropping reading: the probability decline is not uniform across observers. Nuclear powers see probability as dropping (Rope from their perspective — they experience deterrence as solving their coordination problem), while civilian populations see reachability as constant (Snare — they experience persistent existential risk regardless of probability drop). Non-nuclear states experience the constraint as Tangled Rope — they benefit from peer-level deterrence stability but are constrained by alliance dependencies. The deterrence establishment sees institutional persistence (Piton) — the formal structure remains even as its functional necessity may be declining. The analytical observer risks a Mountain classification that naturalizes what is actually a contingent institutional arrangement. The perspectival gaps demonstrate that the 'dropping reading' is fundamentally about the distribution of risk and benefit: probability dropping benefits those with agency in the deterrence calculation (nuclear powers); probability dropping does NOT reduce reachability or existential exposure for those without agency (civilian populations). The reading's classification as Tangled Rope derives from this asymmetry: genuine coordination function exists (preventing war) but is coupled with extraction (risk distribution).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies dramatically across perspectives due to the agents' different structural relationships to the deterrence constraint. Nuclear armed state security apparatus: Beneficiary with arbitrage options → d ≈ 0.15 → low/negative f(d) → negative experienced extraction. They experience deterrence as coordination because they designed it and benefit from its stability. Deterrence credibility maintenance community: Organized beneficiary-victim (genuinely benefits from preventing war, but career locked into deterrence framework) → d ≈ 0.40 → moderate f(d) → moderate experienced extraction. Civilian populations under nuclear threat: Victims with zero agency → d ≈ 0.95 → high f(d) → high experienced extraction. They receive zero agency benefit from the coordination function and bear maximum existential risk. Non-nuclear states: Mixed beneficiary-victims (benefit from peer deterrence stability, constrained by alliance dependencies) → d ≈ 0.55 → moderate-high f(d). The dropping reading's claim that 'deterrence is a Rope' applies specifically to the beneficiary perspectives; from victim perspectives, the constraint is Snare or Tangled Rope. The perspectival gap is the reading's diagnostic signature: probability dropping benefits some agents (those with deterrence agency) more than others (those without).
 *
 * MANDATROPHY ANALYSIS:
 *   The dropping reading resolves the mandatrophy by anchoring deterrence in a specific empirical claim: probability has dropped, and this drop is evidence that deterrence as a coordination mechanism is working. This claim distinguishes the dropping reading from the contraction reading (which claims reachability itself is contracting) and the contingent reading (which claims reachability is stable only because deterrence maintenance is active). The mandatrophy resolution is: deterrence classifies as Tangled Rope (from main analytical perspective) because it contains both genuine coordination function (preventing war between actors that cannot resolve disputes conventionally) and genuine extraction (asymmetric risk distribution). The coordination benefit prevents the constraint from collapsing to Snare; the extraction mechanism prevents it from collapsing to Rope. The dropping reading's empirical claim (probability declining) supports the Tangled Rope classification over Mountain (not an immutable law) or Rope (not purely coordination). The analytical observer's risk of Mountain classification is detected by the false_summit mechanism: if beneficiaries are declared (nuclear powers who benefit from deterrence credibility), the engine computes whether the Mountain naturalization is justified or whether the constraint is actually a contingent institutional arrangement maintained by identified beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probability_reachability_decoupling,
    'Does the drop in war probability reflect a genuine change in the underlying coordination equilibrium, or merely improved confidence in deterrence credibility while reachability remains structurally intact?',
    'Comparative analysis of signaling mechanisms across eras (Cold War crisis signaling vs. contemporary diplomacy); measurement of strategic ambiguity reduction and transparency increases; assessment of whether probability decline correlates with structural changes in arsenal deployment, verification, or communication protocols',
    'If probability drop reflects genuine equilibrium shift: deterrence may be transitioning from Snare to Rope from civilian perspective. If probability drop reflects only improved signaling within unchanged reachability: the constraint remains Tangled Rope — extraction mechanism is stable, only perception has changed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_reachability_decoupling, empirical, 'Whether probability decline represents structural equilibrium change or confidence shift in unchanged reachability').

omega_variable(
    coordination_function_sufficiency,
    'Does deterrence genuinely solve the coordination problem of preventing major power war, or is the apparent peace an artifact of cost-imposing stability that would collapse if actors could coordinate on alternative arrangements?',
    'Historical counterfactual analysis: would major powers have cooperated on an alternative security arrangement if nuclear weapons had not existed? Game-theoretic analysis of incentive structures under nuclear vs. conventional-only scenarios. Comparative institutional analysis of nuclear-armed vs. non-nuclear peer relationships.',
    'If deterrence is the minimal coordination solution: Rope classification is justified from beneficiary perspective, constraint is primarily coordination with secondary extraction. If deterrence masks unexploited mutual gains: the coordination benefit is illusory, and the constraint is primarily extraction (Snare) maintained by mutual threat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_sufficiency, conceptual, 'Whether deterrence represents genuine coordination function or masked extraction mechanism').

omega_variable(
    institutional_inertia_vs_functional_necessity,
    'To what extent does the deterrence institutional framework persist because it is functionally necessary for strategic stability versus persisting due to institutional inertia and career investment in deterrence theory?',
    'Analysis of institutional behavior during periods of geopolitical détente (capability maintenance, force deployment, strategic doctrine updates); comparison of deterrence establishment positions during arms control negotiations vs. deterrence-challenging scenarios; tracking of funding and personnel flows to deterrence institutions across strategic climates.',
    'If primarily functional necessity: Piton classification is incorrect; the framework is a vital Rope or Tangled Rope. If primarily institutional inertia: Piton is correct; the framework''s theater ratio will increase over time as the underlying functional requirement decays. The reading''s claim that ''deterrence is a rope, not a mountain'' depends on the constraint being functionally contingent rather than structurally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_functional_necessity, empirical, 'Degree of institutional inertia versus functional necessity in deterrence framework persistence').

omega_variable(
    reading_contingency,
    'This is the ''dropping reading'' of the total_war_reachability_boundary kernel. What is the core premise that distinguishes this reading from the contraction_reading and contingent_reachability_reading?',
    'Explicit axiom declaration in cs_structure.axioms; mapping of how the probability-dropping-but-reachability-stable observation maps differently to classification depending on whether one reads the boundary as dropped (probability permanently reduced), contracting (reachability narrowing), or contingently reachable (dependent on deterrence maintenance).',
    'Dropped reading: deterrence is a Rope that has proven stable; the coordination mechanism works, probability is declining, extraction is declining. Contraction reading: total war reachability itself is contracting due to verification and transparency; the boundary is moving inward. Contingent reading: reachability is held stable by active maintenance of deterrence; if maintenance fails, the constraint reverts to Snare. The three readings forecast different futures and respond differently to institutional change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contingency, conceptual, 'Distinguishing premise of the dropping reading versus contraction and contingency readings').

omega_variable(
    false_summit_risk_analytical_observer,
    'Does the analytical observer''s ''mountain'' classification represent a genuine natural law of strategic interaction (mutual deterrence is the only possible equilibrium), or is it a naturalization of a contingent institutional arrangement (deterrence is our chosen coordination mechanism, but alternatives might exist)?',
    'Historical analysis of alternative coordination mechanisms proposed or attempted (arms control treaties, transparency regimes, multinational security guarantees); game-theoretic exploration of non-deterrence equilibria in symmetric nuclear scenarios; examination of whether the ''inevitability'' of deterrence rests on empirical facts (reachability) or normative commitments (belief in deterrence stability).',
    'If genuine natural law: deterrence remains a Mountain for all observers; reachability is irreducible. If naturalization: the false_summit_mountain signature should fire; the constraint should reclassify to Tangled Rope at the analytical level, revealing the beneficiaries (nuclear powers) who maintain deterrence as inevitable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_risk_analytical_observer, conceptual, 'Whether analytical observer''s mountain classification naturalizes contingent institution or reflects genuine strategic necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dropping_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drop_theater_cold_war, dropping_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(drop_theater_post_cold_war, dropping_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(drop_theater_contemporary, dropping_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(drop_extractiveness_cold_war, dropping_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(drop_extractiveness_post_cold_war, dropping_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(drop_extractiveness_contemporary, dropping_reading, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dropping_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dropping_reading, contraction_reading).
narrative_ontology:affects_constraint(dropping_reading, contingent_reachability_reading).
narrative_ontology:affects_constraint(dropping_reading, nuclear_first_strike_credibility).
narrative_ontology:affects_constraint(dropping_reading, extended_nuclear_deterrence).
narrative_ontology:affects_constraint(dropping_reading, strategic_ambiguity_maintenance).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel has three distinct readings. The dropping_reading (this file) claims probability is declining while reachability remains constant — deterrence is a maturing Rope/Tangled Rope. The contraction_reading claims reachability itself is narrowing — the boundary is moving inward. The contingent_reachability_reading claims reachability is stable only through active deterrence maintenance — sudden reversion risk exists. Each reading has its own constraint file with distinct ε values, measurement trajectories, and omega variables. They are linked via network.affects_constraints to enable contamination propagation analysis: if one reading's empirical foundations shift, the linked readings are affected. The dropping reading depends on the empirical claim that probability is genuinely declining; the contraction reading depends on the claim that verification improvements are narrowing reachability; the contingent reading depends on the claim that maintenance is active and stability is contingent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dropping_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
