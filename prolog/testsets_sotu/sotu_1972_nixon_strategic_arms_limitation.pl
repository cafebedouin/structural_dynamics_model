% ============================================================================
% CONSTRAINT STORY: sotu_1972_nixon_strategic_arms_limitation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1972_nixon_strategic_arms_limitation, []).

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
 *   constraint_id: sotu_1972_nixon_strategic_arms_limitation
 *   human_readable: Strategic Arms Limitation Treaty (SALT I) Negotiation Framework
 *   domain: military/geopolitics/arms_control
 *
 * SUMMARY:
 *   The Strategic Arms Limitation Talks (SALT I), culminating in the 1972 ABM
 *   Treaty and Interim Agreement on Strategic Offensive Arms, embed a
 *   critical structural innovation into Cold War dynamics: the subordination
 *   of unilateral military escalation to bilateral negotiation and mutual
 *   agreement. This constraint gates U.S. defense spending increases
 *   conditional on first achieving negotiated arms reduction agreements with
 *   the Soviet Union. The mechanism reverses the default Cold War posture of
 *   action-reaction arms racing, replacing it with a presumption of restraint
 *   pending mutual commitment. Benefits accrue to both superpowers through
 *   reduced existential risk and reciprocal verification certainty; costs
 *   fall on domestic resource allocation (defense spending that cannot be
 *   increased without Soviet agreement) and military establishment autonomy
 *   (the capacity for unilateral strategic escalation is constrained). The
 *   constraint exhibits all six DR types from different structural positions,
 *   illustrating how indexical classification operates at the geopolitical
 *   scale. Soviet and U.S. civilians experience it as snare (trapped in
 *   resource competition with no exit). Military establishments experience it
 *   as tangled rope (coordination benefit of deterrent stability against
 *   extraction cost of escalation autonomy). The superpowers institutionally
 *   experience it as rope (mutual coordination without asymmetry). Arms
 *   control advocates perceive it as scaffold (temporary bridge toward
 *   multilateral verification architectures). Cold War ideological structures
 *   maintain it performatively as piton (theater of rational statesmanship).
 *   The analytical observer risks the false summit of naturalizing bilateral
 *   negotiation as inherent to deterrence theory rather than as a contingent
 *   institutional choice.
 *
 * KEY AGENTS:
 *   - U.S. Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — retains escalation authority contingent on SALT failure; lobbies against constraints
 *   - Soviet Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — mirrors U.S. position; retains escalation authority contingent on SALT failure
 *   - U.S. Civilian Population: Primary victim (powerless/trapped) — bears domestic resource reallocation cost; cannot unilaterally enforce the constraint
 *   - Soviet Civilian Population: Primary victim (powerless/trapped) — bears domestic resource reallocation cost under sustained arms racing; contingent on U.S. willingness to negotiate
 *   - U.S. Military Establishment: Secondary actor (organized/constrained) — experiences mixed coordination (deterrent stability) and extraction (escalation autonomy cap)
 *   - Soviet Military Establishment: Secondary actor (organized/constrained) — experiences mixed coordination and extraction; constrained by treaty verification
 *   - Arms Control Coalition: Organized observer (organized/constrained) — perceives SALT as temporary bridge toward multilateral verification; advocates for sunset into alternative mechanisms
 *   - Cold War Ideology Apparatus: Institutional actor (institutional/arbitrage) — maintains performative verification and negotiation theater; has exit option of Cold War termination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1972_nixon_strategic_arms_limitation, 0.38).
domain_priors:suppression_score(sotu_1972_nixon_strategic_arms_limitation, 0.42).
domain_priors:theater_ratio(sotu_1972_nixon_strategic_arms_limitation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1972_nixon_strategic_arms_limitation, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1972_nixon_strategic_arms_limitation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1972_nixon_strategic_arms_limitation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1972_nixon_strategic_arms_limitation, tangled_rope).
narrative_ontology:human_readable(sotu_1972_nixon_strategic_arms_limitation, "Strategic Arms Limitation Treaty (SALT I) Negotiation Framework").
narrative_ontology:topic_domain(sotu_1972_nixon_strategic_arms_limitation, "military/geopolitics/arms_control").

domain_priors:requires_active_enforcement(sotu_1972_nixon_strategic_arms_limitation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1972_nixon_strategic_arms_limitation, both_superpowers_existential_risk_reduction).
narrative_ontology:constraint_beneficiary(sotu_1972_nixon_strategic_arms_limitation, civilian_domestic_resource_allocation).
narrative_ontology:constraint_victim(sotu_1972_nixon_strategic_arms_limitation, military_industrial_complex_profit_asymmetry).
narrative_ontology:constraint_victim(sotu_1972_nixon_strategic_arms_limitation, unilateral_escalation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET CIVILIAN POPULATION (SNARE) — Trapped within the escalation cycle. Soviet civilians bear the suppressed costs of continued arms racing (resource diversion from housing, healthcare, food production) whether or not SALT succeeds. No exit: the constraint's existence is contingent on U.S. willingness to negotiate, which the USSR cannot unilaterally guarantee. Extraction flows toward the military-industrial apparatus on both sides; the Soviet civilian bears the cost with no agency in the negotiation.
constraint_indexing:constraint_classification(sotu_1972_nixon_strategic_arms_limitation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: U.S. CIVILIAN POPULATION (SNARE) — Trapped in resource competition: defense spending increases reduce domestic spending on education, healthcare, infrastructure. The constraint creates a gate that SHOULD protect domestic resources (if SALT succeeds, unilateral escalation is forestalled), but the gate is conditional on Soviet agreement — U.S. civilians cannot unilaterally exit the escalation pressure. If SALT fails, the constraint collapses and defense spending rises without constraint. High suppression (0.42): no alternative allocation mechanism exists; civilians bear the cost as the constraint-or-escalation binary.
constraint_indexing:constraint_classification(sotu_1972_nixon_strategic_arms_limitation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: U.S. MILITARY ESTABLISHMENT (TANGLED ROPE) — Benefits from the coordination function (SALT reduces existential risk of war, stabilizes deterrence) while experiencing extraction: the constraint caps unilateral escalation authority. The military retains the right to increase spending if SALT fails, but the negotiation gate constrains immediate action. Constrained exit: the military can oppose SALT politically and lobby for increased spending, but cannot unilaterally escalate without treaty violation. Both coordination (deterrent stability through mutual restraint) and extraction (loss of autonomous escalation authority) present.
constraint_indexing:constraint_classification(sotu_1972_nixon_strategic_arms_limitation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: SOVIET MILITARY ESTABLISHMENT (TANGLED ROPE) — Mirrors the U.S. military position: benefits from SALT's stabilization function while constrained by the cap on unilateral escalation. The Soviet military cannot unilaterally escalate without treaty violation, but retains political influence over SALT compliance decisions. Like the U.S. military, they experience both coordination gain (deterrent stability) and extraction loss (autonomy cap).
constraint_indexing:constraint_classification(sotu_1972_nixon_strategic_arms_limitation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BOTH SUPERPOWERS (INSTITUTIONAL ROPE) — At the superpower level, SALT is pure coordination: it solves the mutual commitment problem (how to stabilize deterrence without requiring trust) through verifiable constraints. Both superpowers benefit from reduced existential risk. Exit options are high (arbitrage): either superpower can withdraw from SALT, escalate, and pursue advantage. The constraint persists because both find it rational to remain — coordination benefit exceeds exit value. No asymmetric extraction at the institutional level.
constraint_indexing:constraint_classification(sotu_1972_nixon_strategic_arms_limitation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ARMS CONTROL COALITION (SCAFFOLD) — Organized actors (nonproliferation advocates, peace movements, scientific arms control analysts) perceive the SALT framework as a temporary bridge toward institutional governance structures that would make bilateral arms control unnecessary. The sunset vision: as verification capabilities improve and international institutions mature, bilateral treaty architecture becomes superseded by multilateral verification and enforcement. The constraint has low theater at this perspective because the negotiation process is technically rigorous (verification protocols, threshold measurement, data sharing) rather than performative. High-confidence sunset because technological verification and international institutional development enable exit from bilateral dependency.
constraint_indexing:constraint_classification(sotu_1972_nixon_strategic_arms_limitation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: COLD WAR IDEOLOGY APPARATUS (PITON) — At the civilizational timescale, the constraint's functional verification role (counting warheads, monitoring tests) declines relative to its performative role (demonstrating rational superpower statesmanship, legitimizing defense establishments to domestic publics). The theater ratio (0.58) reflects this: much of SALT's institutional function is ceremonial — the optics of negotiation prove that escalation is 'measured' and 'rational' rather than reflexive. The constraint persists through inertia and performative necessity even when the actual verification gap widens (verification is theater; actual compliance determination becomes political). The ideology apparatus has arbitrage-level exit options: Cold War structures can be maintained or abandoned with minimal constraint — the treaty becomes decorative rather than functional.
constraint_indexing:constraint_classification(sotu_1972_nixon_strategic_arms_limitation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective optimizing for deterrent stability, some form of mutual constraint mechanism is mathematically inherent to stable two-player competition when both players face existential risk. The constraint appears as a natural law: any rational superpower dyad must implement verification and bilateral agreement to avoid mutual destruction. This perspective risks naturalizing what is actually a contingent institutional solution (bilateral treaties) rather than recognizing it as one of many possible mechanisms (multilateral governance, technological verification, third-party enforcement). The false summit detector should flag this: the 'inherent to deterrence theory' framing disguises political choices about institutional design.
constraint_indexing:constraint_classification(sotu_1972_nixon_strategic_arms_limitation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1972_nixon_strategic_arms_limitation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1972_nixon_strategic_arms_limitation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1972_nixon_strategic_arms_limitation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1972_nixon_strategic_arms_limitation, TR),
    TR >= 0.70.

:- end_tests(sotu_1972_nixon_strategic_arms_limitation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The constraint creates asymmetric benefits between military establishments (which retain escalation authority contingent on SALT failure) and civilian populations (which depend on continued SALT compliance to prevent resource diversion). The measurement trajectory (0.28 → 0.38) reflects that extractiveness increased as the negotiation progressed — the initial perception of a coordination gate (1969, low extractiveness) shifted to awareness that the gate is conditional on Soviet agreement and can be circumvented by military modernization outside SALT scope (1975, higher extractiveness). Suppression (0.42): Moderate. Significant barriers to exit include: (1) the gate's conditionality on Soviet agreement (civilians cannot unilaterally enforce), (2) military-industrial lobbying against SALT constraints, (3) the action-reaction cycle's institutional embeddedness. But suppression is not total — SALT represents a genuine break from default escalation dynamics, and civilian actors can organize to defend treaty compliance. Theater ratio (0.58): Moderate-high. Significant performative content: the optics of rational superpower negotiation legitimize the defense establishment to domestic publics; verification protocols are technically sophisticated but also serve symbolic reassurance. Theater increased over the interval (0.42 → 0.58) as the constraint matured and verification became routine rather than innovative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence: civilian powerlessness vs. military institutional pragmatism vs. superpower coordination vs. ideological performance. The gap reveals that the same structural mechanism (negotiation gate on escalation) functions as pure extraction for trapped actors (snare), mixed coordination-extraction for constrained military actors (tangled rope), pure coordination for institutional actors with exit options (rope), temporary architecture for organized advocates (scaffold), and performative ritual for ideological structures (piton). The false-summit mountain perspective demonstrates the framework's diagnostic power: the analytical observer can be seduced into naturalizing what is actually a contingent institutional choice by framing bilateral negotiation as 'inherent to deterrence theory.'
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) determines how each agent experiences extractiveness. Soviet and U.S. civilians are victims with trapped exit options: high d (0.85-0.95) → high f(d) → high experienced extraction. Military establishments are constrained actors with both beneficiary and victim status: moderate d (0.50-0.60) → moderate f(d) → moderate experienced extraction. Institutional superpowers are beneficiaries with arbitrage options: low d (0.15-0.25) → low/negative f(d) → low/negative experienced extraction (they gain from the constraint). The engine derives d from the beneficiary/victim declarations and exit options: civilians are victims (d toward 1.0) + trapped (cannot exit) = high d; military establishments are both beneficiary (coordination gain) and victim (autonomy extraction) + constrained (can exit at political cost) = moderate d; superpowers are beneficiaries (mutual coordination) + arbitrage (can withdraw if advantaged) = low d. The scope modifier σ(S) applies to all perspectives equally (global scope = 1.2), scaling extractiveness upward.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that each classification is perspectival and correct from its structural position. The constraint is NOT uniquely snare or rope or mountain — it is all six types simultaneously, experienced differently by agents with different power levels, time horizons, exit options, and spatial positions. The SALT I framework demonstrates the core insight of indexical classification: the same structural mechanism can be simultaneously extraction and coordination, mountain and contingency, depending on the observer's structural relationship. The U.S. military establishment genuinely experiences coordination gain (deterrent stability) combined with extraction loss (autonomy cap) — tangled rope is not a compromise classification but an accurate description of their mixed structural position. Civilians genuinely experience snare (trapped in the outcome without agency), while superpowers genuinely experience rope (mutual coordination). The mandate to choose a single type is satisfied by declaring SALT I's claimed type as tangled rope (the primary functional mechanism: hybrid coordination-extraction that requires active enforcement) while showing that all six types are legitimate perspectival readings. This is not ambiguity or failure of classification — this is the framework's correct output for constraints with distributed agency across power asymmetries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_asymmetry_soviet,
    'Can U.S. verification of Soviet compliance actually detect violations at militarily significant scales, or does the verification theater provide false confidence while substantial cheating goes undetected?',
    'Historical retrospective analysis post-Cold War: declassified documents on Soviet SALT compliance and actual U.S. verification capability. Comparison of detected vs. undetected violations discovered decades later.',
    'If verification effective: SALT is genuine coordination (rope/tangled_rope). If verification largely theatrical: SALT is extractive for the verified party (snare) — U.S. civilians trust a constraint that Soviet violations can undermine. Classification drops from tangled_rope to snare for U.S. civilian perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_asymmetry_soviet, empirical, 'Whether SALT verification actually detects militarily significant Soviet violations').

omega_variable(
    domestic_resource_reallocation_counterfactual,
    'If SALT had not been negotiated, would the U.S. actually have increased defense spending by the amount savings were claimed to represent, or would those resources have been captured by other institutional actors (political gridlock, tax cuts) rather than reallocated to domestic services?',
    'Comparative historical analysis: defense spending trajectories in SALT vs. non-SALT periods, accounting for Congressional budget dynamics and inflation. Modeling of counterfactual defense spending without SALT constraints.',
    'If reallocation would have occurred: SALT genuinely protects domestic resources (rope for civilians). If resources would have been captured elsewhere: SALT''s protection is largely illusory, and domestic civilians experience snare dynamics (trapped in resource competition regardless).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domestic_resource_reallocation_counterfactual, empirical, 'Whether SALT actually prevents defense spending increases or merely channels them').

omega_variable(
    mutual_advantage_stability,
    'Is the ''mutual advantage'' framing of SALT stable, or does it rest on temporary alignment of threat perceptions that become unstable when one superpower perceives technological or strategic advantage?',
    'Dynamic analysis of Cold War threat perception cycles; identification of moments when one superpower believed SALT was constraining it more than the other. Modeling of defection incentives under different technological development scenarios.',
    'If stable: SALT is self-enforcing rope (both superpowers rationally maintain it). If unstable: SALT''s coordination function is fragile, and the constraint depends on suppression of defection incentives (snare-like for the party perceiving disadvantage). Classification of institutional perspective may shift from rope to tangled_rope or snare depending on stability assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_advantage_stability, empirical, 'Whether mutual advantage in SALT is stable or rests on temporary alignment').

omega_variable(
    escalation_gate_enforceability,
    'Can the constraint actually function as a gate preventing unilateral escalation, or does it merely delay escalation by creating political costs while military planning proceeds unhindered?',
    'Analysis of U.S. and Soviet military modernization programs during SALT period: did programs proceed on parallel timelines regardless of treaty status, or did the treaty create measurable delays? Did treaty constraints actually prevent weapons development or merely postpone deployment?',
    'If gate is real: constraint functions as tangled_rope (coordination with enforcement). If gate is theatrical: constraint is piton (maintains optics while escalation proceeds), and theater_ratio should be higher (~0.75+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_gate_enforceability, empirical, 'Whether SALT''s escalation gate actually constrains military modernization or merely creates political delays').

omega_variable(
    false_summit_natural_deterrence_law,
    'Is bilateral arms control negotiation a natural law inherent to stable deterrence, or a contingent institutional choice that could be replaced by multilateral verification, technological automation, or third-party enforcement?',
    'Theoretical analysis of alternative deterrence stabilization mechanisms; empirical comparison with non-bilateral arms control models (multilateral treaties, transparency regimes, automated verification). Post-Cold War evolution toward or away from bilateral dependency.',
    'If natural law: mountain classification is correct; bilateral negotiation is inevitable path to deterrent stability. If contingent: mountain is false summit (naturalization of institutional choice); multilateral alternatives could have been pursued. Classification shifts from mountain to tangled_rope (institutional actors choose bilateral structure despite alternatives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_deterrence_law, conceptual, 'Whether bilateral arms control negotiation is inherent to deterrence or a contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1972_nixon_strategic_arms_limitation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu72_theater_start_1969, sotu_1972_nixon_strategic_arms_limitation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sotu72_theater_mid_1972, sotu_1972_nixon_strategic_arms_limitation, theater_ratio, 3, 0.55).
narrative_ontology:measurement(sotu72_theater_end_1975, sotu_1972_nixon_strategic_arms_limitation, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(sotu72_extractiveness_start_1969, sotu_1972_nixon_strategic_arms_limitation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sotu72_extractiveness_mid_1972, sotu_1972_nixon_strategic_arms_limitation, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(sotu72_extractiveness_end_1975, sotu_1972_nixon_strategic_arms_limitation, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1972_nixon_strategic_arms_limitation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sotu_1972_nixon_strategic_arms_limitation, 0.12).
narrative_ontology:affects_constraint(sotu_1972_nixon_strategic_arms_limitation, soviet_military_modernization_program).
narrative_ontology:affects_constraint(sotu_1972_nixon_strategic_arms_limitation, u_s_domestic_resource_allocation).
narrative_ontology:affects_constraint(sotu_1972_nixon_strategic_arms_limitation, cold_war_deterrence_stability).

% DUAL FORMULATION NOTE:
% SALT I can be decomposed into structurally distinct sub-constraints: (1) verification transparency (ε=0.15, rope), (2) escalation gate on unilateral spending (ε=0.38, tangled rope), (3) ideological legitimation of military establishments (ε=0.58, piton). The story models the escalation gate as primary. Verification transparency and ideological legitimation are downstream effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1972_nixon_strategic_arms_limitation, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
