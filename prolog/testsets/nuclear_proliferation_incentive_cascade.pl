% ============================================================================
% CONSTRAINT STORY: nuclear_proliferation_incentive_cascade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_proliferation_incentive_cascade, []).

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
 *   constraint_id: nuclear_proliferation_incentive_cascade
 *   human_readable: Nuclear Proliferation Incentive Cascade
 *   domain: international_security/geopolitics
 *
 * SUMMARY:
 *   The nuclear proliferation incentive cascade represents a structural trap
 *   embedded in the security dilemma facing non-nuclear states. When one
 *   state acquires nuclear weapons, it creates credible existential threat to
 *   neighbors, incentivizing their own acquisition. The Non-Proliferation
 *   Treaty (NPT) regime attempts to prevent this cascade by offering
 *   non-nuclear states security guarantees in exchange for forgoing weapons,
 *   but the regime's enforcement is weak (theater_ratio = 0.58) and the
 *   underlying security dilemma remains unresolved. The constraint exhibits
 *   pure snare characteristics from the perspective of non-nuclear states and
 *   global stability: they are trapped by cascading incentives with no viable
 *   exit that doesn't involve unacceptable risk. Extractiveness has risen
 *   from 0.42 (1970) to 0.68 (present) as verification mechanisms have become
 *   less reliable, more states have approached or crossed the nuclear
 *   threshold, and the NPT bargain (disarmament by armed states) has been
 *   violated. The theater ratio has increased similarly as the gap widens
 *   between the treaty's stated function (preventing proliferation) and its
 *   actual performance (unable to prevent Iran, North Korea, Pakistan, or
 *   Israel; unable to reverse their acquisition).
 *
 * KEY AGENTS:
 *   - Non-Nuclear States (especially threshold states): Primary victim (powerless/trapped) — face security dilemma incentivizing proliferation while treaty obligations and sanctions suppress that path. No stable exit.
 *   - Global Nuclear Stability: Primary victim (powerless/trapped) — abstract collective good bearing full cost of cascade; no exit mechanism or organizational capacity.
 *   - Proliferating Regional Powers (Iran, North Korea, etc.): Secondary agent (organized/constrained) — face mixed incentives (security benefit, sanctions cost, alliance constraints); constrained exit options.
 *   - Nuclear-Armed Great Powers (P5): Primary beneficiary (institutional/arbitrage) — benefit from deterrence and strategic autonomy; maintain proliferation advantage; arbitrage through selective nonproliferation enforcement.
 *   - Non-Proliferation Treaty Regime: Institutional mechanism (institutional/constrained) — designed to prevent cascade but now degraded (piton); verification incomplete, enforcement selective, disarmament bargain violated.
 *   - Analytical Observer: Sees structural inevitability of cascade (analytical/analytical) — but risks naturalizing what is a contingent institutional arrangement (state sovereignty + anarchic structure) rather than a law of nature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_proliferation_incentive_cascade, 0.68).
domain_priors:suppression_score(nuclear_proliferation_incentive_cascade, 0.72).
domain_priors:theater_ratio(nuclear_proliferation_incentive_cascade, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_proliferation_incentive_cascade, extractiveness, 0.68).
narrative_ontology:constraint_metric(nuclear_proliferation_incentive_cascade, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nuclear_proliferation_incentive_cascade, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_proliferation_incentive_cascade, snare).
narrative_ontology:human_readable(nuclear_proliferation_incentive_cascade, "Nuclear Proliferation Incentive Cascade").
narrative_ontology:topic_domain(nuclear_proliferation_incentive_cascade, "international_security/geopolitics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_proliferation_incentive_cascade, nuclear_armed_states).
narrative_ontology:constraint_victim(nuclear_proliferation_incentive_cascade, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_proliferation_incentive_cascade, global_nuclear_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR STATE (SNARE) — Trapped by security dilemma. Nuclear-armed neighbors create credible existential threat; conventional deterrence insufficient. Exit path (maintaining non-nuclear status) leads to strategic vulnerability. Proliferation is simultaneously forbidden (NPT, sanctions) and incentivized (neighbor proliferation). Suppression is total: diplomatic pressure, sanctions regimes, inspection regimes, and strategic isolation all constrain exit options. The constraint extracts security cost from non-nuclear states while offering no stable alternative.
constraint_indexing:constraint_classification(nuclear_proliferation_incentive_cascade, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL NUCLEAR STABILITY (SNARE) — Collective good that cannot organize, exit, or defend itself. Each state's proliferation decision is individually rational given security dilemma, but aggregate outcome is cascade toward greater instability. The stability architecture (NPT, safeguards, extended deterrence) erodes as more states acquire nuclear weapons. No mechanism to reverse the cascade once begun. Stability bears full cost while beneficiaries capture security advantage.
constraint_indexing:constraint_classification(nuclear_proliferation_incentive_cascade, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PROLIFERATING REGIONAL POWER (TANGLED ROPE) — Genuinely solves coordination problem: nuclear capability deters larger adversaries and stabilizes regional balance (coordination function). Simultaneously extracts cost from regional neighbors and global stability (asymmetric extraction). Exit options exist (forgo weapons, accept conventional inferiority, rely on alliance) but are costly — career risk for leadership, loss of strategic autonomy, vulnerability during alliance dissolution. Constrained by both security dilemma and institutional pressures.
constraint_indexing:constraint_classification(nuclear_proliferation_incentive_cascade, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NUCLEAR-ARMED GREAT POWER (ROPE) — Pure coordination beneficiary. Nuclear weapons deter existential threats; extended deterrence is a coordination mechanism providing security public goods to allies. Exit from nuclear capability would require existential trust and abandonment of deterrence (currently infeasible). Arbitrage options available: sell reactor technology, negotiate arms control treaties, shift proliferation risks to other powers. Experiences the constraint as essential coordination with net benefit.
constraint_indexing:constraint_classification(nuclear_proliferation_incentive_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NON-PROLIFERATION TREATY REGIME (PITON) — Performative enforcement mechanism. NPT established in 1968 to prevent cascade; now degraded. Theater ratio is high because verification (IAEA inspections) cannot detect covert weapons programs reliably (North Korea, Iran), enforcement (UN sanctions) is inconsistent and reversible, and the treaty's core bargain (non-nuclear states forgo weapons in exchange for nuclear disarmament by armed states) has been violated by armed states' failure to disarm. Regime persists through institutional inertia and because alternatives (no agreement, or enforcement mechanisms requiring military action) are worse. Exit costs for states are high but function is minimal.
constraint_indexing:constraint_classification(nuclear_proliferation_incentive_cascade, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational/universal scope, the proliferation incentive cascade is a structural consequence of the security dilemma embedded in anarchic international system. Each rational actor's proliferation decision creates externalities that push other actors toward proliferation, producing a cascade where exit is individually irrational but collectively catastrophic. The analytical view reveals that the constraint is not an immutable law of physics but a contingent institutional arrangement (state sovereignty + anarchic structure + nuclear technology diffusion) that has produced snare-like dynamics. However, the structural data cannot distinguish between (a) irreversible cascades and (b) temporarily reversible dynamics that could be arrested by institutional reform.
constraint_indexing:constraint_classification(nuclear_proliferation_incentive_cascade, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_proliferation_incentive_cascade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_proliferation_incentive_cascade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_proliferation_incentive_cascade, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_proliferation_incentive_cascade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_proliferation_incentive_cascade, TR),
    TR >= 0.70.

:- end_tests(nuclear_proliferation_incentive_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint extracts security cost from non-nuclear states who must accept strategic vulnerability, economic sanctions (if they proliferate), or costly alliance dependence (if they don't). The extraction increases over time as the NPT regime degrades and verification becomes less reliable—states must spend more resources on security as confidence in the regime declines. Suppression (0.72): High. Multiple overlapping suppression mechanisms: (1) Treaty obligations and international law making proliferation formally illegal; (2) Sanctions regimes (economic, military, diplomatic); (3) Coercive inspections and verification regimes limiting sovereignty; (4) Strategic isolation and threat of military intervention (Iraq 2003 precedent); (5) Technology controls and export restrictions limiting access to nuclear technology. Theater ratio (0.58): Moderate. The NPT regime performs some genuine coordination function (slowing proliferation relative to baseline, providing inspection framework, establishing nonproliferation norm), but significant theatrical component: IAEA cannot detect covert programs reliably (Iran's undeclared sites, North Korea's facilities), enforcement is selective (P5 exemptions, enforcement only against weak states), the disarmament bargain has been abandoned, and the regime persists partly because the alternative (no agreement, or enforcement requiring military force) appears worse. The increasing theater ratio reflects growing gap between treaty promise and actual performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how indexical classification reveals the asymmetry at the heart of the proliferation trap. The same structural phenomenon—nuclear deterrence—appears as essential coordination (rope) from the nuclear power's perspective and as extractive snare from the non-nuclear state's perspective. The gap is not an observational ambiguity; it reflects genuine structural asymmetry: the nuclear power has choices (maintain arsenal, negotiate arms control, extend deterrence), while the non-nuclear state has only costs (accept vulnerability, violate treaty with sanctions, proliferate illegally and risk intervention). The piton classification of the NPT regime reveals the mechanism: the regime's performative theater obscures the underlying snare, allowing states to maintain the fiction of a functioning system even as it degrades.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (chi) derives from its structural position. Non-nuclear states have maximum directionality (d ≈ 0.95): they are full targets of the cascade, with trapped exit options and no beneficiary status. The nuclear power derives d ≈ 0.10: beneficiary + arbitrage exit → negative chi (they gain from the constraint). The proliferating regional power derives d ≈ 0.60: victim of suppression + constrained exit → moderate chi (caught between security benefit and suppression cost). Global stability is a powerless agent with no exit: d = 1.0. The piton perspective applies standard institutional directionality (~0.15) modified by the theatrical degradation signal. The cascade effect—each proliferation decision raising d for remaining non-nuclear states—is the mechanism by which the constraint feeds on itself.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids false natural law classification through several mechanisms: (1) The analytical 'mountain' perspective is correctly identified as a false summit — the cascade is not an immutable law of international relations but a contingent outcome of specific institutional arrangements (state sovereignty, anarchic structure, nuclear technology diffusion). (2) The piton classification correctly captures the NPT regime as degraded theater rather than functional constraint. (3) The tangled rope and snare classifications from different perspectives reveal that the constraint's function (coordination for nuclear powers, suppression for non-nuclear states) is asymmetric—it is not a pure coordination mechanism masquerading as constraint, but genuine mixed mechanism with asymmetric distribution of benefits and costs. (4) The measurement trajectory (extractiveness rising from 0.42 to 0.68 over 40 years, theater ratio rising from 0.35 to 0.58) documents Goodhart drift: as NPT verification becomes less reliable and enforcement more selective, the regime's actual constraint function decays while its theatrical performance increases. Mandatrophy is resolved by recognizing the constraint as genuine snare from non-nuclear perspectives, not as natural law or failed coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_dilemma_irreversibility,
    'Is the proliferation cascade irreversible once begun, or can institutional interventions (credible security guarantees, verification breakthroughs, alliance restructuring) arrest it?',
    'Historical analysis of proliferation arrests (South Africa, Libya, Kazakhstan); game-theoretic modeling of credible guarantee mechanisms; empirical testing of extended deterrence stability under proliferation scenarios',
    'If irreversible: snare classification stands. If arrestable: constraint might reclassify as Tangled Rope with institutional exit paths (e.g., security council guarantees), suggesting mandatrophy resolution through verified denuclearization agreements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_dilemma_irreversibility, empirical, 'Whether proliferation cascades are irreversible').

omega_variable(
    covert_weapons_detectability,
    'Can advanced verification technologies (genomic attribution, radiation signature analysis, satellite verification, AI-enhanced inspections) reliably detect covert weapons programs, or is the verification bottleneck inherent to the problem?',
    'Empirical assessment of IAEA detection rates; analysis of Iran, North Korea, and historical cases; technological roadmap for verification capabilities 2026-2050',
    'If detectable: NPT Piton can be reformed into functional Rope (verification bottleneck removed). If inherently undetectable: regime remains Piton and snare extraction persists unchecked.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covert_weapons_detectability, empirical, 'Whether covert weapons programs can be reliably detected').

omega_variable(
    credible_security_guarantee_feasibility,
    'Can nuclear-armed powers credibly guarantee non-nuclear states against existential threats without the guaranteed state acquiring nuclear weapons, or is the guarantee itself rationally suspect given great-power self-interest?',
    'Analysis of alliance commitment mechanisms (NATO Article 5, bilateral defense treaties, NNPT negative security assurances); game-theoretic assessment of guarantee credibility under severe crisis scenarios; historical review of alliance defection cases',
    'If credible: non-nuclear states have genuine exit option from snare (security without weapons). If suspect: security dilemma trap deepens and proliferation becomes individually rational for all non-nuclear states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credible_security_guarantee_feasibility, conceptual, 'Whether credible security guarantees can substitute for nuclear capability').

omega_variable(
    regional_nuclear_stability_equilibrium,
    'Do regional nuclear balances (e.g., India-Pakistan, Israel-regional states) produce mutually assured destruction stability (''Mutual Assured Stability'') or precarious equilibria vulnerable to breakdown under crisis?',
    'Game-theoretic analysis of regional conflict scenarios; empirical assessment of near-miss nuclear incidents; stability analysis of command-and-control systems under stress; crisis simulation modeling',
    'If stable: Tangled Rope classification for regional proliferators is justified (coordination function exists). If precarious: Snare classification deepens and regional proliferation represents destabilization cascade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_nuclear_stability_equilibrium, empirical, 'Whether regional nuclear balances are stable or precarious').

omega_variable(
    nuclear_technology_diffusion_trajectory,
    'Is nuclear weapons-usable material (highly enriched uranium, separated plutonium) becoming more accessible to sub-state actors and rogue regimes, or are supply-side controls (mining, enrichment, reprocessing) becoming more restrictive?',
    'Tracking of known HEU and separated plutonium stockpiles; analysis of uranium enrichment technology proliferation; assessment of IAEA safeguards effectiveness; modeling of future supply constraints',
    'If diffusing: snare extraction accelerates (more actors trapped in cascade). If controllable: supply-side interventions could arrest cascade, moving constraint toward Tangled Rope with policy exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nuclear_technology_diffusion_trajectory, empirical, 'Whether weapons-usable nuclear material is becoming more or less accessible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_proliferation_incentive_cascade, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_proliferation_incentive_cascade, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nucl_tr_t20, nuclear_proliferation_incentive_cascade, theater_ratio, 20, 0.48).
narrative_ontology:measurement(nucl_tr_t40, nuclear_proliferation_incentive_cascade, theater_ratio, 40, 0.58).
narrative_ontology:measurement(nucl_tr_t10, nuclear_proliferation_incentive_cascade, theater_ratio, 10, 0.41).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_proliferation_incentive_cascade, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nucl_be_t20, nuclear_proliferation_incentive_cascade, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(nucl_be_t40, nuclear_proliferation_incentive_cascade, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(nucl_be_t10, nuclear_proliferation_incentive_cascade, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_proliferation_incentive_cascade, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_proliferation_incentive_cascade, weapons_grade_material_access).
narrative_ontology:affects_constraint(nuclear_proliferation_incentive_cascade, extended_deterrence_credibility).
narrative_ontology:affects_constraint(nuclear_proliferation_incentive_cascade, regional_nuclear_balance_stability).

% DUAL FORMULATION NOTE:
% The proliferation incentive cascade decomposes into three structurally distinct constraints: (1) Weapons-grade material supply constraints (verification/control infrastructure), (2) Extended deterrence credibility (alliance commitment mechanisms), (3) Regional nuclear balance stability (conflict escalation pathways). This story addresses the cascade mechanism integrating all three; each component story has its own epsilon value reflecting empirical uncertainty about that component's controllability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_proliferation_incentive_cascade, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
