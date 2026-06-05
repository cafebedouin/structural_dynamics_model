% ============================================================================
% CONSTRAINT STORY: litany_of_the_real
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_litany_of_the_real, []).

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
 *   constraint_id: litany_of_the_real
 *   human_readable: The Litany of the Real (Deferential Realism)
 *   domain: philosophical/cognitive
 *
 * SUMMARY:
 *   The Litany of the Real is a formal cognitive protocol in Deferential
 *   Realism designed to align an agent's internal model with external reality
 *   through disciplined language and epistemic practice. The constraint
 *   exemplifies how a pure coordination mechanism can be layered with
 *   extraction mechanisms, creating institutional variants that range from
 *   genuine rope (voluntary truth-seeking communities) to piton (performative
 *   institutional proclamations of truth while suppressing inconvenient
 *   evidence). The core protocol functions as rope when adopted by agents
 *   seeking improved calibration and by epistemic communities building
 *   reliable shared knowledge. However, the constraint exhibits tangled
 *   properties when psychological defenses block reality acceptance, and
 *   piton properties when institutions instrumentalize the rhetoric of
 *   truth-seeking to legitimize predetermined conclusions. The temporal
 *   measurement shows extractiveness increasing from 0.12 to 0.32 as the
 *   litany matures — partly through legitimate expansion of its scope, but
 *   also through institutional capture and performative adoption that
 *   substitutes form for function.
 *
 * KEY AGENTS:
 *   - Individual Truth-Seeker: Primary beneficiary (moderate/mobile) — gains improved calibration and decision-making reliability from voluntary adoption
 *   - Epistemic Community: Primary beneficiary (institutional/arbitrage) — benefits from coordinated commitment to reality-alignment; reduced verification cost across scientific and investigative domains
 *   - Self-Deceived Agent: Victim (powerless/constrained) — faces psychological cost of reality acceptance; constrained exit due to threat to identity-protective belief structures
 *   - Dogmatic Institution: Secondary actor (institutional/constrained) — maintains performative commitment while suppressing inconvenient truths; trapped between legitimacy claims and institutional interests
 *   - Analytical Observer: Civilizational viewpoint (analytical/analytical) — recognizes litany as fundamental law of agency but notes institutional degradation at scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(litany_of_the_real, 0.32).
domain_priors:suppression_score(litany_of_the_real, 0.25).
domain_priors:theater_ratio(litany_of_the_real, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(litany_of_the_real, extractiveness, 0.32).
narrative_ontology:constraint_metric(litany_of_the_real, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(litany_of_the_real, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(litany_of_the_real, rope).
narrative_ontology:human_readable(litany_of_the_real, "The Litany of the Real (Deferential Realism)").
narrative_ontology:topic_domain(litany_of_the_real, "philosophical/cognitive").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(litany_of_the_real, truth_seeking_agents).
narrative_ontology:constraint_beneficiary(litany_of_the_real, epistemic_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL TRUTH-SEEKER (ROPE) — An agent adopting the litany gains direct epistemological benefit: improved calibration, reduced map-territory confusion, and more reliable decision-making. The litany is a coordination protocol they can adopt or abandon. Mobile exit option reflects voluntary adoption. Low extraction because the cognitive discipline produces genuine epistemic gain without asymmetric cost.
constraint_indexing:constraint_classification(litany_of_the_real, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: EPISTEMIC COMMUNITY (ROPE) — Scientific institutions, research communities, and truth-seeking organizations benefit from widespread adoption of the litany. Shared commitment to reality-alignment enables reliable communication, cumulative knowledge-building, and intersubjective verification. The protocol reduces the cost of verifying others' claims. Arbitrage exit reflects institutional ability to adopt or modify the framework. Net coordination with minimal extraction.
constraint_indexing:constraint_classification(litany_of_the_real, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SELF-DECEIVED AGENT (TANGLED ROPE) — An agent with strong motivated reasoning or existential terror faces the litany as both coordination and extraction. The protocol demands truth-seeking that threatens cherished beliefs. Constrained exit: acknowledging reality may demolish psychological defense structures. Benefits from the truth (eventually, for long-term decisions) but bears severe short-term psychological cost. Active enforcement via social accountability increases suppression.
constraint_indexing:constraint_classification(litany_of_the_real, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: LOGICAL FOUNDATION / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational/universal analytical perspective, the litany expresses a fundamental structural property of cognition itself: any agent with goals must model reality with some fidelity. The constraint is not a human invention but a law of agency. Agents cannot exit reality-checking; it is constitutive of rational action. No beneficiary or victim — the constraint is axiomatic.
constraint_indexing:constraint_classification(litany_of_the_real, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: EPISTEMIC REFORMER (SCAFFOLD) — Organized agents implementing reality-alignment protocols (cognitive behavioral therapy, Bayesian inference education, open science standards) view the litany as a temporary coordination ladder. Initial suppression is high (psychological defense mechanisms resist truth-telling). But as meta-cognition improves and agents internalize reality-testing, the external enforcement burden declines. Mobile exit because reformed agents eventually need the litany less — internalized truth-seeking becomes automatic. Sunset clause: as epistemic competence spreads, the formal litany becomes vestigial.
constraint_indexing:constraint_classification(litany_of_the_real, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DOGMATIC INSTITUTION (PITON) — An organization that formally endorses the litany but functions through suppression of inconvenient truths maintains the performative ritual without the functional commitment. Academic departments that claim scientific rigor while protecting bad researchers, governments that proclaim transparency while classifying evidence, churches that nominally value truth but enforce orthodoxy. Theater ratio ≥ 0.70: the formal litany is invoked but systematically violated. Constrained exit because institutional members have careers and reputations tied to the false doctrine.
constraint_indexing:constraint_classification(litany_of_the_real, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(litany_of_the_real_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(litany_of_the_real, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(litany_of_the_real, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(litany_of_the_real, TR),
    TR >= 0.70.

:- end_tests(litany_of_the_real_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate, reflecting the protocol's dual nature. Core adoption by truth-seeking agents produces genuine epistemic benefit with minimal extraction. But institutional layering introduces asymmetry: organizations can weaponize the litany's legitimacy to enforce silence about institutional failures. The value increased from 0.12 (early adoption by self-selected communities) to 0.32 (institutional-scale implementation with enforcement mechanisms). Suppression (0.25): Moderate. The protocol requires agents to overcome psychological defense mechanisms and motivated reasoning. But suppression is not coercive in the individual case — agents can choose to maintain delusion and exit. Institutional contexts show higher suppression (piton perspective) as enforced conformity replaces voluntary adoption. Theater ratio (0.38): Moderate and increasing. The explicit formulation of the litany creates risk of ritual substitution for reality: institutional actors can invoke the protocol while systematically violating it, using the litany's language as cover for predetermined conclusions. Theater increased from 0.15 (informal communities practicing implicit truth-seeking) to 0.38 (institutions requiring formal litany recitations and reality-alignment statements that mask actual decision-making processes).
 *
 * PERSPECTIVAL GAP:
 *   The individual truth-seeker and epistemic community see rope — genuine coordination with mutual benefit. The self-deceived agent sees tangled rope — coordination function exists (shared truth-seeking) but extraction mechanism (psychological coercion) blocks exit. The dogmatic institution sees piton — the protocol persists as performative ritual (theater_ratio ≥ 0.70) despite loss of functional commitment to truth-seeking. The logical foundation sees mountain — reality-alignment is axiomatic to agency, not contingent institutional choice. The epistemic reformer sees scaffold — the formal litany is a temporary tool for building internalized reality-testing competence; as meta-cognition develops, external enforcement becomes unnecessary and the sunset clause triggers. The fundamental gap is between genuine voluntary adoption (rope) and institutional appropriation (piton) — the same constraint word-for-word, but with radically different extraction properties.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for voluntary adopters is derived from beneficiary status (truth-seeking) and mobile exit options — low d, resulting in low or negative effective extraction. Directionality for institutional contexts is inverted: constrained exit (career, institutional position) + enforced participation + suppressed evidence = high d despite nominal beneficiary status. The piton perspective shows how institutional actors can be nominal beneficiaries (claiming commitment to truth) while functioning as victims (trapped by institutional contradictions). The self-deceived agent's high d is derived from constrained exit and victim status — they bear psychological cost while the constraint extracts conformity. The analytical mountain perspective recognizes that d is irrelevant at the civilizational scale: all agents are equally subject to the law of agency, regardless of power or exit options.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bootstrapping_problem,
    'How can an agent adopt the litany if their current model is deeply deluded? Does reality-alignment require prior access to reality?',
    'Empirical testing of agents'' ability to incrementally improve calibration from false starting positions; analysis of error correction mechanisms that work without external ground truth',
    'If bootstrapping is possible: litany is a rope for all agents. If not: litany is snare for deeply deluded agents (they cannot escape without external intervention). Classification shifts from rope to tangled_rope/snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bootstrapping_problem, conceptual, 'Whether deluded agents can bootstrap reality-alignment without external ground truth').

omega_variable(
    psychological_necessity_of_delusion,
    'Is some degree of self-deception necessary for psychological survival in the presence of true existential threats? Can the litany be adopted without psychological harm?',
    'Longitudinal studies of mental health outcomes in agents pursuing strict reality-alignment vs those maintaining limited self-deception; analysis of failure modes in reality-accepting populations under genuine existential duress',
    'If delusion is necessary: litany becomes snare for vulnerable populations (extraction via psychological harm). If optional: litany remains rope with constrained exit for some agents but mobile exit overall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_necessity_of_delusion, empirical, 'Whether psychological integrity requires some self-deception').

omega_variable(
    institutional_capacity_for_truth,
    'Can institutional structures reliably implement the litany, or do institutions inevitably distort reality to preserve power and structure?',
    'Historical analysis of institutional truth-telling capacity; examination of failure modes in organizations with formal reality-alignment commitments; study of institutional immunity to inconvenient evidence',
    'If institutions can implement the litany: rope classification holds for institutional perspective. If institutions are structurally incapable: institutional perspective is piton (performative, degraded), and the constraint exhibits far greater theater ratio at organizational scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capacity_for_truth, empirical, 'Whether institutions can structurally implement the litany').

omega_variable(
    tacit_vs_explicit_litany,
    'Does adopting the litany as an explicit, verbalized protocol increase or decrease its effectiveness? Does making the constraint visible destabilize it?',
    'Comparison of outcomes in agents with explicit vs implicit commitment to reality-alignment; analysis of whether articulating the litany creates performative theatre that substitutes for genuine truth-seeking',
    'If explicit articulation decreases effectiveness: the litany contains a piton mechanism (theater_ratio increases). If explicit articulation increases effectiveness: theater_ratio stays low and rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_vs_explicit_litany, conceptual, 'Whether explicit litany articulation improves or degrades reality-alignment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(litany_of_the_real, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(litany_tr_t0, litany_of_the_real, theater_ratio, 0, 0.15).
narrative_ontology:measurement(litany_tr_t25, litany_of_the_real, theater_ratio, 25, 0.3).
narrative_ontology:measurement(litany_tr_t50, litany_of_the_real, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(litany_be_t0, litany_of_the_real, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(litany_be_t25, litany_of_the_real, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(litany_be_t50, litany_of_the_real, base_extractiveness, 50, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(litany_of_the_real, information_standard).
narrative_ontology:affects_constraint(litany_of_the_real, map_territory_distinction).
narrative_ontology:affects_constraint(litany_of_the_real, motivated_reasoning_suppression).
narrative_ontology:affects_constraint(litany_of_the_real, institutional_truth_capacity).

% DUAL FORMULATION NOTE:
% The Litany of the Real decomposes into multiple structurally distinct claims: (1) the logical/mathematical claim that agents must model reality (mountain), (2) the epistemic claim that humans can improve calibration through discipline (rope), (3) the psychological claim that reality-acceptance requires overcoming defense mechanisms (tangled rope), and (4) the institutional claim that organizations can coordinate around truth-seeking (rope-to-piton spectrum depending on institutional structure). Stories for map-territory distinction (ε≈0.08, mountain) and motivated-reasoning suppression (ε≈0.55, tangled_rope) should be linked as components of the broader litany framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(litany_of_the_real, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
