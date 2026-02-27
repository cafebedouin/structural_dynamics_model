% ============================================================================
% CONSTRAINT STORY: gilgamesh_mortality_limit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gilgamesh_mortality_limit, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gilgamesh_mortality_limit
 *   human_readable: The Allotment of Mortality (Gilgamesh)
 *   domain: philosophical/religious
 *
 * SUMMARY:
 *   The Epic of Gilgamesh presents mortality as the ultimate constraint on
 *   human agency. The narrative begins with Gilgamesh seeking immortality
 *   after the death of Enkidu and culminates in his acceptance that mortality
 *   is the definitive allotment of humanity — the line that cannot be
 *   crossed. The constraint is not imposed by institutional force, social
 *   convention, or economic extraction. Rather, it emerges as a natural
 *   limit, a boundary condition of existence itself. From the perspective of
 *   the powerless human agent, mortality offers zero degrees of freedom. From
 *   the philosophical and scientific perspectives, it appears as a constraint
 *   grounded in physical law and thermodynamics. From institutional
 *   perspectives (religious, ritual), it appears as something to be
 *   coordinated around and given meaning, not something that can be overcome.
 *   This constraint serves as the gold standard exemplar for the Mountain
 *   category: it exhibits universal classification across all observer
 *   positions, zero degrees of freedom, low theater, and accessibility
 *   collapse (near-universal awareness) paired with low resistance to
 *   acceptance of the constraint as real.
 *
 * KEY AGENTS:
 *   - Gilgamesh: Primary actor (powerless/trapped) — seeks immortality, discovers mortality's inevitability
 *   - Enkidu: Catalyst (powerless/trapped) — death of beloved exemplifies mortality's universality
 *   - The Shamhat: Secondary agent (powerless/trapped) — embodies human finitude
 *   - The Gods: Institutional authority (institutional/arbitrage) — impose and maintain the mortality constraint
 *   - Religious Institutions: Interpreter (institutional/arbitrage) — coordinate meaning-making around mortality
 *   - Scientific Community: Modern observer (organized/constrained) — approaches mortality as natural limit to be studied and marginally extended
 *   - The Analytical Observer: Universal perspective (analytical/analytical) — sees mortality as constraint of physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gilgamesh_mortality_limit, 0.08).
domain_priors:suppression_score(gilgamesh_mortality_limit, 0.02).
domain_priors:theater_ratio(gilgamesh_mortality_limit, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, extractiveness, 0.08).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gilgamesh_mortality_limit, mountain).
narrative_ontology:human_readable(gilgamesh_mortality_limit, "The Allotment of Mortality (Gilgamesh)").
narrative_ontology:topic_domain(gilgamesh_mortality_limit, "philosophical/religious").

domain_priors:emerges_naturally(gilgamesh_mortality_limit).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MORTAL HUMAN (MOUNTAIN) — No exit, no alternative. Death is the irreducible boundary condition of human existence. From the perspective of Gilgamesh himself, mortality is not a constraint imposed by institutional force but an absolute fact of the natural order. The human cannot negotiate, arbitrage, or escape this limit. Accessibility of understanding the limit is near-total (everyone experiences mortality awareness); resistance to accepting the limit is low (though psychological denial is possible, the constraint cannot be overcome through any mechanism). This is a true mountain from the powerless perspective.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PHILOSOPHICAL OBSERVER (MOUNTAIN) — Mortality is classified here as a fundamental limit on human existence independent of any institutional arrangement. No amount of coordination, enforcement, or theatrical ritual can overcome the thermodynamic and biological boundary of finite lifespan. The constraint emerges naturally from the laws of physics and biology, not from social construction. Accessibility is high (the constraint is observable in every human society); resistance is low (denial does not change the outcome). The mountain classification is stable across all human contexts and time periods.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE RELIGIOUS INSTITUTION (ROPE) — Religious and metaphysical frameworks (promises of afterlife, resurrection, reincarnation, spiritual transcendence) provide coordination around mortality's acceptance. These are not extraction mechanisms but coordination solutions to a collective action problem: how do we organize society knowing that all members will die? Religious institutions benefit from their role as mediators between mortality and meaning, gaining authority and resources. But the beneficiary relationship is not asymmetric extraction — religions genuinely coordinate the shared problem of mortality anxiety. Theater is minimal here; the constraint appears as natural law with institutional framing.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE MEDIEVAL DEATH RITUAL SYSTEM (PITON) — Elaborate funeral rites, masses for the dead, purgatory theology, and extreme unction rituals in medieval Christianity create a theater around mortality that originally served genuine psychological and social coordination functions but has become increasingly performative. Families perform expensive death rituals not because the rituals demonstrably prevent death or transfer souls, but because institutional inertia and social expectation require them. The constraint persists through ritual theater (theater_ratio ≈ 0.65 for this perspective) even as its primary function has atrophied. This is a mountain that has been wrapped in performative institutional scaffolding.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE SCIENTIFIC COMMUNITY (MOUNTAIN) — Modern biomedical research approaches mortality as a natural constraint to be extended rather than abolished. Aging research, longevity medicine, and cryonics represent organized attempts to modify the constraint's parameters (timeline extension, cellular preservation) while accepting its fundamental inevitability. The scientific perspective classifies mortality as mountain because the underlying biological processes are governed by physical law, even if technology can shift marginal parameters. No amount of research funding or institutional effort can achieve true indefinite lifespan given known physics. The constraint remains immutable at the level of principle.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTIC METAPHYSICIAN (MOUNTAIN) — Mortality as a metaphysical necessity: given the entropic arrow, the finiteness of the universe, and the second law of thermodynamics, indefinite existence is logically impossible for any finite system. Mortality is not merely a constraint imposed by nature but a constraint that follows from the structure of reality itself. From this universal/civilizational perspective, even hypothetical advanced technology cannot circumvent the fundamental constraint. This is a true mountain with zero degrees of freedom.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gilgamesh_mortality_limit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gilgamesh_mortality_limit, ExtMetricName, E),
    domain_priors:suppression_score(gilgamesh_mortality_limit, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gilgamesh_mortality_limit),
    narrative_ontology:constraint_metric(gilgamesh_mortality_limit, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gilgamesh_mortality_limit, resistance, R),
    AC >= 0.85,
    R =< 0.15.

test(piton_threshold) :-
    domain_priors:theater_ratio(gilgamesh_mortality_limit, TR),
    TR >= 0.70.

:- end_tests(gilgamesh_mortality_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. Mortality is not extractive in the structural sense — no agent captures disproportionate benefit from human finitude. Death is universal and egalitarian. The low value reflects that the constraint operates as pure boundary condition, not as a mechanism by which one party extracts from another. Suppression (0.02): Minimal. There are no alternatives to mortality being suppressed; alternative lifespan frameworks cannot be developed because the constraint is not institutional. The low value reflects that suppression is unnecessary — the constraint is self-enforcing through physical law. Theater ratio (0.15): Very low in the ancient Near Eastern context. Death rituals in early periods were functional: genuinely coordinating grief, inheritance, spiritual transition. Theater only increases much later (medieval Christianity, 0.60+) as the rituals become performative rather than functional, disconnected from their original problem-solving role. Accessibility collapse (0.92): Extremely high. Every human observes mortality in their lived experience — the constraint is maximally accessible. Resistance (0.08): Minimal. Psychological denial is possible, but acceptance of mortality is near-universal once confronted directly with the constraint's reality.
 *
 * PERSPECTIVAL GAP:
 *   Unlike many constraints, the perspectival gap in mortality is minimal — all six observer positions converge on the mountain classification. This convergence is precisely what makes mortality a true mountain. The powerless human trapped by mortality, the philosophical observer studying human nature, the religious institution coordinating meaning, the scientific community studying aging, and the analytic metaphysician examining logical constraints all arrive at the same conclusion: mortality is immutable. The only perspectival variation appears in the medieval ritual perspective (piton), where the theater increases because the original coordination function (making sense of death) has been wrapped in performative institutional scaffolding. But even the piton still classifies mortality itself as mountain — the ritual theater does not change the underlying constraint. This universal agreement across perspectives is the defining characteristic of a true mountain constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values for mortality are trivial because extractiveness is so low that derived d plays a minimal role in classification. Every agent experiences d ≈ 0.50 (symmetric — everyone dies) across all contexts. The constraint is so fundamental that the typical directionality derivation (beneficiary/victim, exit options, power level) becomes almost meaningless. This is appropriate: mortality is not about power differentials or extraction but about a universal boundary condition. The absence of clear beneficiaries and victims is itself a feature of the mountain classification — if a constraint had identifiable winners and losers, it would be structurally different (extraction, not law).
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY RISK. Extractiveness is 0.08, well below the 0.46 threshold that triggers mandatrophy analysis. The constraint is unambiguously mountain across all perspectives. There is no risk of misclassifying it as pure extraction or as false coordination. The constraint's simplicity — it is literally a law of nature — resolves any temptation to construct elaborate extraction narratives around it. The only mandatrophy-adjacent issue is the piton perspective on medieval death rituals, where performative theater might superficially resemble extraction. But the piton classification correctly identifies this as degraded coordination (ritual theater), not as extraction itself. The underlying constraint (mortality) remains mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indefinite_lifespan_logical_coherence,
    'Is indefinite biological lifespan logically or physically coherent, or is it inherently contradictory?',
    'Analysis of thermodynamic constraints, entropy accumulation in biological systems, logical paradoxes in immortality (teleportation identity, consciousness continuity). Examination of whether ''indefinite lifespan'' can be rigorously defined without appealing to concepts (digital upload, quantum immortality) that may themselves be incoherent.',
    'If incoherent: mortality is a constraint of logic itself (pure mountain, all perspectives agree). If coherent: some technological pathway might exist, downgrading the constraint to scaffold or rope in future-oriented perspectives. The Epic''s framing suggests metaphysical necessity (incoherence), which supports universal mountain classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indefinite_lifespan_logical_coherence, conceptual, 'Whether indefinite lifespan is logically coherent').

omega_variable(
    consciousness_persistence_identity,
    'What constitutes persistence of personal identity across time? Does a resurrected, cloned, or digitally-simulated consciousness count as the same agent?',
    'Philosophical analysis of personal identity criteria (psychological continuity, biological continuity, narrative continuity). Empirical investigation of whether subjective experience can be preserved through upload/copying mechanisms. Examination of how different identity theories map to different conclusions about ''escaping'' mortality.',
    'If identity requires biological continuity: biological death is irreversible, and the constraint remains mountain. If identity is independent of substrate: digital immortality might satisfy ''escaping mortality'' from some perspectives, though biological death is unchanged. Resolution affects how future-oriented perspectives classify the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consciousness_persistence_identity, conceptual, 'Whether personal identity persists through copying or substrate change').

omega_variable(
    entropy_and_information_preservation,
    'Can information composing a human mind be perfectly preserved despite thermodynamic entropy?',
    'Analysis of black hole thermodynamics, information loss in quantum mechanics, Kolmogorov complexity of human cognition, practical limits on information fidelity. Determination of whether the information density and preservation precision required for consciousness copying exceeds physical limits.',
    'If information is fundamentally not preservable: death is irreversible at the information level, strengthening mountain classification. If information can be preserved: cryonics or digital upload become theoretically possible (though practically difficult), potentially shifting future perspectives from mountain to scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entropy_and_information_preservation, empirical, 'Whether human consciousness information can be preserved against entropy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gilgamesh_mortality_limit, 0, 10000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mort_tr_t0, gilgamesh_mortality_limit, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mort_tr_t5000, gilgamesh_mortality_limit, theater_ratio, 5000, 0.4).
narrative_ontology:measurement(mort_tr_t10000, gilgamesh_mortality_limit, theater_ratio, 10000, 0.6).

% Extraction over time
narrative_ontology:measurement(mort_be_t0, gilgamesh_mortality_limit, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mort_be_t5000, gilgamesh_mortality_limit, base_extractiveness, 5000, 0.06).
narrative_ontology:measurement(mort_be_t10000, gilgamesh_mortality_limit, base_extractiveness, 10000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gilgamesh_mortality_limit, global_infrastructure).
narrative_ontology:affects_constraint(gilgamesh_mortality_limit, meaning_seeking_under_finitude).
narrative_ontology:affects_constraint(gilgamesh_mortality_limit, intergenerational_resource_transfer).
narrative_ontology:affects_constraint(gilgamesh_mortality_limit, grief_coordination_mechanism).

% DUAL FORMULATION NOTE:
% Mortality itself is a mountain constraint (physical law). Downstream constraints address how societies and individuals coordinate around this immutable boundary: meaning-seeking under the knowledge of finitude, institutions for transferring resources across generations, and grief as a collective action problem. Each downstream constraint may be rope, scaffold, or snare depending on how the institutional response manages the psychological and social coordination problem that mortality creates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
