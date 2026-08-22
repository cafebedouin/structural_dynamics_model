% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__structural_contraction_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Mutual Annihilation: Structural Contraction Reading
 *   domain: strategic/military/physical law
 *
 * SUMMARY:
 *   This is the structural_contraction_reading of the
 *   nuclear_impossibility_kernel. It holds that nuclear weapons have created
 *   a physical impossibility: no rational path to victory exists between
 *   nuclear-armed powers because mutual annihilation is guaranteed. The
 *   constraint emerges from physics (thermonuclear reaction yields,
 *   atmospheric distribution, climatic feedbacks) and the technical
 *   achievement of invulnerable second-strike forces. War between nuclear
 *   powers does not happen because it cannot happen rationally—the M-set has
 *   contracted such that the war cell is no longer reachable. This reading is
 *   distinct from two sibling readings: the credibility_paradox_reading
 *   (deterrence is logically unstable because the threat is incredible) and
 *   the rational_dropout_reading (victory is possible but exceeds conceivable
 *   benefit). The structural_contraction_reading makes the stronger claim:
 *   the war cell is logically empty, not merely unattractive.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: institutional power, observing the constraint as physical law they cannot violate
 *   - non_nuclear_states_under_umbrella: organized power, beneficiaries of the constraint's stabilization of the international system
 *   - conventional_militaries: organized power, operating within the constraint's shadow but not directly constrained by it
 *   - disarmament_advocates: moderate power, excluded from deterrence discourse, arguing the constraint is dangerous despite being logical
 *   - physics_of_thermonuclear_reaction: non-agent referent, the foundational source of the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.0).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.0).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Mutual Annihilation: Structural Contraction Reading").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic/military/physical law").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, 'b50eae3e-8691-4856-8735-6bd5b97a200c').
narrative_ontology:cs_kernel_codification('b50eae3e-8691-4856-8735-6bd5b97a200c', formalized).
narrative_ontology:cs_authority_grounding('b50eae3e-8691-4856-8735-6bd5b97a200c', expertise).
narrative_ontology:cs_interpretation_layer_present('b50eae3e-8691-4856-8735-6bd5b97a200c').
narrative_ontology:cs_reading_relation('b50eae3e-8691-4856-8735-6bd5b97a200c', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_reading_relation('b50eae3e-8691-4856-8735-6bd5b97a200c', nuclear_impossibility_kernel__rational_dropout_reading, influences).
narrative_ontology:cs_axiom('b50eae3e-8691-4856-8735-6bd5b97a200c', foundational, mutual_annihilation_physically_guaranteed).
narrative_ontology:cs_axiom_status(mutual_annihilation_physically_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('b50eae3e-8691-4856-8735-6bd5b97a200c', mutual_annihilation_physically_guaranteed, empirically_contingent).
narrative_ontology:cs_axiom('b50eae3e-8691-4856-8735-6bd5b97a200c', foundational, rational_actor_cannot_choose_mutual_destruction).
narrative_ontology:cs_axiom_status(rational_actor_cannot_choose_mutual_destruction, holdable).
narrative_ontology:cs_axiom_grounding('b50eae3e-8691-4856-8735-6bd5b97a200c', rational_actor_cannot_choose_mutual_destruction, instrumental).
narrative_ontology:cs_reference_frame('b50eae3e-8691-4856-8735-6bd5b97a200c', mutual_assured_destruction_doctrine).
narrative_ontology:cs_drift_state('b50eae3e-8691-4856-8735-6bd5b97a200c', contemporary_strategic_community, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b50eae3e-8691-4856-8735-6bd5b97a200c', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states_under_umbrella).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, second_strike_capability_invulnerability).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, assured_destruction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess arsenals capable of destroying civilizations. Structural position: they cannot rationally use the weapons against other nuclear powers because the consequences are mutual annihilation. They observe the physical constraint and adapt strategic posture accordingly (second-strike forces, crisis stability protocols, deterrence doctrine). They benefit from the constraint's stabilization of the international system, though they did not cause the constraint and cannot remove it.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapon_states, observer,
    institutional, civilizational, analytical, global).

% Are extended-deterrence clients of nuclear powers. They benefit from the structural impossibility of large-scale warfare between nuclear-armed guarantors (which would render their security guarantees moot). They can exit by acquiring their own arsenals or seeking alternative security arrangements, but currently accrue deterrence benefit from the constraint's operation.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states_under_umbrella, beneficiary,
    organized, generational, mobile, regional).

% Operate within the constraint's shadow. Major-power conventional forces cannot be deployed against each other with escalation-uncontrolled outcomes because the nuclear threshold exists. They observe this structural limit and work within it (proxy conflicts, conventional deterrence, force structuring that avoids existential confrontation).
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, conventional_militaries, observer,
    organized, generational, analytical, global).

% Would argue that the constraint is a reason to eliminate nuclear weapons entirely: the catastrophic consequences of miscalculation or accident make the system inherently unstable regardless of rational intention. Their objection to the constraint is not that it exists but that it is insufficient safeguard—they are structurally excluded from deterrence-doctrine discourse because their premise (the constraint is dangerous despite being logical) is orthogonal to the constraint's classification.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% Is not a sentient actor but the foundational referent: the physical fact that thermonuclear weapons release energy on a civilizational-destruction scale, that no defensive shield exists to stop all warheads, and that atmospheric and climatic consequences are irreversible on human timescales. This physical fact is the source of the constraint's emergence.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, physics_of_thermonuclear_reaction, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(nuclear_impossibility_kernel__structural_contraction_reading, physics_of_thermonuclear_reaction).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint solves no coordination problem. It is a physical fact: mutual annihilation capability makes large-scale nuclear war between equipped powers structurally irrational. Nuclear-armed states do not choose to cooperate because of the constraint; they cooperate in order to manage a fact they cannot choose.
% TRANSFER_FUNCTION: No transfer occurs. The constraint is not extractive; nothing moves from one party to another because the constraint's operation is null—war does not happen because it is impossible, not because parties exchange value.
% ABSENT_VOICES: Escalation-ladder theorists argue the constraint is contestable (some scenarios permit limited nuclear exchange without mutual destruction). Adversary-collapse theorists argue irrational actors might use weapons despite the consequences. Their objections are that the constraint's edge cases are more porous than structural-contraction logic admits, not that they would benefit from its removal.
% DISAPPEARANCE_RATIONALE: If nuclear weapons ceased to exist or became strategically useless (perfect defense, universal disarmament, technology supercession), large-scale warfare between major powers would re-enter the option set. The constraint's disappearance would require decades of multi-state negotiation, verification systems, and enforcement mechanisms because the physical fact (mutual annihilation capability) persists until actively reversed—but its removal would structurally enable war that is currently impossible.
% FOUNDING_PROBLEM: The founding problem is not a social problem the constraint solves; it is a physical problem the constraint instantiates. Nuclear weapons were built to be the ultimate deterrent and threat-instrument in great-power competition. The constraint emerges from their development: once second-strike arsenals became invulnerable (submarine-based, mobile, hardened), the possibility of rational victory in large-scale nuclear war became mathematically and physically impossible. The founding problem is the discovery of this impossibility.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is attested by: (1) Strategic military doctrine across all nuclear powers (NATO doctrine, Russian military strategy, Chinese strategic assessments all rest on assumed mutual annihilation). (2) Physics (peer-reviewed literature on radiative transfer, atmospheric science, climatic consequences of nuclear war). (3) Independent strategic analysis outside any benefiting party (Nuclear Threat Initiative, Stimson Center, academic strategic studies). The problem is not disputed: whether mutual annihilation is a stabilizing fact or a dangerous gamble is contested, but the physical impossibility of rational victory is accepted by credible sources across strategic traditions.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as mountain because it emerges naturally from physics. Extractiveness is zero: the constraint produces no transfer, no coercion, no asymmetric benefit to any party. No one collects from the impossibility of nuclear war. Suppression is zero: the constraint requires no active suppression or enforcement because it operates at the level of physical law. Once second-strike forces are in place and understood, no party chooses to fight large-scale nuclear war regardless of enforcement. Theater ratio is zero: there is no performative component; the constraint's function is its entire operation. Accessibility collapse is very high (0.95) because once the physical facts are established and integrated into strategic doctrine, the war cell simply ceases to be a reachable option—alternatives to nuclear exchange (proxy conflicts, conventional deterrence, diplomacy, crisis management) are observed as the only available paths. Resistance is near-zero (0.02) because the constraint does not require active defense or enforcement—no party defends the impossibility of nuclear war; it is simply accepted as a boundary condition of strategic action. The measurement series are flat because the constraint's properties are stable across the 75-year interval; the only variation would be in the depth of understanding and acceptance, but the physical facts do not change. This flatness is appropriate for a genuine natural law.
 *
 * PERSPECTIVAL GAP:
 *   Different institutional seats may experience the constraint differently, but not in a way that produces different classifications. A nuclear-armed state sees the constraint as a physical boundary on its options; a non-nuclear state sees it as a guarantee of security from large-power war. Both perceive the same constraint (war is impossible), but the beneficiary structure differs. The gap is acknowledged but does not produce seat divergence in classification because the constraint's logical structure is the same from every seat—no rational path exists.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply to a mountain with no extraction. The beneficiaries listed (nuclear_weapon_states and non_nuclear_states_under_umbrella) do not collect from the constraint; they benefit from its stabilizing effect on the international system, but this is not extraction. The beneficiaries declaration is here to trigger FSM evaluation: does a genuine natural law—the physical impossibility of rational nuclear victory—have beneficiaries who prefer its preservation? The answer is yes: nuclear-armed states benefit from the perception that large-scale war is impossible, and non-nuclear states benefit from extended deterrence. This creates a false-summit candidate: the constraint might be presented as natural law when it actually serves the interests of those who benefit from its acceptance. The omega variable addresses this ambiguity directly.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply. The constraint has no mandate; it is a physical fact. The founding problem (the discovery that nuclear weapons create mutual annihilation capability) is alive: the problem that motivated nuclear weapons development (deterring great-power war) persists. The constraint's function has not outlived its founding purpose; if anything, the founding purpose is more salient now than in 1950.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the constraint a natural law (physical impossibility derived from thermodynamics, atmospheric science, and ballistics) or a constructed doctrine (the strategic belief that mutual annihilation is guaranteed, which could be false if defensive systems were deployed or if first-strike could eliminate enough weapons)?',
    'Empirical resolution requires: (1) verification that second-strike forces remain survivable under plausible first-strike scenarios, (2) demonstration that no shield-penetration or selective-exchange strategy remains strategically viable, (3) independent peer-reviewed physics confirming climatic consequences are irreversible. If any of these fails, the constraint shifts from physical law to strategic doctrine.',
    'If natural law: classification as mountain is correct. If constructed doctrine: classification should be tangled_rope (states coordinate on mutual deterrence through shared acceptance of the impossibility). The beneficiary structure (nuclear-armed states) would become salient if the constraint is doctrine rather than physics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, empirical, 'Whether the constraint is grounded in irreducible physics or contingent strategic belief.').

omega_variable(
    reading_foreclosure_among_siblings,
    'Does the structural_contraction_reading''s claim (war is logically impossible) foreclose the rational_dropout_reading (war is possible but exceeds conceivable benefit) within the same strategic framework?',
    'If both readings can coexist in a single state''s military doctrine (e.g., assuming war is impossible for planning purposes while maintaining contingency plans for limited-exchange scenarios), then coexists_with is correct. If the structural_contraction reading would, if true, make the rational_dropout reading logically incoherent (because if war is impossible, the cost-benefit analysis becomes moot), then forecloses is correct.',
    'If forecloses: the readings have a strict hierarchical relationship (structural_contraction is upstream). If coexists_with: they represent genuinely different strategic traditions held by different state actors with different threat assessments. The classification of the reading_relations edge depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_among_siblings, conceptual, 'Whether the structural impossibility claim logically forecloses the cost-benefit rationality claim.').

omega_variable(
    proxy_war_as_substitution_not_continuation,
    'Are proxy wars (conflicts between nuclear powers fought through non-nuclear intermediaries) a genuine substitute for direct war or a different phenomenon that does not test the constraint?',
    'Analysis of proxy wars during the Cold War and after: do they represent rational continuation of great-power conflict under the constraint, or are they genuinely different in structure and stakes? Does the absence of direct great-power war correlate with the existence of proxy-war substitutes?',
    'If substitution: the constraint''s operation includes channeling conflict into proxy forms; the strategic import is that direct war is replaced by indirect war. If different phenomenon: the constraint operates only on the direct interaction level; proxy wars do not test it. The claim about M-set contraction depends on which interpretation is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_war_as_substitution_not_continuation, conceptual, 'Whether proxy conflicts represent substitution for direct war or are orthogonal to the constraint''s scope.').

omega_variable(
    false_summit_beneficiary_capture,
    'Is the constraint presented as a natural physical law when it actually serves the strategic interests of nuclear-armed states that prefer the international order it stabilizes?',
    'Genealogical analysis: who benefits from the narrative that nuclear war is physically impossible versus who benefits from the narrative that nuclear war is merely catastrophic? Do nuclear-armed states and non-nuclear extended-deterrence clients actively promote the impossibility framing precisely because it delegitimizes first-strike thinking? Disarmament advocates argue the constraint is a false summit—a natural-law framing that obscures human choices about maintaining arsenals.',
    'If false summit: the constraint is a constructed doctrine that benefits identifiable parties (nuclear-armed states, their allies), and classification should account for the beneficiary structure. The emerging_naturally and beneficiaries declarations would trigger FSM evaluation. If genuine natural law: beneficiary presence is coincidental to the physics, and the constraint remains a mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_capture, preference, 'Whether the constraint is presented as natural law to serve the interests of nuclear-armed states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1950, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1950, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t1950, observed).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1962, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t1962, observed).
narrative_ontology:measurement(nucl_tr_t1975, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1975, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t1975, observed).
narrative_ontology:measurement(nucl_tr_t1990, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1990, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t1990, observed).
narrative_ontology:measurement(nucl_tr_t2010, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2010, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t2010, observed).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2025, 0.0).
narrative_ontology:measurement_basis(nucl_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1950, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1950, 0.0).
narrative_ontology:measurement_basis(nucl_be_t1950, observed).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1962, 0.0).
narrative_ontology:measurement_basis(nucl_be_t1962, observed).
narrative_ontology:measurement(nucl_be_t1975, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1975, 0.0).
narrative_ontology:measurement_basis(nucl_be_t1975, observed).
narrative_ontology:measurement(nucl_be_t1990, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1990, 0.0).
narrative_ontology:measurement_basis(nucl_be_t1990, observed).
narrative_ontology:measurement(nucl_be_t2010, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2010, 0.0).
narrative_ontology:measurement_basis(nucl_be_t2010, observed).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2025, 0.0).
narrative_ontology:measurement_basis(nucl_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1950, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1950, 0.0).
narrative_ontology:measurement_basis(nucl_su_t1950, observed).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1962, 0.0).
narrative_ontology:measurement_basis(nucl_su_t1962, observed).
narrative_ontology:measurement(nucl_su_t1975, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1975, 0.0).
narrative_ontology:measurement_basis(nucl_su_t1975, observed).
narrative_ontology:measurement(nucl_su_t1990, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1990, 0.0).
narrative_ontology:measurement_basis(nucl_su_t1990, observed).
narrative_ontology:measurement(nucl_su_t2010, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2010, 0.0).
narrative_ontology:measurement_basis(nucl_su_t2010, observed).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2025, 0.0).
narrative_ontology:measurement_basis(nucl_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__structural_contraction_reading, 0.0).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, extended_deterrence_credibility_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, crisis_stability_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the nuclear_impossibility_kernel. The kernel is contested across three readings: (1) structural_contraction_reading (this file) — war is logically impossible; (2) credibility_paradox_reading — deterrence requires credible threat but use guarantees mutual destruction, so the threat is incredible; (3) rational_dropout_reading — victory is possible but cost exceeds benefit. Each reading has its own constraint story with distinct ε values, beneficiary structures, and classifications. They share a referent (nuclear weapons' strategic consequences) but differ in scope and logical structure. Link all three via network.affects_constraints to indicate the constraint family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
