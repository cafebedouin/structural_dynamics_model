% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Contraction (Nuclear Impossibility)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the total war reachability
 *   kernel: the contraction reading asserts that nuclear weapons physically
 *   contracted the strategic space such that a winnable total war is no
 *   longer reachable. The reading treats the contraction as a structural fact
 *   (a mountain — irreversible, no alternatives, no beneficiary structure)
 *   rather than a contingent arrangement that could be reversed or a
 *   coordination equilibrium that actors maintain. The constraint is claimed
 *   as a mountain because, under this reading, the laws of nuclear physics
 *   and the logic of mutual assured destruction make total war infeasible
 *   regardless of any actor's preferences. No one benefits from the
 *   constraint; no one could profitably maintain it; no alternatives remain.
 *
 * KEY AGENTS:
 *   - all_human_populations: species-level risk bearers, in the extinction set if reachability reversed
 *   - nuclear_weapons_states: the actors whose capability instantiates the constraint; none can unilaterally leave it
 *   - military_strategists: operators within the contracted space, for whom total war is no longer a planning option
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.0).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Contraction (Nuclear Impossibility)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '09ec218d-cfc9-4c89-a362-710d2eeb08cf').
narrative_ontology:cs_kernel_codification('09ec218d-cfc9-4c89-a362-710d2eeb08cf', distributed).
narrative_ontology:cs_authority_grounding('09ec218d-cfc9-4c89-a362-710d2eeb08cf', diffuse_epistemic).
narrative_ontology:cs_reading_relation('09ec218d-cfc9-4c89-a362-710d2eeb08cf', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('09ec218d-cfc9-4c89-a362-710d2eeb08cf', total_war_reachability_boundary__contingent_reachability_reading, influences).
narrative_ontology:cs_axiom('09ec218d-cfc9-4c89-a362-710d2eeb08cf', foundational, total_war_physically_unreachable).
narrative_ontology:cs_axiom_status(total_war_physically_unreachable, holdable).
narrative_ontology:cs_axiom_grounding('09ec218d-cfc9-4c89-a362-710d2eeb08cf', total_war_physically_unreachable, empirically_contingent).
narrative_ontology:cs_axiom('09ec218d-cfc9-4c89-a362-710d2eeb08cf', foundational, second_strike_assurance_irreversible).
narrative_ontology:cs_axiom_status(second_strike_assurance_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('09ec218d-cfc9-4c89-a362-710d2eeb08cf', second_strike_assurance_irreversible, empirically_contingent).
narrative_ontology:cs_reference_frame('09ec218d-cfc9-4c89-a362-710d2eeb08cf', nuclear_deterrence_impossibility).
narrative_ontology:cs_drift_state('09ec218d-cfc9-4c89-a362-710d2eeb08cf', contemporary_2025, gap(stable, minor, true)).
narrative_ontology:cs_created_at('09ec218d-cfc9-4c89-a362-710d2eeb08cf', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, all_human_populations).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, mutually_assured_destruction_logic).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, second_strike_capability_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every person on Earth is in the extinction-risk set if total war becomes feasible. No population can exit, negotiate, or change the constraint's operation. Humanity collectively inhabits the reachable state space; total war is now unreachable within that space given mutual nuclear capability.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, all_human_populations, observer,
    powerless, civilizational, trapped, universal).

% Possess the weapons whose existence contracts reachability. Each state maintains second-strike capability as deterrent against the others; the mutual possession IS the constraint on what strategies are feasible to execute. No state can unilaterally 'leave' the constraint by disarming without accepting strategic vulnerability.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_weapons_states, observer,
    institutional, civilizational, analytical, global).

% Operate within the contracted reachability space. Total war is no longer a strategic option in their planning calculus; all workable strategies must assume mutual deterrence holds. Their exit from the constraint is not a choice but a professional fact.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, military_strategists, observer,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no coordination function — this is not a coordination mechanism. The constraint is a physical/logical impossibility: given mutual nuclear capability and second-strike assurance, a winnable total war is unreachable in state space. It is analogous to gravity: universal, inescapable, not constructed.
% TRANSFER_FUNCTION: No transfer. A mountain produces no rents; no party collects from it. The constraint does not move resources, status, or outcomes from one actor to another.
% ABSENT_VOICES: Future technological trajectories that might alter the reachability boundary (fusion power, ABM systems, asteroid-based platforms) cannot speak in the present. Non-nuclear states have limited voice in the constraint's operation, though they are in its victim set.
% DISAPPEARANCE_RATIONALE: The constraint does not disappear overnight because it is a structural fact of weaponized physics, not an arrangement that could be abandoned. If the nuclear arsenals vanished instantaneously, humanity would remain bounded by the history of the weapons' invention — the knowledge would persist and the constraint would eventually re-instantiate. The constraint is not contingent on an enforceable decision; it is contingent on the laws of nuclear physics and the logical impossibility of winning against an opponent with second-strike capability.
% FOUNDING_PROBLEM: Uncontrolled conquest through total war was a feasible military strategy before nuclear weapons granted all major powers assured destruction capacity. Statecraft operated within a reachability set that included wars of annihilation.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and nuclear strategists from outside any nuclear weapons state (e.g., Japanese historians analyzing pre-nuclear deterrence, international relations scholars in non-nuclear states) document that total war was feasible and was carried out (WWII as exemplar). Post-1945 strategists within nuclear powers (Schelling, Waltz, Jervis) document the contraction of reachability. The physical fact is corroborated by weapons scientists: second-strike capability is mathematically assured given current submarine and missile technology, and no technological pathway to perfect ABM has materialized despite decades of research.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set to 0.05 (minimal) because there is no asymmetric rents or transfers — no actor collects from the constraint's operation. The only non-zero value reflects deep uncertainty about whether a mountain with universal extinction risk might carry some asymmetric effect we have not identified. Suppression is zero because a pure natural law does not suppress resistance; resistance itself is zero because there is no meaningful resistance to physics. Accessibility collapse is 0.98 because once the constraint is understood (mutual nuclear capability, second-strike assurance), no alternative to it remains accessible — actors cannot choose a world in which they can win a nuclear war. Theater ratio is zero because there is no performative maintenance; the constraint operates by physical law, not by ritual. The measurements are flat across the interval because a natural law does not drift.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a nuclear weapons state, the constraint is a brute fact of statecraft — a boundary condition within which strategy must operate. From the perspective of a non-nuclear state or a civilian population, the constraint is a species-level risk: extinction remains possible, merely not via 'winnable' total war (extinction via accidental or limited nuclear exchange remains reachable). The engine should compute identical types from all seats because the constraint itself is not extractive; seats diverge in their exposure to the extinction risk, not in their relationship to an extractive mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries and therefore no extractive directionality structure. All stakeholders are observers or trapped bearers of extinction risk. Nuclear weapons states are institutional actors, but they are not beneficiaries of a constraint they maintain — they are the agents through whose capability the constraint inheres. The absence of asymmetric extraction is the defining feature of a mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   A pure mountain has no mandate to become corrupted; it has no mandate at all. It is a feature of reality, not a policy. Therefore, mandatrophy does not apply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_contingent_contraction,
    'Is the contraction of total war reachability a property of nuclear physics (a genuine natural law), or a contingent feature of current weapons technology and doctrine that future innovation could reverse?',
    'Long-term technological monitoring: do anti-ballistic missile systems achieve near-perfect reliability? Do autonomous weapons or space-based platforms materially alter second-strike assurance? Historical analysis of analogous technology shifts (e.g., emergence of submarines in WWI changing naval warfare reachability).',
    'If the contraction is purely technological and reversible, the reading classification remains mountain but is weaker (contingent on engineering, not law). If reversible, the sibling ''contingent_reachability_reading'' constraint becomes more structurally plausible. If truly irreversible (physics proves second-strike is mathematically undefeatable), the mountain classification is unshakeable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_contraction, empirical, 'Whether total war''s unreachability is a natural law or a contingent technological fact.').

omega_variable(
    kernel_reading_under_determination,
    'Is this reading''s core claim (total war is unreachable as a matter of physics/logic) the same as the contraction-reading kernel entry, or does the reading instantiate a narrower or broader version of that claim?',
    'Comparison of this story''s referent (the standing strategic arrangement under contest) against the kernel''s documented intent. Does the reading''s ε-invariant constraint capture what the kernel framing intended, or have we authored a different constraint under the same label?',
    'If the referent diverges from the kernel intent, we may have missed the actual constraint the contest is about. The kernel may concern persistence of deterrence doctrine, technological reversibility, or beneficiary structures in nuclear policy — different ε entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether this reading faithfully instantiates the contraction-reading kernel or has authored a related but distinct constraint.').

omega_variable(
    victim_set_realism,
    'Is the victim set ''all human populations'' substantively different from ''no one'' (the definition of a pure natural law with no extractive agents)? Can a mountain with a universal victim set but zero extraction and zero suppression be distinguished from a natural law with no beneficiaries?',
    'Examine whether the universal victim set (extinction risk) is a structural feature of the constraint or a rhetorical consequence of the ε measurement. In a true mountain with no rents, all humans are ''at risk'' only in the sense that no one can escape the physical law — the same as with gravity. Is declaring ''all_human_populations'' as victims a meaningful structural fact, or a categorization error?',
    'If the victim set is rhetorical rather than structural, it should be removed from base_properties.victims, and the constraint becomes a pure mountain with no beneficiary/victim asymmetry. The current authoring treats extinction-in-possibility-space as a victimhood relation, which may be semantic overreach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_realism, conceptual, 'Whether a universal extinction-risk ''victim set'' is a structural property of this constraint or a categorization artifact.').

omega_variable(
    enforcement_infrastructure_vs_physics,
    'Does this constraint require active enforcement (verification treaties, missile monitoring, second-strike deployment doctrine) to persist, or does it persist purely from physics and thus requires no enforcement?',
    'Counterfactual: if all verification regimes, treaties, and deployment postures were abandoned tomorrow but the weapons and capability remained, would total war reachability change? If the answer is ''no, physics holds regardless,'' this is a mountain. If the answer is ''reachability would become contested again without enforcement,'' it is a tangled_rope under the appearance of a mountain.',
    'If enforcement is required, the constraint is structurally dependent on choices, not physics alone, and the classification drops from mountain to tangled_rope (beneficiaries: states that benefit from deterrence stability; victims: populations at extinction risk; requires_active_enforcement: true).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_infrastructure_vs_physics, empirical, 'Whether the constraint''s persistence depends on active enforcement infrastructure or physics alone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__contraction_reading, theater_ratio, 1962, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1980, total_war_reachability_boundary__contraction_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1980, observed).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(tota_tr_t2000, observed).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__contraction_reading, theater_ratio, 2025, 0.0).
narrative_ontology:measurement_basis(tota_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1962, 0.04).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1980, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement_basis(tota_be_t1980, observed).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement_basis(tota_be_t2000, observed).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2025, 0.05).
narrative_ontology:measurement_basis(tota_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_reachability_boundary__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel decomposes into three constraint stories, each a different reading. This story (contraction_reading) treats reachability as physically contracted and irreversible. The dropping_reading treats reachability as unchanged but probability-shifted (coordination, not physics). The contingent_reachability_reading treats current contraction as contingent on tech and doctrine (piton, not mountain). The three stories share the same referent (what happened to total war feasibility) and the same interval, but instantiate different ε values and claim different types. All three link via network.affects_constraints to document the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
