% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Nuclear-Enforced Total War Reachability Boundary (Contraction Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint story instantiates the contraction_reading of the
 *   total_war_reachability_boundary kernel. The reading holds that nuclear
 *   weapons physically contracted the strategic space such that winnable
 *   total war exited the feasible set entirely — not probabilistically, not
 *   contingently, but as a Mountain-class physical impossibility. The
 *   constraint's enforcement mechanism is thermonuclear physics: the energy
 *   release, firestorm dynamics, atmospheric soot injection, and agricultural
 *   collapse that follow any large-scale exchange make 'victory' semantically
 *   void. No actor benefits (no beneficiary structure exists); the victim set
 *   is universal (all human populations, future generations, biosphere
 *   continuity). The suppression is maximal (0.95) because alternatives to
 *   living under MAD collapsed when deliverable megatonnage arrived — but
 *   this suppression is structural/physical, not institutional. Theater ratio
 *   is near-zero (0.05) because the constraint's operation is not
 *   performative; the weapons work, the physics is real, the boundary holds.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.0).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.95).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Nuclear-Enforced Total War Reachability Boundary (Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '6ba50c3a-bd86-46c2-aeca-14fb4451a015').
narrative_ontology:cs_kernel_codification('6ba50c3a-bd86-46c2-aeca-14fb4451a015', implicit).
narrative_ontology:cs_authority_grounding('6ba50c3a-bd86-46c2-aeca-14fb4451a015', practice).
narrative_ontology:cs_interpretation_layer_present('6ba50c3a-bd86-46c2-aeca-14fb4451a015').
narrative_ontology:cs_reading_relation('6ba50c3a-bd86-46c2-aeca-14fb4451a015', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('6ba50c3a-bd86-46c2-aeca-14fb4451a015', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('6ba50c3a-bd86-46c2-aeca-14fb4451a015', foundational, total_war_physically_unwinnable_under_mad).
narrative_ontology:cs_axiom_status(total_war_physically_unwinnable_under_mad, holdable).
narrative_ontology:cs_axiom_grounding('6ba50c3a-bd86-46c2-aeca-14fb4451a015', total_war_physically_unwinnable_under_mad, empirically_contingent).
narrative_ontology:cs_axiom('6ba50c3a-bd86-46c2-aeca-14fb4451a015', foundational, extinction_risk_is_universal_and_non_transferable).
narrative_ontology:cs_axiom_status(extinction_risk_is_universal_and_non_transferable, holdable).
narrative_ontology:cs_axiom_grounding('6ba50c3a-bd86-46c2-aeca-14fb4451a015', extinction_risk_is_universal_and_non_transferable, deontological).
narrative_ontology:cs_reference_frame('6ba50c3a-bd86-46c2-aeca-14fb4451a015', pre_nuclear_total_war_feasibility).
narrative_ontology:cs_drift_state('6ba50c3a-bd86-46c2-aeca-14fb4451a015', contemporary_arsenal_modernization, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6ba50c3a-bd86-46c2-aeca-14fb4451a015', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, all_human_populations).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, future_generations).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, biosphere_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, mutually_assured_destruction_as_physical_fact).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, strategic_parity_precludes_decisive_victory).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, extinction_risk_as_system_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals and deterrence doctrines as constitutive of their great-power status. Their strategic identity is fused with the possession and operational readiness of weapons that make total war unwinnable. Exit would require abandoning the identity-structure of nuclear-armed statehood itself — not merely disarming but ceasing to be what they structurally are in the international system.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, identity_locked, global).

% Live under the extinction risk created by arsenals they do not control and cannot influence. Their security is structurally dependent on the restraint of nuclear-armed states; they pay the costs of deterrence stability (proliferation constraints, security guarantees, crisis instability) without holding the decision levers. Exit options are constrained by alliance structures and the NPT regime.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, non_nuclear_weapon_states, payer,
    organized, biographical, constrained, global).

% Bear the species-level extinction risk as a background condition of existence. No individual or collective exit exists — the constraint operates at the civilizational scale where 'exit' is semantically void. The suppression is total: alternatives to living under MAD collapsed when the first thermonuclear arsenals reached deliverable megatonnage.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, all_human_populations, payer,
    powerless, biographical, trapped, global).

% Inherit the structural risk without consent or voice. The constraint's time horizon exceeds any political cycle; the weapons and the doctrines that make total war unreachable persist across generations regardless of democratic turnover. Their situation is the definition of trapped: they cannot exit a condition that precedes their existence.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, future_generations, payer,
    powerless, generational, trapped, global).

% Study the boundary from outside the operational chain of command. Their analytical seat sees the full structure: the physical impossibility of winnable total war, the identity-lock of nuclear-armed states, the universal victim set. They do not collect from the constraint nor pay into it — they map its topology.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, strategic_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates mutual restraint among nuclear-armed states by making the cost of total war physically infinite — the only coordination problem that admits a Mountain solution because the enforcement mechanism is physics itself (energy release, atmospheric effects, civilizational collapse).
% TRANSFER_FUNCTION: Transfers nothing between actors — there is no extractive flow because no actor can win. The constraint operates by removing the payoff structure entirely: the 'prize' of total war (decisive victory, territorial conquest, regime change by force) is deleted from the feasible set. What looks like a transfer (deterrence stability buying non-war) is actually a subtracted possibility space.
% ABSENT_VOICES: No voices are absent in the structural sense — the victim set is universal (all human populations, future generations, biosphere continuity). The 'absent' party would be any actor who could credibly claim winnable total war remains reachable; that actor does not exist in the physical world, only in the contingent_reachability_reading's technological optimism.
% DISAPPEARANCE_RATIONALE: If the nuclear-enforced boundary disappeared overnight (arsenals vanished, physics suspended), the strategic space would expand to include winnable total war as a reachable option. Great-power conflict would revert to pre-1945 logic: decisive victory becomes thinkable, conquest re-enters the feasible set, and the coordination function of MAD evaporates. The world rearranges because the Mountain's physical enforcement is gone — the constraint IS the physics.
% FOUNDING_PROBLEM: The problem of great-power total war producing unbounded destruction without decisive outcome — the WWI/WWII sequence showed industrial total war could kill tens of millions and still not settle the strategic question. Nuclear weapons solved this by making the cost physically infinite and the outcome physically indeterminate.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live per the universal victim set: the risk of uncontrolled escalation to civilizational collapse persists as long as arsenals exist. Corroboration comes from outside the nuclear-armed states' self-justification: the 1985 Gorbachev-Reagan 'nuclear war cannot be won' statement (adversarial principals), the 2022 G20 Bali declaration (consensus including non-nuclear states), and the persistent Doomsday Clock at 90 seconds to midnight (independent scientific assessment). No beneficiary group exists to self-assert the problem — the constraint has no beneficiaries.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.0, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is zero because no party collects — the constraint deletes the payoff rather than redirecting it. Suppression is 0.95 because the alternative (winnable total war) is physically inaccessible, not merely prohibited. Accessibility collapse is 0.98: once the physics of MAD is understood, no strategic doctrine can restore winnable total war to the feasible set without denying physics. Resistance is 0.02: the constraint meets almost no active resistance because resistance would require denying thermonuclear physics. The Mountain classification rests on emerges_naturally=true — the boundary emerges from physics, not human design, even though the weapons are human-made. The victim declarations (all_human_populations, future_generations, biosphere_continuity) are structural: they bear the extinction risk without consent, exit, or compensation.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute identical per-seat types (mountain) because ε = 0 makes χ = 0 regardless of directionality. The perspectival gap here is not in classification but in lived reality: nuclear weapon states experience the constraint as identity-constitutive (they ARE the deterrent), non-nuclear states as structural dependency, populations as existential background condition, analysts as topological fact. The divergence is phenomenological, not classificatory — the Mountain is the one type where all seats agree on the type but disagree on what it means to live inside it.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are agenda_setters (institutional power, identity_locked exit) — they administer the arsenals but are themselves trapped by the identity-structure of nuclear-armed statehood. Their d-value is structurally derived near 0.5 (symmetric): they neither purely benefit nor purely pay; they are constitutive of the constraint. Non-nuclear weapon states are payers (organized power, constrained exit) — they bear costs (NPT constraints, crisis instability) without control. All human populations and future generations are payers (powerless, trapped) — they bear the full extinction risk with zero exit. Strategic analysts are observers (analytical, analytical) — they see the structure without collecting or paying. The universal victim set means effective extraction is near-zero for all seats: χ ≈ ε × f(d,scope) ≈ 0 because ε = 0. The Mountain is experienced identically from every seat: physics does not negotiate.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy risk: the constraint's function (making total war physically unwinnable) remains live and is physically enforced. The mandate has not outlived its function because the function IS the physics. There is no administrative apparatus to atrophy — the weapons either exist or they don't; the physics either holds or it doesn't. Theater ratio near zero confirms no performative maintenance layer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physics_vs_equilibrium_fundament,
    'Is the total war boundary grounded in thermonuclear physics (Mountain) or in a stable deterrence equilibrium that could theoretically break (Rope/Tangled Rope)?',
    'Counterfactual analysis: if all nuclear-armed states simultaneously decided to fight a total war, would physics prevent decisive victory (Mountain) or would the equilibrium simply collapse into a winnable war (Rope)? The Cuban Missile Crisis and numerous near-misses test the equilibrium reading; the physics of firestorms and nuclear winter test the Mountain reading.',
    'If equilibrium-grounded, the constraint is a Rope (coordination) or Tangled Rope (if enforcement extracts) and could fail without physics changing. If physics-grounded, it is a Mountain and cannot fail short of physics changing. This is the core kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physics_vs_equilibrium_fundament, conceptual, 'Whether the boundary''s enforcement is physical law or strategic equilibrium').

omega_variable(
    technological_reversal_feasibility,
    'Could advances in missile defense, cyber warfare, AI-enabled targeting, or novel delivery systems restore winnable total war to the feasible set?',
    'Technical assessment of whether any foreseeable technology can negate the physical basis of MAD (assured second strike, atmospheric effects, civilizational collapse). Requires distinguishing between degrading deterrence stability (equilibrium) and negating the physics of extinction (Mountain).',
    'If technological reversal is feasible, the Mountain claim fails and the constraint becomes a Piton (atrophied) or Tangled Rope (actively maintained). This is the contingent_reachability_reading''s core claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_reversal_feasibility, empirical, 'Whether future technology could reopen the strategic space').

omega_variable(
    identity_lock_mechanism_nuclear_states,
    'What specific identity-fusion mechanism binds nuclear weapon states to the deterrent posture — professional identity (strategic community), institutional identity (state-as-nuclear-power), ideological identity (deterrence as moral necessity), or relational identity (adversarial coupling)?',
    'Historical analysis of disarmament decisions (South Africa, Ukraine, Kazakhstan, Belarus) vs. non-disarmament trajectories. Compare identity narratives in nuclear policy elites across states. Test whether exit from nuclear-armed statehood correlates with identity-structure shifts.',
    'If identity_lock is the primary barrier to disarmament, the constraint''s persistence has a social-psychological component alongside the physical one. This would support a Piton or Tangled Rope reading for the *institutional* layer even while the physical boundary remains Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_nuclear_states, empirical, 'Identity-fusion mechanism binding nuclear-armed states to deterrent posture').

omega_variable(
    kernel_committer_structure,
    'This constraint is one reading (contraction_reading) of the contested kernel total_war_reachability_boundary. The sibling readings are dropping_reading and contingent_reachability_reading. Where exactly is the structural disagreement located?',
    'Map the structural delta: contraction_reading claims ε=0, Mountain, universal victims, no beneficiaries. dropping_reading claims ε>0 (deterrence stability as coordination rent), Rope, beneficiaries=nuclear_weapon_states. contingent_reachability_reading claims ε≈0 but rising, Piton→Tangled_Rope trajectory, victims=current_generations only. The disagreement is located in (a) the ε referent (physics vs. equilibrium), (b) the victim set scope (universal vs. current), (c) the beneficiary existence (none vs. nuclear states).',
    'Resolving this identifies which kernel reading corresponds to the actual constraint structure. The engine will classify each reading independently; the corpus measures the divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer-frame structural delta between kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.02).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__contraction_reading, theater_ratio, 1962, 0.03).
narrative_ontology:measurement(tota_tr_t1985, total_war_reachability_boundary__contraction_reading, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contraction_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__contraction_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 0.0).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1962, 0.0).
narrative_ontology:measurement(tota_be_t1985, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1985, 0.0).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2025, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1962, 0.9).
narrative_ontology:measurement(tota_su_t1985, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1985, 0.95).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(tota_su_t2025, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2025, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contraction_reading, 0.0).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, nuclear_nonproliferation_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, extended_deterrence_architecture).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, strategic_arms_control_treaties).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, crisis_stability_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint (contraction_reading) and dropping_reading are dual formulations of the same kernel: one treats MAD as physics (Mountain, ε=0), the other as equilibrium (Rope, ε>0). contingent_reachability_reading is a third formulation treating the boundary as technologically contingent (Piton→Tangled_Rope). The three stories form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__contraction_reading, institutional, 0.5).
constraint_indexing:directionality_override(total_war_reachability_boundary__contraction_reading, powerless, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
