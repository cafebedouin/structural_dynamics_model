% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Nuclear Deterrence as Coordination Equilibrium (Dropping Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint story represents the 'dropping_reading' of the
 *   total_war_reachability_boundary kernel: the view that total war
 *   probability has declined from its Cold War peak but remains structurally
 *   reachable — deterrence is a coordination equilibrium (rope) that requires
 *   active maintenance, not a natural law (mountain). The constraint is the
 *   standing arrangement of nuclear deterrence as practiced by nuclear-armed
 *   states: declaratory policies, force postures, alliance commitments, and
 *   the intellectual framework that legitimizes them. Beneficiaries
 *   (nuclear-armed states, alliances, deterrence establishment) gain
 *   credibility, cohesion, and professional standing. Victims (populations,
 *   non-nuclear states, future generations) bear existential risk without
 *   consent. The arrangement requires active enforcement (modernization
 *   programs, exercises, signaling, nonproliferation regime) to maintain
 *   credibility — hence tangled_rope, not rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.62).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.78).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Nuclear Deterrence as Coordination Equilibrium (Dropping Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '862f33a4-078b-4338-a75b-d2b5b9546d10').
narrative_ontology:cs_kernel_codification('862f33a4-078b-4338-a75b-d2b5b9546d10', distributed).
narrative_ontology:cs_authority_grounding('862f33a4-078b-4338-a75b-d2b5b9546d10', practice).
narrative_ontology:cs_interpretation_layer_present('862f33a4-078b-4338-a75b-d2b5b9546d10').
narrative_ontology:cs_reading_relation('862f33a4-078b-4338-a75b-d2b5b9546d10', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('862f33a4-078b-4338-a75b-d2b5b9546d10', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('862f33a4-078b-4338-a75b-d2b5b9546d10', foundational, deterrence_is_maintained_coordination).
narrative_ontology:cs_axiom_status(deterrence_is_maintained_coordination, holdable).
narrative_ontology:cs_axiom_grounding('862f33a4-078b-4338-a75b-d2b5b9546d10', deterrence_is_maintained_coordination, empirically_contingent).
narrative_ontology:cs_axiom('862f33a4-078b-4338-a75b-d2b5b9546d10', foundational, total_war_reachability_is_contingent_on_active_signaling).
narrative_ontology:cs_axiom_status(total_war_reachability_is_contingent_on_active_signaling, holdable).
narrative_ontology:cs_axiom_grounding('862f33a4-078b-4338-a75b-d2b5b9546d10', total_war_reachability_is_contingent_on_active_signaling, empirically_contingent).
narrative_ontology:cs_reference_frame('862f33a4-078b-4338-a75b-d2b5b9546d10', cold_war_peak_deterrence_stability).
narrative_ontology:cs_drift_state('862f33a4-078b-4338-a75b-d2b5b9546d10', post_cold_war_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('862f33a4-078b-4338-a75b-d2b5b9546d10', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_armed_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, alliance_structures).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, deterrence_intellectual_establishment).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, future_generations).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, mutually_assured_destruction_stability).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, nuclear_taboo_persistence).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, deterrence_credibility_through_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals and set declaratory policy, force posture, and escalation doctrines. Their institutional identity is fused with nuclear possession — disarmament is treated as existential identity loss rather than policy choice. They coordinate deterrence credibility through signaling, exercises, and alliance commitments, extracting security guarantees and strategic autonomy while imposing existential risk on populations.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_armed_states, agenda_setter,
    institutional, generational, identity_locked, global).

% NATO and similar alliances gain cohesion and deterrence credibility from nuclear umbrellas. The nuclear guarantee binds members together and deters conventional aggression. Alliance managers benefit from the coordination function but are constrained by the nuclear-armed patrons' doctrine — they cannot independently exit the nuclear dependency without fracturing the alliance.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, alliance_structures, beneficiary,
    institutional, generational, constrained, continental).

% Strategic studies departments, think tanks, and advisory bodies derive professional standing, funding, and policy access from the deterrence framework. Their expertise is constituted by the nuclear order — career trajectories, publication venues, and advisory roles all presuppose its persistence. Exit means professional obsolescence in their self-conceived domain.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, deterrence_intellectual_establishment, beneficiary,
    organized, biographical, constrained, global).

% Civilian populations in nuclear-armed states, allied states, and adversary states bear the existential risk of deterrence failure. They have no meaningful exit — geographic relocation does not escape global fallout or nuclear winter scenarios. They pay through taxes for arsenals, through civil defense rituals, and through the perpetual background risk of inadvertent or deliberate escalation. No consent mechanism exists for this imposition.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat, payer,
    powerless, biographical, trapped, global).

% States party to the NPT forego nuclear weapons in exchange for disarmament commitments and peaceful-use cooperation. They bear the strategic asymmetry of facing nuclear-armed adversaries without reciprocal deterrence, and the treaty regime's enforcement machinery (IAEA safeguards, export controls) constrains their technological development. Their exit option — withdrawal and proliferation — carries severe diplomatic, economic, and security costs.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).

% Inherit the accumulated radiological legacy, the risk of deterrence breakdown, and the opportunity cost of resources diverted to arsenals. They have zero exit and zero voice in the arrangements that structure their existential risk profile. The coordination equilibrium's stability is purchased with their unwitnessed exposure.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Diplomats, verification experts, and NGO analysts who work within the constraint to reduce its extractiveness — treaties, risk reduction measures, transparency regimes. They see the full structure: the coordination function is real but brittle, the extraction is structural, and the identity lock of nuclear-armed states is the primary obstacle to transformation.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, arms_control_practitioners, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the security dilemma among nuclear-armed actors by establishing a mutually recognized existential threshold: no rational actor initiates total war because the cost (mutually assured destruction) exceeds any conceivable gain. The coordination is the shared, continuous signaling of resolve and capability that makes the threat credible without execution.
% TRANSFER_FUNCTION: Moves existential risk from nuclear-armed states' decision-makers onto their own populations, allied populations, non-nuclear states, and future generations. Moves strategic autonomy and security guarantees to nuclear-armed states and their alliances. Moves professional standing and resources to the deterrence intellectual establishment.
% ABSENT_VOICES: Populations under nuclear threat have never been consulted on the deterrence arrangement — no referendum, no consent mechanism exists. Future generations are structurally excluded by non-existence. Non-nuclear weapon states' objections (via NPT review conferences, TPNW) are formally heard but structurally overridden by nuclear-armed states' veto power in security councils and alliance councils.
% DISAPPEARANCE_RATIONALE: If the deterrence coordination equilibrium vanished overnight, nuclear-armed states would face an immediate security dilemma with no shared threshold — crisis instability would spike, alliance structures would fracture or reconfigure around conventional deterrence, the intellectual establishment would lose its organizing framework, and populations would face a different (arguably higher near-term) risk profile as the taboo dissolved. The world would rearrange around a new, unstabilized strategic geometry.
% FOUNDING_PROBLEM: Post-1945: how to prevent great-power war in an era of weapons that make total war unwinnable and potentially species-ending? The founding problem was stabilizing the security dilemma among actors who could destroy each other and civilization.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear-armed states and alliances attest the problem remains live (great-power competition, emerging arsenals). Arms control practitioners and historians (e.g., Mueller, Wilson, Tannenwald) attest the founding problem has mutated — the original bipolar stability problem is gone, replaced by multipolar complexity and new escalation pathways. TPNW states-parties attest the founding problem was mis-specified: the problem is not stabilizing deterrence but eliminating the weapons. No consensus exists.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the massive resource diversion to arsenals and the existential risk transfer to non-consenting populations. Suppression (0.78) reflects the active machinery preventing alternatives: nonproliferation regime, alliance discipline, taboo enforcement against disarmament advocacy, and the identity lock that makes nuclear possession constitutive of great-power status. Theater ratio (0.31) reflects genuine coordination function (crisis hotlines, arms control, risk reduction) mixed with performative signaling (exercises, declaratory rhetoric) that maintains the equilibrium's credibility. Accessibility collapse (0.58) — alternatives exist (disarmament, minimum deterrence, no-first-use) but are structurally suppressed by identity lock and alliance dependencies. Resistance (0.47) — sustained but fragmented: anti-nuclear movements, TPNW, arms control treaties, but no coherent counter-arrangement has displaced the equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-armed states' seat, the arrangement is a rope — genuine coordination preventing great-power war, maintained by responsible stewardship. From populations' seats, it is a snare — existential risk imposed without consent, maintained by suppressing disarmament alternatives. From non-nuclear states' seats, it is a tangled rope — they gain negative security assurances but pay with strategic asymmetry and constrained development. The engine computes this divergence from structural data; the claimed_type (tangled_rope) reflects the authoring seat's assessment that BOTH coordination and asymmetric extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed states are agenda_setters with identity_locked exit — their institutional self-concept is fused with nuclear possession (d ~ 0.15, beneficiary end). Alliance structures are beneficiaries with constrained exit — they gain cohesion but cannot leave the nuclear umbrella without fracturing (d ~ 0.35). Deterrence establishment are beneficiaries with constrained exit — professional identity depends on the framework (d ~ 0.4). Populations under threat are payers with trapped exit — no geographic or political escape from global effects (d ~ 0.95). Non-nuclear states are payers with constrained exit — withdrawal from NPT carries prohibitive costs (d ~ 0.75). Future generations are payers with trapped exit — zero voice, zero exit (d ~ 1.0). Arms control practitioners are observers with mobile exit — they can leave the field (d ~ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great-power war among nuclear-capable states) has mutated but not disappeared — multipolar competition and new escalation pathways (cyber, space, hypersonics, AI) revive the security dilemma in altered form. However, the arrangement's extraction has decoupled from its coordination function: arsenal sizes far exceed minimum deterrence requirements, modernization programs extend the arrangement's lifespan without strategic necessity, and the identity lock prevents adaptation. This is mandatrophy — the coordination mandate (stability) has been hollowed out by the extraction mandate (institutional identity, alliance cohesion, intellectual establishment). The constraint persists because the beneficiaries' identity is fused to it, not because the founding problem demands its current form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability,
    'Is the deterrence coordination function (preventing great-power war) structurally separable from the current extraction level (arsenal sizes, modernization, identity lock), or does the extraction constitute the price of coordination?',
    'Counterfactual analysis: minimum deterrence postures (China''s historical posture, proposed ''deterrence-only'' force levels) vs. current postures. If minimum deterrence maintains coordination with far lower extraction, the current extraction is not the price of coordination.',
    'If separable, the constraint is a snare with a rope cover — the coordination function is real but the extraction is gratuitous. If inseparable, the high extraction is the necessary cost of credible coordination, making tangled_rope the accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable or inherently coupled.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds nuclear-armed states to nuclear possession — is it great-power status identity, bureaucratic-institutional self-preservation, or the deterrence intellectual framework''s hold on strategic imagination?',
    'Comparative historical analysis: states that disarmed (South Africa, Ukraine, Kazakhstan, Belarus) vs. states that retained. Process-tracing of identity narratives in nuclear decision-making elites.',
    'If great-power status identity: the lock is symbolic and potentially reversible by status redefinition. If bureaucratic-institutional: the lock is organizational and requires institutional redesign. If intellectual framework: the lock is epistemic and requires paradigm shift in strategic studies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'The specific mechanism of identity lock that prevents nuclear-armed states from treating disarmament as a live policy option.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.78) primarily structural (nonproliferation regime, alliance discipline, taboo enforcement) or partially internalized (populations and elites have absorbed the deterrence framework as ''realism'' and self-suppress disarmament imagination)?',
    'Post-Cold War trajectory: if suppression were purely structural, the 1991-2000 disarmament momentum should have continued. Its reversal and the re-expansion of arsenals suggest internalized suppression — the framework reconstituted itself in elites'' self-concept.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agents even if structural barriers lower. This would amplify the tangled_rope classification toward snare for identity-locked seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in the nuclear deterrence arrangement.').

omega_variable(
    kernel_reading_framing,
    'Does the ''dropping_reading'' framing (probability dropped but reachability remains) genuinely differ from the ''contraction_reading'' (feasible set contracted) and ''contingent_reachability_reading'' (current state is a piton), or do they describe the same strategic geometry from different analytical angles?',
    'Formal modeling: does each reading produce distinct, testable predictions about crisis behavior, arms racing dynamics, or disarmament feasibility? If predictions converge, the readings are observational frames on one constraint; if they diverge, they are structurally distinct constraints.',
    'If readings converge, the kernel decomposition is analytical artifact — one constraint story suffices. If they diverge, each reading instantiates a genuinely distinct constraint with different ε, beneficiaries, victims, and type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the kernel''s declared readings are structurally distinct constraints or analytical frames on one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__dropping_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__dropping_reading, theater_ratio, 1962, 0.28).
narrative_ontology:measurement(tota_tr_t1972, total_war_reachability_boundary__dropping_reading, theater_ratio, 1972, 0.22).
narrative_ontology:measurement(tota_tr_t1983, total_war_reachability_boundary__dropping_reading, theater_ratio, 1983, 0.35).
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__dropping_reading, theater_ratio, 1991, 0.18).
narrative_ontology:measurement(tota_tr_t2001, total_war_reachability_boundary__dropping_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(tota_tr_t2014, total_war_reachability_boundary__dropping_reading, theater_ratio, 2014, 0.3).
narrative_ontology:measurement(tota_tr_t2022, total_war_reachability_boundary__dropping_reading, theater_ratio, 2022, 0.38).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__dropping_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.72).
narrative_ontology:measurement(tota_be_t1972, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1972, 0.65).
narrative_ontology:measurement(tota_be_t1983, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1983, 0.78).
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1991, 0.52).
narrative_ontology:measurement(tota_be_t2001, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(tota_be_t2014, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2014, 0.63).
narrative_ontology:measurement(tota_be_t2022, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2022, 0.68).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1945, 0.65).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.85).
narrative_ontology:measurement(tota_su_t1972, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1972, 0.75).
narrative_ontology:measurement(tota_su_t1983, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1983, 0.88).
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1991, 0.6).
narrative_ontology:measurement(tota_su_t2001, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement(tota_su_t2014, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2014, 0.78).
narrative_ontology:measurement(tota_su_t2022, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2022, 0.83).
narrative_ontology:measurement(tota_su_t2025, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__dropping_reading, 0.12).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nuclear_nonproliferation_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, extended_deterrence_credibility).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, strategic_stability_dialogue).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nuclear_modernization_programs).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% This constraint (dropping_reading) and its siblings (contraction_reading, contingent_reachability_reading) form a kernel family decomposing the 'total war reachability' concept. dropping_reading: deterrence is a coordination equilibrium requiring active maintenance (tangled_rope). contraction_reading: nuclear weapons altered the feasible set itself (mountain-like). contingent_reachability_reading: current low probability is a piton — atrophied capability that could reverse. Each has distinct ε, beneficiaries, victims, and type. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, institutional, 0.15).
constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
