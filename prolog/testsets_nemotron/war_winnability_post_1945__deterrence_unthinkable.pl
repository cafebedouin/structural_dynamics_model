% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear Deterrence: Total War Unwinnable (Deterrence Unthinkable Reading)
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the 'deterrence_unthinkable' reading
 *   of the contested kernel 'war_winnability_post_1945'. The reading holds
 *   that nuclear weapons made great-power total war categorically unwinnable
 *   by physical logic of escalation — planning for victory is not merely
 *   difficult but incoherent. Strategic planning therefore shifts from
 *   war-winning to war-prevention. The beneficiary is civilian populations of
 *   great-power states (and by extension, global population) who avoid total
 *   war. The victim is military establishments whose traditional mission
 *   (winning total wars) becomes incoherent. This reading stands in tension
 *   with two sibling readings: 'countervailing_thinkable' (limited victory
 *   remains achievable via counterforce targeting) and
 *   'rhetorical_contraction' (winnability became unsayable but remained
 *   operationally planned).
 *
 * KEY AGENTS:
 *   - civilian_populations_great_power_states: Primary beneficiary (powerless/analytical) — avoids total war
 *   - military_establishments_great_powers: Primary victim (institutional/organized) — mission incoherence
 *   - political_leadership_nuclear_states: Agenda setter (institutional) — authorizes deterrence posture
 *   - strategic_analysts_arms_control_community: Observer (analytical) — monitors stability
 *   - counterforce_planners_targeting_officers: Secondary victim/beneficiary (organized) — mission transformed not eliminated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.12).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.88).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.12).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, mountain).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear Deterrence: Total War Unwinnable (Deterrence Unthinkable Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, 'f01275d1-02dc-4d29-b1b2-889bb6e1632a').
narrative_ontology:cs_kernel_codification('f01275d1-02dc-4d29-b1b2-889bb6e1632a', formalized).
narrative_ontology:cs_authority_grounding('f01275d1-02dc-4d29-b1b2-889bb6e1632a', lineage).
narrative_ontology:cs_interpretation_layer_present('f01275d1-02dc-4d29-b1b2-889bb6e1632a').
narrative_ontology:cs_reading_relation('f01275d1-02dc-4d29-b1b2-889bb6e1632a', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_reading_relation('f01275d1-02dc-4d29-b1b2-889bb6e1632a', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('f01275d1-02dc-4d29-b1b2-889bb6e1632a', foundational, total_nuclear_war_categorically_unwinnable).
narrative_ontology:cs_axiom_status(total_nuclear_war_categorically_unwinnable, holdable).
narrative_ontology:cs_axiom_grounding('f01275d1-02dc-4d29-b1b2-889bb6e1632a', total_nuclear_war_categorically_unwinnable, empirically_contingent).
narrative_ontology:cs_axiom('f01275d1-02dc-4d29-b1b2-889bb6e1632a', secondary, deterrence_is_war_prevention_not_warfighting).
narrative_ontology:cs_axiom_status(deterrence_is_war_prevention_not_warfighting, holdable).
narrative_ontology:cs_axiom_grounding('f01275d1-02dc-4d29-b1b2-889bb6e1632a', deterrence_is_war_prevention_not_warfighting, deontological).
narrative_ontology:cs_reference_frame('f01275d1-02dc-4d29-b1b2-889bb6e1632a', mutual_assured_destruction_stability).
narrative_ontology:cs_drift_state('f01275d1-02dc-4d29-b1b2-889bb6e1632a', post_cold_war_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f01275d1-02dc-4d29-b1b2-889bb6e1632a', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_great_power_states).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments_great_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, counterforce_planners_targeting_officers).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, counterforce_planners_targeting_officers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their survival depends on the constraint holding. They cannot exit the nuclear condition — no individual or collective action removes them from the target set of adversary arsenals. They benefit from war prevention but have no leverage over the deterrence posture. Their situation is structural captivity to a constraint that subsidizes their existence.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_great_power_states, beneficiary,
    powerless, generational, trapped, global).

% Their traditional mission — winning total wars between great powers — is structurally negated by the constraint. They are identity-locked to the warfighting self-concept (professional identity, institutional culture, budget justification). The constraint forces mission transformation: deterrence operations, strategic command, arms control implementation. Some factions resist (counterforce advocates); others adapt. Exit means institutional suicide or radical redefinition — neither is organizationally viable.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments_great_powers, payer,
    institutional, biographical, identity_locked, global).

% They authorize and sustain the deterrence posture. They bear the burden of credible threat-making (which requires maintaining forces capable of the very war they must prevent). They benefit from war prevention but are constrained by the logic they invoke — they cannot credibly threaten what they structurally cannot execute. Their exit is constrained by alliance commitments and the security dilemma: unilateral disarmament is politically and strategically unavailable.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, political_leadership_nuclear_states, agenda_setter,
    institutional, biographical, constrained, global).

% They monitor, model, and critique the deterrence structure. They neither collect from nor pay into the constraint. Their analytical freedom is high (analytical exit), but their influence is mediated through political leadership. They are the seat that sees the full structure — including the sibling readings and the tensions between them.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, strategic_analysts_arms_control_community, observer,
    analytical, generational, analytical, global).

% They plan for limited nuclear options (counterforce strikes, damage limitation) that the deterrence_unthinkable reading says are incoherent. They pay in career risk and cognitive dissonance — planning missions their own strategic doctrine says cannot succeed. They benefit from the institutional resources allocated to counterforce capabilities (budgets, billets, technological development). Their exit is constrained: they are embedded in the nuclear enterprise, but some transition to arms control or conventional roles.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, counterforce_planners_targeting_officers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__deterrence_unthinkable, counterforce_planners_targeting_officers, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great-power total war by making it structurally unwinnable. Solves the collective action problem of mutual restraint: no great power can unilaterally defect to war-winning because the physics of escalation makes defection suicidal. The coordination is self-enforcing once the arsenal threshold is crossed.
% TRANSFER_FUNCTION: Transfers the mission-space of 'total war victory' from military establishments to the null set — no one receives it. Transfers strategic initiative from warfighting to deterrence posture maintenance. Civilian populations receive the negative good of 'no total war' without a corresponding positive transfer to any actor. Military establishments lose their traditional mission but gain deterrence missions (budget, authority, technological bureaucracy).
% ABSENT_VOICES: Populations of non-nuclear-weapon states who are held hostage to great-power deterrence logic but have no voice in its maintenance. Future generations who inherit the nuclear condition without consent. Anti-nuclear movements and disarmament advocates who are structurally excluded from nuclear planning discourse — their objection is treated as category error rather than policy disagreement.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight (nuclear weapons disappeared or the logic of mutual assured destruction was falsified), great-power total war would return to the reachable space. Military establishments would regain coherent warfighting missions. Political leadership would face the pre-1945 security dilemma. Civilian populations would lose the structural guarantee against total war. The international system would reorganize around war-winning rather than war-prevention.
% FOUNDING_PROBLEM: The founding problem was the recurrence of great-power total war (1914-1918, 1939-1945) that destroyed the European state system and killed tens of millions. Nuclear weapons emerged as a technical solution to the political problem of great-power conflict: make the cost of war so catastrophically high that no rational leadership could choose it.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (great-power total war) is attested as live by the continued existence of nuclear arsenals, the persistence of great-power rivalry (US-Russia, US-China), and the ongoing investment in nuclear modernization by all nuclear-armed states. No major strategic actor claims the problem is solved. Corroboration comes from outside the military-industrial complex: historical scholarship on the pre-nuclear era, humanitarian impact conferences, and the Treaty on the Prohibition of Nuclear Weapons all attest that the problem the constraint was built for remains live — though they dispute whether the constraint is the right solution.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, ExtMetricName, E),
    domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint's primary operation is preventing a catastrophic outcome that would harm all parties — the 'extraction' from military establishments is mission transformation, not resource transfer to a beneficiary. Suppression is very high (0.88) because the constraint operates by making certain strategic options (total war planning) structurally unavailable — not by active enforcement but by the physics of mutual destruction. Theater ratio is low (0.15) because the deterrence posture has genuine operational content (arsenals, command/control, exercises) rather than being performative. Accessibility collapse is very high (0.92) because once the logic of mutual assured destruction is understood, the alternative of 'winning a nuclear war' collapses as a coherent concept. Resistance is near-zero (0.08) because no serious strategic actor advocates for total war — resistance manifests only in the sibling readings that contest the scope of unwinnability, not the core claim.
 *
 * PERSPECTIVAL GAP:
 *   From the civilian/analytical seat, this is a mountain — a physical fact that war is unwinnable. From the military establishment seat, it feels like a snare — their core mission is extracted from them without consent. From the political leadership seat, it is a tangled rope — they must coordinate deterrence while managing military institutions that resist mission contraction. The engine computes these per-seat classifications from the structural data; the divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are full beneficiaries (d ≈ 0.0): the constraint subsidizes their survival by removing total war from the reachable space. Military establishments are near-full targets (d ≈ 0.9) for mission incoherence: their traditional warfighting role is structurally negated, though they capture new roles (deterrence operations, strategic command). Political leadership sits near symmetric (d ≈ 0.5): they bear the burden of credible threat-making but gain the benefit of war prevention. Strategic analysts are analytical observers (d = 0.5 by definition). Counterforce planners are constrained (d ≈ 0.6): their mission is narrowed but not eliminated, creating the structural tension with the countervailing_thinkable reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great-power total war) remains live — the weapons exist, the logic holds. The constraint has not outlived its function. However, the mandate has expanded: deterrence now covers limited nuclear use scenarios, extended deterrence commitments, and non-proliferation enforcement. This expansion creates the structural space where sibling readings (countervailing_thinkable, rhetorical_contraction) operate. The mountain core remains; the penumbra is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_taboo,
    'Is the unwinnability of total nuclear war a genuine natural law (physics of escalation) or a constructed normative taboo sustained by institutional practice?',
    'Counterfactual test: if nuclear arsenals were dismantled tomorrow but the institutional memory of deterrence remained, would great powers still treat total war as categorically unwinnable? Historical cases of near-use (Cuban Missile Crisis, Able Archer) where institutional choices overrode mechanical escalation logic.',
    'If constructed, the constraint is a false summit mountain — beneficiaries (civilian populations) are real but the ''natural law'' framing serves institutional interests; would trigger FSM reclassification to tangled_rope. If natural, mountain classification holds and the taboo is an epiphenomenon of physical reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_taboo, conceptual, 'Whether the unwinnability constraint is a physical necessity or an institutional achievement').

omega_variable(
    reading_kernel_relationship,
    'Does the deterrence_unthinkable reading foreclose, coexist with, or influence the countervailing_thinkable and rhetorical_contraction sibling readings of the war_winnability_post_1945 kernel?',
    'Analyze whether a single strategic framework can simultaneously hold that (a) total war is categorically unwinnable, (b) limited nuclear victory remains achievable via counterforce, and (c) winnability is discursively taboo but operationally planned. Examine doctrinal documents and planning guidance across nuclear-armed states for internal consistency.',
    'If forecloses countervailing_thinkable: the kernel has a logical fault line — states cannot coherently plan for both unwinnability and limited victory. If coexists_with: different bureaucratic factions hold different readings simultaneously. If influences: deterrence_unthinkable creates legitimacy pressure on countervailing planning without eliminating it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Structural relationship between this reading and its sibling readings of the same kernel').

omega_variable(
    military_mission_incoherence_as_extraction,
    'Does the mission incoherence experienced by military establishments constitute genuine extraction, or is it the necessary cost of a coordination function (war prevention) that benefits everyone including the military?',
    'Measure whether military budgets, force structure, and doctrinal authority have contracted or expanded since 1945. If expanded: the constraint created new missions (deterrence operations, arms control, strategic command) that replaced the lost total-war mission — no net extraction. If contracted: the constraint extracted institutional resources without substitution.',
    'If net extraction: mountain claim is falsified — the constraint has a victim paying for a beneficiary''s gain, making it at least tangled_rope. If no net extraction: mountain classification holds; military ''victimhood'' is role transformation, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_mission_incoherence_as_extraction, empirical, 'Whether military establishments are net extractees or net beneficiaries of the deterrence regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0, 0.08).
narrative_ontology:measurement(war__tr_t10, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 10, 0.1).
narrative_ontology:measurement(war__tr_t20, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 20, 0.12).
narrative_ontology:measurement(war__tr_t30, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 30, 0.14).
narrative_ontology:measurement(war__tr_t40, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 40, 0.15).
narrative_ontology:measurement(war__tr_t50, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(war__be_t10, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 10, 0.07).
narrative_ontology:measurement(war__be_t20, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(war__be_t30, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 30, 0.1).
narrative_ontology:measurement(war__be_t40, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(war__be_t50, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 50, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(war__su_t10, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(war__su_t20, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(war__su_t30, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(war__su_t40, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(war__su_t50, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__deterrence_unthinkable, 0.1).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, nuclear_arms_control_regimes).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, extended_deterrence_commitments).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, nonproliferation_regime).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, strategic_stability_dialogue_mechanisms).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the 'war_winnability_post_1945' kernel. The deterrence_unthinkable reading (this story) treats unwinnability as structural fact (mountain). The countervailing_thinkable reading treats limited victory as achievable (tangled_rope — coordination of deterrence + extraction via counterforce investment). The rhetorical_contraction reading treats unwinnability as discursive taboo (piton — atrophied coordination function maintained theatrically). All three are linked via network.affects_constraints and share the kernel_id in their provenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__deterrence_unthinkable, institutional, 0.5).
constraint_indexing:directionality_override(war_winnability_post_1945__deterrence_unthinkable, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
