% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total War Reachability Boundary (Contingent Reachability Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This reading of the total war reachability kernel holds that the boundary
 *   of what constitutes 'reachable' total war is not a fixed feature of
 *   nuclear physics or strategic logic, but a contingent artifact of the
 *   current technological equilibrium. The post-1945 contraction of total war
 *   from the feasible set — what the contraction_reading treats as a mountain
 *   and the dropping_reading treats as a rope — is here interpreted as a
 *   scaffold: a temporary coordination structure maintained by mutual
 *   vulnerability that will dissolve when technological change (hypersonics,
 *   AI targeting, cyber, directed energy, novel delivery systems) restores
 *   the feasibility of disarming first strikes or limited nuclear use. The
 *   current low probability of total war is a piton — an atrophied capability
 *   maintained by institutional inertia and performative deterrence rituals —
 *   not a permanent achievement. Beneficiaries are states investing in
 *   technologies that destabilize mutual vulnerability; victims are the
 *   global populations who would bear the consequences if the scaffold
 *   collapses without a successor coordination mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.45).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.75).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total War Reachability Boundary (Contingent Reachability Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3').
narrative_ontology:cs_kernel_codification('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3', fixed_text).
narrative_ontology:cs_authority_grounding('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3', lineage).
narrative_ontology:cs_interpretation_layer_present('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3').
narrative_ontology:cs_reading_relation('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3', total_war_reachability_boundary__contraction_reading, influences).
narrative_ontology:cs_reading_relation('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3', foundational, reachability_is_technology_contingent).
narrative_ontology:cs_axiom_status(reachability_is_technology_contingent, holdable).
narrative_ontology:cs_axiom_grounding('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3', reachability_is_technology_contingent, empirically_contingent).
narrative_ontology:cs_axiom('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3', foundational, current_contraction_is_atrophied_not_eliminated).
narrative_ontology:cs_axiom_status(current_contraction_is_atrophied_not_eliminated, holdable).
narrative_ontology:cs_axiom_grounding('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3', current_contraction_is_atrophied_not_eliminated, empirically_contingent).
narrative_ontology:cs_reference_frame('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3', mutual_vulnerability_equilibrium).
narrative_ontology:cs_drift_state('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3', emerging_tech_disruption_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9fe2b8a9-d1f2-4496-ba7b-6e99af6269b3', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_destabilizing_technologies).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, global_populations_under_deterrence_failure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contingent_reachability_reading, deterrence_is_technology_contingent).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contingent_reachability_reading, strategic_stability_is_not_permanent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals and deterrence postures; set the rules of strategic stability; bear the financial and political costs of arsenal modernization and arms control compliance. Exit from the deterrence framework would require unilateral disarmament or regime change, both politically constrained.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, nuclear_weapon_states, payer).

% Invest in hypersonics, AI-enabled targeting, cyber capabilities, and novel delivery systems that could undermine mutual vulnerability. Benefit from technological churn that makes the current deterrence equilibrium obsolete, opening strategic options previously foreclosed. Can pivot investments across technology domains.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_destabilizing_technologies, beneficiary,
    powerful, biographical, mobile, global).

% Bear existential risk of nuclear conflict without possessing deterrent capability. Subject to extended deterrence commitments that may not be credible. Limited exit: can pursue own nuclear programs (constrained by NPT, security guarantees, technical capacity) or advocate for disarmament regimes.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, regional).

% Ultimate victims if deterrence fails — would bear the catastrophic humanitarian consequences of total war. No meaningful exit from the constraint; cannot opt out of being target populations. Their situation is structurally fixed by geography and the physics of nuclear weapons.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, global_populations_under_deterrence_failure, payer,
    powerless, biographical, trapped, global).

% Monitor, verify, and negotiate the technical and legal architecture of strategic stability (NPT, New START, CTBT, etc.). Administer the constraint's enforcement mechanisms. Their authority depends on the constraint's perceived legitimacy; technological change that outpaces treaty frameworks erodes their relevance.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, arms_control_institutions, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, arms_control_institutions, observer).

% Produce the knowledge base that informs all other seats' understanding of the constraint. Compete across theoretical frameworks (contingent reachability, contraction, dropping). Their analytical output shapes whether the constraint is seen as scaffold, mountain, or rope — which in turn shapes policy.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates mutual vulnerability among nuclear-armed states to prevent total war: each state's assured second-strike capability makes the cost of aggression exceed any conceivable gain, creating a stable equilibrium without central enforcement.
% TRANSFER_FUNCTION: Transfers existential risk from nuclear-armed states' decision calculi to global populations (who bear the consequences of failure); transfers strategic initiative to states developing destabilizing technologies (who gain leverage by threatening the equilibrium); transfers verification burden to arms control institutions.
% ABSENT_VOICES: Future generations who would inherit the consequences of deterrence failure or technological surprise; populations in non-nuclear states excluded from deterrence decision-making but subject to its effects; civil society movements for disarmament structurally excluded from nuclear planning circles.
% DISAPPEARANCE_RATIONALE: If the reachability constraint vanished — i.e., if mutual vulnerability ceased to structure great-power relations — the strategic landscape would fundamentally reorganize: states would either proliferate rapidly to restore deterrence, or a hegemon would exploit the vulnerability window, or a new coordination mechanism (e.g., AI-enabled strategic stability) would need to be constructed from scratch. The current order of major-power non-war depends on this constraint.
% FOUNDING_PROBLEM: Preventing great-power total war after WWII by making the cost of aggression unacceptably high through mutual nuclear vulnerability, replacing the failed collective security of the League era with a self-enforcing strategic equilibrium.
% FOUNDING_PROBLEM_CORROBORATION: Cold War historical record (no direct great-power war) corroborated by both deterrence proponents and critics; arms control community attests the founding problem has transformed — mutual vulnerability persists but its technological basis is eroding; strategic studies literature documents the shift from 'deterrence works' to 'deterrence works until technology changes it.'
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).
:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects that the constraint transfers existential risk to populations who have no say in its maintenance, while nuclear states collect the strategic benefits of great-power peace. Suppression (0.75) is high because the constraint actively suppresses alternatives (disarmament, limited war doctrines, strategic defense) through the threat of catastrophic retaliation. Theater ratio (0.40) captures the growing gap between deterrence rhetoric and the technical reality that mutual vulnerability is eroding — modernization programs, posture reviews, and signaling rituals increasingly perform a stability that the technology no longer guarantees. Accessibility collapse (0.55) is moderate: alternatives exist conceptually (disarmament, missile defense, new arms control) but are structurally blocked by the constraint's own logic. Resistance (0.30) is low because few institutional actors benefit from openly challenging the framework; the constraint's beneficiaries (destabilizing-technology investors) work within it to undermine it.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon state seat, the constraint appears as a successful coordination mechanism (rope-like) that has prevented great-power war for 80 years. From the global population seat, it appears as a snare: a structure that extracts existential risk from the powerless to subsidize the strategic stability of the powerful. From the destabilizing-technology investor seat, it appears as a scaffold with an approaching sunset: a temporary equilibrium they are actively accelerating the collapse of. The engine computes these divergent seat types from the structural data; the authored claim (scaffold) reflects the analytical observer's structural reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states sit near the beneficiary end (d ~ 0.2): they set the agenda, collect the peace dividend, and control the enforcement machinery. States investing in destabilizing technologies are beneficiaries (d ~ 0.15): they gain strategic leverage from the constraint's technological obsolescence. Non-nuclear states are constrained payers (d ~ 0.6): bear risk without control, limited exit. Global populations are trapped payers (d ~ 0.95): zero exit, maximal extraction if failure occurs. Arms control institutions are agenda-setters with analytical exit (d ~ 0.3): they administer the constraint but can observe its erosion. Strategic analysts are pure observers (d = 0.5): symmetric analytical position.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (preventing great-power total war) is contested: deterrence proponents argue it remains live and the constraint is still solving it; critics argue the problem has transformed (great-power war is now prevented by economic interdependence, norms, and the constraint itself creates new risks). The scaffold classification captures this: the constraint was built for a specific technological era and its justification is the transition to whatever comes next, not the steady state. The sunset clause is implicit in technological change — the constraint carries its own obsolescence mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct structural entity from its sibling readings, or a measurement variant of the same constraint?',
    'Test ε-invariance: if evaluating the constraint via different observables (probability of total war, feasibility of disarming first strike, stability of mutual vulnerability) yields different ε values, the readings are distinct constraints. Author separate constraint stories for each ε-invariant claim.',
    'If readings are distinct constraints, each gets its own classification and the kernel is a family linked by network.affects_constraints. If they are measurement variants, the framework''s ε-invariance principle is violated and the kernel concept needs revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings of the total_war_reachability_boundary kernel are structurally distinct constraints with independent ε values.').

omega_variable(
    technology_dependence_of_reachability,
    'Is total war reachability fundamentally technology-contingent, or does the nuclear revolution represent a permanent structural break?',
    'Track whether emerging technologies (hypersonic glide vehicles, AI-enabled ISR, cyber preemption, directed energy, novel delivery systems) demonstrably restore first-strike feasibility or limited nuclear use options. A sustained trend of vulnerability restoration across multiple technology domains would confirm technology-dependence.',
    'If technology-contingent, the scaffold classification holds and the sunset is real. If permanent structural break, the contraction_reading''s mountain classification would be structurally correct and this reading would be a misreading of a mountain as a scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_dependence_of_reachability, empirical, 'Whether the current technological equilibrium permanently eliminated total war or merely suppressed it contingent on mutual vulnerability.').

omega_variable(
    beneficiary_structure_of_destabilization,
    'Do states investing in destabilizing technologies structurally benefit from the constraint''s erosion, or do they merely accelerate a collapse that harms all parties?',
    'Analyze whether any state gains net strategic advantage from a post-mutual-vulnerability world. If first-strike capability becomes achievable, does the possessor gain coercive leverage, or does multi-polar instability create a security dilemma that leaves all worse off?',
    'If destabilizing-tech investors are net beneficiaries, the constraint is a tangled_rope (coordination + asymmetric extraction). If all parties lose, it is a piton (atrophied coordination with no concentrated beneficiary). The scaffold classification assumes the former.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_of_destabilization, conceptual, 'Whether the beneficiary declaration (states_investing_destabilizing_technologies) captures a real structural extraction dynamic or a projected one.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s suppression structural (existential threat architecture) or internalized (deterrence theory as cognitive framework that makes alternatives unthinkable)?',
    'Post-Cold War suppression trajectory: if suppression persists after the structural threat of Soviet-level adversary is removed, reclassify as partially internalized. Compare deterrence rhetoric in unipolar vs. multipolar eras.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint survives even when its enforcement machinery atrophies, because the target populations (elites, publics) carry the suppression cognitively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in nuclear deterrence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1945, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_crr_tr_t1945, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(twrb_crr_tr_t1962, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement(twrb_crr_tr_t1985, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(twrb_crr_tr_t2000, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(twrb_crr_tr_t2015, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(twrb_crr_tr_t2025, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement(twrb_crr_tr_t2035, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2035, 0.45).

% Extraction over time
narrative_ontology:measurement(twrb_crr_be_t1945, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(twrb_crr_be_t1962, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement(twrb_crr_be_t1985, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(twrb_crr_be_t2000, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(twrb_crr_be_t2015, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(twrb_crr_be_t2025, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2025, 0.45).
narrative_ontology:measurement(twrb_crr_be_t2035, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2035, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(twrb_crr_su_t1945, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(twrb_crr_su_t1962, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1962, 0.8).
narrative_ontology:measurement(twrb_crr_su_t1985, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(twrb_crr_su_t2000, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(twrb_crr_su_t2015, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(twrb_crr_su_t2025, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2025, 0.75).
narrative_ontology:measurement(twrb_crr_su_t2035, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2035, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contingent_reachability_reading, 0.12).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, nuclear_deterrence_coordination).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, strategic_stability_arms_control).

% DUAL FORMULATION NOTE:
% This reading decomposes the total_war_reachability_boundary kernel by treating technological contingency as the defining structural feature. The contraction_reading treats the same historical facts as a permanent structural break (mountain). The dropping_reading treats them as a probabilistic shift within a persistent coordination equilibrium (rope). All three share the referent (post-1945 great-power non-war) but author different ε values and different beneficiary/victim structures, making them distinct constraints per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, institutional, 0.25).
constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, powerful, 0.15).
constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
