% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment Civic-Right Reading: Individual Right Conditioned on Militia Participation
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint story models the civic-right reading of the Second
 *   Amendment: the individual right to keep and bear arms is structurally
 *   conditioned on participation in, or eligibility for, the organized
 *   militia. The right is not a freestanding entitlement to private firearms
 *   ownership for self-defense or recreation, but a civic entitlement tied to
 *   the republican obligation of collective defense. The arrangement
 *   coordinates universal militia service (the coordination function) with
 *   the distribution of arms and training (the transfer function), but
 *   extracts compliance from those who would exercise the right without
 *   fulfilling the service condition. Regulatory authority (state and
 *   federal) holds moderate power to define militia eligibility, training
 *   standards, and armament rules. The constraint is actively enforced
 *   through licensing, militia statutes, and judicial review; its persistence
 *   depends on maintaining the service-conditioned gate.
 *
 * KEY AGENTS:
 *   - militia_eligible_citizens: Primary beneficiary (moderate power, constrained exit) — holds the right contingent on service
 *   - state_militia_authorities: Agenda setter (institutional power, arbitrage exit) — defines militia structure, eligibility, and armament
 *   - non_militia_participating_individuals: Primary victim (moderate power, constrained exit) — seeks arms access without service condition
 *   - gun_control_advocacy_groups: Secondary victim (organized power, mobile exit) — seeks broader regulation than the civic reading permits
 *   - constitutional_originalists_civic_reading: Beneficiary observer (analytical power, analytical exit) — advances this reading in legal discourse
 *   - federal_judiciary: Observer (institutional power, analytical exit) — adjudicates the reading's scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.45).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.3).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment Civic-Right Reading: Individual Right Conditioned on Militia Participation").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, 'de9c303b-7ad2-4066-b86a-5bd93a401226').
narrative_ontology:cs_kernel_codification('de9c303b-7ad2-4066-b86a-5bd93a401226', fixed_text).
narrative_ontology:cs_authority_grounding('de9c303b-7ad2-4066-b86a-5bd93a401226', lineage).
narrative_ontology:cs_interpretation_layer_present('de9c303b-7ad2-4066-b86a-5bd93a401226').
narrative_ontology:cs_reading_relation('de9c303b-7ad2-4066-b86a-5bd93a401226', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('de9c303b-7ad2-4066-b86a-5bd93a401226', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('de9c303b-7ad2-4066-b86a-5bd93a401226', foundational, right_conditioned_on_civic_obligation).
narrative_ontology:cs_axiom_status(right_conditioned_on_civic_obligation, holdable).
narrative_ontology:cs_axiom_grounding('de9c303b-7ad2-4066-b86a-5bd93a401226', right_conditioned_on_civic_obligation, deontological).
narrative_ontology:cs_axiom('de9c303b-7ad2-4066-b86a-5bd93a401226', foundational, militia_as_constitutional_institution).
narrative_ontology:cs_axiom_status(militia_as_constitutional_institution, holdable).
narrative_ontology:cs_axiom_grounding('de9c303b-7ad2-4066-b86a-5bd93a401226', militia_as_constitutional_institution, conventional).
narrative_ontology:cs_reference_frame('de9c303b-7ad2-4066-b86a-5bd93a401226', founding_militia_republicanism).
narrative_ontology:cs_drift_state('de9c303b-7ad2-4066-b86a-5bd93a401226', post_heller_bruen_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('de9c303b-7ad2-4066-b86a-5bd93a401226', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_militia_authorities).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, constitutional_originalists_civic_reading).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, non_militia_participating_individuals).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, gun_control_advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, gun_control_advocacy_groups).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republican_virtue_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, well_regulated_militia_premise).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, collective_security_through_universal_service).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the Second Amendment right conditional on militia participation or eligibility. They receive access to arms and training through the organized militia structure. Exit requires either accepting the service condition or forgoing the constitutional right; emigration or conscientious objection are the only full exits.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    moderate, biographical, constrained, national).

% Define militia eligibility, organization, training standards, and armament rules under state and federal law. They administer the constraint and collect compliance as the price of the right. They can modify the constraint through legislation and regulation; their exit is shifting to a different constitutional reading or federal preemption.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_militia_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Seek firearms access for self-defense, recreation, or collection without militia service. The civic reading denies them the right unless they accept the condition. Their options: join a militia (costly), litigate for a different reading (uncertain), or accept exclusion. Exit is constrained by the legal gate.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, non_militia_participating_individuals, payer,
    moderate, biographical, constrained, national).

% Advocate for broader firearms regulation than the civic reading permits (e.g., universal background checks, assault weapon bans). The civic reading's individual right component blocks some regulations they seek, making them payers. But the reading's service condition and regulatory authority also enable regulations the individual-right reading would forbid, making them incidental beneficiaries. They can shift strategy, jurisdiction, or public messaging — mobile exit.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, gun_control_advocacy_groups, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, gun_control_advocacy_groups, beneficiary).

% Advance the civic-right reading in legal scholarship, judicial nominations, and public discourse. They neither collect nor pay from the constraint's operation; they contest its interpretation. Their exit is intellectual: adopting a different reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, constitutional_originalists_civic_reading, observer,
    analytical, civilizational, analytical, universal).

% Adjudicate Second Amendment challenges and define the right's scope. Their rulings determine which reading governs. They are structurally positioned to enforce or reject the civic reading's service condition. Exit is analytical: they interpret, they do not bear the right's costs or benefits directly.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates universal civic obligation (militia service) with the distribution of arms and training, ensuring the polity's collective defense capacity is maintained through citizen participation rather than solely professional forces.
% TRANSFER_FUNCTION: Moves the entitlement to keep and bear arms from the state (which holds the monopoly on legitimate force) to militia-eligible individuals, conditioned on their acceptance of the service obligation. The state retains regulatory authority over the militia's organization and standards.
% ABSENT_VOICES: Non-citizen residents, conscientious objectors, and populations historically excluded from militia service (e.g., enslaved persons, women before 20th century) would object to a right conditioned on a civic obligation from which they were excluded. They are structurally excluded from the beneficiary set and from the agenda-setting authority.
% DISAPPEARANCE_RATIONALE: If the civic reading vanished overnight, the legal gate conditioning the right on militia service would disappear. The individual-right reading would likely become the sole operative framework (as post-Heller doctrine trends), expanding the right's scope and constraining regulation. State militia authorities would lose their constitutional anchor for defining the right. The regulatory landscape, political coalitions, and judicial doctrine would reorganize substantially.
% FOUNDING_PROBLEM: The Founding generation faced the problem of securing a free state's defense without a standing army, which they viewed as a threat to liberty. The solution was a universal militia of armed citizens, making the right to arms a civic corollary of the duty to serve.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (civic republican scholars, some state attorneys general) attest the problem persists: asymmetric threats, civic disengagement, and the need for resilient civil defense make the militia ideal relevant. Critics (Heller/Bruen majority, originalist individual-right scholars, professional military historians) attest the problem is dead: the National Guard, professional military, and police have superseded the universal militia; the civic obligation no longer exists in practice. No consensus corroboration exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).
:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.45) reflects the moderate cost the constraint imposes on non-participants who must either join militia structures or forgo the right; it is not zero because the gating mechanism excludes a class of would-be holders, but it is not high because the condition is a civic duty, not a rent extraction. Suppression (0.3) is moderate: alternatives (private ownership unconditioned) are suppressed by law and precedent, but the civic reading itself permits substantial regulated access. Theater ratio (0.25) is low-moderate: the militia-conditioned framework has genuine historical and doctrinal coherence; performative invocations exist but do not dominate. Accessibility collapse (0.4) is partial: the individual-right and collective-right readings remain live alternatives in public and judicial discourse. Resistance (0.55) is significant: Heller (2008) and Bruen (2022) rejected the civic reading as the sole framework, and political mobilization against service-conditioned rights is strong.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible citizens are structural beneficiaries (d ~ 0.2): they receive the right in exchange for a civic obligation they would owe regardless. State militia authorities are agenda setters with arbitrage exit (d ~ 0.1): they administer the constraint and can shift its parameters. Non-militia-participating individuals are payers (d ~ 0.7): they bear the cost of exclusion from the right unless they accept the condition. Gun-control advocates are payers (d ~ 0.6): the civic reading blocks stricter regulation they prefer. Originalist civic-reading scholars are analytical observers (d ~ 0.5). Federal judiciary is analytical (d ~ 0.5). The service gate creates the asymmetry: the same structure that coordinates collective defense extracts from those who refuse the coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (universal militia defense) is contested: the civic reading's proponents argue the problem persists (asymmetric threats, civic virtue decay); critics argue it is dead (professional military, police, nuclear deterrence). The constraint persists because the reading serves as a doctrinal bridge: it preserves an individual right while permitting regulation the individual-right reading forbids and the collective-right reading exceeds. Mandatrophy is unresolved — the arrangement is neither fully live nor fully dead, and its persistence is contested, not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_reading_kernel_commitment,
    'Is the civic-right reading a genuine structural reading of the Second Amendment kernel, or a strategic compromise that collapses under pressure from individual-right and collective-right framings?',
    'Track judicial adoption: if a stable coalition of courts adopts the civic reading as the governing framework for a generation, it is a genuine reading; if it remains only a dissenting or academic position, it is a strategic compromise.',
    'If genuine, the constraint is a stable Tangled Rope coordinating identity and defense; if compromise, it is a collapsing Scaffold whose sunset is the individual-right reading''s dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civic_reading_kernel_commitment, conceptual, 'Whether the civic reading is a live structural commitment or a transitional position.').

omega_variable(
    militia_service_modern_equivalence,
    'What modern institutions or practices satisfy the ''militia participation'' condition — National Guard, selective service, community emergency response, or none?',
    'Legislative and judicial definition of ''militia'' in contemporary statutes and rulings; empirical study of which institutions citizens and courts treat as fulfilling the civic obligation.',
    'If the condition maps to an existing universal or near-universal institution (e.g., selective service), extraction drops toward zero (the gate is open to all). If the condition maps to a defunct or voluntary institution, extraction rises (the gate excludes most).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_service_modern_equivalence, empirical, 'Whether the service condition is a meaningful gate in the modern era.').

omega_variable(
    service_gate_extraction_mechanism,
    'Does the service condition function as a genuine coordination mechanism (solving collective defense) or as an extraction mechanism (rationing a valued right to a compliant class)?',
    'Compare regulatory outcomes under the civic reading vs. individual-right reading: if the civic reading enables regulations that reduce harm without banning ownership, coordination dominates; if it primarily excludes non-compliant individuals while permitting the same harms, extraction dominates.',
    'If coordination, the constraint is a legitimate Tangled Rope; if extraction, it trends toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_gate_extraction_mechanism, conceptual, 'Whether the service gate coordinates or extracts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__civic_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1868, second_amendment_scope__civic_right_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_scope__civic_right_reading, theater_ratio, 1934, 0.2).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__civic_right_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_scope__civic_right_reading, theater_ratio, 2010, 0.23).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_scope__civic_right_reading, theater_ratio, 2022, 0.25).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__civic_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement(seco_be_t1868, second_amendment_scope__civic_right_reading, base_extractiveness, 1868, 0.25).
narrative_ontology:measurement(seco_be_t1934, second_amendment_scope__civic_right_reading, base_extractiveness, 1934, 0.35).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__civic_right_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement(seco_be_t2010, second_amendment_scope__civic_right_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(seco_be_t2022, second_amendment_scope__civic_right_reading, base_extractiveness, 2022, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_scope__civic_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(seco_su_t1868, second_amendment_scope__civic_right_reading, suppression_requirement, 1868, 0.15).
narrative_ontology:measurement(seco_su_t1934, second_amendment_scope__civic_right_reading, suppression_requirement, 1934, 0.2).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__civic_right_reading, suppression_requirement, 2008, 0.25).
narrative_ontology:measurement(seco_su_t2010, second_amendment_scope__civic_right_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(seco_su_t2022, second_amendment_scope__civic_right_reading, suppression_requirement, 2022, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__civic_right_reading, 0.08).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% Kernel second_amendment_scope decomposes into three constraint stories: civic_right_reading (this file, Tangled Rope), individual_right_reading (Rope/Snare depending on seat), collective_right_reading (Mountain/Scaffold). The civic reading coordinates identity and collective defense; the individual reading extracts regulatory latitude; the collective reading coordinates state authority. Epsilon differs structurally: civic reading gates right on service (moderate extraction), individual reading removes gate (high extraction from regulatory authority), collective reading vests right in state (low extraction from individuals, high from state autonomy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_scope__civic_right_reading, moderate, 0.7).
constraint_indexing:directionality_override(second_amendment_scope__civic_right_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
