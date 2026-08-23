% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Non-Delegation Doctrine
 *   domain: constitutional_law/administrative_law
 *
 * SUMMARY:
 *   The formalist reading of the separation of powers holds that the
 *   Constitution establishes strict, impermeable boundaries between the
 *   branches, and that Congress cannot delegate legislative authority to
 *   administrative agencies. This reading treats the non-delegation doctrine
 *   as a fixed constitutional command, not a flexible principle. The
 *   constraint story models this reading as an operative constraint: if
 *   enforced, it would strip agencies of rulemaking power, drastically reduce
 *   regulatory capacity, and force Congress to legislate in granular detail.
 *   The claimed_type is 'rope' (the formalist reading's own framing: a pure
 *   coordination mechanism that solves the tyranny problem), but the authored
 *   metrics describe a constraint with high extractiveness and suppression —
 *   the engine will compute per-seat classifications from the structural
 *   data, and the divergence between claim and computed type is the
 *   measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.82).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.88).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Non-Delegation Doctrine").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, 'eef5be1a-6709-428f-881b-eed9a19dfff9').
narrative_ontology:cs_kernel_codification('eef5be1a-6709-428f-881b-eed9a19dfff9', fixed_text).
narrative_ontology:cs_authority_grounding('eef5be1a-6709-428f-881b-eed9a19dfff9', lineage).
narrative_ontology:cs_interpretation_layer_present('eef5be1a-6709-428f-881b-eed9a19dfff9').
narrative_ontology:cs_reading_relation('eef5be1a-6709-428f-881b-eed9a19dfff9', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('eef5be1a-6709-428f-881b-eed9a19dfff9', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('eef5be1a-6709-428f-881b-eed9a19dfff9', foundational, legislative_power_non_delegable).
narrative_ontology:cs_axiom_status(legislative_power_non_delegable, holdable).
narrative_ontology:cs_axiom_grounding('eef5be1a-6709-428f-881b-eed9a19dfff9', legislative_power_non_delegable, deontological).
narrative_ontology:cs_axiom('eef5be1a-6709-428f-881b-eed9a19dfff9', foundational, separation_of_powers_as_liberty_protection).
narrative_ontology:cs_axiom_status(separation_of_powers_as_liberty_protection, holdable).
narrative_ontology:cs_axiom_grounding('eef5be1a-6709-428f-881b-eed9a19dfff9', separation_of_powers_as_liberty_protection, deontological).
narrative_ontology:cs_reference_frame('eef5be1a-6709-428f-881b-eed9a19dfff9', originalist_non_delegation_framework).
narrative_ontology:cs_drift_state('eef5be1a-6709-428f-881b-eed9a19dfff9', contemporary_administrative_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eef5be1a-6709-428f-881b-eed9a19dfff9', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, the_people).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, congress_as_legislature).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, regulatory_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, congress).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, non_delegation_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, separation_of_powers).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, legislative_power_non_delegable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress is barred from delegating legislative authority to agencies; must legislate in detail or leave regulatory gaps. Bears the cost of lost flexibility and expertise in complex policy domains.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, congress, payer,
    institutional, generational, constrained, national).

% Agencies are deprived of rulemaking authority; their existence and function depend on delegations the formalist reading declares unconstitutional. Cannot exit the constraint without ceasing to exist.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    institutional, generational, trapped, national).

% Formalist reading claims the people gain liberty protection from tyranny; they also lose expert regulation and responsive governance. Identity-locked into the polity — cannot exit the constitutional order.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, the_people, beneficiary,
    organized, generational, identity_locked, national).

% Courts police the non-delegation boundary, striking down statutes that delegate legislative power. They enforce the constraint but are not its beneficiaries or victims.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Scholars and practitioners who argue for flexible delegation and the 'intelligible principle' standard are structurally excluded from the formalist framework; their objections are ruled out of bounds by the formalist premise.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, functionalist_scholars, excluded,
    organized, biographical, mobile, national).

% Industries that rely on expert agency regulation for stability and predictability have no voice in the formalist reading; they would object to the chaos of non-delegation but are not consulted.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulated_industries, excluded,
    powerful, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents concentration of legislative power in the executive branch by requiring Congress to make all legislative decisions itself, preserving accountability and preventing tyranny.
% TRANSFER_FUNCTION: Moves rulemaking authority from administrative agencies back to Congress, reducing regulatory capacity but preserving legislative accountability and separation of powers.
% ABSENT_VOICES: Functionalist scholars, regulated industries that rely on expert agencies, and citizens who benefit from regulation are excluded from the formalist framework; they would argue for flexible delegation but are kept out by the formalist premise that delegation is categorically unconstitutional.
% DISAPPEARANCE_RATIONALE: If the non-delegation doctrine vanished overnight, Congress would delegate broadly, agencies would regain rulemaking power, and the administrative state would operate as it has since the New Deal — the regulatory state would expand dramatically.
% FOUNDING_PROBLEM: The founding problem was the fear of legislative tyranny and the desire to prevent Congress from transferring its legislative power to the executive, which would combine legislative and executive powers in the same hands.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and some judges attest the problem is live; functionalist scholars and most of the administrative law academy attest it is dead. The corroboration is split — no consensus outside the benefiting parties (originalist movement).
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint strips agencies of their core function and forces Congress into impossibly detailed legislating; suppression is higher (0.88) because the constraint's persistence depends on courts actively striking down delegations, not on voluntary compliance. Theater ratio is moderate (0.35): the formalist reading presents itself as principled constitutionalism, but a growing share of its enforcement energy serves to disable the administrative state rather than protect liberty. Accessibility collapse is high (0.85) because once the formalist premise is accepted, delegation alternatives collapse entirely. Resistance is high (0.72) from the administrative state, functionalist scholars, and regulated industries.
 *
 * PERSPECTIVAL GAP:
 *   From the formalist seat (courts, originalist scholars), the constraint is genuine coordination (rope) preserving liberty. From the payer seats (Congress, agencies), the same structure operates as enforced extraction (tangled_rope/snare) disabling governance. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress and agencies are structural payers (d near target end): Congress loses legislative flexibility, agencies lose their raison d'être. The people are declared beneficiaries but bear diffuse costs of under-regulation (d near symmetric). Courts are agenda_setters (d near beneficiary end). Functionalist scholars and regulated industries are excluded — their exclusion is the enforcement object itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing legislative tyranny) is contested as live or dead. If dead, the constraint persists as a piton (inertial performance) or snare (extraction via originalist revival). The formalist reading denies mandatrophy; functionalists see it. The classification prevents mislabeling coordination as pure extraction by forcing the beneficiary/victim structure into the open: the people are declared beneficiaries, agencies declared victims — the engine computes whether the coordination function is genuine or cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure,
    'How does this constraint relate to the contested kernel separation_of_powers_text and its sibling readings?',
    'Structural comparison of the three readings'' beneficiary/victim sets, ε values, and drift states. The kernel_context field records the reading_id and sibling_ids; this omega makes the committer structure explicit for the engine.',
    'If the kernel framework is rejected, this constraint story loses its committer-axis location and becomes a flat constraint. The reading_relations and axioms in cs_structure would become inapplicable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure, conceptual, 'Commitment-system framing: this constraint is one reading of a contested kernel.').

omega_variable(
    natural_law_vs_constructed,
    'Is the non-delegation doctrine a genuine constitutional mountain (fixed by text and structure) or a constructed constraint that benefits identifiable agents (originalist judges, political actors)?',
    'Historical analysis of founding-era understanding vs. modern originalist construction; empirical study of who benefits from the doctrine''s enforcement.',
    'If constructed, the constraint is a false summit mountain (or tangled_rope) with beneficiaries; if genuine mountain, emerges_naturally would be true and beneficiaries would be spurious. The formalist reading claims mountain; the metrics suggest otherwise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed, conceptual, 'Natural-law vs. constructed ambiguity for the non-delegation doctrine.').

omega_variable(
    regulatory_capacity_victimhood,
    'Is ''regulatory_capacity'' a legitimate victim group, or is the victim only the agencies themselves?',
    'Empirical assessment of whether the public suffers net harm from reduced regulation (public health, safety, environment) that is not offset by liberty gains.',
    'If the public is a net victim, the constraint''s extraction is broader and the formalist reading''s beneficiary claim is undermined. If only agencies are victims, extraction is narrower and targeted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capacity_victimhood, empirical, 'Whether the public bears costs of non-delegation or only agencies do.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sop_formalist_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sop_formalist_tr_t50, separation_of_powers_text__formalist_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(sop_formalist_tr_t100, separation_of_powers_text__formalist_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(sop_formalist_tr_t150, separation_of_powers_text__formalist_reading, theater_ratio, 150, 0.4).
narrative_ontology:measurement(sop_formalist_tr_t200, separation_of_powers_text__formalist_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement(sop_formalist_tr_t235, separation_of_powers_text__formalist_reading, theater_ratio, 235, 0.35).

% Extraction over time
narrative_ontology:measurement(sop_formalist_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sop_formalist_be_t50, separation_of_powers_text__formalist_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(sop_formalist_be_t100, separation_of_powers_text__formalist_reading, base_extractiveness, 100, 0.65).
narrative_ontology:measurement(sop_formalist_be_t150, separation_of_powers_text__formalist_reading, base_extractiveness, 150, 0.35).
narrative_ontology:measurement(sop_formalist_be_t200, separation_of_powers_text__formalist_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement(sop_formalist_be_t235, separation_of_powers_text__formalist_reading, base_extractiveness, 235, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sop_formalist_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sop_formalist_su_t50, separation_of_powers_text__formalist_reading, suppression_requirement, 50, 0.3).
narrative_ontology:measurement(sop_formalist_su_t100, separation_of_powers_text__formalist_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement(sop_formalist_su_t150, separation_of_powers_text__formalist_reading, suppression_requirement, 150, 0.5).
narrative_ontology:measurement(sop_formalist_su_t200, separation_of_powers_text__formalist_reading, suppression_requirement, 200, 0.6).
narrative_ontology:measurement(sop_formalist_su_t235, separation_of_powers_text__formalist_reading, suppression_requirement, 235, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__formalist_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, administrative_state_legitimacy).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, chevron_deference).

% DUAL FORMULATION NOTE:
% Part of the separation_of_powers_text constraint family. The formalist reading (this story) has high ε and claims rope; the functionalist reading has low ε and claims rope; the unitary executive reading has high ε and claims snare/tangled_rope. They share the same constitutional text but instantiate different constraints with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__formalist_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
