% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Civic Republican Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the civic republican reading of the
 *   Second Amendment: the right to keep and bear arms protects armed
 *   citizenship as a prerequisite for republican self-governance. Unlike the
 *   individual-right reading (which treats the right as a pre-political
 *   liberty) or the collective-right reading (which treats it as a state
 *   militia power), this reading positions the citizen-militia member as a
 *   dual beneficiary—bearing both the right to arms and the civic duty to
 *   train—while constraining regulatory authority by a civic participation
 *   norm. The constraint extracts moderately (ε=0.45) through
 *   training/qualification requirements that serve as the price of militia
 *   membership. It requires active enforcement (the state must maintain
 *   militia infrastructure and adjudicate qualifications). The claimed type
 *   is tangled_rope because the arrangement simultaneously coordinates civic
 *   defense and extracts asymmetric burdens on those who cannot meet
 *   qualifications, with the state as both agenda-setter (setting standards)
 *   and constrained party (losing plenary regulatory power). The ε-invariance
 *   principle is satisfied by treating this reading as a distinct constraint
 *   from its siblings, each with its own ε and beneficiary/victim structure.
 *
 * KEY AGENTS:
 *   - citizen_militia_members: Primary beneficiaries/payers (organized/constrained) — hold the right and bear the duty of training
 *   - regulatory_authority: Agenda-setter/payer (institutional/analytical) — administers qualifications but is constrained by the civic norm
 *   - excluded_citizens: Payers (powerless/trapped) — cannot meet qualifications and are excluded from the militia/political community
 *   - individual_right_advocates: Excluded (powerful/mobile) — would object to the civic duty framing as infringing personal liberty
 *   - collective_right_advocates: Excluded (organized/constrained) — would object to the individual dimension of the right
 *   - analytical_observer: Observer (analytical/analytical) — sees the full structural field of the kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.45).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.35).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Civic Republican Reading").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '100a6509-e5c0-4766-8afb-f59dbb115ce9').
narrative_ontology:cs_kernel_codification('100a6509-e5c0-4766-8afb-f59dbb115ce9', fixed_text).
narrative_ontology:cs_authority_grounding('100a6509-e5c0-4766-8afb-f59dbb115ce9', lineage).
narrative_ontology:cs_interpretation_layer_present('100a6509-e5c0-4766-8afb-f59dbb115ce9').
narrative_ontology:cs_reading_relation('100a6509-e5c0-4766-8afb-f59dbb115ce9', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('100a6509-e5c0-4766-8afb-f59dbb115ce9', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('100a6509-e5c0-4766-8afb-f59dbb115ce9', foundational, armed_citizenship_prerequisite_for_republic).
narrative_ontology:cs_axiom_status(armed_citizenship_prerequisite_for_republic, holdable).
narrative_ontology:cs_axiom_grounding('100a6509-e5c0-4766-8afb-f59dbb115ce9', armed_citizenship_prerequisite_for_republic, deontological).
narrative_ontology:cs_axiom('100a6509-e5c0-4766-8afb-f59dbb115ce9', secondary, training_qualification_as_civic_duty).
narrative_ontology:cs_axiom_status(training_qualification_as_civic_duty, holdable).
narrative_ontology:cs_axiom_grounding('100a6509-e5c0-4766-8afb-f59dbb115ce9', training_qualification_as_civic_duty, instrumental).
narrative_ontology:cs_reference_frame('100a6509-e5c0-4766-8afb-f59dbb115ce9', founding_era_militia_constitutionalism).
narrative_ontology:cs_drift_state('100a6509-e5c0-4766-8afb-f59dbb115ce9', contemporary_post_heller_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('100a6509-e5c0-4766-8afb-f59dbb115ce9', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, excluded_citizens).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, regulatory_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, civic_republican_self_governance).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, armed_citizenship_as_civic_duty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the constitutional right to bear arms as part of their civic identity; must meet training and qualification standards to maintain militia membership; gain political standing and collective defense capability but bear the costs of time, equipment, and compliance; exit is constrained because the duty is tied to citizenship and the political community.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, payer).

% Sets and administers training/qualification standards for the militia; derives legitimacy from the civic republican norm but loses plenary regulatory power over arms; bears the cost of maintaining militia infrastructure and adjudicating qualifications; cannot exit the constraint because it is constitutional, but can interpret it within the civic participation framework.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, regulatory_authority, payer).

% Cannot meet training/qualification requirements (due to disability, poverty, conscientious objection, or legal disqualification); are excluded from the militia and the associated civic status; bear the cost of political marginalization without the offsetting benefits of militia membership; have no effective exit because the constraint defines the terms of political community.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, excluded_citizens, payer,
    powerless, biographical, trapped, national).

% Advocate for the individual-right reading; would object to the civic duty framing as an infringement on personal liberty; are structurally excluded from this reading's framework because the civic republican norm treats the right as collective and duty-bound; their preferred reading coexists as a competing constraint in the kernel family.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, individual_right_advocates, excluded,
    powerful, biographical, mobile, national).

% Advocate for the collective-right reading; would object to the individual dimension of the civic republican reading (which allows personal arms ownership tied to militia duty); are excluded because this reading does not vest the right solely in the state; their reading coexists as a competing constraint.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, collective_right_advocates, excluded,
    organized, biographical, constrained, national).

% Observes the full kernel contest; sees the structural relationships among the three readings; does not collect rents or bear costs from any single reading; provides the meta-level classification that the engine computes.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the armed citizenry as a militia for republican self-governance, ensuring the populace is capable of collective defense and civic participation without a standing army.
% TRANSFER_FUNCTION: Moves the burden of military preparedness from the state to the citizen-militia members through training and qualification requirements, while constraining state regulatory authority over arms by a civic participation norm.
% ABSENT_VOICES: Individual-right advocates (who see the right as personal liberty) and collective-right advocates (who see it as state militia power) are excluded from this reading's framework; pacifist citizens and those disqualified by training requirements are structurally excluded from the militia and the political community it constitutes.
% DISAPPEARANCE_RATIONALE: The constraint structures the relationship between armed citizenship and republican governance; its removal would eliminate the civic duty to bear arms and the corresponding limit on state power, fundamentally altering the constitutional order and the citizen-state relationship.
% FOUNDING_PROBLEM: The founding problem was how to ensure a free republic's defense without a standing army, by making armed citizenship a civic duty and a check on tyranny.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the framing era (e.g., Federalist Papers, Anti-Federalist writings, state militia acts) corroborate the militia purpose; modern legal scholars and historians debate its continued relevance — some argue the problem is dead (superseded by professional military), others that it is live (civic virtue requires participatory defense). No consensus outside the benefiting parties (civic republican theorists).
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because training/qualification requirements impose real costs (time, resources, exclusion) but are justified as the price of civic participation. Suppression is moderate (0.35) because the constraint operates by limiting state regulatory power rather than directly coercing individuals; the primary suppression is the exclusion of unqualified citizens from the militia. Theater ratio is low-moderate (0.25) because the militia function is genuinely operationalized in historical practice (musters, inspections) though modern dormancy increases performative aspects. Accessibility collapse (0.6) reflects that the civic republican framework partially closes off alternative conceptions of the right (individual liberty, state monopoly). Resistance (0.5) comes from competing readings that challenge the civic duty framing. The measurement series shows a gradual increase in extractiveness and suppression as the militia system atrophies and qualifications become more bureaucratic, while theater rises as the civic function becomes more symbolic.
 *
 * PERSPECTIVAL GAP:
 *   The citizen-militia member experiences the constraint as a rope (coordination for self-governance with shared costs). The excluded citizen experiences it as a snare (barrier to political inclusion). The regulatory authority experiences it as a mountain (a fixed limit on its power) but also as a payer (it must fund the militia infrastructure). The engine will compute these seat divergences from the structural data: the same constraint is coordination for some, extraction for others, and a fixed limit for the state.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizen-militia members are beneficiaries (they gain the right and the civic status) but also payers (they bear training costs) — directionality near symmetric (d≈0.5). Regulatory authority is an agenda-setter that sets qualifications but is constrained by the norm — directionality pulled toward target (d≈0.6) because it loses plenary regulatory power. Excluded citizens are pure payers (trapped, no benefit) — directionality near full target (d≈0.9). Individual-right and collective-right advocates are excluded from the conversation but would experience the constraint as suppression of their preferred reading — directionality not computed for excluded seats. The analytical observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (republican defense without a standing army) is contested: some argue it is dead (modern military makes militia obsolete), others that it is live (civic participation remains essential to republican legitimacy). The constraint persists despite the contested status, suggesting mandatrophy is unresolved. The classification as tangled_rope prevents mislabeling: it acknowledges the genuine coordination function (militia as civic bond) while flagging the asymmetric extraction (qualifications as exclusionary barriers). If the founding problem is dead, the constraint drifts toward piton (theatrical maintenance of a defunct militia system); if live, it remains a functional tangled_rope. The corridation of moderate ε with active enforcement and dual beneficiaries/payers captures this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the civic republican reading a genuine coordination mechanism for republican self-governance, or a constructed interpretation that serves to legitimize state control over arms?',
    'Comparative historical analysis of founding-era militia statutes vs. modern judicial interpretations; empirical study of whether training requirements correlate with civic participation outcomes.',
    'If the reading is a cover for state control, the constraint reclassifies toward snare; if genuine coordination, it remains tangled_rope with moderate extraction as the price of civic duty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the civic republican framing is structurally descriptive or ideological cover.').

omega_variable(
    training_extraction_boundary,
    'Do the training/qualification requirements function as genuine civic preparation or as barriers that exclude disfavored groups from the political community?',
    'Disparate impact analysis of historical and contemporary militia qualification standards; examination of whether alternatives (e.g., universal service) achieve the coordination function with less exclusion.',
    'If barriers are exclusionary, extraction is higher and victims include excluded_citizens more severely; if preparation is genuine and accessible, extraction is lower and the constraint leans toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(training_extraction_boundary, empirical, 'Whether moderate ε on training masks asymmetric exclusion.').

omega_variable(
    committer_structure,
    'How does this reading''s classification change if the kernel (Second Amendment) is framed as a fixed text vs. an evolving practice?',
    'Trace the drift_state of each sibling reading; if the kernel''s codification shifts from fixed_text to distributed, the civic republican reading''s authority_grounding (lineage) loses force.',
    'A shift to distributed codification would weaken this reading''s claim to unique legitimacy and strengthen coexistence_with relations; the axiom_overriding direction would activate for empirically_contingent axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure, conceptual, 'Commitment-system framing under-determination for the Second Amendment kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa2_civic_rep_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sa2_civic_rep_tr_t0, observed).
narrative_ontology:measurement(sa2_civic_rep_tr_t6, second_amendment_arms_right__civic_republican_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(sa2_civic_rep_tr_t6, observed).
narrative_ontology:measurement(sa2_civic_rep_tr_t12, second_amendment_arms_right__civic_republican_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(sa2_civic_rep_tr_t12, observed).
narrative_ontology:measurement(sa2_civic_rep_tr_t18, second_amendment_arms_right__civic_republican_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement_basis(sa2_civic_rep_tr_t18, observed).
narrative_ontology:measurement(sa2_civic_rep_tr_t24, second_amendment_arms_right__civic_republican_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement_basis(sa2_civic_rep_tr_t24, observed).
narrative_ontology:measurement(sa2_civic_rep_tr_t30, second_amendment_arms_right__civic_republican_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(sa2_civic_rep_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(sa2_civic_rep_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(sa2_civic_rep_be_t0, observed).
narrative_ontology:measurement(sa2_civic_rep_be_t6, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement_basis(sa2_civic_rep_be_t6, observed).
narrative_ontology:measurement(sa2_civic_rep_be_t12, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement_basis(sa2_civic_rep_be_t12, observed).
narrative_ontology:measurement(sa2_civic_rep_be_t18, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement_basis(sa2_civic_rep_be_t18, observed).
narrative_ontology:measurement(sa2_civic_rep_be_t24, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement_basis(sa2_civic_rep_be_t24, observed).
narrative_ontology:measurement(sa2_civic_rep_be_t30, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement_basis(sa2_civic_rep_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(sa2_civic_rep_su_t0, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(sa2_civic_rep_su_t0, observed).
narrative_ontology:measurement(sa2_civic_rep_su_t6, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 6, 0.28).
narrative_ontology:measurement_basis(sa2_civic_rep_su_t6, observed).
narrative_ontology:measurement(sa2_civic_rep_su_t12, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement_basis(sa2_civic_rep_su_t12, observed).
narrative_ontology:measurement(sa2_civic_rep_su_t18, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 18, 0.32).
narrative_ontology:measurement_basis(sa2_civic_rep_su_t18, observed).
narrative_ontology:measurement(sa2_civic_rep_su_t24, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement_basis(sa2_civic_rep_su_t24, observed).
narrative_ontology:measurement(sa2_civic_rep_su_t30, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement_basis(sa2_civic_rep_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__civic_republican_reading, 0.08).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the Second Amendment kernel into a civic republican constraint that coordinates armed citizenship as a prerequisite for republican self-governance, distinct from individual liberty (individual_right_reading) and state militia authority (collective_right_reading). The ε values differ because this reading imposes moderate extraction via training/qualification requirements, whereas the individual reading claims near-zero extraction (natural right) and the collective reading claims state authority with minimal individual extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__civic_republican_reading, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
