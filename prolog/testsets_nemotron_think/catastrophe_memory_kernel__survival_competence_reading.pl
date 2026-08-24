% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Ritual as Persecution-Survival Training
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A persecuted community maintains ritual practices that encode specific
 *   survival competencies: resource concealment, escape routing,
 *   trust-discrimination under duress, and collective decision-making under
 *   threat. The ritual is presented as sacred mourning and remembrance, but
 *   its structural function rehearses catastrophe-response patterns.
 *   Community elders enforce participation; those facing assimilation
 *   pressure (intermarriage, economic integration, cultural dilution) bear
 *   the cost of boundary maintenance — exclusion from majority opportunities,
 *   social friction, identity strain. The constraint is a tangled rope:
 *   genuine coordination (survival training) coexists with asymmetric
 *   extraction (assimilation costs). Over 100 time units (generations),
 *   extraction and enforcement have gradually increased as persecution memory
 *   recedes but ritual strictness persists.
 *
 * KEY AGENTS:
 *   - community_elders: agenda_setter (institutional/biographical/constrained) — maintain and enforce ritual practice, justify as sacred duty
 *   - threatened_community_members: beneficiary (moderate/biographical/constrained) — receive survival training through ritual rehearsal
 *   - assimilation_pressured_members: payer (moderate/biographical/identity_locked) — bear boundary-maintenance costs, excluded from majority integration
 *   - external_scholars: observer (analytical/civilizational/analytical) — analyze ritual as collective memory system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Ritual as Persecution-Survival Training").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, 'b1170395-a290-447e-9430-62453c9ab1df').
narrative_ontology:cs_kernel_codification('b1170395-a290-447e-9430-62453c9ab1df', distributed).
narrative_ontology:cs_authority_grounding('b1170395-a290-447e-9430-62453c9ab1df', practice).
narrative_ontology:cs_interpretation_layer_present('b1170395-a290-447e-9430-62453c9ab1df').
narrative_ontology:cs_reading_relation('b1170395-a290-447e-9430-62453c9ab1df', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1170395-a290-447e-9430-62453c9ab1df', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1170395-a290-447e-9430-62453c9ab1df', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('b1170395-a290-447e-9430-62453c9ab1df', foundational, ritual_rehearsal_preserves_operational_competence).
narrative_ontology:cs_axiom_status(ritual_rehearsal_preserves_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('b1170395-a290-447e-9430-62453c9ab1df', ritual_rehearsal_preserves_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('b1170395-a290-447e-9430-62453c9ab1df', secondary, boundary_costs_are_necessary_for_resilience).
narrative_ontology:cs_axiom_status(boundary_costs_are_necessary_for_resilience, holdable).
narrative_ontology:cs_axiom_grounding('b1170395-a290-447e-9430-62453c9ab1df', boundary_costs_are_necessary_for_resilience, instrumental).
narrative_ontology:cs_reference_frame('b1170395-a290-447e-9430-62453c9ab1df', ancestral_catastrophe_survival_practice).
narrative_ontology:cs_drift_state('b1170395-a290-447e-9430-62453c9ab1df', contemporary_liberal_democracy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b1170395-a290-447e-9430-62453c9ab1df', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, threatened_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, community_resilience).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilation_pressured_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, boundary_maintenance_bearers).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, ritual_rehearsal_preserves_operational_competence).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, collective_memory_enables_adaptive_response).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and enforce the ritual calendar, determine participation requirements, interpret the ritual's survival lessons for current conditions. Their authority derives from lineage transmission of the practice. Exit would mean abdicating communal leadership and severing the transmission chain they embody.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, community_elders, agenda_setter,
    institutional, generational, constrained, local).

% Participate in rituals that rehearse concealment, escape, and collective decision-making under duress. They gain practical competencies that have historically improved survival during persecution events. Their exit options are constrained by kinship, language, and the community's mutual-aid network which the ritual sustains.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, threatened_community_members, beneficiary,
    moderate, biographical, constrained, local).

% Bear the costs of boundary maintenance: exclusion from majority economic and social opportunities, friction with non-community institutions (schools, employers), identity strain from maintaining distinct practices. They face persecution less directly than previous generations but pay the ritual's enforcement costs most acutely. Exit requires severing identity fused with the community — not merely changing belief but leaving family, marriage pool, and support system.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilation_pressured_members, payer,
    moderate, biographical, identity_locked, local).

% Study the ritual as a collective memory system, a coordination mechanism, and a case of identity-coordination with extraction. They analyze but do not participate; their exit is unconstrained. Their frame shapes external perception of whether the constraint is coordination or extraction.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, external_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits specific persecution-survival competencies across generations: resource concealment strategies, escape route knowledge, trust discrimination under duress, collective decision-making when state protection fails. The ritual rehearses these as sacred practice so they are available when needed.
% TRANSFER_FUNCTION: Moves compliance costs (time, conformity, foregone assimilation opportunities) from assimilation-pressured members to the community's shared survival competence. The elders administer the transfer; the threatened members receive the competence dividend.
% ABSENT_VOICES: Descendants who fully assimilated and left no testimony; neighboring communities that abandoned similar rituals and did not survive persecution (silent on whether the ritual was causal); state authorities who persecuted the community and whose archives frame the ritual as seditious rather than adaptive.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community would lose its primary vehicle for transmitting persecution-response competencies. During the next persecution event, survival rates would likely drop. The mutual-aid network sustained by shared practice would fray. Assimilation-pressured members would gain immediate relief from boundary costs. The community's institutional coherence would reorganize around whatever replaces the ritual — likely a weaker, less transferable form of collective memory.
% FOUNDING_PROBLEM: Recurrent persecution events (pogroms, expulsions, forced conversions) that threatened community survival. The ritual was built to encode lessons from each catastrophe so the next generation would not have to relearn survival from scratch.
% FOUNDING_PROBLEM_CORROBORATION: Community elders attest the founding problem is live, citing rising antisemitism/persecution metrics globally. Assimilation-pressured members and external scholars (historians, sociologists of religion) attest the founding problem is substantially diminished in liberal democracies with legal protections, and the ritual now primarily maintains boundaries. No neutral arbiter exists; the status is structurally contested across seats.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.45) because the ritual does transmit genuine survival competence — communities that maintained it survived persecutions at higher rates — but the cost falls disproportionately on those who could otherwise assimilate. Suppression (0.38) reflects social enforcement of participation, not physical coercion. Theater ratio is low (0.22) because the survival-training function remains operationally relevant during active persecution periods. Accessibility collapse (0.52) is moderate: alternative survival strategies exist (secular preparation, state protection) but are less reliable for this community. Resistance (0.41) comes from assimilation-pressured members who experience the constraint as extraction without proportional benefit.
 *
 * PERSPECTIVAL GAP:
 *   From the elder/agenda-setter seat, the constraint is a rope: pure coordination for survival. From the assimilation-pressured payer seat, it is a snare: extraction via boundary enforcement. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) reflects the analyst's judgment that both functions are structurally real and neither reduces to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Community elders (agenda_setter) sit near beneficiary end (d ~0.2): they control the ritual, gain authority from it, and face minimal exit pressure. Threatened members (beneficiary) sit near symmetric (d ~0.5): they gain survival competence but must invest time and conformity. Assimilation-pressured members (payer) sit near target end (d ~0.8): they pay boundary costs without proportional survival benefit (they are the ones least likely to face persecution). The identity_locked exit for payers reflects that leaving the community means severing kinship, identity, and support networks — not merely changing beliefs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (persecution survival) is contested: elders claim it remains live; assimilation-pressured members and external scholars argue it is dead or diminished. The constraint persists because the ritual has become the primary vehicle for community coherence — removing it would collapse the coordination structure even if the original survival function has atrophied. This is not mandatrophy (the arrangement still solves a live problem for some seats) but a seat-divergent persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_training_vs_boundary_maintenance,
    'Does the ritual''s persecution-survival function genuinely transmit operational competence, or is the survival-training narrative a cover for boundary maintenance that extracts compliance from assimilation-pressured members?',
    'Comparative analysis of communities that maintained the ritual vs. those that abandoned it: measure actual survival outcomes during persecution events and correlate with ritual adherence intensity. If survival competence correlates with ritual practice independent of boundary strictness, the training function is genuine.',
    'If genuine training, the constraint is a coordination mechanism with moderate extraction (tangled_rope). If boundary maintenance is primary, extraction is higher and the constraint shifts toward snare for assimilation-pressured members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_training_vs_boundary_maintenance, empirical, 'Whether the adaptive capacity transmission is functionally real or narratively constructed.').

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is the survival_competence_reading of the catastrophe_memory_kernel. The sibling readings (boundary_maintenance_reading, symbol_continuity_reading, trauma_encoding_reading) disagree on which structural element is primary. Where exactly is the disagreement located?',
    'Trace each reading''s beneficiary/victim structure: survival_competence names community resilience as beneficiary and assimilation pressure as victim; boundary_maintenance names group coherence as beneficiary and boundary-crossers as victim; symbol_continuity names identity continuity as beneficiary and cultural loss as victim; trauma_encoding names warning-system function as beneficiary and forgetting as victim. The structural delta is in who pays and who benefits.',
    'Each reading instantiates a different constraint with different ε, different stakeholders, different classification. The kernel is the contested commitment; the readings are distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Committee structure: this reading instantiates one constraint from a contested kernel; sibling readings instantiate other constraints.').

omega_variable(
    intergenerational_transmission_fidelity,
    'How faithfully does the ritual transmit specific persecution-response competencies across generations versus preserving only the symbolic form?',
    'Ethnographic measurement of competence transfer: test whether ritual participants demonstrate better crisis decision-making, resource-hiding strategies, or escape-route knowledge than non-participants from the same community.',
    'High fidelity supports the coordination function (rope/tangled_rope). Low fidelity with high enforcement suggests extraction dominates (snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_transmission_fidelity, empirical, 'Whether the ritual''s adaptive content survives transmission or degrades into performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_memory_survival_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(catastrophe_memory_survival_tr_t20, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(catastrophe_memory_survival_tr_t40, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(catastrophe_memory_survival_tr_t60, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(catastrophe_memory_survival_tr_t80, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement(catastrophe_memory_survival_tr_t100, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(catastrophe_memory_survival_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(catastrophe_memory_survival_be_t20, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(catastrophe_memory_survival_be_t40, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(catastrophe_memory_survival_be_t60, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(catastrophe_memory_survival_be_t80, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(catastrophe_memory_survival_be_t100, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_memory_survival_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(catastrophe_memory_survival_su_t20, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement(catastrophe_memory_survival_su_t40, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(catastrophe_memory_survival_su_t60, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 60, 0.36).
narrative_ontology:measurement(catastrophe_memory_survival_su_t80, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(catastrophe_memory_survival_su_t100, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__survival_competence_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel decomposes into four constraint stories, each a distinct reading with its own ε and stakeholder structure. This reading (survival_competence) and boundary_maintenance_reading share the same ritual practice but differ on who benefits and who pays. They coexist as live positions held by different factions within the community and scholarly discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
