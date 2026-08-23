% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta Due Process Constraint (Living Constitutionalism Reading)
 *   domain: constitutional_legal_political
 *
 * SUMMARY:
 *   Magna Carta (1215) is read under living constitutionalism as establishing
 *   an inherited kernel of due process and lawful restraint that binds all
 *   subsequent rulers not by static textual literalism but through
 *   evolutionary juridical interpretation. The constraint coordinates the
 *   relationship between sovereign power and legal subjecthood by requiring
 *   lawful judgment before deprivation of liberty or property. Under this
 *   reading, the charter is not a defunct feudal compact nor a statute
 *   revocable by Parliament, but a perpetually binding common-law principle
 *   that gains new content through precedent while maintaining its structural
 *   function. The royal prerogative and executive discretion are the payers;
 *   subjects and juridical institutions are the beneficiaries. The reading
 *   competes with feudal obsolescence (historical) and parliamentary
 *   sovereignty (statute-based) readings of the same kernel.
 *
 * KEY AGENTS:
 *   - subjects_under_rule: Primary beneficiary (organized/generational) â protected by due process shield against arbitrary executive action
 *   - juridical_institutions: Agenda-setter and secondary beneficiary (institutional/generational) â interpret and enforce evolutionary precedent, gaining institutional authority from continuity
 *   - crown_executive: Primary target (powerful/constrained) â bears the loss of arbitrary discretion and must proceed through lawful judgment
 *   - legal_scholars: Analytical observer (analytical) â trace evolutionary interpretation and debate historical-to-modern continuity
 *   - absolutist_jurists: Excluded voice (moderate/analytical) â would argue sovereignty cannot be self-bound by predecessor documents but are marginalized in constitutional jurisprudence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.4).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.42).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta Due Process Constraint (Living Constitutionalism Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_legal_political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, 'eda917a3-06c1-43df-8da5-b664cf4c70ee').
narrative_ontology:cs_kernel_codification('eda917a3-06c1-43df-8da5-b664cf4c70ee', fixed_text).
narrative_ontology:cs_authority_grounding('eda917a3-06c1-43df-8da5-b664cf4c70ee', lineage).
narrative_ontology:cs_interpretation_layer_present('eda917a3-06c1-43df-8da5-b664cf4c70ee').
narrative_ontology:cs_reading_relation('eda917a3-06c1-43df-8da5-b664cf4c70ee', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_reading_relation('eda917a3-06c1-43df-8da5-b664cf4c70ee', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('eda917a3-06c1-43df-8da5-b664cf4c70ee', foundational, inherited_due_process_binds_successors).
narrative_ontology:cs_axiom_status(inherited_due_process_binds_successors, holdable).
narrative_ontology:cs_axiom_grounding('eda917a3-06c1-43df-8da5-b664cf4c70ee', inherited_due_process_binds_successors, conventional).
narrative_ontology:cs_axiom('eda917a3-06c1-43df-8da5-b664cf4c70ee', foundational, evolutionary_interpretation_preserves_kernel).
narrative_ontology:cs_axiom_status(evolutionary_interpretation_preserves_kernel, holdable).
narrative_ontology:cs_axiom_grounding('eda917a3-06c1-43df-8da5-b664cf4c70ee', evolutionary_interpretation_preserves_kernel, conventional).
narrative_ontology:cs_reference_frame('eda917a3-06c1-43df-8da5-b664cf4c70ee', inherited_due_process_principle).
narrative_ontology:cs_drift_state('eda917a3-06c1-43df-8da5-b664cf4c70ee', contemporary_constitutional_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('eda917a3-06c1-43df-8da5-b664cf4c70ee', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_under_rule).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, juridical_institutions).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, crown_executive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive procedural protections against arbitrary imprisonment and seizure of property; the constraint operates as a shield by requiring lawful judgment of peers or the law of the land before deprivation. Their security and liberty depend on the continuity of this inherited restraint across generations.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_under_rule, beneficiary,
    organized, generational, constrained, national).

% Inherit the authority to interpret and apply the charter's principles through evolutionary precedent; they define what lawful judgment and the law of the land mean in contemporary contexts. They gain institutional legitimacy from the constraint's continuity but do not extract material rents from it.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, juridical_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, juridical_institutions, beneficiary).

% Exercises governance power but is legally barred from arbitrary imprisonment, seizure, or rule by personal will; must proceed through established legal process and seek judgment before depriving subjects of rights. Bears the loss of discretionary authority that unfettered sovereignty would enjoy.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, crown_executive, payer,
    powerful, biographical, constrained, national).

% Analyze the historical continuity and doctrinal evolution of the charter; they debate whether modern due process is genuinely inherited from 1215 or constructed retrospectively, providing the reflexive account of the constraint's authority.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% Would argue that sovereignty cannot be self-bound by predecessor documents and that emergency prerogative transcends inherited due process, but their voice is excluded from the constitutional jurisprudence that treats the charter as perpetually operative.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, absolutist_jurists, excluded,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable, inherited framework of lawful restraint that coordinates the relationship between rulers and ruled across generations by specifying that no person may be imprisoned or stripped of rights except by lawful judgment of peers or the law of the land, reducing bargaining uncertainty and violence in sovereign-subject disputes.
% TRANSFER_FUNCTION: Transfers authority from arbitrary royal and executive discretion to juridical process and peer judgment; transfers security from sovereign caprice to subjects who gain procedural protections against deprivation.
% ABSENT_VOICES: Absolutist jurists who deny that sovereignty can be self-bound by predecessor documents, and feudal historians who treat the charter as a time-bound compact rather than a perpetually binding principle, are excluded from constitutional jurisprudence; they would argue for unfettered executive discretion or historical obsolescence but are not in the room where the constraint's authority is affirmed.
% DISAPPEARANCE_RATIONALE: If the inherited due process constraint vanished overnight, modern constitutional protections against arbitrary imprisonment and seizure would lose their historical and jurisprudential anchoring; courts would need alternative grounds for procedural rights, and the legitimacy structure binding executive power to law would require fundamental reconstruction rather than simple continuation.
% FOUNDING_PROBLEM: How to secure subjects against arbitrary imprisonment, seizure of property, and rule by royal will without establishing a competing sovereign power; how to make the king accountable to law rather than merely to feudal contract.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the immediate juridical beneficiary tradition attest that the original 1215 charter was a feudal compact addressing specific baronial grievances, not a universal constitutional principle. However, international human rights jurists and legal philosophers outside the Anglo-American common-law beneficiary set corroborate that the underlying problem of arbitrary executive overreach remains live in modern administrative and emergency states, supporting the evolved-problem reading while remaining neutral on the charter's specific trans-historical authority.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.4, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.40 at interval end) because the constraint primarily coordinates by providing legal certainty and procedural regularity; the cost to sovereign power is a loss of arbitrary discretion rather than a material rent transfer. Suppression is moderate-low (0.42) because the constraint suppresses executive claims of emergency or prerogative power primarily through institutional habit and judicial expectation rather than raw coercion. Theater ratio is low (0.22): while ceremonial invocations exist, the bulk of the constraint's operation is functional adjudication. Resistance is moderate-low (0.35) because modern executives generally accept the framework while episodically seeking to narrow it during emergencies. Accessibility collapse is moderate (0.38): alternatives to lawful process are institutionally disfavored but remain conceptually available to sovereign actors under pressure. The measurement series share one time grid so temporal analysis samples every metric at each examined point.
 *
 * PERSPECTIVAL GAP:
 *   The crown_executive seat experiences the constraint as a loss of discretionary authority â an extraction of its capacity to act unilaterally â while the subjects_under_rule and juridical_institutions experience it as coordination (predictable rights, institutional authority). The engine computes this divergence from the structural data: same constraint, opposite directionalities depending on beneficiary versus victim position. The legal scholar observer sees both simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   subjects_under_rule and juridical_institutions are structural beneficiaries (low d, subsidized by the constraint's protections and authority). crown_executive is the structural target (high d, pays through constrained discretion). The juridical institutions sit near the agenda-setting end but are better understood as beneficiaries of inherited authority who administer coordination rather than capturing rents. Subjects have constrained exit because emigration from the jurisdiction is costly; the crown has constrained exit because abdication or constitutional rupture is institutionally extreme.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â arbitrary royal power â has evolved rather than disappeared. While the 13th-century feudal context is dead, the problem of executive overreach persists in modern administrative and emergency states. The constraint is therefore not a piton (atrophied mandate) but a rope whose coordination function has migrated across historical contexts. Were the underlying problem of arbitrary power to disappear entirely, the constraint would risk mandatrophy; as it stands, the living constitutionalism reading successfully claims the problem is live in evolved form, corroborated by ongoing executive resistance to due process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the living constitutionalism reading of Magna Carta discover a pre-existing juridical constraint that transcends its origin, or construct one through retrospective ascription and interpretive evolution?',
    'Historical jurisprudential analysis comparing the intentions of the 1215 parties with the doctrinal content attributed by modern courts; empirical study of judicial opinion language measuring the gap between original feudal context and modern due process claims.',
    'If constructed, the constraint''s extractiveness is higher (modern norms imposed on historical sovereignty claims without genuine inheritance); if discovered, the coordination function is more genuinely inherited and the extraction metric overstates the cost to the Crown.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether the living constitutionalism reading is discovery or construction').

omega_variable(
    due_process_extraction_boundary,
    'Is the constraint on sovereign power genuinely coordinating (providing legal certainty that benefits all seats including long-term sovereign legitimacy) or extractive (transferring authority from the Crown to juridical institutions without net coordination gain)?',
    'Comparative analysis of political stability and sovereign capacity in jurisdictions with strong Magna Carta lineage versus those without; measuring whether constrained sovereigns experience improved governance legitimacy or pure authority loss.',
    'If juridical institutions gain authority at sovereign expense without improving coordination, the constraint tilts toward tangled_rope; if legal certainty benefits all seats including the Crown''s long-term legitimacy, it remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(due_process_extraction_boundary, empirical, 'Coordination versus extraction boundary for due process restraint').

omega_variable(
    evolutionary_drift_ambiguity,
    'At what point does evolutionary interpretation of the charter''s kernel depart so far from historical content that it becomes a different constraint entirely, violating epsilon-invariance?',
    'Tracing specific doctrinal innovations (e.g., modern procedural due process, administrative law constraints) back to charter text; assessing whether interpretive moves are continuous extensions or discontinuous inventions requiring decomposition.',
    'If the gap is discontinuous, the epsilon-invariance principle requires splitting the story into separate constraints (historical compact versus modern doctrine), collapsing the single constraint reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolutionary_drift_ambiguity, conceptual, 'Evolutionary drift and epsilon-invariance threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_lc_tr_t0, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(magna_carta_lc_tr_t10, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(magna_carta_lc_tr_t20, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(magna_carta_lc_tr_t30, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(magna_carta_lc_tr_t40, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(magna_carta_lc_tr_t50, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement(magna_carta_lc_tr_t60, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(magna_carta_lc_be_t0, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(magna_carta_lc_be_t10, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(magna_carta_lc_be_t20, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(magna_carta_lc_be_t30, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement(magna_carta_lc_be_t40, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(magna_carta_lc_be_t50, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 50, 0.36).
narrative_ontology:measurement(magna_carta_lc_be_t60, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 60, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_lc_su_t0, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(magna_carta_lc_su_t10, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(magna_carta_lc_su_t20, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(magna_carta_lc_su_t30, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(magna_carta_lc_su_t40, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(magna_carta_lc_su_t50, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(magna_carta_lc_su_t60, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The kernel magna_carta_constraint_authority decomposes into three structurally distinct constraints per the epsilon-invariance principle: the feudal_obsolescence_reading (historical descriptor, negligible extraction), the living_constitutionalism_reading (coordination through evolutionary precedent, low-moderate extraction), and the parliamentary_sovereignty_reading (statute-absorbed authority, different victim/beneficiary structure). Each reading has a distinct epsilon, stakeholder topology, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
