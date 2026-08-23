% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta (1215) — Universal Due Process Reading (Clause 39)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story models the UNIVERSAL RIGHTS READING of Magna
 *   Carta's Clause 39 ('No free man shall be seized...except by the lawful
 *   judgment of his equals or by the law of the land'). The reading treats
 *   'free man' as a transhistorical category meaning all persons, and Clause
 *   39 as emitting a universal due process constraint on all state power over
 *   individuals. This is one of three contested readings of the same kernel
 *   (magna_carta_1215). The reading's structural profile: low extractiveness
 *   (the constraint extracts from sovereign power to protect persons),
 *   negligible suppression (it enables rather than coerces), declining
 *   theater (the gap between text and practice has narrowed over 800 years),
 *   moderate accessibility collapse (alternatives to due process exist but
 *   are structurally illegitimate), significant resistance (authoritarian
 *   regimes, emergency powers, carceral states resist the constraint). The
 *   claimed type is ROPE: a genuine coordination mechanism that solves the
 *   credible commitment problem between state and subject with minimal
 *   coercive overhead.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.18).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.08).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta (1215) — Universal Due Process Reading (Clause 39)").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a').
narrative_ontology:cs_kernel_codification('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a', fixed_text).
narrative_ontology:cs_authority_grounding('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a', lineage).
narrative_ontology:cs_interpretation_layer_present('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a').
narrative_ontology:cs_reading_relation('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a', foundational, all_persons_are_rights_bearers_under_clause_39).
narrative_ontology:cs_axiom_status(all_persons_are_rights_bearers_under_clause_39, holdable).
narrative_ontology:cs_axiom_grounding('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a', all_persons_are_rights_bearers_under_clause_39, deontological).
narrative_ontology:cs_axiom('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a', foundational, due_process_binds_all_sovereign_power).
narrative_ontology:cs_axiom_status(due_process_binds_all_sovereign_power, holdable).
narrative_ontology:cs_axiom_grounding('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a', due_process_binds_all_sovereign_power, deontological).
narrative_ontology:cs_reference_frame('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a', clause_39_universal_due_process).
narrative_ontology:cs_drift_state('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a', contemporary_human_rights_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('0ad8c70d-29fc-44fd-b3ae-39dcf35d8e4a', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, all_persons_subject_to_state_power).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, judicial_institutions).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, constitutional_lawyers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, sovereign_power).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, due_process_clause_39_universal).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, habeas_corpus_precedent).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, rule_of_law_above_sovereign).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every person subject to state coercion — arrest, detention, punishment — is the intended beneficiary of the reading's universal protection. They cannot exit the state's jurisdiction; the constraint is the only structural barrier between them and arbitrary power. The reading asserts this protection applies to them regardless of status, wealth, or citizenship.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, all_persons_subject_to_state_power, beneficiary,
    powerless, biographical, trapped, universal).

% Courts and judges are the institutional beneficiaries and co-administrators of this reading: they gain authoritative interpretive power when the constraint is read as universal, because every detention decision becomes subject to due process review. They set the agenda of what process is due. Their exit is analytical — they remain inside the system but can reinterpret the constraint's reach.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, judicial_institutions, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, judicial_institutions, agenda_setter).

% The professional bar that litigates due process claims benefits from an expansive reading — more cases, more doctrinal space, more professional authority. They can exit to other practice areas or jurisdictions (mobile), but within constitutional practice the reading expands their domain.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, constitutional_lawyers, beneficiary,
    organized, biographical, mobile, global).

% The state — executive, legislative, administrative — bears the constraint's costs: it must provide process before depriving liberty, it loses the power of arbitrary detention, it must justify coercive acts in courts. It cannot fully exit the constraint (constrained exit: constitutional amendment or revolution are the only exits, both prohibitive). The reading treats the sovereign as the primary payer of the coordination function.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, sovereign_power, payer,
    institutional, generational, constrained, national).

% The 1215 contracting parties — the barons who forced the charter — are excluded from this reading's beneficiary set. The universal reading deliberately supersedes their particularist claim. They would object that the charter was a feudal contract for their class; the reading treats their intent as historically superseded.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, historical_barons, excluded,
    powerful, immediate, analytical, local).

% Scholars who study the charter's text, context, and reception history. They do not collect from or pay into the constraint; they analyze how the universal reading emerged from the baronial text and what structural work it does in contemporary constitutional orders.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of credible commitment: the sovereign binds itself to procedural regularity so that subjects can plan their lives without fear of arbitrary seizure. The constraint coordinates expectations between state and subject — the state gets legitimacy and compliance; subjects get predictability and protection.
% TRANSFER_FUNCTION: Moves the burden of justification from the subject (who would have to prove innocence against arbitrary power) to the sovereign (who must show lawful process before depriving liberty). Transfers procedural authority to courts. Transfers legitimacy from sovereign will to legal form.
% ABSENT_VOICES: The historical barons (excluded) would object that their feudal privilege is being universalized beyond the charter's text. Contemporary authoritarian regimes (not seated) would object that the constraint impedes sovereign prerogative. Indigenous peoples under colonial legal orders (not seated) would note the constraint's universal claim coexisted with their systematic exclusion — the reading's 'all persons' was not implemented for them.
% DISAPPEARANCE_RATIONALE: If the universal due process reading vanished overnight, the structural barrier between sovereign power and individual liberty would collapse to whatever the positive law of each jurisdiction provides. States with weak statutory protections would revert to arbitrary detention; courts would lose their primary textual anchor for procedural review; the global human rights framework would lose its foundational precedent. The world would rearrange toward executive discretion.
% FOUNDING_PROBLEM: The 1215 charter was built to solve the barons' problem: a king who seized their persons and property without process. The universal reading re-founds the constraint on a different problem: any state that can detain, punish, or kill without due process is a tyranny — the constraint exists to make tyranny structurally difficult.
% FOUNDING_PROBLEM_CORROBORATION: The universal reading's founding problem (arbitrary state power over persons) is attested by the entire post-WWII human rights architecture (UDHR Article 9, ICCPR Article 9, ECHR Article 5) — instruments drafted by states that had experienced or witnessed arbitrary power, not by the charter's original beneficiaries. The baronial reading's founding problem (baronial privilege) is attested only by the 1215 text itself and its contemporary defenders.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint's operation transfers justification burden to the sovereign — a cost of legitimate governance, not extraction from subjects. Suppression is near-zero (0.08) because the constraint functions through legal authorization, not coercion of subjects. Theater ratio is low (0.12) because contemporary due process institutions (habeas corpus, judicial review, procedural rights) are functional, not performative — though the uptick from 1948 reflects new performative compliance in authoritarian regimes. Accessibility collapse is moderate (0.35) because alternatives (arbitrary detention, executive discretion) remain practically available but are structurally delegitimized. Resistance is high (0.65) because the constraint is actively contested by states that claim emergency powers, by carceral systems that minimize process, and by sovereigntist movements that reject universal rights frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign seat, the constraint appears as a costly imposition (high effective extraction for the payer). From the all_persons seat, it appears as essential protection (negative effective extraction — a subsidy). From the judicial_institutions seat, it appears as both coordination (legitimate authority) and extraction (caseload burden). The engine computes this divergence; the authored claim (rope) does not adjudicate it. The baronial_privilege_reading would compute a different divergence: barons as beneficiaries, king as payer, everyone else excluded.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign_power seat is the primary payer (d near 1.0): it bears the cost of providing process, loses arbitrary power, cannot exit. All_persons_subject_to_state_power is the primary beneficiary (d near 0.0): they gain protection they cannot provide for themselves, are trapped in the state's jurisdiction. Judicial_institutions and constitutional_lawyers are secondary beneficiaries with analytical/mobile exit — they gain authority and domain from the reading. Historical_barons are excluded: their particularist claim is structurally superseded by the universal reading. The engine computes per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary state power) remains live — the constraint has not outlived its function. The universal reading has EXPANDED the constraint's mandate from protecting barons to protecting all persons, which is the opposite of mandatrophy. However, the theater uptick since 1948 suggests performative compliance in some regimes — a potential future mandatrophy risk if the constraint becomes ritual without substance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_particular_beneficiary_set,
    'Does the universal reading''s expansion of ''free men'' to ''all persons'' faithfully extend the charter''s logic, or does it impose a modern category on a feudal text?',
    'Historical-linguistic analysis of ''liber homo'' in 1215 context vs. reception history of the clause in subsequent constitutional struggles (1628 Petition of Right, 1679 Habeas Corpus Act, 1791 Bill of Rights, 1868 14th Amendment, 1948 UDHR). If each expansion was contested as textual infidelity but prevailed as structural necessity, the universal reading is the constraint''s actual evolutionary path.',
    'If the universal reading is a faithful extension, its low extractiveness and rope classification are structurally warranted. If it is a modern imposition, the constraint''s actual 1215 form was a tangled_rope (coordination for barons, extraction from everyone else), and the universal reading is a later scaffold that has not yet sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_particular_beneficiary_set, conceptual, 'Whether the universal beneficiary set is the constraint''s telos or a category error.').

omega_variable(
    sovereign_compliance_mechanism,
    'What enforces the constraint on the sovereign when the sovereign controls the enforcement apparatus?',
    'Comparative study of regimes where due process collapsed vs. survived: identify the structural conditions (independent judiciary, federalism, international treaties, military subordination, civil society) that make the constraint self-enforcing rather than dependent on sovereign consent.',
    'If the constraint requires sovereign consent to function, its suppression metric is understated — the low suppression reflects sovereign forbearance, not structural impotence. If it is self-enforcing through institutional ecology, the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereign_compliance_mechanism, empirical, 'Whether the constraint''s low suppression reflects structural self-enforcement or sovereign forbearance.').

omega_variable(
    reading_identity_foreclosure,
    'Does the universal reading''s core premise (all persons are rights-bearers under Clause 39) logically foreclose the baronial reading, or do they coexist as competing framings?',
    'Analyze whether any single legal framework can simultaneously hold: (a) the charter protects only the 1215 contracting parties, and (b) the charter protects all persons. If a framework must choose, the relation is forecloses. If different frameworks can hold each without contradiction, the relation is coexists_with.',
    'Determines the reading_relation declaration in cs_structure. A forecloses relation means the universal reading structurally displaces the baronial reading within any coherent framework. A coexists_with relation means both remain live in different frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_foreclosure, conceptual, 'Structural relationship between the universal and baronial readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__universal_rights_reading, theater_ratio, 1215, 0.65).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_1215__universal_rights_reading, theater_ratio, 1689, 0.4).
narrative_ontology:measurement(magn_tr_t1791, magna_carta_1215__universal_rights_reading, theater_ratio, 1791, 0.25).
narrative_ontology:measurement(magn_tr_t1868, magna_carta_1215__universal_rights_reading, theater_ratio, 1868, 0.18).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_1215__universal_rights_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_1215__universal_rights_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__universal_rights_reading, base_extractiveness, 1215, 0.42).
narrative_ontology:measurement(magn_be_t1689, magna_carta_1215__universal_rights_reading, base_extractiveness, 1689, 0.35).
narrative_ontology:measurement(magn_be_t1791, magna_carta_1215__universal_rights_reading, base_extractiveness, 1791, 0.28).
narrative_ontology:measurement(magn_be_t1868, magna_carta_1215__universal_rights_reading, base_extractiveness, 1868, 0.22).
narrative_ontology:measurement(magn_be_t1948, magna_carta_1215__universal_rights_reading, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement(magn_be_t2025, magna_carta_1215__universal_rights_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__universal_rights_reading, suppression_requirement, 1215, 0.85).
narrative_ontology:measurement(magn_su_t1689, magna_carta_1215__universal_rights_reading, suppression_requirement, 1689, 0.45).
narrative_ontology:measurement(magn_su_t1791, magna_carta_1215__universal_rights_reading, suppression_requirement, 1791, 0.3).
narrative_ontology:measurement(magn_su_t1868, magna_carta_1215__universal_rights_reading, suppression_requirement, 1868, 0.15).
narrative_ontology:measurement(magn_su_t1948, magna_carta_1215__universal_rights_reading, suppression_requirement, 1948, 0.08).
narrative_ontology:measurement(magn_su_t2025, magna_carta_1215__universal_rights_reading, suppression_requirement, 2025, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__universal_rights_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, habeas_corpus_act_1679).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, us_constitution_5th_amendment).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, us_constitution_14th_amendment).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, echr_article_5).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, iccpr_article_9).

% DUAL FORMULATION NOTE:
% This is the universal_rights_reading of the magna_carta_1215 kernel. The baronial_privilege_reading and living_document_reading are sibling constraints. All three form a constraint family linked by affects_constraints. This reading's ε (0.18) differs from the baronial reading's expected ε (~0.35, coordination for few with extraction from many) and the living document reading's expected ε (~0.25, adaptive coordination with interpretive extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__universal_rights_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
