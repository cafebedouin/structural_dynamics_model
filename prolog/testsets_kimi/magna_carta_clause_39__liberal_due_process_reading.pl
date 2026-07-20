% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Magna Carta Clause 39 â Liberal Due Process Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the liberal due process reading of Magna
 *   Carta Clause 39 ('No free man shall be seized or imprisoned... except by
 *   the lawful judgment of his peers or by the law of the land'), treating
 *   the clause as establishing universal individual rights against arbitrary
 *   state power. Under this reading, the clause is not a narrow feudal
 *   privilege but a foundational limitation on executive discretion that
 *   requires all state power to operate through established legal procedure.
 *   The kernel (the text of Clause 39) is contested: the feudal prerogative
 *   reading sees it as preserving baronial privileges within hierarchy, while
 *   the originalist limitation reading restricts it to documented 1215 royal
 *   abuses. This reading expands the clause into an expansive rights
 *   constraint with high extractiveness from unchecked executive authority.
 *
 * KEY AGENTS:
 *   - individual_rights_bearers (beneficiary): All persons protected by due process guarantees; gain security against arbitrary imprisonment and deprivation of rights.
 *   - executive_authority (payer): State executive power constrained by procedural requirements; loses discretionary capacity to punish or seize without legal process.
 *   - judicial_institutions (agenda_setter): Courts and judges who interpret and enforce the lawful judgment and law-of-the-land requirements; administer the constraint.
 *   - constitutional_observers (observer): Legal historians and constitutional theorists who trace the clause's interpretive migration from feudal privilege to universal right.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.72).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.65).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Magna Carta Clause 39 â Liberal Due Process Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, 'fbcecfbe-aecf-48aa-a817-b743b204807b').
narrative_ontology:cs_kernel_codification('fbcecfbe-aecf-48aa-a817-b743b204807b', fixed_text).
narrative_ontology:cs_authority_grounding('fbcecfbe-aecf-48aa-a817-b743b204807b', lineage).
narrative_ontology:cs_interpretation_layer_present('fbcecfbe-aecf-48aa-a817-b743b204807b').
narrative_ontology:cs_reading_relation('fbcecfbe-aecf-48aa-a817-b743b204807b', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('fbcecfbe-aecf-48aa-a817-b743b204807b', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('fbcecfbe-aecf-48aa-a817-b743b204807b', foundational, universal_personal_rights_against_state_arbitrariness).
narrative_ontology:cs_axiom_status(universal_personal_rights_against_state_arbitrariness, holdable).
narrative_ontology:cs_axiom_grounding('fbcecfbe-aecf-48aa-a817-b743b204807b', universal_personal_rights_against_state_arbitrariness, deontological).
narrative_ontology:cs_axiom('fbcecfbe-aecf-48aa-a817-b743b204807b', foundational, law_of_land_as_binding_procedure).
narrative_ontology:cs_axiom_status(law_of_land_as_binding_procedure, holdable).
narrative_ontology:cs_axiom_grounding('fbcecfbe-aecf-48aa-a817-b743b204807b', law_of_land_as_binding_procedure, conventional).
narrative_ontology:cs_reference_frame('fbcecfbe-aecf-48aa-a817-b743b204807b', liberal_rule_of_law_limitation).
narrative_ontology:cs_drift_state('fbcecfbe-aecf-48aa-a817-b743b204807b', contemporary_administrative_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fbcecfbe-aecf-48aa-a817-b743b204807b', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, individual_rights_bearers).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_authority).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, limited_government_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All persons who possess procedural protections against arbitrary imprisonment, dispossession, or punishment by the state. They gain security and predictability from the requirement that executive action proceed only by lawful judgment and established legal process. Exit from this protection is effectively impossible without leaving the jurisdiction or the human rights framework entirely.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, individual_rights_bearers, beneficiary,
    moderate, generational, constrained, global).

% The aggregate of state executive power â monarchs, ministers, presidents, and administrative agencies â that loses the discretionary capacity to seize, imprison, or punish without prior legal process. The constraint extracts arbitrary power and redirects it through courts and statutes. Executives persistently seek workarounds through emergency decrees, statutory expansion, and administrative discretion.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_authority, payer,
    institutional, biographical, constrained, national).

% Courts and judges who interpret and enforce the lawful judgment of peers and law of the land requirements. They administer the constraint by reviewing executive action, setting aside arbitrary decisions, and defining the procedural boundaries of legitimate state power. Their authority is constituted by the constraint; they cannot exit without dissolving their own function.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, judicial_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Legal historians, constitutional theorists, and human rights scholars who track the interpretive migration of Clause 39 from feudal privilege to universal right. They document the divergence between the kernel text and modern liberal readings, and assess whether the expansion is authentic legal development or retrospective projection.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, constitutional_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__liberal_due_process_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__liberal_due_process_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces arbitrary personal rule with predictable legal procedure, coordinating the relationship between state power and individual security through known rules administered by courts.
% TRANSFER_FUNCTION: Transfers discretionary power from executive authority to judicial procedure and legal process; individuals receive security against arbitrary state action while the executive loses capacity to punish or seize outside the law.
% ABSENT_VOICES: Advocates of absolute executive prerogative and emergency supremacy are structurally disadvantaged in liberal constitutional discourse; their objections are treated as outside the framework rather than addressed within it.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, executives could imprison and dispossess without legal process; the structure of limited government would collapse and the relationship between citizen and state would reorganize around personal discretion rather than law.
% FOUNDING_PROBLEM: Arbitrary royal power in medieval England allowed the king to imprison, dispossess, or exile subjects without legal process, creating pervasive insecurity and unpredictable rule.
% FOUNDING_PROBLEM_CORROBORATION: Feudal and originalist historians outside the liberal rights tradition attest that the founding problem was narrower â baronial privilege against the king, not universal individual rights. They corroborate that the liberal reading's broad claim is contested and may be retrospective projection.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the liberal reading substantially strips executive power of arbitrary discretion, transferring authority to judicial procedure. Suppression (0.65) reflects the active enforcement required to maintain this transfer against executive resistance. Theater ratio (0.30) captures the ritual and procedural formalism that can substitute for substantive protection. Accessibility collapse (0.85) is high because once the rule-of-law framework is accepted, arbitrary power collapses as a legitimate alternative. Resistance (0.55) reflects persistent executive efforts to evade or narrow the constraint through emergency claims, administrative discretion, and statutory expansion. The measurement series tracks the gradual expansion of the liberal reading from its medieval origins through the Enlightenment and into the modern human rights era on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The executive seat experiences the constraint as pure extraction â a forced surrender of discretionary power to procedural requirements. The individual rights-bearer seat experiences it as genuine coordination â predictable protection against arbitrary state violence. The agenda-setter seat (judiciary) experiences it as both: a genuine coordination mechanism it is sworn to uphold, and a source of institutional authority. The engine computes these divergences from the structural data; the authored claim (tangled_rope) captures that both experiences are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual rights bearers are declared beneficiaries (low d, subsidized by the constraint's protection). Executive authority is declared victim/payer (high d, extraction target). Judicial institutions sit near symmetric but slightly toward beneficiary as administrators of the transferred authority. The directionality derivation is straightforward from these declarations: the constraint extracts from state power and subsidizes individual security.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this constraint could be misread as a mountain (natural law of governance) or a rope (pure coordination). The mountain reading would ignore the active enforcement required to suppress executive resistance and the asymmetric cost borne by state power. The rope reading would ignore the extraction from executive discretion. Tangled_rope captures that the constraint coordinates society around rule of law while simultaneously extracting power from the executive â both functions are carried by the same structure and require active judicial enforcement to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liberal_reading_historical_validity,
    'Does the liberal due process reading represent an authentic development of the kernel text, or a retrospective projection of modern rights frameworks onto medieval feudal law?',
    'Comparative historical analysis of 13th-century legal practice against the textual claims of the liberal reading; identification of anachronistic concepts (e.g., universal individual rights) in the 1215 context.',
    'If the reading is largely retrospective, its epsilon should be read as extraction from historical accuracy to serve modern political needs, shifting the classification toward snare at the analytical level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_reading_historical_validity, conceptual, 'Whether the liberal reading is authentic legal development or anachronistic projection').

omega_variable(
    executive_power_adaptation,
    'Has modern executive power (administrative state, emergency prerogative, national security frameworks) structurally evaded the constraint while maintaining formal compliance?',
    'Empirical audit of executive actions nominally compliant with due process but effectively arbitrary â e.g., administrative detention, emergency decrees, statutory loopholes.',
    'If evasion is widespread, the constraint''s effective extraction is lower than its formal claim, and the theater_ratio should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_power_adaptation, empirical, 'Whether modern executives have rendered the constraint formally present but functionally hollow').

omega_variable(
    universal_rights_enforcement_scope,
    'Does the constraint''s claimed global scope correspond to enforceable limits on state power, or is it primarily declarative beyond specific national jurisdictions?',
    'Cross-jurisdictional comparison of enforcement rates, judicial independence metrics, and actual remedial outcomes for individuals invoking due-process protections.',
    'If enforcement is sparse beyond Western liberal democracies, the global scope claim inflates the coordination story while extraction remains geographically concentrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_rights_enforcement_scope, empirical, 'Gap between claimed universal scope and actual enforceable jurisdiction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcc39_ldp_tr_t0, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mcc39_ldp_tr_t100, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement(mcc39_ldp_tr_t200, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement(mcc39_ldp_tr_t400, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 400, 0.2).
narrative_ontology:measurement(mcc39_ldp_tr_t600, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 600, 0.25).
narrative_ontology:measurement(mcc39_ldp_tr_t800, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 800, 0.3).

% Extraction over time
narrative_ontology:measurement(mcc39_ldp_be_t0, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(mcc39_ldp_be_t100, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 100, 0.25).
narrative_ontology:measurement(mcc39_ldp_be_t200, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 200, 0.35).
narrative_ontology:measurement(mcc39_ldp_be_t400, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 400, 0.5).
narrative_ontology:measurement(mcc39_ldp_be_t600, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 600, 0.65).
narrative_ontology:measurement(mcc39_ldp_be_t800, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 800, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(mcc39_ldp_su_t0, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(mcc39_ldp_su_t100, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 100, 0.3).
narrative_ontology:measurement(mcc39_ldp_su_t200, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 200, 0.35).
narrative_ontology:measurement(mcc39_ldp_su_t400, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 400, 0.45).
narrative_ontology:measurement(mcc39_ldp_su_t600, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 600, 0.55).
narrative_ontology:measurement(mcc39_ldp_su_t800, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 800, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the liberal due process reading of Magna Carta Clause 39, decomposed from the colloquial label 'Clause 39' which conflates feudal prerogative, originalist limitation, and liberal universal rights interpretations. Each reading instantiates a structurally distinct constraint with different epsilon values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
