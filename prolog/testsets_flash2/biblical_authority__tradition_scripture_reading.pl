% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Scripture Requires Tradition for Authoritative Interpretation (Magisterial Reading)
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint describes the theological position that Scripture
 *   requires the living Tradition of the Church, as interpreted and guarded
 *   by the Magisterium (the teaching authority of the Church), for its
 *   authoritative understanding. It is a specific reading of the broader
 *   'biblical_authority' kernel, emphasizing the role of institutional
 *   mediation in divine revelation. This reading leads to high clerical
 *   extraction and low doctrinal fragmentation, with sacraments playing a
 *   central, grace-conferring role.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.85).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.9).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, snare).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Scripture Requires Tradition for Authoritative Interpretation (Magisterial Reading)").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, '07d6b859-cdce-4900-9e15-7d7d23dde2ae').
narrative_ontology:cs_kernel_codification('07d6b859-cdce-4900-9e15-7d7d23dde2ae', formalized).
narrative_ontology:cs_authority_grounding('07d6b859-cdce-4900-9e15-7d7d23dde2ae', lineage).
narrative_ontology:cs_interpretation_layer_present('07d6b859-cdce-4900-9e15-7d7d23dde2ae').
narrative_ontology:cs_reading_relation('07d6b859-cdce-4900-9e15-7d7d23dde2ae', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('07d6b859-cdce-4900-9e15-7d7d23dde2ae', biblical_authority__conciliar_reading, influences).
narrative_ontology:cs_axiom('07d6b859-cdce-4900-9e15-7d7d23dde2ae', foundational, magisterial_infallibility_in_faith_and_morals).
narrative_ontology:cs_axiom_status(magisterial_infallibility_in_faith_and_morals, holdable).
narrative_ontology:cs_axiom_grounding('07d6b859-cdce-4900-9e15-7d7d23dde2ae', magisterial_infallibility_in_faith_and_morals, theological).
narrative_ontology:cs_axiom('07d6b859-cdce-4900-9e15-7d7d23dde2ae', foundational, tradition_as_co_equal_source_of_revelation).
narrative_ontology:cs_axiom_status(tradition_as_co_equal_source_of_revelation, holdable).
narrative_ontology:cs_axiom_grounding('07d6b859-cdce-4900-9e15-7d7d23dde2ae', tradition_as_co_equal_source_of_revelation, theological).
narrative_ontology:cs_reference_frame('07d6b859-cdce-4900-9e15-7d7d23dde2ae', apostolic_deposit_of_faith).
narrative_ontology:cs_drift_state('07d6b859-cdce-4900-9e15-7d7d23dde2ae', contemporary_pluralistic_theology, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('07d6b859-cdce-4900-9e15-7d7d23dde2ae', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, magisterium).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, clerical_hierarchy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_interpretive_agency).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, individual_believers).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, apostolic_succession_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, sacramental_mediation_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Church, responsible for guarding the deposit of faith and providing the sole authoritative interpretation of Scripture and Tradition. Benefits from the centralization of interpretive power and the necessity of its mediation for access to grace.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).

% The ordained ministers who administer sacraments and teach in communion with the Magisterium. Benefits from the system that requires their mediation for spiritual life and doctrinal understanding, reinforcing their authority and social position.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, clerical_hierarchy, beneficiary,
    institutional, generational, identity_locked, global).

% Individual believers whose direct interpretive access to Scripture is constrained by the requirement for magisterial guidance. They bear the cost of intellectual dependence and the suppression of personal theological inquiry outside approved channels.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_interpretive_agency, payer,
    powerless, biographical, trapped, global).

% The general body of adherents who rely on the Magisterium and clerical hierarchy for authoritative teaching and sacramental grace. They pay through their submission to interpretive authority and the financial/social support of the institutional structure.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, individual_believers, payer,
    powerless, biographical, constrained, global).

% Scholars and thinkers who might offer alternative interpretations of Scripture and Tradition but are not part of the official teaching authority. Their work is often marginalized or suppressed if it deviates from magisterial pronouncements.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, theologians_outside_magisterium, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures doctrinal unity and prevents fragmentation of belief by centralizing interpretive authority, providing a consistent framework for understanding divine revelation across generations and cultures.
% TRANSFER_FUNCTION: Transfers interpretive authority and spiritual mediation from individual believers to the Magisterium and clerical hierarchy, in exchange for doctrinal certainty and access to grace through sacraments.
% ABSENT_VOICES: Proponents of 'sola scriptura' and those advocating for conciliar or communal interpretation are excluded from the authoritative interpretive process. They would argue for direct access to Scripture and a more decentralized, participatory theological discourse.
% DISAPPEARANCE_RATIONALE: If the magisterial requirement for tradition vanished, the institutional structure of the Church would undergo a profound crisis. Doctrinal authority would decentralize, leading to diverse interpretations and potentially schism. The role of the clergy would diminish, and individual believers would seek direct engagement with scripture, fundamentally altering the religious landscape.
% FOUNDING_PROBLEM: The early Christian community faced diverse interpretations of apostolic teaching and scriptural texts, leading to heresies and fragmentation. A centralized authority was deemed necessary to preserve the 'deposit of faith' and maintain unity.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium itself attests that the problem of doctrinal fragmentation remains live, citing ongoing theological disputes and challenges to traditional teachings. Historical accounts of early Church councils and patristic writings from outside the current Magisterium corroborate the historical existence of the problem, though not necessarily the necessity of the current solution.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because interpretive authority and spiritual mediation are centralized, requiring institutional channels for access to divine truth and grace. Suppression is very high (0.90) as alternative interpretations are actively discouraged or condemned, and lay interpretive agency is largely foreclosed. The theater ratio is low (0.10) because the Magisterium's function of guarding the 'deposit of faith' is genuinely performed and central to the system's self-conception, not merely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Magisterium and clerical hierarchy, this constraint is a necessary 'rope' for doctrinal unity and the preservation of truth. From the perspective of lay interpretive agency and individual believers, it operates as a 'snare' that extracts their interpretive autonomy and reinforces institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and clerical hierarchy are clear beneficiaries, as the constraint centralizes their authority and makes their mediation indispensable. Lay interpretive agency and individual believers are the primary victims, as their direct access to and interpretation of Scripture is suppressed. The constraint subsidizes the institutional structure while extracting interpretive freedom from the laity.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'snare' prevents mislabeling this as pure coordination. While it claims to solve a coordination problem (doctrinal unity), the high extractiveness and suppression, coupled with the clear beneficiaries and victims, indicate that the coordination story serves as a cover for institutional power and resource capture. The mandate (preserving faith) is still 'live', but the mechanism has become highly extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_of_magisterial_mediation,
    'Is magisterial mediation truly necessary for doctrinal unity and the preservation of faith, or are alternative, less extractive models of interpretive authority viable?',
    'Empirical study of religious communities with decentralized interpretive models: do they exhibit greater doctrinal fragmentation or a different, potentially more resilient, form of unity?',
    'If viable alternatives exist, the constraint''s necessity claim weakens, supporting a reclassification towards a higher extraction type (e.g., a stronger snare or even a piton if the original problem is solved by other means).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_magisterial_mediation, empirical, 'Examines the functional necessity of centralized interpretive authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional barriers, formal condemnations) or internalized (cognitive patterns, identity fusion with the institution)?',
    'Post-exit suppression trajectory: if suppression of interpretive agency persists after individuals leave the institutional structure, reclassify as partially internalized. If it dissipates, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — individuals carry the suppression with them after exit, making true interpretive freedom harder to achieve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for interpretive agency.').

omega_variable(
    sacramental_mediation_necessity,
    'Is the necessity of sacramental mediation for grace an intrinsic theological truth or an institutional construct that reinforces clerical authority?',
    'Conceptual analysis of theological arguments for and against direct access to grace, and historical examination of shifts in sacramental theology and practice.',
    'If primarily an institutional construct, the extractiveness associated with clerical mediation is amplified, further solidifying the ''snare'' classification. If intrinsic, it represents a ''mountain'' within the theological framework, reducing the perceived extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_mediation_necessity, conceptual, 'Theological vs. institutional grounding of sacramental necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__tradition_scripture_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement(bibl_tr_t200, biblical_authority__tradition_scripture_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(bibl_tr_t300, biblical_authority__tradition_scripture_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(bibl_tr_t400, biblical_authority__tradition_scripture_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement(bibl_tr_t500, biblical_authority__tradition_scripture_reading, theater_ratio, 500, 0.1).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__tradition_scripture_reading, base_extractiveness, 100, 0.8).
narrative_ontology:measurement(bibl_be_t200, biblical_authority__tradition_scripture_reading, base_extractiveness, 200, 0.82).
narrative_ontology:measurement(bibl_be_t300, biblical_authority__tradition_scripture_reading, base_extractiveness, 300, 0.83).
narrative_ontology:measurement(bibl_be_t400, biblical_authority__tradition_scripture_reading, base_extractiveness, 400, 0.84).
narrative_ontology:measurement(bibl_be_t500, biblical_authority__tradition_scripture_reading, base_extractiveness, 500, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__tradition_scripture_reading, suppression_requirement, 100, 0.85).
narrative_ontology:measurement(bibl_su_t200, biblical_authority__tradition_scripture_reading, suppression_requirement, 200, 0.88).
narrative_ontology:measurement(bibl_su_t300, biblical_authority__tradition_scripture_reading, suppression_requirement, 300, 0.89).
narrative_ontology:measurement(bibl_su_t400, biblical_authority__tradition_scripture_reading, suppression_requirement, 400, 0.9).
narrative_ontology:measurement(bibl_su_t500, biblical_authority__tradition_scripture_reading, suppression_requirement, 500, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, theological_education_curriculum).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, liturgical_practice_norms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
