% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: Thinkability and First-Holding Independence Test (M4/M5 Collapse)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   The synchronic_diachronic_seam reading tests whether the kernel
 *   'ip_category_emergence' has two structurally independent moments —
 *   category emergence (thinkability: when ownable expression became legally
 *   thinkable) and occupancy change (first-holding: when the author entered
 *   the legitimate claimant set) — or whether their coincidence at 1710 is a
 *   temporal framing artifact. The M4/M5 collapse test asks: can these
 *   moments vary independently across counterfactual histories? If they
 *   cannot, the kernel's bipartite structure is spurious — a single event
 *   masquerading as two. This reading does not adjudicate the kernel's
 *   content; it adjudicates the kernel's structural authenticity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.42).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.25).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.42).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, mountain).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "Thinkability and First-Holding Independence Test (M4/M5 Collapse)").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:emerges_naturally(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '12cbdfb0-38df-417b-b5fc-745dbd23f7fc').
narrative_ontology:cs_kernel_codification('12cbdfb0-38df-417b-b5fc-745dbd23f7fc', formalized).
narrative_ontology:cs_authority_grounding('12cbdfb0-38df-417b-b5fc-745dbd23f7fc', lineage).
narrative_ontology:cs_interpretation_layer_present('12cbdfb0-38df-417b-b5fc-745dbd23f7fc').
narrative_ontology:cs_reading_relation('12cbdfb0-38df-417b-b5fc-745dbd23f7fc', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('12cbdfb0-38df-417b-b5fc-745dbd23f7fc', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_axiom('12cbdfb0-38df-417b-b5fc-745dbd23f7fc', foundational, category_emergence_independent_of_occupancy).
narrative_ontology:cs_axiom_status(category_emergence_independent_of_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('12cbdfb0-38df-417b-b5fc-745dbd23f7fc', category_emergence_independent_of_occupancy, conventional).
narrative_ontology:cs_axiom('12cbdfb0-38df-417b-b5fc-745dbd23f7fc', secondary, temporal_coupling_is_contingent_not_necessary).
narrative_ontology:cs_axiom_status(temporal_coupling_is_contingent_not_necessary, holdable).
narrative_ontology:cs_axiom_grounding('12cbdfb0-38df-417b-b5fc-745dbd23f7fc', temporal_coupling_is_contingent_not_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('12cbdfb0-38df-417b-b5fc-745dbd23f7fc', statute_of_anne_origin).
narrative_ontology:cs_drift_state('12cbdfb0-38df-417b-b5fc-745dbd23f7fc', contemporary_critical_legal_history, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('12cbdfb0-38df-417b-b5fc-745dbd23f7fc', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, legal_institutions).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, formalist_jurists).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, copyright_maximalists).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, legal_historians).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, critical_scholars).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, reform_advocates).
narrative_ontology:constraint_vindicates(ip_category_emergence__synchronic_diachronic_seam, formal_independence_of_category_emergence_from_occupancy).
narrative_ontology:constraint_vindicates(ip_category_emergence__synchronic_diachronic_seam, temporal_artifact_thesis_rebuttal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the IP system grounded in the 1710 Statute of Anne as a clean origin. The collapse of category emergence and occupancy change into a single event stabilizes institutional authority and legislative continuity. They can shift frameworks across jurisdictions but the origin story remains the anchor.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legal_institutions, agenda_setter,
    institutional, generational, arbitrage, universal).

% Benefit from a doctrinal structure where IP categories appear as necessary formal truths rather than historical contingencies. The independence test, if it holds, validates their method; if it fails, their framework absorbs the distinction through interpretive layering. Career capital is invested in the coherence of the category system.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, formalist_jurists, beneficiary,
    organized, biographical, mobile, global).

% Leverage the collapsed origin to argue for expansive rights — if the category and its first occupant emerge together, the scope of the category feels coterminous with the scope of the grant. Exit from this framing means conceding historical contingency, which weakens expansionary arguments.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, copyright_maximalists, beneficiary,
    organized, biographical, constrained, global).

% Bear the cost of a collapsed history: the archive shows category emergence (thinkability) and occupancy change (first-holding) as distinct, protracted processes, but the institutional kernel demands they coincide at 1710. Their work is either marginalized as 'mere history' or conscripted to serve the origin myth. Exit means leaving the field or accepting the framing.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legal_historians, payer,
    organized, biographical, constrained, global).

% Need the distinction between thinkability and first-holding to mount structural critiques of IP (e.g., that the category was thinkable long before it was ownable, or that first-holding served political ends the category obscures). The collapse test, if it confirms independence, is a resource; if it confirms collapse, their critical purchase is reduced. Exit is constrained by the dominance of the formalist framework in courts and legislatures.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, critical_scholars, payer,
    moderate, biographical, constrained, global).

% Seek to decouple category scope from historical origin to enable tailored regimes (e.g., shorter terms for software, broader exceptions for education). The collapsed kernel treats any reform as an attack on the category itself. They are excluded from the interpretive conversation because the kernel's authority structure recognizes only 'faithful' readings. Exit is trapped — the reform conversation happens inside the kernel's terms or not at all.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, reform_advocates, excluded,
    organized, biographical, trapped, national).

% Observe the structural test from outside the dispute: does the M4/M5 collapse reveal a genuine logical independence or an enforced coincidence? They do not collect rents or pay costs from the constraint's operation but their classification feeds back into the discourse as a meta-level intervention.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legally administrable origin point for intellectual property as a coherent category system — solving the coordination problem of how courts, legislatures, and practitioners can treat 'copyright' as a single intelligible object across jurisdictions and centuries.
% TRANSFER_FUNCTION: Moves interpretive authority from historical contingency (the messy, protracted emergence of ownable expression and its first legal recognition) to formal necessity (the category and its first instance as a unified logical event), concentrating doctrinal control in institutions that administer the kernel.
% ABSENT_VOICES: Indigenous legal traditions that never recognized the thinkability/first-holding distinction; non-Western IP frameworks (Ottoman, Chinese, Islamic) where category emergence and state grant followed different logics; practitioners outside the statutory system (guilds, trade secrets, customary norms) who operated without the kernel's origin story.
% DISAPPEARANCE_RATIONALE: If the independence test vanished, legal institutions would lose the meta-constraint that polices the kernel's structural coherence — formalists would lose a validation resource, critical scholars would lose a critical lever, and reform advocates would lose a structural argument for decoupling. But the kernel (Statute of Anne as origin) would likely persist because its authority does not depend on this test; the test only diagnoses the kernel's structure.
% FOUNDING_PROBLEM: The need to establish intellectual property as a coherent legal category with a determinate origin, rather than a patchwork of privileges, monopolies, and customary practices — so that the category could function as a stable object of law, commerce, and state administration across borders and generations.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Mark Rose, Ronan Deazley, Adrian Johns) document that the founding problem was real but the solution (Statute of Anne as clean origin) was a retrospective construction. Institutional actors (WIPO, major copyright offices, courts) attest the problem remains live — the category still requires a stable origin. Critical scholars (James Boyle, Jessica Litman, Pamela Samuelson) corroborate from outside the beneficiary set that the founding problem has mutated: the original coordination need is solved, but the kernel now serves extraction.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, contested).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, ExtMetricName, E),
    domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ip_category_emergence__synchronic_diachronic_seam),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the collapse, if enforced, extracts interpretive labor from historians and critical leverage from reformers — but the test itself is a diagnostic, not the extraction mechanism. Suppression is low (0.25) because the test is a conceptual tool; suppression operates at the kernel level (enforcing the 1710 origin), not at the test level. Theater is low (0.18) — the test is genuinely used by scholars on all sides. Accessibility collapse is high (0.78) because if the moments are formally independent, the kernel's bipartite structure is authenticated; if they collapse, the kernel is structurally univalent. Resistance is low (0.22) — the test meets little active opposition because it is framed as a scholarly clarification, not a threat.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (legal_institutions) experiences this test as a harmless diagnostic that confirms the kernel's coherence. The payer seats (legal_historians, critical_scholars) experience it as a gatekeeping device: if the test 'proves' collapse, their critical purchase evaporates; if it 'proves' independence, the kernel gains structural legitimacy that much harder to challenge. The excluded seat (reform_advocates) experiences it as irrelevant — the kernel's authority structure already forecloses their reforms. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal institutions and formalist jurists are structural beneficiaries (d ~0.15-0.25): the test's validation of independence secures their framework. Copyright maximalists benefit indirectly (d ~0.3) by locking category scope to origin. Legal historians and critical scholars are payers (d ~0.7-0.8): their work is constrained by the collapse. Reform advocates are trapped (d ~0.9): excluded from the interpretive conversation entirely. Analytical observers sit at d=0.5. The test itself has no enforcement machinery — directionality derives from the kernel it diagnoses.
 *
 * MANDATROPHY ANALYSIS:
 *   The kernel's mandate (stable IP category with determinate origin) is live — coordination need persists. But the test reveals a possible mandatrophy within the kernel: if the two moments collapse, the kernel's bipartite structure is a vestigial distinction maintained for institutional legitimacy. The test prevents mislabeling the kernel's structural coherence as natural law (Mountain) when it may be enforced coincidence (Snare/Tangled Rope). The test itself is not mandatrophic — it is the diagnostic that detects mandatrophy in the kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    independence_vs_artifact,
    'Are thinkability (category emergence) and first-holding (occupancy change) formally independent across counterfactual legal histories, or does their coincidence at 1710 reflect a necessary temporal coupling?',
    'Counterfactual legal history: construct alternative timelines where the Statute of Anne grants rights to printers (not authors) or where ownable expression emerges without a first-holding event. If the moments separate cleanly, independence holds; if they resist separation, the coupling is structural.',
    'If independent, the kernel has authentic bipartite structure (Mountain for each moment, Tangled Rope for their relation). If coupled, the kernel is univalent — one moment is epiphenomenal — and the bipartite structure is a spurious Mountain claim (FSM candidate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independence_vs_artifact, conceptual, 'Whether the kernel''s two structural moments are genuinely independent or a temporal framing artifact.').

omega_variable(
    institutional_interest_in_collapse,
    'Do legal institutions have a structural interest in maintaining the collapse of thinkability and first-holding, independent of the historical truth?',
    'Institutional discourse analysis: trace how courts, legislatures, and treaty bodies treat challenges to the 1710 origin story. If challenges that preserve one moment but not the other are systematically dismissed, the institution has an interest in the collapse.',
    'If institutions enforce the collapse, the test''s Mountain claim is a false summit — the independence is a doctrinal requirement, not a logical necessity. The kernel becomes a Snare or Tangled Rope with the test as its enforcement mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_interest_in_collapse, empirical, 'Whether the collapse is maintained by institutional enforcement rather than logical necessity.').

omega_variable(
    historical_evidence_threshold,
    'What standard of historical evidence would suffice to falsify the independence claim, and who controls that standard?',
    'Epistemic governance audit: examine which historical claims about pre-1710 ownable expression or authorial rights are admitted in legal proceedings vs. relegated to ''mere history.'' The threshold for falsification is a procedural choice, not an evidentiary given.',
    'If the falsification threshold is set by beneficiaries of the collapse, the test cannot be a genuine Mountain diagnostic — it becomes a Snare''s self-validation. The omega documents the epistemic capture risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_threshold, empirical, 'Who controls the evidentiary standard for the independence test, and whether it is calibrated to resist falsification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 0, 314).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ip_c_tr_t50, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 50, 0.08).
narrative_ontology:measurement(ip_c_tr_t100, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 100, 0.12).
narrative_ontology:measurement(ip_c_tr_t150, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 150, 0.15).
narrative_ontology:measurement(ip_c_tr_t200, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 200, 0.16).
narrative_ontology:measurement(ip_c_tr_t250, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 250, 0.17).
narrative_ontology:measurement(ip_c_tr_t300, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 300, 0.18).
narrative_ontology:measurement(ip_c_tr_t314, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 314, 0.18).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ip_c_be_t50, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(ip_c_be_t100, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 100, 0.3).
narrative_ontology:measurement(ip_c_be_t150, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 150, 0.35).
narrative_ontology:measurement(ip_c_be_t200, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 200, 0.38).
narrative_ontology:measurement(ip_c_be_t250, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 250, 0.4).
narrative_ontology:measurement(ip_c_be_t300, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 300, 0.41).
narrative_ontology:measurement(ip_c_be_t314, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 314, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t0, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(ip_c_su_t50, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 50, 0.12).
narrative_ontology:measurement(ip_c_su_t100, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 100, 0.15).
narrative_ontology:measurement(ip_c_su_t150, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 150, 0.18).
narrative_ontology:measurement(ip_c_su_t200, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 200, 0.2).
narrative_ontology:measurement(ip_c_su_t250, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 250, 0.22).
narrative_ontology:measurement(ip_c_su_t300, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 300, 0.24).
narrative_ontology:measurement(ip_c_su_t314, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 314, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, identity_coordination).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__synchronic_diachronic_seam, 0.08).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% This reading is the structural diagnostic for the ip_category_emergence kernel. The thinkability_reading isolates category emergence as the kernel's true mark; the first_holding_reading isolates occupancy change. This reading tests whether the kernel's bipartite structure is authentic (moments independent) or spurious (moments coupled). All three readings share the kernel_id 'ip_category_emergence' but instantiate different constraints with different ε values and different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__synchronic_diachronic_seam, institutional, 0.15).
constraint_indexing:directionality_override(ip_category_emergence__synchronic_diachronic_seam, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
