% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: NSL as Mainland Legal System Transplantation (Jurisdictional Capture Reading)
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story captures the National Security Law (NSL) through
 *   the jurisdictional_capture_reading: the NSL operates not merely as a
 *   security instrument but as a structural vehicle for transplanting
 *   mainland legal concepts, procedures, and interpretive authority into Hong
 *   Kong's common law system. The erosion of judicial autonomy is the
 *   operative transfer. The claim (tangled_rope) asserts that a genuine
 *   coordination function (centralized security governance) is present but
 *   inseparable from asymmetric extraction (institutional capture).
 *
 * KEY AGENTS:
 *   - NPCSC Standing Committee: agenda_setter (institutional/constrained/national) â enacts and interprets the law, cannot retreat without sovereignty narrative collapse
 *   - Mainland security organs: beneficiary (institutional/constrained/national) â collect operational jurisdiction previously held by HK courts
 *   - Hong Kong judiciary: payer (institutional/identity_locked/regional) â bears the erosion of common law adjudicative autonomy
 *   - Hong Kong legal profession: payer (organized/identity_locked/regional) â bears procedural displacement and identity crisis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.72).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.78).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "NSL as Mainland Legal System Transplantation (Jurisdictional Capture Reading)").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '922f20e4-3929-4a2e-8db8-c5297cf4769b').
narrative_ontology:cs_kernel_codification('922f20e4-3929-4a2e-8db8-c5297cf4769b', formalized).
narrative_ontology:cs_authority_grounding('922f20e4-3929-4a2e-8db8-c5297cf4769b', extraction).
narrative_ontology:cs_interpretation_layer_present('922f20e4-3929-4a2e-8db8-c5297cf4769b').
narrative_ontology:cs_reading_relation('922f20e4-3929-4a2e-8db8-c5297cf4769b', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('922f20e4-3929-4a2e-8db8-c5297cf4769b', nsl_legal_text__democratic_enclosure_reading, influences).
narrative_ontology:cs_axiom('922f20e4-3929-4a2e-8db8-c5297cf4769b', foundational, hk_judicial_autonomy_is_constitutionally_enshrined).
narrative_ontology:cs_axiom_status(hk_judicial_autonomy_is_constitutionally_enshrined, holdable).
narrative_ontology:cs_axiom_grounding('922f20e4-3929-4a2e-8db8-c5297cf4769b', hk_judicial_autonomy_is_constitutionally_enshrined, conventional).
narrative_ontology:cs_axiom('922f20e4-3929-4a2e-8db8-c5297cf4769b', foundational, mainland_security_jurisdiction_constitutes_legal_transplantation).
narrative_ontology:cs_axiom_status(mainland_security_jurisdiction_constitutes_legal_transplantation, holdable).
narrative_ontology:cs_axiom_grounding('922f20e4-3929-4a2e-8db8-c5297cf4769b', mainland_security_jurisdiction_constitutes_legal_transplantation, empirically_contingent).
narrative_ontology:cs_reference_frame('922f20e4-3929-4a2e-8db8-c5297cf4769b', mainland_legal_supremacy).
narrative_ontology:cs_drift_state('922f20e4-3929-4a2e-8db8-c5297cf4769b', post_nsl_imposition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('922f20e4-3929-4a2e-8db8-c5297cf4769b', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_organs).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the National Security Law through an NPCSC decision and maintains interpretive supremacy through the power to issue binding interpretations. Sets the legal and political parameters under which Hong Kong institutions must operate, but is constrained by the broader Party-state need to maintain a narrative of successful 'one country, two systems' implementation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, npcsc_standing_committee, agenda_setter,
    institutional, generational, constrained, national).

% Gain operational jurisdiction and procedural presence in Hong Kong through the NSL, including the Office for Safeguarding National Security and direct investigative authority in defined circumstances. Collect institutional control over cases and suspects that were previously exclusively within Hong Kong's common law jurisdiction.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_organs, beneficiary,
    institutional, generational, constrained, national).

% Presides over NSL cases under the shadow of NPCSC interpretive overrides and mainland procedural concepts (such as closed hearings, limited bail, and evidentiary standards drawn from mainland practice). Their professional identity is constituted by common law adversarial traditions, which are progressively displaced by the transplant. Exit means abandoning the judicial role or publicly capitulating.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    institutional, generational, identity_locked, regional).

% Must represent clients in a procedural environment where mainland legal concepts constrain adversarial practice, solicitor-client privileges are eroded in security contexts, and the interpretive canon they trained in is superseded by NPCSC directives. Their career capital and professional identity are bound to the common law system being supplanted.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession, payer,
    organized, biographical, identity_locked, regional).

narrative_ontology:fixing_cost_class(nsl_legal_text__jurisdictional_capture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified national security legal framework across Hong Kong and the mainland, eliminating jurisdictional fragmentation on security matters and asserting centralized interpretive authority over previously autonomous common law institutions.
% TRANSFER_FUNCTION: Transfers case jurisdiction, procedural control, interpretive authority, and substantive legal standards from Hong Kong's common law institutions to mainland security organs and Beijing-directed oversight mechanisms.
% ABSENT_VOICES: Hong Kong common law judges and senior counsel operating from the traditional adversarial framework are structurally sidelined; mainland criminal defense lawyers familiar with rule-of-law constraints are absent from the design room; international jurist bodies with no standing in the NPCSC process are excluded.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, Hong Kong courts would regain exclusive jurisdiction over security cases, mainland security organs would lose their authorized operational presence in Hong Kong, common law procedural safeguards would reconstitute as the operative framework, and the legal profession's autonomy would cease to be overridden by NPCSC interpretations.
% FOUNDING_PROBLEM: Following the 2019 unrest, the central government perceived that Hong Kong's common law system was unwilling or unable to address threats to national security through existing sedition, treason, and public order statutes, creating a perceived enforcement vacuum.
% FOUNDING_PROBLEM_CORROBORATION: The central government and mainland security apparatus attest the problem is live and necessitates this arrangement. The Hong Kong Bar Association, international law scholars, and foreign jurist bodies outside the beneficiary set contest that the common law system already possessed sufficient legal tools; they corroborate that the arrangement exceeds the founding problem's scope and targets institutional autonomy itself.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) is moderate-high because the constraint systematically transfers jurisdictional and interpretive authority from Hong Kong common law institutions to mainland organs. Suppression (0.78) is high because the constraint depends on NPCSC interpretive overrides and the physical presence of mainland security infrastructure to prevent common law alternatives from reasserting themselves. Theater ratio (0.45) reflects the growing performative maintenance of 'one country, two systems' rhetoric while substantive practice shifts to mainland procedural norms. Accessibility collapse (0.82) is high: once the NSL is in force, common law alternatives (full adversarial process, bail as of right, jury trials for security offenses) structurally collapse. Resistance (0.55) is moderate: the HK Bar Association and some judges offer doctrinal resistance, but institutional power asymmetry keeps it contained.
 *
 * PERSPECTIVAL GAP:
 *   The NPCSC seat experiences the constraint as necessary coordination restoring constitutional order; the Hong Kong judiciary and legal profession seats experience the same text as extraction of their institutional identity. The engine computes this divergence from identical structural data because beneficiary/victim declarations push directionalities to opposite ends of the spectrum despite both seats holding institutional power. The identity_locked exit of the legal profession amplifies their effective extraction relative to their nominal power.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainland security organs are declared beneficiaries with institutional power but constrained exit (they operate inside the Party-state), producing a low directionality (near-beneficiary). The Hong Kong judiciary and legal profession are declared victims with identity_locked exit (professional identity fused with common law traditions that are being abolished), producing a high directionality (near-target). The NPCSC is agenda_setter, not beneficiary; its structural relationship is administrative rather than extractive. Effective extraction is therefore amplified for the HK seats and damped for the mainland security seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare prevents mislabeling the genuine coordination function that the NSL performs for the central state (security governance integration) while still registering the asymmetric extraction visited upon Hong Kong institutions. A pure snare classification would miss the real coordination problem (fragmented security jurisdiction across a sovereign boundary) that the constraint's defenders sincerely address. A pure rope classification would miss the capture. Tangled_rope is the only category that structurally requires both coordination beneficiaries and extraction victims with active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the Hong Kong judiciary''s compliance with NSL procedures driven by structural coercion (NPCSC override threats, political appointment pressure) or by internalized identity collapse (judges reconceptualizing their role as subordinate to national security primacy)?',
    'Post-decision judicial behavior analysis: if suppression of common law reasoning persists even when explicit NPCSC threats are absent, reclassify as partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s hold on the judiciary is deeper than external enforcement alone would indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism for HK judiciary').

omega_variable(
    kernel_reading_validity,
    'Which reading of the NSL kernel captures its operative structure â jurisdictional capture, democratic enclosure, or sovereignty restoration?',
    'Longitudinal empirical tracking of case outcomes: if prosecutions concentrate on political opposition, enclosure dominates; if procedural mainlandization outpaces case volume, capture dominates; if prosecutions are rare and symbolic, restoration framing gains support.',
    'Determines which Îµ and victim set is structurally true; misidentification would misallocate the extraction seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Which kernel reading matches operative structure').

omega_variable(
    common_law_resilience_trajectory,
    'Will Hong Kong courts develop doctrinal resistance (common law evasion techniques) that slows transplantation, or will full mainland procedural assimilation occur?',
    'Tracking NSL case reports for survival of common law evidentiary and procedural doctrines over a 5-year window.',
    'If resilience is high, base_extractiveness may plateau below current trajectory; if collapse is rapid, the constraint approaches snare-like severity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(common_law_resilience_trajectory, empirical, 'Whether HK common law institutions resist or assimilate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nsl__tr_t6, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(nsl__tr_t18, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(nsl__tr_t30, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(nsl__tr_t36, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 36, 0.45).
narrative_ontology:measurement(nsl__tr_t42, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 42, 0.47).
narrative_ontology:measurement(nsl__tr_t48, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 48, 0.48).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(nsl__be_t6, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(nsl__be_t18, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(nsl__be_t30, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 30, 0.69).
narrative_ontology:measurement(nsl__be_t36, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 36, 0.72).
narrative_ontology:measurement(nsl__be_t42, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 42, 0.74).
narrative_ontology:measurement(nsl__be_t48, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 48, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(nsl__su_t6, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(nsl__su_t18, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(nsl__su_t30, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(nsl__su_t36, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 36, 0.77).
narrative_ontology:measurement(nsl__su_t42, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 42, 0.78).
narrative_ontology:measurement(nsl__su_t48, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 48, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested NSL kernel, decomposed per the Îµ-invariance principle from the sovereignty_restoration and democratic_enclosure readings. The jurisdictional_capture reading isolates the institutional-transplantation claim; the democratic_enclosure reading isolates the political-closure claim; the sovereignty_restoration reading isolates the constitutional-legitimacy claim. Each carries distinct Îµ, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
