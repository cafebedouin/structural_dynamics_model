% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Reading of Usul al-Fiqh: Maximal Textual Restrictiveness
 *   domain: religious/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the Hanbali reading of the usul al-fiqh
 *   kernel: a jurisprudential method that maximizes the restrictiveness of
 *   Quranic and hadith sources, minimizes analogical reasoning (qiyas) to
 *   cases of explicit textual silence, prefers even weak hadith over qiyas,
 *   and deploys sadd al-dhara'i to block innovations. It is presented as a
 *   rope (preserving textual fidelity and communal continuity) but
 *   structurally operates as a tangled rope: it genuinely coordinates Islamic
 *   legal interpretation around a stable kernel while asymmetrically
 *   extracting epistemic authority from rationalist jurists and customary
 *   practitioners. The reading is one of four major madhhab-based readings of
 *   the same kernel; it is not the kernel itself.
 *
 * KEY AGENTS:
 *   - Hanbali textualist jurists: Primary agenda-setter (institutional/civilizational/identity_locked) â administer the method and capture interpretive authority
 *   - State authority: Primary beneficiary (powerful/generational/mobile) â derives governance legitimacy from textual fixity
 *   - Rationalist jurists: Primary payer (organized/generational/constrained) â delegitimized by restriction on qiyas and ra'y
 *   - Customary practitioners: Secondary payer (moderate/biographical/constrained) â local urf subordinated to hadith authentication
 *   - Rationalist theologians: Excluded voice (organized/generational/constrained) â structurally barred from source hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.72).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.8).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Reading of Usul al-Fiqh: Maximal Textual Restrictiveness").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "religious/legal_theory").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, '3a0c8b7e-9699-4ff0-b629-472be9ba875d').
narrative_ontology:cs_kernel_codification('3a0c8b7e-9699-4ff0-b629-472be9ba875d', fixed_text).
narrative_ontology:cs_authority_grounding('3a0c8b7e-9699-4ff0-b629-472be9ba875d', lineage).
narrative_ontology:cs_interpretation_layer_present('3a0c8b7e-9699-4ff0-b629-472be9ba875d').
narrative_ontology:cs_reading_relation('3a0c8b7e-9699-4ff0-b629-472be9ba875d', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a0c8b7e-9699-4ff0-b629-472be9ba875d', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a0c8b7e-9699-4ff0-b629-472be9ba875d', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('3a0c8b7e-9699-4ff0-b629-472be9ba875d', foundational, weak_hadith_preferred_over_qiyas).
narrative_ontology:cs_axiom_status(weak_hadith_preferred_over_qiyas, holdable).
narrative_ontology:cs_axiom_grounding('3a0c8b7e-9699-4ff0-b629-472be9ba875d', weak_hadith_preferred_over_qiyas, conventional).
narrative_ontology:cs_axiom('3a0c8b7e-9699-4ff0-b629-472be9ba875d', foundational, innovation_blocking_mandatory).
narrative_ontology:cs_axiom_status(innovation_blocking_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('3a0c8b7e-9699-4ff0-b629-472be9ba875d', innovation_blocking_mandatory, conventional).
narrative_ontology:cs_reference_frame('3a0c8b7e-9699-4ff0-b629-472be9ba875d', scriptural_sovereignty).
narrative_ontology:cs_drift_state('3a0c8b7e-9699-4ff0-b629-472be9ba875d', contemporary_reformist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3a0c8b7e-9699-4ff0-b629-472be9ba875d', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, state_authority).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_practitioners).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, textual_fidelity_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, sadd_al_dhara_i_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the usul al-fiqh method that treats the Quran and authenticated hadith as maximally restrictive legal sources, minimizes qiyas to rare textual silences, prefers even weak hadith over analogy, and deploys sadd al-dhara'i to block innovations. Their scholarly authority, institutional prestige, and collective identity are fused to this textualist frame; abandoning it would dissolve their juridical seat.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_jurists, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Derives governance legitimacy from a fixed, ostensibly unchanging textual kernel. The Hanbali method reduces jurisdictional ambiguity and lowers the political cost of enforcing uniform law across diverse populations by outsourcing normative change to scriptural hermeneutics rather than legislative discretion.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, state_authority, beneficiary,
    powerful, generational, mobile, national).

% Bear the epistemic and institutional costs of a method that delegitimizes qiyas, ra'y, istihsan, and systematic reasoning as independent legal sources. Their interpretive toolkit is treated as presumptive innovation; they must either adopt the textualist frame or accept marginalization in jurisprudential discourse and state appointments.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    organized, generational, constrained, global).

% Local legal customs, urf, and regional praxis are systematically subordinated to authenticated hadith. Their adjudicative practices are treated as suspect innovations unless explicitly anchored in textual precedent, forcing either abandonment of custom or chronic defensive justification.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_practitioners, payer,
    moderate, biographical, constrained, regional).

% Would argue that reason (ra'y), systematic theology (kalam), and expansive qiyas are epistemically necessary for legal derivation. They are structurally excluded because the Hanbali source hierarchy disqualifies their contributions a priori; their objections are classified as breaches of textual fidelity rather than valid dissent.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_theologians, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_jurists).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves normative continuity across generations by binding legal derivation to a fixed scriptural kernel, preventing unregulated innovation (bid'a) and arbitrary judicial discretion in religious practice.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional prestige from rationalist jurists and customary practitioners to textualist scholars and state actors who derive legitimacy from scriptural fixity.
% ABSENT_VOICES: Rationalist theologians who hold that reason should inform law; Maliki jurists who treat Medinan practice and urf as autonomous sources; Hanafi jurists who defend expansive qiyas and istihsan. They are excluded because the method's source hierarchy structurally disqualifies their evidentiary categories.
% DISAPPEARANCE_RATIONALE: If the Hanbali usul method vanished, rationalist jurists would expand qiyas and ra'y, customary law would reassert regional autonomy, and the textualist monopoly on legitimate interpretation would dissolve. The Islamic legal field would reorganize around plural methodologies, and state legitimacy would need a new grounding.
% FOUNDING_PROBLEM: The early Islamic community faced unregulated innovation and inconsistent legal rulings as it expanded beyond Arabia; it required a stable, textually anchored method to preserve continuity and prevent arbitrary judicial discretion.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars attest the problem remains live, citing contemporary innovations. Rationalist jurists, historians, and reformist scholars outside the beneficiary set attest that the founding problem of unregulated discretion was substantially resolved by the 3rd-4th centuries AH and the method now functions primarily as authority preservation; no independent non-textualist corroboration supports the live-problem claim.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the method restricts legal development beyond what the textual kernel alone requires, concentrating authority in the textualist scholarly class. Suppression (0.80) is higher still because the constraint's persistence depends on actively marginalizing rationalist methodology and customary law, not on voluntary adherence. Theater ratio (0.42) reflects moderate performative maintenance: citation of weak hadith and formalistic sadd al-dhara'i often function as ritual demonstrations of textual fidelity rather than substantive legal reasoning. Accessibility collapse (0.85) is high because once the textualist frame is accepted, rationalist alternatives appear as innovation rather than valid method. Resistance (0.58) is moderate: rationalist scholars and reformists mount sustained epistemic and institutional resistance, but are structurally disadvantaged.
 *
 * PERSPECTIVAL GAP:
 *   From the Hanbali jurist's seat, the arrangement is necessary coordination: without maximal textual restrictiveness, the community drifts into innovation and normative chaos. From the rationalist jurist's seat, the same structure is epistemic extraction: a scholarly monopoly that uses the language of fidelity to suppress competing methodologies. The engine computes this divergence from the structural asymmetry in exit options (identity_locked agenda-setters vs constrained payers) and the beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali textualist jurists are structural beneficiaries (low d): the constraint subsidizes their authority and locks in their interpretive monopoly. State authority is also a beneficiary (low-to-mid d): it gains stability at low political cost. Rationalist jurists and customary practitioners are targets (high d): the constraint extracts from them by delegitimizing their methods and restricting their institutional access. The excluded rationalist theologians sit at the far target end, effectively outside the coordination entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â unregulated innovation in the early expanding community â is contested as live or dead. If dead, the method persists as a zombie coordination mechanism, which would push toward piton. However, the method still delivers genuine coordination (textual continuity, anti-innovation boundary maintenance) that parties value independently of extraction, preventing a clean mandatrophy resolution. The contested status is itself signal that the constraint's coordination story is partially operational and partially cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    weak_hadith_epistemic_status,
    'Does the preference for weak hadith over qiyas reflect an epistemically defensible source hierarchy, or does it function primarily to constrain rationalist jurists and preserve textualist institutional authority?',
    'Comparative epistemic analysis of hadith criticism versus analogical reasoning accuracy in legal prediction; historical sociology of the Hanbali school''s institutional formation and resource flows.',
    'If the hierarchy is epistemically arbitrary, the constraint''s extraction component dominates its coordination function, increasing effective extractiveness and tightening the tangled rope classification; if defensible, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_hadith_epistemic_status, conceptual, 'Whether weak-hadith preference is epistemic or constructed extraction').

omega_variable(
    kernel_reading_contest,
    'To what extent does the Hanbali reading''s maximal restrictiveness foreclose legitimate legal development that sibling readings (Hanafi, Maliki) would permit without contradiction?',
    'Cross-madhhab comparison of legal outcomes on identical cases; analysis of whether the restrictive method produces systematic under-supply of legal remedies or excessive hardship.',
    'If restrictiveness systematically under-supplies remedies, the victim category expands and the constraint''s effective extraction rises; if remedies remain sufficient through text alone, the coordination story is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether Hanbali restrictiveness is one coherent reading among many or a closure device').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_hanbali_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(usul_hanbali_tr_t240, usul_al_fiqh_method__hanbali_reading, theater_ratio, 240, 0.25).
narrative_ontology:measurement(usul_hanbali_tr_t480, usul_al_fiqh_method__hanbali_reading, theater_ratio, 480, 0.3).
narrative_ontology:measurement(usul_hanbali_tr_t720, usul_al_fiqh_method__hanbali_reading, theater_ratio, 720, 0.35).
narrative_ontology:measurement(usul_hanbali_tr_t960, usul_al_fiqh_method__hanbali_reading, theater_ratio, 960, 0.4).
narrative_ontology:measurement(usul_hanbali_tr_t1200, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1200, 0.42).

% Extraction over time
narrative_ontology:measurement(usul_hanbali_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(usul_hanbali_be_t240, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 240, 0.52).
narrative_ontology:measurement(usul_hanbali_be_t480, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 480, 0.58).
narrative_ontology:measurement(usul_hanbali_be_t720, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 720, 0.63).
narrative_ontology:measurement(usul_hanbali_be_t960, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 960, 0.68).
narrative_ontology:measurement(usul_hanbali_be_t1200, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1200, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(usul_hanbali_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(usul_hanbali_su_t240, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 240, 0.6).
narrative_ontology:measurement(usul_hanbali_su_t480, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 480, 0.65).
narrative_ontology:measurement(usul_hanbali_su_t720, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 720, 0.7).
narrative_ontology:measurement(usul_hanbali_su_t960, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 960, 0.75).
narrative_ontology:measurement(usul_hanbali_su_t1200, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1200, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'usul al-fiqh method' conflates four structurally distinct madhhab-based readings. Each reading has a different source hierarchy, different epsilon, different beneficiary/victim structure, and different classification. They are modeled as a constraint family linked by affects_constraints, not as one constraint with adjustable parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
