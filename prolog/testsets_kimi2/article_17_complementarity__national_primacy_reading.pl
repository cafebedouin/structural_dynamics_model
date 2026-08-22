% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity â National Primacy Reading
 *   domain: international_law/criminal_justice
 *
 * SUMMARY:
 *   This constraint story instantiates the national_primacy_reading of the
 *   article_17_complementarity kernel. Under this reading, Article 17 of the
 *   Rome Statute operates as a sovereignty-protection mechanism: national
 *   courts are presumptively adequate to prosecute atrocity crimes, and the
 *   ICC bears the burden of proving a state's unwillingness or inability
 *   before a case is admissible. The arrangement coordinates the relationship
 *   between international and national criminal jurisdictions but
 *   asymmetrically extracts access to justice from victims in states with
 *   weak-but-genuine proceedings. The structural delta from the sibling
 *   international_oversight_reading is a high admissibility threshold, a
 *   restricted victim set, and state cooperation prioritized over victim
 *   access.
 *
 * KEY AGENTS:
 *   - sovereignty_maximizing_states: Primary beneficiary/agenda_setter (institutional/constrained) â shields domestic proceedings from ICC review
 *   - national_judiciaries: Primary beneficiary (institutional/constrained) â retains territorial jurisdiction and procedural control
 *   - victims_in_weak_state_proceedings: Primary target (powerless/trapped) â denied ICC recourse by presumption of adequacy
 *   - icc_prosecutor: Administrative agenda-setter (institutional/constrained) â bears burden of proving inadmissibility
 *   - international_civil_society: Analytical observer (organized/mobile) â monitors and contests the high threshold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.58).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.55).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity â National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '00011f38-a0ce-47b3-b35a-eba1fc006898').
narrative_ontology:cs_kernel_codification('00011f38-a0ce-47b3-b35a-eba1fc006898', formalized).
narrative_ontology:cs_authority_grounding('00011f38-a0ce-47b3-b35a-eba1fc006898', lineage).
narrative_ontology:cs_interpretation_layer_present('00011f38-a0ce-47b3-b35a-eba1fc006898').
narrative_ontology:cs_reading_relation('00011f38-a0ce-47b3-b35a-eba1fc006898', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('00011f38-a0ce-47b3-b35a-eba1fc006898', foundational, national_primacy_as_default_locus).
narrative_ontology:cs_axiom_status(national_primacy_as_default_locus, holdable).
narrative_ontology:cs_axiom_grounding('00011f38-a0ce-47b3-b35a-eba1fc006898', national_primacy_as_default_locus, conventional).
narrative_ontology:cs_axiom('00011f38-a0ce-47b3-b35a-eba1fc006898', foundational, high_threshold_for_inability).
narrative_ontology:cs_axiom_status(high_threshold_for_inability, holdable).
narrative_ontology:cs_axiom_grounding('00011f38-a0ce-47b3-b35a-eba1fc006898', high_threshold_for_inability, conventional).
narrative_ontology:cs_reference_frame('00011f38-a0ce-47b3-b35a-eba1fc006898', state_sovereignty_primacy_norm).
narrative_ontology:cs_drift_state('00011f38-a0ce-47b3-b35a-eba1fc006898', contemporary_icc_practice, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('00011f38-a0ce-47b3-b35a-eba1fc006898', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_weak_state_proceedings).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, subsidiarity_in_international_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Presumed competent under the Rome Statute to investigate and prosecute atrocity crimes. Shielded from ICC intervention unless proceedings are proven sham. Retain domestic control over evidence, witnesses, and sentencing.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, constrained, national).

% Negotiated and uphold the sovereignty-preserving architecture of the Rome Statute. Invoke complementarity to challenge ICC admissibility, prioritize territorial jurisdiction, and withhold cooperation when proceedings are challenged as inadequate.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, agenda_setter,
    institutional, civilizational, constrained, national).

% Survivors of atrocity crimes in states where national proceedings are technically active but substantively weak or partial. Barred from ICC access by the presumption of national adequacy; lack leverage to compel genuine domestic prosecution or to trigger international review.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_in_weak_state_proceedings, payer,
    powerless, immediate, trapped, local).

% Bears the burden of proving inadmissibility under Article 17. Must overcome the presumption of national adequacy and demonstrate unwillingness or inability to a high threshold before opening or continuing an investigation.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_prosecutor, agenda_setter,
    institutional, generational, constrained, global).

% Monitors state proceedings and ICC admissibility practice; advocates for broader victim access and against sovereignty shields. Files amicus curiae briefs but is not a formal party to admissibility determinations.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_civil_society, observer,
    organized, generational, mobile, global).

narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the ICC from being overwhelmed by cases that national courts can plausibly handle; preserves state sovereignty by establishing national criminal jurisdiction as the default forum for atrocity crimes.
% TRANSFER_FUNCTION: Transfers the formal responsibility for investigating and prosecuting atrocity crimes from the international plane to national judiciaries, and transfers access to international justice away from victims whenever national proceedings exist, however weak.
% ABSENT_VOICES: Victims in states with weak-but-technically-active proceedings are structurally sidelined in admissibility hearings; their testimony is not formally required for the ICC to find a case inadmissible. International civil society advocates for lower thresholds but is often overruled by the presumption of adequacy.
% DISAPPEARANCE_RATIONALE: Without the national-primacy filter, the ICC's docket would expand dramatically as victims in weak-state proceedings gained direct access; state sovereignty would be substantially eroded as the default locus of prosecution shifted upward to The Hague; national judiciaries would lose their statutory shield from external review.
% FOUNDING_PROBLEM: How to create a permanent international criminal court without extinguishing state sovereignty and without overburdening a single global tribunal with every allegation of genocide, crimes against humanity, or war crimes.
% FOUNDING_PROBLEM_CORROBORATION: State parties and public international law scholars corroborate the sovereignty rationale as the original design intent. Independent human rights monitors, victim representatives, and ICC Office of the Prosecutor filings in admissibility proceedings attest that the arrangement is frequently invoked to shield non-cooperative states; corroboration from outside the benefiting parties supports the claim that the founding problem has been subverted into an impunity shield.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the presumption of national adequacy systematically denies ICC access to victims in weak states while preserving a genuine coordination function (preventing ICC overload and respecting sovereignty). Suppression (0.55) reflects the active legal enforcement of admissibility barriers through Pre-Trial Chamber litigation and state non-cooperation. Theater ratio (0.42) captures the growing sophistication of states in maintaining proceedings that are just sufficient to trigger complementarity without delivering substantive justice. Accessibility collapse (0.72) is high because once national proceedings existâhowever partialâthe alternative of ICC recourse collapses for victims. Resistance (0.48) reflects sustained advocacy by victim groups and international civil society against the high threshold. Metrics and claim are authored independently; the engine measures the divergence.
 *
 * PERSPECTIVAL GAP:
 *   Sovereignty-maximizing states and national judiciaries experience the constraint as a legitimate defense of territorial jurisdiction and institutional competence. Victims in weak-state proceedings experience it as a denial of access to international justice. The ICC Prosecutor occupies an intermediate seat: structurally constrained by the burden of proof, yet operationally empowered by the same legal framework in cases of total judicial collapse. The engine should compute divergent per-seat classifications from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (national_judiciaries, sovereignty_maximizing_states) sit near the low-d beneficiary pole: the constraint subsidizes their control over criminal process and shields them from external intervention. Victims_in_weak_state_proceedings sit near the high-d target pole: they bear the cost of the sovereignty shield through lost access to international justice. The ICC Prosecutor is not declared in either beneficiary or victim arrays and receives the canonical institutional fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by preserving the genuine coordination function (sovereignty protection, ICC docket management) alongside the asymmetric extraction (victim access denial). A snare classification would erase the coordination function and treat sovereignty as mere cover; a rope classification would ignore the victims excluded by weak proceedings. The tangled_rope typing is justified by the coexistence of both functions enforced through active admissibility litigation and state cooperation diplomacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Is the admissibility threshold under Article 17 a sovereignty-protection device or an impunity-enabling shield?',
    'Comparative analysis of ICC Pre-Trial Chamber admissibility rulings against the qualitative standard of ''genuine'' proceedings; state cooperation rates under competing interpretive frameworks.',
    'If resolved as impunity-shield, the victim set is larger than this reading admits and the constraint trends toward snare; if resolved as sovereignty-protection, the current extraction boundary holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Structural ambiguity between sovereignty and impunity in Article 17 interpretation.').

omega_variable(
    weak_proceeding_victim_access,
    'Do victims in states with technically active but substantively weak proceedings experience the complementarity regime as a systematic loss of access to justice?',
    'Empirical victim-surveys and case-load analysis of ICC OTP preliminary examinations closed due to national proceedings; outcome monitoring of domestic trials that triggered inadmissibility.',
    'If victim access loss is systematic, the asymmetric extraction component of this tangled rope is larger than the coordination component; may push reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_proceeding_victim_access, empirical, 'Empirical test of victim harm under weak domestic proceedings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arti_tr_t4, article_17_complementarity__national_primacy_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(arti_tr_t8, article_17_complementarity__national_primacy_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(arti_tr_t12, article_17_complementarity__national_primacy_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(arti_tr_t16, article_17_complementarity__national_primacy_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(arti_tr_t22, article_17_complementarity__national_primacy_reading, theater_ratio, 22, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(arti_be_t4, article_17_complementarity__national_primacy_reading, base_extractiveness, 4, 0.43).
narrative_ontology:measurement(arti_be_t8, article_17_complementarity__national_primacy_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(arti_be_t12, article_17_complementarity__national_primacy_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(arti_be_t16, article_17_complementarity__national_primacy_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(arti_be_t22, article_17_complementarity__national_primacy_reading, base_extractiveness, 22, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(arti_su_t4, article_17_complementarity__national_primacy_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(arti_su_t8, article_17_complementarity__national_primacy_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(arti_su_t12, article_17_complementarity__national_primacy_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(arti_su_t16, article_17_complementarity__national_primacy_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(arti_su_t22, article_17_complementarity__national_primacy_reading, suppression_requirement, 22, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling are dual readings of Article 17: national_primacy_reading treats the kernel as a sovereignty shield with high admissibility threshold, while international_oversight_reading treats it as an accountability trigger with low threshold. They share the same statutory text but instantiate structurally distinct constraints with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
