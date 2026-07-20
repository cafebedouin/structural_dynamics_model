% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3 Negative Liberty Reading
 *   domain: constitutional law / human rights / political philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the negative_liberty_reading of the
 *   udhr_article_3 kernel, interpreting Article 3 of the Universal
 *   Declaration of Human Rights as a prohibition on state deprivation of
 *   life, liberty, and security of person except through narrow procedural
 *   justice. Security is read as freedom from state violence, not material
 *   welfare. The reading imposes high extraction on executive security
 *   agencies through capital punishment abolition, restrictive self-defense
 *   doctrine, and expansive due process requirements. It is one of three
 *   structurally distinct readings of a contested kernel; sibling readings
 *   include the positive_entitlement_reading (state provision of welfare) and
 *   the procedural_hybrid_reading (pure due process without resolving the
 *   liberty/welfare contest).
 *
 * KEY AGENTS:
 *   - individual_rights_holders: Primary beneficiary (powerless/constrained) â receive protection from arbitrary state violence.
 *   - executive_security_agencies: Primary target (institutional/constrained) â bear the operational and legal costs of due process and restraint.
 *   - domestic_judiciaries: Agenda setter (institutional/analytical) â administers and enforces the procedural constraints.
 *   - international_human_rights_bodies: Analytical observer (institutional/analytical) â monitors compliance and elaborates the reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.72).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.68).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3 Negative Liberty Reading").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional law / human rights / political philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, 'dc64de4f-372d-4586-8484-bd4775200a6f').
narrative_ontology:cs_kernel_codification('dc64de4f-372d-4586-8484-bd4775200a6f', fixed_text).
narrative_ontology:cs_authority_grounding('dc64de4f-372d-4586-8484-bd4775200a6f', lineage).
narrative_ontology:cs_interpretation_layer_present('dc64de4f-372d-4586-8484-bd4775200a6f').
narrative_ontology:cs_reading_relation('dc64de4f-372d-4586-8484-bd4775200a6f', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc64de4f-372d-4586-8484-bd4775200a6f', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('dc64de4f-372d-4586-8484-bd4775200a6f', foundational, life_as_non_deprivation).
narrative_ontology:cs_axiom_status(life_as_non_deprivation, holdable).
narrative_ontology:cs_axiom_grounding('dc64de4f-372d-4586-8484-bd4775200a6f', life_as_non_deprivation, deontological).
narrative_ontology:cs_axiom('dc64de4f-372d-4586-8484-bd4775200a6f', foundational, security_as_absence_of_state_violence).
narrative_ontology:cs_axiom_status(security_as_absence_of_state_violence, holdable).
narrative_ontology:cs_axiom_grounding('dc64de4f-372d-4586-8484-bd4775200a6f', security_as_absence_of_state_violence, deontological).
narrative_ontology:cs_reference_frame('dc64de4f-372d-4586-8484-bd4775200a6f', classical_liberal_negative_liberty).
narrative_ontology:cs_drift_state('dc64de4f-372d-4586-8484-bd4775200a6f', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc64de4f-372d-4586-8484-bd4775200a6f', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individual_rights_holders).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, executive_security_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary persons within state jurisdiction who receive legal protection against arbitrary killing, torture, and detention. They depend on courts and habeas process to restrain executive violence and cannot practically exit the legal systems that bind them.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individual_rights_holders, beneficiary,
    powerless, biographical, constrained, global).

% Police, military, and intelligence services required to conduct security operations under strict procedural constraints, evidentiary standards, and judicial oversight. They bear the operational costs of due process, torture prohibitions, and capital punishment abolition.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, executive_security_agencies, payer,
    institutional, immediate, constrained, national).

% Courts and judges that enforce Article 3 through habeas corpus, criminal procedure, and constitutional review. They set the boundaries of permissible state violence and interpret the procedural justice requirements that constrain executive action.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, domestic_judiciaries, agenda_setter,
    institutional, generational, analytical, national).

% Treaty monitoring bodies and special rapporteurs that review state compliance, issue general comments, and adjudicate individual complaints against states. They observe whether state practice conforms to the negative liberty reading.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a predictable legal boundary between the individual and state violence by requiring procedural justice before any deprivation of life or liberty, thereby preventing arbitrary execution, torture, and disappearance.
% TRANSFER_FUNCTION: Transfers security discretion and operational latitude from executive agencies to judicial and procedural oversight mechanisms; individual persons receive liberty from arbitrary state action in exchange for due process burdens borne by the state.
% ABSENT_VOICES: Victims of serious crime and terrorism who might prefer more aggressive security measures; majoritarian communities seeking retributive justice through capital punishment; security officials advocating for extraordinary rendition or enhanced interrogation. They are present in policy debate but structurally disadvantaged when courts enforce the negative-liberty reading.
% DISAPPEARANCE_RATIONALE: If the negative liberty reading vanished overnight, states would revert to extrajudicial killing, arbitrary detention, and torture as standard security tools. The global human rights architecture built on habeas corpus and due process would collapse, and the relationship between the individual and the state would fundamentally reorganize around unfettered executive violence.
% FOUNDING_PROBLEM: The twentieth century demonstrated that unrestrained state violence against individuals â arbitrary execution, disappearance, and torture â produces mass atrocity and destroys the legal personhood necessary for any other right to exist.
% FOUNDING_PROBLEM_CORROBORATION: Transitional justice commissions and historical records of totalitarian regimes attest the founding problem from outside the immediate beneficiary set. Security studies scholars and some state parties contest its current magnitude, arguing that asymmetric threats justify restored executive discretion, corroborating the 'contested' status from a non-beneficiary seat.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the reading substantially restricts security discretion, abolishes capital punishment in many jurisdictions, and imposes expansive habeas and due process obligations that exceed minimal procedural floors. Suppression (0.68) reflects the active force required to suppress alternative security paradigms such as extrajudicial killing, torture, and arbitrary detention, which persist as live temptations for security agencies. Theater ratio (0.40) acknowledges that some state compliance is performative â reports to treaty bodies, formal habeas filings â while arbitrary detention continues in shadow systems. Accessibility collapse (0.60) is moderate: once the reading is adopted, legal alternatives to due process partially collapse, but extralegal alternatives (black sites, extraordinary rendition) remain accessible to determined executives. Resistance (0.70) is high because security agencies and authoritarian states actively contest the constraint through noncompliance, reservation, and derogation.
 *
 * PERSPECTIVAL GAP:
 *   Individual rights holders experience the constraint as essential protective coordination; executive security agencies experience the identical legal structure as extractive impediment to operational effectiveness. Domestic judiciaries sit closer to symmetric because they are simultaneously empowered by the interpretive role and constrained by their own procedural obligations. The engine computes this divergence from the structural data rather than from any authored reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual_rights_holders are declared beneficiaries and receive low directionality; their constrained exit amplifies the subsidy effect of the constraint. Executive_security_agencies are declared victims and receive high directionality; their institutional power is offset by legally constrained exit options that trap them within the procedural framework. Domestic_judiciaries are agenda_setters with analytical exit; the derivation places them near the low-target end, though their role is administrative rather than extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â unrestrained twentieth-century state violence â is contested rather than dead. If it were dead, the constraint would risk mandatrophy as a piton or snare. Because security apparatuses still challenge the norm and authoritarian backsliding is live, the coordination function retains structural justification, preventing misclassification as pure extraction. The contested status is the firewall against mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    udhr_article_3_kernel_contest,
    'Does Article 3 ground a negative liberty against state violence, a positive entitlement to welfare, or a purely procedural guarantee?',
    'Comparative constitutional analysis of state obligations under the ICESCR versus ICCPR; jurisprudential tracking of which reading dominant regional courts adopt over time.',
    'Resolves whether the constraint''s beneficiaries are individuals shielded from state action or individuals demanding state provision; changes the victim set from security agencies to taxpayers or legislative majorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(udhr_article_3_kernel_contest, conceptual, 'Kernel-level contest between negative liberty, positive entitlement, and procedural hybrid readings.').

omega_variable(
    security_extraction_necessity,
    'Is the due process burden on state security agencies a necessary cost of liberty coordination, or has it become extractive beyond the coordination function?',
    'Empirical study of security outcomes and rule-of-law indicators in jurisdictions with strict negative-liberty readings versus those with procedural-hybrid or security-heavy readings.',
    'If due process costs exceed the coordination benefit, the constraint slides toward snare; if proportionate, it remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_extraction_necessity, empirical, 'Whether procedural burdens are proportionate to the coordination function.').

omega_variable(
    collective_security_voice_exclusion,
    'Are collective security interests genuinely excluded by this reading, or do they find representation through procedural-hybrid readings in practice?',
    'Mapping of judicial opinions that incorporate security necessity as a procedural factor versus those that treat it as an illegitimate override.',
    'Determines whether the victim declaration is structurally accurate or overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_security_voice_exclusion, empirical, 'Accuracy of collective security victim status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_art3_neg_tr_t0, udhr_article_3__negative_liberty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(udhr_art3_neg_tr_t15, udhr_article_3__negative_liberty_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(udhr_art3_neg_tr_t30, udhr_article_3__negative_liberty_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(udhr_art3_neg_tr_t45, udhr_article_3__negative_liberty_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement(udhr_art3_neg_tr_t60, udhr_article_3__negative_liberty_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(udhr_art3_neg_tr_t75, udhr_article_3__negative_liberty_reading, theater_ratio, 75, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_art3_neg_be_t0, udhr_article_3__negative_liberty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(udhr_art3_neg_be_t15, udhr_article_3__negative_liberty_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(udhr_art3_neg_be_t30, udhr_article_3__negative_liberty_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(udhr_art3_neg_be_t45, udhr_article_3__negative_liberty_reading, base_extractiveness, 45, 0.65).
narrative_ontology:measurement(udhr_art3_neg_be_t60, udhr_article_3__negative_liberty_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(udhr_art3_neg_be_t75, udhr_article_3__negative_liberty_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(udhr_art3_neg_su_t0, udhr_article_3__negative_liberty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(udhr_art3_neg_su_t15, udhr_article_3__negative_liberty_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(udhr_art3_neg_su_t30, udhr_article_3__negative_liberty_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(udhr_art3_neg_su_t45, udhr_article_3__negative_liberty_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement(udhr_art3_neg_su_t60, udhr_article_3__negative_liberty_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement(udhr_art3_neg_su_t75, udhr_article_3__negative_liberty_reading, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% The udhr_article_3 kernel decomposes into three structurally distinct constraints: negative_liberty_reading (substantive liberty from state violence), positive_entitlement_reading (state provision of welfare), and procedural_hybrid_reading (due process without resolving the liberty/welfare contest). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
