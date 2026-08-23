% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 â Proportionality Balancing Reading
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint story captures the proportionality_balancing reading of
 *   Common Article 3 of the Geneva Conventions. Under this reading, CA3 does
 *   not absolutely prohibit harsh treatment (the absolute_prohibition sibling
 *   reading) nor does it permit unlimited state discretion (the
 *   contextual_necessity sibling reading). Instead, it requires domestic
 *   courts to balance detainee dignity against state security needs
 *   case-by-case. The reading produces a judicial gatekeeping structure that
 *   coordinates state security operations within legal bounds while
 *   extracting dignity costs from detainees when the balance tips toward
 *   security. The story is authored as a kernel reading (Rule 1): only the
 *   proportionality_balancing constraint is modeled here, with sibling
 *   relationships routed to omega variables and cs_structure.
 *
 * KEY AGENTS:
 *   - detaining_state: Primary beneficiary (institutional/constrained) â gains legal flexibility and operational cover
 *   - detainees: Primary target (powerless/trapped) â bear dignity costs of the balance
 *   - domestic_judiciary: Agenda-setter (institutional/constrained) â administers the proportionality test
 *   - human_rights_organizations: Observer (organized/analytical) â monitor from outside
 *   - international_courts: Observer (institutional/analytical) â provide overarching interpretive guidance
 *   - detainee_advocates: Excluded (moderate/constrained) â would argue for absolute prohibition but lack access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.66).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.68).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.66).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 â Proportionality Balancing Reading").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, 'ca562903-8908-4706-b0c5-e64daebdd1b0').
narrative_ontology:cs_kernel_codification('ca562903-8908-4706-b0c5-e64daebdd1b0', formalized).
narrative_ontology:cs_authority_grounding('ca562903-8908-4706-b0c5-e64daebdd1b0', lineage).
narrative_ontology:cs_interpretation_layer_present('ca562903-8908-4706-b0c5-e64daebdd1b0').
narrative_ontology:cs_reading_relation('ca562903-8908-4706-b0c5-e64daebdd1b0', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('ca562903-8908-4706-b0c5-e64daebdd1b0', humane_treatment_standard__contextual_necessity, coexists_with).
narrative_ontology:cs_axiom('ca562903-8908-4706-b0c5-e64daebdd1b0', foundational, proportionality_governs_humanitarian_limit).
narrative_ontology:cs_axiom_status(proportionality_governs_humanitarian_limit, holdable).
narrative_ontology:cs_axiom_grounding('ca562903-8908-4706-b0c5-e64daebdd1b0', proportionality_governs_humanitarian_limit, conventional).
narrative_ontology:cs_reference_frame('ca562903-8908-4706-b0c5-e64daebdd1b0', humanitarian_minimum_proportionality).
narrative_ontology:cs_drift_state('ca562903-8908-4706-b0c5-e64daebdd1b0', contemporary_security_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ca562903-8908-4706-b0c5-e64daebdd1b0', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, detaining_state).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains legal flexibility to conduct security operations and interrogations under a framework that legitimizes proportionally justified treatment limits. Benefits from judicial deference in security contexts while claiming compliance with international humanitarian law.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detaining_state, beneficiary,
    institutional, generational, constrained, national).

% Held in detention without ability to exit; their dignity and treatment standards are subject to judicial balancing against state security claims. Bear the direct cost when courts find security imperatives outweigh dignity interests.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees, payer,
    powerless, immediate, trapped, local).

% Interprets and applies the proportionality test case-by-case, functioning as the gatekeeper between detainee dignity claims and state security assertions. Derives institutional authority from its role as the balancing forum, though constrained by legal precedent and state pressure.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, domestic_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Monitor and litigate against disproportionate treatment, advocating for a stricter reading of Common Article 3. They observe the constraint's operation from outside the judicial gatekeeper structure, documenting when the balance tips toward extraction.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_organizations, observer,
    organized, generational, analytical, global).

% Provide overarching interpretive guidance on Common Article 3, reviewing whether domestic proportionality assessments meet international standards. They observe and correct, but do not directly administer the balancing test for individual detainees.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_courts, observer,
    institutional, civilizational, analytical, global).

% Would argue for absolute prohibition and against the proportionality framework, but are frequently denied access to detainees or excluded from classified security proceedings where the balancing actually occurs.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainee_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__proportionality_balancing, detaining_state).
narrative_ontology:fixing_cost_class(humane_treatment_standard__proportionality_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal framework for non-international armed conflicts that permits states to conduct security operations and detain enemies while maintaining minimum humanitarian standards through judicial oversight, avoiding both lawless discretion and operationally paralyzing absolutism.
% TRANSFER_FUNCTION: Moves discretion over the permissibility of treatment from an absolute prohibition rule to a case-by-case judicial balancing test; moves dignity costs from the state to detainees when security imperatives are weighed heavily.
% ABSENT_VOICES: Detainees are physically present but procedurally excluded from the security-classified proceedings that determine their treatment; advocates for absolute prohibition are structurally sidelined by the proportionality framework's dominance in judicial discourse.
% DISAPPEARANCE_RATIONALE: If the proportionality balancing reading vanished, states would face either absolute prohibition (which many reject as operationally unworkable) or unlimited discretion (which dissolves IHL protections); domestic judicial review structures would lose their gatekeeping function and interrogation practices would reorganize around whichever alternative reading filled the vacuum.
% FOUNDING_PROBLEM: How to restrain state violence and abuse against detainees in non-international armed conflict without granting states unlimited discretion or rendering legitimate security operations impossible.
% FOUNDING_PROBLEM_CORROBORATION: The International Committee of the Red Cross and human rights organizations attest that unchecked state violence against detainees remains a live problem, though they contest that proportionality balancing is the appropriate remedy, advocating instead for absolute prohibition. Independent legal scholars outside state security apparatuses corroborate the persistence of the founding problem but dispute the proportionality solution.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.66, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66) is substantial because the framework structurally permits treatment that would be barred under absolute prohibition whenever a court accepts a security justification; the detainee's dignity is the variable being traded. Suppression (0.68) reflects both the physical trapping of detainees and the marginalization of absolute prohibition discourse. Theater_ratio (0.56) captures the performative aspect of judicial review in security contexts, where procedural form may obscure substantive deference. Accessibility_collapse (0.72) is high because once a detainee is within the system, alternatives (absolute protection) collapse. Resistance (0.48) is moderate: human rights organizations actively contest specific outcomes but do not broadly reject the judicial framework itself. The temporal series show rising extractiveness and theater over the interval, consistent with drift toward security deference.
 *
 * PERSPECTIVAL GAP:
 *   The detaining state experiences the constraint as a coordination mechanism that legitimizes security operations; the domestic judiciary experiences it as institutional empowerment with genuine legal constraints; the detainee experiences it as conditional protection that may be withdrawn by judicial fiat. The engine computes these divergent seat classifications from the same structural data: beneficiary status plus constrained exit yields low directionality for the state, while victim status plus trapped exit yields high directionality for the detainee.
 *
 * DIRECTIONALITY LOGIC:
 *   Detaining_state is declared beneficiary with constrained exit: it gains operational flexibility and legal cover, placing it at the low-d end of the spectrum despite its institutional power. Detainees are declared victims with trapped exit and powerless status, placing them at the high-d end. Domestic_judiciary is not declared in either base_properties array; its directionality derives from agenda_setter role and constrained exit, landing near symmetric (it neither pays nor collects rents, but exercises authority). Human_rights_organizations and international_courts are observers with analytical exit, placing them outside the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â unchecked state violence against detainees â remains live, so the scaffold gate does not apply. However, the proportionality balancing reading risks drift toward a snare if judicial review becomes purely performative (high theater) or if the balance systematically resolves in favor of security. The temporal measurements show rising extractiveness and theater, suggesting accumulation rather than resolution. Classification as tangled_rope is warranted because the coordination function (preventing unlimited discretion) is genuine and distinct from the extraction function (detainee dignity as a balanceable interest).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_indeterminacy,
    'Does the proportionality balancing test provide a genuinely determinate constraint on state action, or does it function as a legal vocabulary that masks unreviewable state discretion?',
    'Comparative outcome analysis across jurisdictions: if similarly situated detainees receive radically different treatment depending on the reviewing court, the test is indeterminate and extraction is higher than the formal structure suggests.',
    'If indeterminate, the constraint reclassifies toward snare (pure extraction with judicial veneer); if determinate, it remains tangled_rope with bounded extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_indeterminacy, conceptual, 'Indeterminacy of the proportionality test as constraint or cover.').

omega_variable(
    judicial_capture_ambiguity,
    'Are domestic courts administering the proportionality test genuinely independent of state security interests, or are they structurally captured by the detaining state?',
    'Empirical analysis of judicial outcomes in security-deference regimes: high rates of state success in classified proceedings indicate capture; robust detainee victories indicate independence.',
    'Capture would push directionality for the judiciary toward the beneficiary end and increase effective extraction for detainees; independence would maintain the judiciary in a symmetric or coordinating position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_capture_ambiguity, empirical, 'Judicial independence versus state capture in security proceedings.').

omega_variable(
    kernel_reading_sibling_relationship,
    'This constraint is one reading of the humane_treatment_standard kernel; how would adoption of the absolute_prohibition or contextual_necessity sibling readings change the structural classification?',
    'Cross-reading comparison: the absolute_prohibition reading would eliminate the beneficiary asymmetry and likely classify as rope or mountain; the contextual_necessity reading would eliminate the judicial buffer and likely classify as snare.',
    'Confirms that the current classification as tangled_rope depends on the specific proportionality-balancing structural features (courts as gatekeepers, case-by-case weighing) that sibling readings remove.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_relationship, conceptual, 'Sibling reading structural delta and classification impact.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by detainees primarily structural (physical incarceration and legal exclusion) or internalized (learned helplessness and identification with the state''s security narrative)?',
    'Post-release behavior and testimony: if former detainees continue to act as if they have no rights or voice, suppression is partially internalized; if they immediately seek redress, it was structural.',
    'Internalized suppression raises effective extraction above the structural measure because the target carries the constraint beyond physical detention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for detainees.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__proportionality_balancing, theater_ratio, 0, 0.22).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__proportionality_balancing, theater_ratio, 10, 0.28).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__proportionality_balancing, theater_ratio, 20, 0.38).
narrative_ontology:measurement(huma_tr_t30, humane_treatment_standard__proportionality_balancing, theater_ratio, 30, 0.46).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__proportionality_balancing, theater_ratio, 40, 0.52).
narrative_ontology:measurement(huma_tr_t50, humane_treatment_standard__proportionality_balancing, theater_ratio, 50, 0.56).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__proportionality_balancing, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__proportionality_balancing, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(huma_be_t30, humane_treatment_standard__proportionality_balancing, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__proportionality_balancing, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(huma_be_t50, humane_treatment_standard__proportionality_balancing, base_extractiveness, 50, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__proportionality_balancing, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__proportionality_balancing, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(huma_su_t30, humane_treatment_standard__proportionality_balancing, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__proportionality_balancing, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(huma_su_t50, humane_treatment_standard__proportionality_balancing, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one member of the humane_treatment_standard kernel family. Sibling readings (absolute_prohibition, contextual_necessity) are separate constraints linked by shared kernel, not causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
