% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__categorical_balancing_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Categorical Balancing Doctrine (Judicial Gatekeeping)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The categorical balancing reading of the First Amendment holds that the
 *   judiciary has authority to define protected and unprotected speech
 *   categories by weighing the speech's constitutional value against the
 *   harms it causes. Obscenity, incitement to imminent lawless action, and
 *   true threats are excluded from protection; borderline cases are resolved
 *   through ad hoc balancing tests (strict scrutiny for content-based
 *   restrictions, intermediate scrutiny for content-neutral time/place/manner
 *   rules). The beneficiary is the federal judiciary, which maintains
 *   interpretive control and authority to refine categories. The victims are
 *   speakers in unprotected categories, who face legal unpredictability and
 *   disproportionate enforcement risk, and legal-risk-averse institutions
 *   that must absorb litigation costs. This reading COEXISTS with absolutist
 *   and harm-limited readings as live constitutional positions held by
 *   different judicial factions, scholars, and political movements — no
 *   single framework within American constitutionalism rules out the
 *   alternatives, though this reading's institutional dominance suppresses
 *   them from current doctrine.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Maintains categorical framework and gatekeeping authority; defines what counts as protected speech through precedent and case-by-case balancing
 *   - excluded_speech_categories: Powerless speakers whose speech receives no First Amendment protection once classified; bear full legal liability
 *   - legal_certainty_seekers: Publishers, broadcasters, and institutional speech actors forced to absorb legal risk and litigation costs as they navigate uncertain category boundaries
 *   - minority_speakers_in_unprotected_categories: Identity-locked speakers (radical, obscene, harassing) face disproportionate legal risk because their speech is more likely to be classified as unprotected
 *   - absolutist_reading_advocates: Excluded from doctrine; contest the judiciary's categorical authority and argue 'no law' is textually limiting
 *   - harm_prevention_advocates: Excluded from doctrine; argue harm-thresholds should replace categorical balancing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.62).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.58).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing Doctrine (Judicial Gatekeeping)").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, 'e0a3b86d-c881-4620-9d19-83299fce8bde').
narrative_ontology:cs_kernel_codification('e0a3b86d-c881-4620-9d19-83299fce8bde', fixed_text).
narrative_ontology:cs_authority_grounding('e0a3b86d-c881-4620-9d19-83299fce8bde', extraction).
narrative_ontology:cs_interpretation_layer_present('e0a3b86d-c881-4620-9d19-83299fce8bde').
narrative_ontology:cs_reading_relation('e0a3b86d-c881-4620-9d19-83299fce8bde', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0a3b86d-c881-4620-9d19-83299fce8bde', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('e0a3b86d-c881-4620-9d19-83299fce8bde', foundational, speech_value_harm_balancing_is_constitutionally_permissible).
narrative_ontology:cs_axiom_status(speech_value_harm_balancing_is_constitutionally_permissible, holdable).
narrative_ontology:cs_axiom_grounding('e0a3b86d-c881-4620-9d19-83299fce8bde', speech_value_harm_balancing_is_constitutionally_permissible, deontological).
narrative_ontology:cs_axiom('e0a3b86d-c881-4620-9d19-83299fce8bde', foundational, categorical_exclusions_do_not_violate_no_law_text).
narrative_ontology:cs_axiom_status(categorical_exclusions_do_not_violate_no_law_text, holdable).
narrative_ontology:cs_axiom_grounding('e0a3b86d-c881-4620-9d19-83299fce8bde', categorical_exclusions_do_not_violate_no_law_text, empirically_contingent).
narrative_ontology:cs_reference_frame('e0a3b86d-c881-4620-9d19-83299fce8bde', judicial_categorical_framework_for_speech_protection).
narrative_ontology:cs_drift_state('e0a3b86d-c881-4620-9d19-83299fce8bde', contemporary_doctrinal_equilibrium_2020s, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e0a3b86d-c881-4620-9d19-83299fce8bde', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_speech_governance_actors).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, excluded_speech_categories).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_certainty_seekers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers_in_unprotected_categories).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the judicial authority to impose category boundaries on speakers without their consent; speakers must either obey (self-censor), litigate (bear legal costs), or violate law (face prosecution). Suppression (0.58) is moderately high because the constraint's persistence depends on judicial enforcement of category boundaries and prosecution of unprotected speech; but it is not overwhelming because absolutist and harm-based alternatives remain plausible constitutional readings. Theater ratio (0.41) indicates that some categorical maintenance is performative — the judiciary articulates balancing rationales that shift over time (e.g., obscenity doctrine's evolution from Roth to Miller to contemporary digital contexts) while the fundamental category structure persists. Accessibility_collapse (0.48) is below 0.5 because alternatives remain; speakers can exit to other jurisdictions (functionally constrained), absolutists can litigate (constrained), and harm-prevention advocates can lobby legislatively (constrained but present). Resistance (0.72) is high because absolutist scholars, civil libertarians, and harm-prevention advocates actively contest categorical balancing; the doctrine's continuation requires sustained judicial defense against competing constitutional readings. Measurement series show extractiveness and theater rising from 1950 to ~2005, then plateauing — the doctrine crystallized as the judiciary settled on current category definitions (Miller obscenity test, Brandenburg incitement test, Virginia true-threats test) and further extraction accrues through increasingly creative application rather than doctrinal expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, categorical balancing is genuine coordination: it provides a stable framework enabling speech governance without suppressing protected speech. The judiciary sees categories as evolutionary, refined by case law, and subject to principled judicial review. From the speaker's seat, especially in unprotected or borderline categories, the same constraint operates as gatekeeping extraction: speakers must predict which side of an unstable category boundary they fall on, absorb litigation costs if they guess wrong, and accept that judicial reinterpretation can reclassify their speech retroactively. From the legal-certainty seeker's perspective, the constraint imposes costs (legal risk, compliance burdens) that would vanish under bright-line rules. The absolutist and harm-limited readings further diverge: absolutists see the categories as unconstitutional judicial overreach; harm-advocates see categories as insufficiently protective of harm victims. The engine should compute radically different directionalities from these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary: federal_judiciary (role=agenda_setter; they set category boundaries, maintain interpretive authority, collect doctrinal legitimacy, face near-zero legal risk). Victims: excluded_speech_categories (powerless; receive no protection once classified), legal_certainty_seekers (moderate power but constrained exit; forced to absorb litigation costs), minority_speakers_in_unprotected_categories (powerless + identity_locked; doubly constrained). The constraint extracts legal predictability and category sovereignty from speakers and transfers them to the judiciary. Institutional beneficiary (law enforcement, agencies) gains framework stability; legal-risk seekers pay costs. No directionality override is needed; the structural data (beneficiary/victim, power atoms, exit options) derives appropriate d values. Absolutists and harm-advocates are excluded from rule-setting, which is a structural fact captured by role=excluded, not overridden directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was genuine: early-20th-century doctrine offered no operational framework for distinguishing protected speech from suppressible speech, leaving the judiciary to decide ad hoc with no principled limits. Categorical balancing solved that: it provided a framework (identify categories, apply scrutiny). However, the founding problem's status is now contested. Absolutists (Justice Thomas, some originalists) argue the problem was illicit — no categories should exist and the solution (categorical balancing) is itself a violation. Harm-prevention advocates argue the founding problem persists (unprotected speech still causes harm) but categorical balancing fails to address it adequately. The current doctrine shows mandatrophy signals: the theater_ratio rose from 0.22 to 0.41 (performative maintenance increased as category boundaries ossified), and extractiveness plateaued (further gains come from application, not doctrinal expansion). The judiciary continues to articulate balancing rationales but is operating more conservatively, protecting institutional authority rather than refining categories. The constraint persists despite contested founding legitimacy because judicial gatekeeping power suppresses alternative readings from becoming law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_boundary_drift,
    'Are the category boundaries (obscenity, incitement, true threats) stable constitutional commitments, or doctrinal positions that shift with judicial composition and social values?',
    'Historical analysis of category definition changes (Miller replacing Roth; Brandenburg replacing Schenck; developments in true-threats doctrine after social media). Qualitative: track whether boundaries are described as ''discovered'' (stable, constitutional) or ''evolved'' (shifting, contingent).',
    'If boundaries drift with judicial politics, the constraint is an unstable extraction mechanism (targets bear predictability costs and disproportionate risk from doctrinal shifts). If boundaries are stable, the constraint is more coordinate — participants can rely on categorical framework. Current evidence favors drift: Miller replaced Roth, Brandenburg replaced Schenck, true-threats doctrine expanded post-internet.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_boundary_drift, empirical, 'Whether speech categories are stable constitutional commitments or shifting doctrinal positions.').

omega_variable(
    suppression_mechanism_internalization,
    'Is measured suppression (0.58) primarily structural (judicial gatekeeping prevents alternative readings) or internalized (speakers have learned to self-censor and accept judicial authority)?',
    'Post-doctrine suppression trajectory: if free-speech litigation increases (speakers test boundaries), suppression is structural and actively resisted. If speakers withdraw from risky speech categories even when legal barriers lower, suppression is internalized (targets carry learned constraint after formal barrier removal).',
    'If structural, removing judicial gatekeeping (adopting absolutism or harm-limits) might liberate speech. If internalized, speakers would continue self-censoring after formal regime change because they have internalized the categorical framework. Current evidence: litigation remains high, suggesting structural suppression dominates, but some self-censoring persists (speakers in borderline categories withdraw preemptively).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in speech regulation.').

omega_variable(
    kernel_reading_contingency,
    'Is categorical balancing the inevitable constitutional reading of the First Amendment, or a contingent choice among structurally distinct alternatives (absolutism, harm-limits)?',
    'Comparative constitutional law: jurisdictions with different speech frameworks (EU hate-speech laws, Canadian harm-based thresholds, absolute protections in some democracies) demonstrate that alternative readings are institutionally viable. Logical analysis: text of ''no law'' could support absolutism, and empirical harm arguments support harm-limits. Neither alternative is foreclosed by constitutional logic; both are held by credible constitutional scholars and some judges.',
    'If categorical balancing is contingent, the constraint''s type classification depends on which reading (and which parties'' seats) we evaluate. From the judiciary''s seat it is stable coordination; from absolutist and harm-based seats it is institutional extraction of constitutional authority. The engine''s per-seat classification should reveal this divergence. If categorical balancing is inevitable, the classification should be more uniform across seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether categorical balancing is the inevitable First Amendment reading or a contingent institutional choice.').

omega_variable(
    speech_value_measurement_problem,
    'Can ''speech value'' be measured in a principled way that justifies judicial balancing, or is judicial balancing an implicit assertion that some speech is worth less without principled grounds?',
    'Examine balancing test applications: do courts articulate principled speech-value measures (e.g., contribution to public discourse, individual autonomy interest), or do value judgments vary with judge and context? Logical analysis: if different judges assign different speech values to the same speech (e.g., political satire vs. crude harassment), the value metric lacks objectivity.',
    'If speech value is principled and measurable, balancing is a genuine coordination mechanism enabling judicial review of suppression claims. If unmeasurable, balancing is a cover story for judicial discretion to suppress disfavored speech. Current jurisprudence shows value assignments varying by judge and changing over time, suggesting the metric is unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_value_measurement_problem, empirical, 'Whether speech value can be measured in principled ways or serves as cover for judicial discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1950, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1950, 0.22).
narrative_ontology:measurement(firs_tr_t1975, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(firs_tr_t1990, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(firs_tr_t2005, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2005, 0.39).
narrative_ontology:measurement(firs_tr_t2015, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(firs_be_t1950, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement(firs_be_t1975, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(firs_be_t1990, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1990, 0.59).
narrative_ontology:measurement(firs_be_t2005, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement(firs_be_t2015, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1950, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement(firs_su_t1975, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1975, 0.48).
narrative_ontology:measurement(firs_su_t1990, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1990, 0.53).
narrative_ontology:measurement(firs_su_t2005, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2005, 0.57).
narrative_ontology:measurement(firs_su_t2015, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__categorical_balancing_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, obscenity_doctrine_miller_test).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, incitement_doctrine_brandenburg_test).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, true_threats_doctrine_virginia_jurisprudence).

% DUAL FORMULATION NOTE:
% The categorical_balancing reading is one of three structurally distinct instantiations of the first_amendment_speech_protection kernel. Sibling readings (absolutist_reading, harm_limited_reading) have different ε values, beneficiary/victim structures, and type classifications. All three should be consulted together to see how the same constitutional text grounds different constraints. The categorical_balancing reading influences downstream doctrine in obscenity, incitement, and true-threats constraints — those constraints inherit the category framework and apply it to specific speech types. Decomposition is driven by ε-invariance: the absolutist reading prohibits categories entirely (near-zero extracted authority from speakers), while categorical balancing extracts interpretive authority (moderate extraction); harm-limited reading extracts authority to suppress based on empirical harm (moderate-to-high extraction). One kernel, three readings, three different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__categorical_balancing_reading, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
