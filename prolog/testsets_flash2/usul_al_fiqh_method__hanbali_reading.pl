% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Hanbali Reading of Usul al-Fiqh: Textual Restrictiveness
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint represents the Hanbali reading of Usul al-Fiqh,
 *   emphasizing maximal restrictiveness to textual sources (Quran and
 *   authenticated Hadith), minimizing qiyas (analogical reasoning),
 *   preferring weak hadith over qiyas, and actively blocking innovations
 *   (sadd al-dhara'i) to preserve textual fidelity. It is one reading of the
 *   broader 'usul_al_fiqh_method' kernel. The claimed type is 'tangled_rope'
 *   because it provides a coordination function (methodological clarity) but
 *   also extracts by suppressing alternative interpretive approaches and
 *   benefiting a specific class of textualist scholars.
 *
 * KEY AGENTS:
 *   - textualist_scholars: Primary agenda-setter (institutional/identity_locked) — benefits from constraint
 *   - conservative_jurists: Beneficiary (organized/constrained) — benefits from constraint
 *   - rationalist_legal_developers: Primary payer (moderate/constrained) — bears extraction
 *   - customary_law_advocates: Payer (powerless/trapped) — bears extraction
 *   - innovative_muftis: Payer (moderate/constrained) — bears extraction
 *   - hanafi_scholars: Excluded (institutional/analytical) — would object but not in conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.75).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Reading of Usul al-Fiqh: Textual Restrictiveness").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, 'e613ba23-f5b6-499f-abc0-22b34f802332').
narrative_ontology:cs_kernel_codification('e613ba23-f5b6-499f-abc0-22b34f802332', fixed_text).
narrative_ontology:cs_authority_grounding('e613ba23-f5b6-499f-abc0-22b34f802332', lineage).
narrative_ontology:cs_interpretation_layer_present('e613ba23-f5b6-499f-abc0-22b34f802332').
narrative_ontology:cs_reading_relation('e613ba23-f5b6-499f-abc0-22b34f802332', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('e613ba23-f5b6-499f-abc0-22b34f802332', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('e613ba23-f5b6-499f-abc0-22b34f802332', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('e613ba23-f5b6-499f-abc0-22b34f802332', foundational, textual_primacy_over_reason).
narrative_ontology:cs_axiom_status(textual_primacy_over_reason, holdable).
narrative_ontology:cs_axiom_grounding('e613ba23-f5b6-499f-abc0-22b34f802332', textual_primacy_over_reason, deontological).
narrative_ontology:cs_axiom('e613ba23-f5b6-499f-abc0-22b34f802332', foundational, prevention_of_innovation_as_duty).
narrative_ontology:cs_axiom_status(prevention_of_innovation_as_duty, holdable).
narrative_ontology:cs_axiom_grounding('e613ba23-f5b6-499f-abc0-22b34f802332', prevention_of_innovation_as_duty, deontological).
narrative_ontology:cs_reference_frame('e613ba23-f5b6-499f-abc0-22b34f802332', early_islamic_textual_purity).
narrative_ontology:cs_drift_state('e613ba23-f5b6-499f-abc0-22b34f802332', contemporary_globalized_islam, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e613ba23-f5b6-499f-abc0-22b34f802332', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, conservative_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_legal_developers).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_law_advocates).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, innovative_muftis).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, textual_fidelity_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, blocking_means_to_evil_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere strictly to the Quran and authenticated Hadith, minimizing the role of human reason or local custom in legal derivation. They benefit from the authority derived from this perceived fidelity to foundational texts and actively enforce this methodology.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, textualist_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Find their interpretations and rulings legitimized by the Hanbali methodology's emphasis on textual sources and its skepticism towards innovation. They gain influence and authority by aligning with this restrictive approach.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, conservative_jurists, beneficiary,
    organized, biographical, constrained, regional).

% Seek to expand the scope of qiyas (analogical reasoning) or ra'y (reasoned opinion) to address contemporary issues not explicitly covered by foundational texts. They face significant resistance and delegitimization from the Hanbali framework, limiting their ability to innovate.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_legal_developers, payer,
    moderate, biographical, constrained, national).

% Represent local practices and customs ('urf) that may not have direct textual support. The Hanbali reading's strict textualism suppresses the integration of these customs into formal legal rulings, forcing them into informal spheres or outright rejection.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_law_advocates, payer,
    powerless, generational, trapped, local).

% Issue fatwas (legal opinions) that attempt to reconcile Islamic law with modern challenges, often requiring more expansive interpretive tools. They are constrained by the Hanbali methodology's preference for weak hadith over qiyas and its strong stance against bid'a (innovation), limiting their interpretive freedom.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, innovative_muftis, payer,
    moderate, immediate, constrained, regional).

% Advocate for a more expansive use of qiyas and istihsan (juristic preference) to serve public interest. Their methodology is fundamentally at odds with the Hanbali school's restrictiveness, and they are excluded from the Hanbali framework's internal legitimization processes.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanafi_scholars, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, consistent methodology for deriving legal rulings directly from foundational Islamic texts, ensuring fidelity to the earliest sources and minimizing subjective interpretation or innovation.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual juristic reasoning and local custom to the explicit textual sources, thereby concentrating interpretive power within the textualist scholarly class and limiting the scope for legal development outside of strict textual derivation.
% ABSENT_VOICES: Scholars from other schools (Hanafi, Maliki, Shafi'i) who advocate for more expansive interpretive tools (e.g., istihsan, maslaha mursala, broader qiyas) are structurally excluded from the Hanbali framework's internal discourse, as their foundational premises are considered deviations from textual fidelity.
% DISAPPEARANCE_RATIONALE: If the Hanbali methodology's strict textualism vanished, there would be a significant shift in Islamic legal discourse, with increased reliance on analogical reasoning, juristic preference, and customary law. This would lead to a proliferation of new legal opinions and a re-evaluation of existing rulings, fundamentally altering the landscape of Islamic jurisprudence.
% FOUNDING_PROBLEM: The problem of preserving the purity of Islamic law from unwarranted innovation (bid'a) and ensuring its direct derivation from the Quran and Sunnah, particularly in the face of diverse local customs and philosophical influences.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars and conservative jurists within the Hanbali tradition attest that the threat of innovation and deviation from foundational texts remains live. However, rationalist legal developers and comparative law scholars outside the benefiting parties argue that the problem is overemphasized to justify a restrictive methodology that hinders necessary legal adaptation.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) stems from the Hanbali reading's suppression of alternative interpretive methodologies, which limits the scope for legal development and concentrates authority. Suppression (0.75) is high due to the active enforcement of textual primacy and the systematic delegitimization of broader qiyas or customary law. Theater ratio (0.15) is low, indicating that the methodology is genuinely applied, not merely performed. Accessibility collapse (0.7) is high because alternative interpretive paths are significantly curtailed within this framework. Resistance (0.4) is moderate, as other schools and rationalist jurists actively contest this restrictive approach.
 *
 * PERSPECTIVAL GAP:
 *   Textualist scholars perceive this methodology as a pure rope, ensuring the integrity and authenticity of Islamic law. However, rationalist legal developers and customary law advocates experience it as a snare or tangled rope, as it actively suppresses their interpretive tools and delegitimizes their contributions, forcing them to operate outside the mainstream or conform to restrictive norms.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars and conservative jurists are beneficiaries, as their authority and interpretations are validated and amplified by this methodology. Rationalist legal developers, customary law advocates, and innovative muftis are victims, as their approaches are suppressed, and they bear the cost of limited interpretive freedom. Hanafi scholars are excluded, as their entire methodology is fundamentally incompatible with the Hanbali reading's premises.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling coordination as pure extraction by acknowledging the genuine coordination function of providing a clear, text-centric legal methodology. However, it avoids mislabeling extraction as coordination by highlighting the active suppression of alternative interpretive approaches and the identifiable victims who bear the costs of this methodological restrictiveness. The 'live' status of the founding problem (preserving textual purity) is contested, suggesting that while a coordination function exists, its current operation may be over-serving that function at the expense of other legitimate legal needs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_restrictiveness_necessity,
    'Is the Hanbali reading''s maximal textual restrictiveness a necessary condition for preserving the purity of Islamic law, or does it unduly limit the law''s adaptability to new contexts?',
    'Comparative analysis of legal outcomes and societal impact in jurisdictions predominantly influenced by Hanbali vs. other schools, particularly concerning modern ethical and social dilemmas.',
    'If deemed unduly limiting, the extractiveness and suppression metrics would be re-evaluated upwards, potentially reclassifying it closer to a snare. If deemed necessary, its coordination function would be emphasized, potentially reclassifying it closer to a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_restrictiveness_necessity, conceptual, 'Whether strict textualism is essential for legal purity or a barrier to adaptation.').

omega_variable(
    weak_hadith_vs_qiyas_validity,
    'Is the preference for weak hadith over qiyas a robust methodological choice, or does it introduce interpretive fragility and limit rational legal development?',
    'Scholarly consensus on the epistemological strength of weak hadith in comparison to well-reasoned qiyas in specific legal cases, and the long-term coherence of rulings derived from each.',
    'If the preference for weak hadith is found to introduce fragility, the suppression of qiyas would be seen as more extractive, increasing the overall extractiveness score. If it is robust, the suppression would be seen as a legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_hadith_vs_qiyas_validity, empirical, 'Epistemological justification for preferring weak hadith over analogical reasoning.').

omega_variable(
    sadd_al_dhara_i_scope,
    'Is the application of sadd al-dhara''i (blocking means to evil) appropriately scoped to prevent genuine harm, or is it over-applied to suppress legitimate innovation and diversity of practice?',
    'Case-by-case analysis of rulings based on sadd al-dhara''i, assessing whether the ''evil'' being blocked is clearly demonstrable and whether less restrictive means could achieve the same preventative goal.',
    'If over-applied, the suppression metric would increase, and the justification for the constraint''s enforcement would weaken, pushing it towards a snare. If appropriately scoped, it reinforces the coordination function of preventing harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_al_dhara_i_scope, preference, 'Scope and justification of blocking innovations to prevent harm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method__hanbali_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__hanbali_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(usul_tr_t30, usul_al_fiqh_method__hanbali_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__hanbali_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(usul_tr_t50, usul_al_fiqh_method__hanbali_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(usul_be_t30, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(usul_be_t50, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(usul_su_t30, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(usul_su_t50, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'usul_al_fiqh_method' kernel. Its strict textualism influences and coexists with other schools, but its core premises are distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
