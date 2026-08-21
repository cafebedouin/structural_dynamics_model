% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Speech Protection
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the absolutist reading of the First
 *   Amendment's speech protection, where 'no law' means virtually no law, and
 *   protection is categorical except for a few narrow, historically
 *   recognized exclusions. This reading prioritizes maximum expressive
 *   liberty, often externalizing the costs of harmful speech onto targeted
 *   minorities. The constraint is claimed as a 'Mountain' by its proponents,
 *   asserting its natural and unchangeable status as an inherent
 *   constitutional principle. However, the authored metrics reflect its
 *   operational reality, which includes high extraction from victims and
 *   significant suppression of regulatory alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.78).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.85).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, mountain).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).
domain_priors:emerges_naturally(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, 'ce7b1136-f797-48cd-90bf-cf125e7595ee').
narrative_ontology:cs_kernel_codification('ce7b1136-f797-48cd-90bf-cf125e7595ee', fixed_text).
narrative_ontology:cs_authority_grounding('ce7b1136-f797-48cd-90bf-cf125e7595ee', lineage).
narrative_ontology:cs_interpretation_layer_present('ce7b1136-f797-48cd-90bf-cf125e7595ee').
narrative_ontology:cs_reading_relation('ce7b1136-f797-48cd-90bf-cf125e7595ee', first_amendment_speech_protection__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('ce7b1136-f797-48cd-90bf-cf125e7595ee', first_amendment_speech_protection__categorical_balancing_reading, forecloses).
narrative_ontology:cs_axiom('ce7b1136-f797-48cd-90bf-cf125e7595ee', foundational, no_abridgment_principle).
narrative_ontology:cs_axiom_status(no_abridgment_principle, holdable).
narrative_ontology:cs_axiom_grounding('ce7b1136-f797-48cd-90bf-cf125e7595ee', no_abridgment_principle, deontological).
narrative_ontology:cs_axiom('ce7b1136-f797-48cd-90bf-cf125e7595ee', secondary, marketplace_of_ideas_maximization).
narrative_ontology:cs_axiom_status(marketplace_of_ideas_maximization, holdable).
narrative_ontology:cs_axiom_grounding('ce7b1136-f797-48cd-90bf-cf125e7595ee', marketplace_of_ideas_maximization, instrumental).
narrative_ontology:cs_reference_frame('ce7b1136-f797-48cd-90bf-cf125e7595ee', original_intent_categorical_protection).
narrative_ontology:cs_drift_state('ce7b1136-f797-48cd-90bf-cf125e7595ee', contemporary_social_justice_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ce7b1136-f797-48cd-90bf-cf125e7595ee', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_groups).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, victims_of_hate_speech).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, regulators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legal scholars, civil liberties organizations, and judges who interpret and defend the categorical protection of speech, viewing any limitation as a dangerous precedent. Their professional identity is often tied to this interpretation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, absolutist_advocates, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Individuals and groups who benefit from broad, categorical protection of their expression, allowing them to speak without fear of government censorship or content-based regulation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, speakers, beneficiary,
    moderate, biographical, mobile, national).

% Groups whose speech is rarely targeted for harm or regulation, and who benefit from the general climate of free expression without bearing significant costs.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majority_groups, beneficiary,
    powerful, generational, mobile, national).

% Groups who disproportionately bear the social, psychological, and physical costs of harmful speech (e.g., hate speech, incitement) that is protected under this absolutist reading. They have limited avenues for legal redress or protection.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minorities, payer,
    powerless, generational, trapped, national).

% Individuals who are directly and immediately harmed by speech protected by the absolutist reading, experiencing psychological distress, harassment, or incitement to violence, with little legal recourse.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, victims_of_hate_speech, payer,
    powerless, immediate, trapped, local).

% Government bodies and officials tasked with maintaining public order, protecting vulnerable groups, and preventing harm, whose ability to enact and enforce speech regulations is severely curtailed by this absolutist interpretation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, regulators, payer,
    institutional, biographical, constrained, national).

% Legal scholars and civil rights organizations who argue that speech protection should yield when it causes demonstrable, unconsented-to harm. Their alternative reading is largely foreclosed by the absolutist framework.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, harm_limited_advocates, excluded,
    organized, generational, constrained, national).

% Legal scholars and jurists who advocate for a framework where speech is categorized, and each category is balanced against potential harms on a case-by-case basis. This approach is rejected by the absolutist reading.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, categorical_balancing_advocates, excluded,
    organized, generational, constrained, national).

% The ultimate arbiter of First Amendment meaning, whose precedents solidify or shift the prevailing interpretation. While not a direct beneficiary, its institutional power is maintained through its role in defining such fundamental rights.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, high bar for speech regulation, aiming to prevent government overreach and chilling effects on expression, thereby coordinating a broad zone of expressive liberty.
% TRANSFER_FUNCTION: Transfers the burden of harmful speech from the speaker and the state (which is prevented from regulating) to targeted individuals and groups, who bear the social, psychological, and sometimes physical costs of such speech.
% ABSENT_VOICES: Harm-limited and categorical balancing advocates, as well as the direct victims of speech, are structurally marginalized in this absolutist framework. Their concerns regarding speech-induced harm are deemed secondary to the categorical protection of expression, and their proposed regulatory mechanisms are largely excluded from consideration.
% DISAPPEARANCE_RATIONALE: If this absolutist reading vanished overnight, the legal landscape for speech regulation would fundamentally shift. Courts would likely adopt more permissive standards for restricting harmful speech, leading to a significant reorganization of public discourse, regulatory power, and the avenues for redress available to victims of speech.
% FOUNDING_PROBLEM: To prevent government censorship and ensure a robust marketplace of ideas, protecting individual liberty of expression from state interference, particularly in political and religious discourse.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal scholars and civil liberties organizations corroborate the original intent to prevent censorship. However, contemporary social justice advocates and legal scholars argue that the founding problem has evolved, and the absolutist reading now creates new harms, a view supported by sociological studies of hate speech impact and its disproportionate effect on marginalized communities.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(first_amendment_speech_protection__absolutist_reading),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the substantial social and psychological costs borne by targeted minorities and victims of hate speech, which are treated as unavoidable externalities of broad speech protection. Suppression (0.85) is high because this reading actively suppresses the ability of regulators and affected groups to limit or seek redress for harmful speech. Accessibility collapse (0.9) is high for any alternative regulatory framework, as the absolutist interpretation forecloses most attempts to balance speech against harm. Resistance (0.7) is also high, reflecting ongoing legal and political challenges to this interpretation. The theater ratio is low (0.1) because the principle is genuinely applied by courts, not merely performed. The claimed type 'mountain' reflects the proponents' view of the First Amendment as an unchangeable natural law, while the metrics reveal the extractive and suppressive consequences of this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The absolutist reading creates a significant perspectival gap: proponents (speakers, absolutist advocates) experience it as a fundamental protection of liberty (akin to a Rope or even a Mountain), while those who bear the costs (targeted minorities, victims, regulators) experience it as a highly extractive and suppressive force (akin to a Snare or Tangled Rope). The engine's per-seat classification will highlight this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and majority groups are clear beneficiaries, experiencing low directionality as the constraint subsidizes their expressive freedom. Targeted minorities, victims of hate speech, and regulators are targets, experiencing high directionality as they bear the costs of unregulated harmful speech or the suppression of their regulatory power. Absolutist advocates and the Supreme Court, as agenda-setters, benefit from the stability and authority of this interpretation, placing them at the beneficiary end.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutist_vs_constructed_interpretation,
    'Is the categorical protection of speech a genuine natural law inherent in the First Amendment''s text, or a constructed interpretation that benefits identifiable agents (speakers, majority groups) by externalizing harm?',
    'Historical and textual analysis of the First Amendment''s drafting and early interpretations, alongside contemporary sociological analysis of the impact of speech on marginalized groups.',
    'If found to be a constructed interpretation, the ''mountain'' claim would be reclassified, likely to a ''tangled_rope'' or ''snare'' for affected parties, highlighting its contingent and extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_vs_constructed_interpretation, conceptual, 'Ambiguity between natural law and constructed interpretation for speech protection.').

omega_variable(
    harm_definition_and_scope,
    'How should ''harm'' from speech be defined, and at what threshold does it become a legitimate basis for limiting speech, even under a broad protection framework?',
    'Empirical studies on the psychological, social, and physical impacts of various forms of speech, combined with legal and philosophical consensus-building on the scope of ''unconsented-to harm''.',
    'A broader definition of cognizable harm would shift the balance, potentially reducing the extractiveness for victims and increasing the suppression for certain types of speech, pushing the constraint towards a ''tangled_rope'' or ''rope'' for regulators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_and_scope, empirical, 'Ambiguity in defining and recognizing harm caused by speech.').

omega_variable(
    identity_lock_for_absolutist_advocates,
    'Are absolutist advocates identity-locked into this reading due to professional identity, ideological commitment, or institutional inertia, making alternative interpretations unthinkable or highly resistant to change?',
    'Qualitative sociological studies of legal communities and civil liberties organizations, examining the social and professional costs of departing from established absolutist interpretations.',
    'If identity-locked, the persistence of the absolutist reading is less about its inherent truth and more about the social structures that maintain it, potentially increasing the ''theater_ratio'' for the maintenance of the ''mountain'' claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_for_absolutist_advocates, empirical, 'Whether absolutist advocates are identity-locked into their interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__absolutist_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__absolutist_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__absolutist_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__absolutist_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(firs_tr_t50, first_amendment_speech_protection__absolutist_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 40, 0.77).
narrative_ontology:measurement(firs_be_t50, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(firs_su_t50, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, public_discourse_norms).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, civil_rights_protections).

% DUAL FORMULATION NOTE:
% This constraint is the 'absolutist_reading' of the 'first_amendment_speech_protection' kernel. It is one of three distinct readings, each with its own structural properties and classification, linked by their common origin in the First Amendment text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
