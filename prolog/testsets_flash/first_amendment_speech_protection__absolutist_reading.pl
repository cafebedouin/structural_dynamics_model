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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   This constraint represents the 'absolutist' reading of the First
 *   Amendment's speech protection, where 'no law' is interpreted as a
 *   categorical prohibition on government regulation of speech, with only a
 *   few narrow, historically recognized exceptions (e.g., incitement, true
 *   threats). This reading maximizes the scope of protected speech, often
 *   externalizing the costs of harmful speech onto targeted groups. It is a
 *   specific interpretation of a contested constitutional kernel, distinct
 *   from readings that would balance speech against harm or create new
 *   categories of unprotected speech.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.6).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.2).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '43ea5db7-982b-4b37-9822-4cc621898941').
narrative_ontology:cs_kernel_codification('43ea5db7-982b-4b37-9822-4cc621898941', fixed_text).
narrative_ontology:cs_authority_grounding('43ea5db7-982b-4b37-9822-4cc621898941', lineage).
narrative_ontology:cs_interpretation_layer_present('43ea5db7-982b-4b37-9822-4cc621898941').
narrative_ontology:cs_reading_relation('43ea5db7-982b-4b37-9822-4cc621898941', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('43ea5db7-982b-4b37-9822-4cc621898941', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('43ea5db7-982b-4b37-9822-4cc621898941', foundational, no_law_means_no_law).
narrative_ontology:cs_axiom_status(no_law_means_no_law, holdable).
narrative_ontology:cs_axiom_grounding('43ea5db7-982b-4b37-9822-4cc621898941', no_law_means_no_law, deontological).
narrative_ontology:cs_axiom('43ea5db7-982b-4b37-9822-4cc621898941', foundational, historical_exclusions_are_exhaustive).
narrative_ontology:cs_axiom_status(historical_exclusions_are_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('43ea5db7-982b-4b37-9822-4cc621898941', historical_exclusions_are_exhaustive, conventional).
narrative_ontology:cs_reference_frame('43ea5db7-982b-4b37-9822-4cc621898941', original_intent_anti_censorship).
narrative_ontology:cs_drift_state('43ea5db7-982b-4b37-9822-4cc621898941', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('43ea5db7-982b-4b37-9822-4cc621898941', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, speakers_majority).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, political_dissidents).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, hate_speech_targets).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, free_speech_absolutism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from broad protection of their speech, including potentially offensive or controversial expressions, without significant government interference. They can express views freely across various platforms.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, speakers_majority, beneficiary,
    powerful, generational, mobile, national).

% Relies on the broad protection to voice opposition to established power, often engaging in speech that might be considered disruptive or offensive by the majority. Their ability to exit is constrained by the need for public platforms.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, political_dissidents, beneficiary,
    moderate, biographical, constrained, national).

% Bears the costs of speech that targets them with hate, harassment, or incitement, which remains protected under this absolutist reading. They have limited legal recourse and often face systemic harm, with no easy 'exit' from the public sphere.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minorities, payer,
    powerless, generational, trapped, national).

% Individuals or small groups directly subjected to hate speech, experiencing psychological distress, fear, and potential incitement to violence. Their identity is often the target, making 'exit' from the harm impossible without abandoning their identity.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, hate_speech_targets, payer,
    powerless, immediate, identity_locked, local).

% Tasked with upholding the First Amendment, they are constrained from enacting broad speech regulations. They benefit from avoiding the political and legal complexities of drawing lines around harmful speech, but face pressure from victims of such speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, government_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Monitor government actions to ensure speech protections are upheld, often siding with speakers against regulation. They analyze the impact of court decisions and advocate for interpretations that maximize free expression.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, speakers_majority).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, broad zone of protected expression, reducing uncertainty for speakers about what speech is permissible and minimizing the chilling effect of potential government censorship.
% TRANSFER_FUNCTION: Transfers the burden of harmful speech from the government (which is constrained from regulating it) and speakers (who are protected) to individuals and groups targeted by such speech, who bear the social and psychological costs.
% ABSENT_VOICES: Victims of systemic hate speech and those advocating for a more communitarian or harm-sensitive approach to free expression are often marginalized in the discourse, their concerns framed as threats to 'free speech' rather than legitimate claims for protection.
% DISAPPEARANCE_RATIONALE: If this absolutist reading vanished, the legal landscape for speech would fundamentally shift. Governments would likely move to regulate more categories of speech, leading to a period of intense legal and political contestation over new boundaries, and a significant reordering of power dynamics between speakers and targeted groups.
% FOUNDING_PROBLEM: The founding problem was to prevent government censorship and ensure a robust public discourse, drawing from historical experiences with state suppression of dissent.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside the absolutist camp corroborate the historical intent to prevent government censorship. However, the 'live' status of the problem is contested by those who argue that the current interpretation has over-corrected, creating new harms while solving old ones. Civil liberties advocates generally attest to the problem's continued relevance, while targeted minorities highlight the new forms of harm that remain unaddressed.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the costs borne by those targeted by harmful speech that remains protected under this absolutist interpretation. Suppression (0.2) is low because the constraint actively prevents government suppression of speech, rather than enforcing it. Resistance (0.7) is high, primarily from groups advocating for greater protection against hate speech and other forms of harmful expression. Accessibility collapse (0.3) is moderate; while direct government censorship is collapsed, other forms of social or economic pressure on speech may still exist.
 *
 * PERSPECTIVAL GAP:
 *   Speakers (especially those expressing unpopular or controversial views) experience this as a robust protection of liberty, while targeted minorities experience it as a structural vulnerability that permits harm. The 'absolutist' interpretation prioritizes the speaker's right over the target's right to be free from harm, leading to a significant divergence in how the constraint is experienced.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers (majority and dissidents) are primary beneficiaries, as their speech is maximally protected (low d). Targeted minorities and hate speech targets are victims, bearing the costs of unmitigated harmful speech (high d). The government, while nominally constrained, also benefits from avoiding the complex and politically charged task of speech regulation (moderate d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutist_vs_harm_limited_framing,
    'Is the First Amendment''s ''no law'' clause an absolute prohibition on speech regulation, or does it implicitly yield to demonstrable unconsented-to harm?',
    'Supreme Court precedent explicitly adopting a harm-based limitation, or a constitutional amendment clarifying the scope of speech protection.',
    'If a harm-limited reading were adopted, the set of protected speech would shrink, and the current victims of speech-related harm would gain legal recourse, shifting the constraint''s extractiveness and beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_vs_harm_limited_framing, conceptual, 'This constraint is the ''absolutist_reading'' of the ''first_amendment_speech_protection'' kernel. Sibling readings (''harm_limited_reading'', ''categorical_balancing_reading'') would alter the balance between speech protection and harm prevention.').

omega_variable(
    historical_exclusions_scope,
    'What is the precise and exhaustive scope of ''narrow historical exclusions'' to First Amendment protection, and how are new forms of speech/harm categorized?',
    'A definitive historical and legal consensus on the original intent and application of the First Amendment''s exclusions, or a clear jurisprudential framework for analogizing new phenomena to old categories.',
    'A broader interpretation of historical exclusions would reduce the scope of protected speech, potentially reclassifying some currently protected speech as unprotected and reducing extraction from targeted groups. A narrower interpretation would reinforce the absolutist stance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_exclusions_scope, empirical, 'Ambiguity in the scope of historical exclusions allows for interpretive drift that can expand or contract the effective protection of speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__absolutist_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__absolutist_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__absolutist_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 20, 0.19).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 30, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, hate_speech_regulation_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the First Amendment's speech protection kernel. Each reading has a different structural impact on beneficiaries, victims, and the scope of protected speech.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
