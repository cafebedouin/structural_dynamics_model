% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Speech Protection: Harm-Limited Reading
 *   domain: Constitutional Law / Political Philosophy / Speech Regulation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'harm-limited' reading of First
 *   Amendment speech protection, where free speech yields when it causes
 *   demonstrable, unconsented-to harm. This reading seeks to balance
 *   expressive freedom with the protection of individuals and groups from
 *   direct injury. It is one of several competing interpretations of the
 *   First Amendment's scope, contrasting with absolutist and categorical
 *   balancing approaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.68).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.75).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Speech Protection: Harm-Limited Reading").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "Constitutional Law / Political Philosophy / Speech Regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '0aaed21f-aec9-46b0-974e-3ed118918523').
narrative_ontology:cs_kernel_codification('0aaed21f-aec9-46b0-974e-3ed118918523', fixed_text).
narrative_ontology:cs_authority_grounding('0aaed21f-aec9-46b0-974e-3ed118918523', lineage).
narrative_ontology:cs_interpretation_layer_present('0aaed21f-aec9-46b0-974e-3ed118918523').
narrative_ontology:cs_reading_relation('0aaed21f-aec9-46b0-974e-3ed118918523', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('0aaed21f-aec9-46b0-974e-3ed118918523', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('0aaed21f-aec9-46b0-974e-3ed118918523', foundational, speech_causing_demonstrable_harm_is_not_protected).
narrative_ontology:cs_axiom_status(speech_causing_demonstrable_harm_is_not_protected, holdable).
narrative_ontology:cs_axiom_grounding('0aaed21f-aec9-46b0-974e-3ed118918523', speech_causing_demonstrable_harm_is_not_protected, empirically_contingent).
narrative_ontology:cs_axiom('0aaed21f-aec9-46b0-974e-3ed118918523', foundational, protection_of_vulnerable_groups_is_a_compelling_state_interest).
narrative_ontology:cs_axiom_status(protection_of_vulnerable_groups_is_a_compelling_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('0aaed21f-aec9-46b0-974e-3ed118918523', protection_of_vulnerable_groups_is_a_compelling_state_interest, deontological).
narrative_ontology:cs_reference_frame('0aaed21f-aec9-46b0-974e-3ed118918523', balancing_individual_rights_with_social_order).
narrative_ontology:cs_drift_state('0aaed21f-aec9-46b0-974e-3ed118918523', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0aaed21f-aec9-46b0-974e-3ed118918523', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, public_safety_advocates).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_of_harmful_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive protection from speech that causes demonstrable, unconsented-to harm, such as incitement to violence, defamation, or harassment. Their ability to participate in public life is enhanced by these limits.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    powerless, immediate, constrained, local).

% Champion the principle that speech should not cause harm, advocating for legal frameworks that balance free expression with public safety and individual dignity. They benefit from the legal recognition of speech harms.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, public_safety_advocates, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of having their speech restricted or penalized when it is deemed to cause demonstrable, unconsented-to harm. Their expressive freedom is curtailed at the boundary of harm.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speakers_of_harmful_speech, payer,
    moderate, immediate, constrained, local).

% Interpret and apply the harm-limited reading of the First Amendment, adjudicating cases where speech is alleged to cause harm and setting precedents for its regulation. They enforce the boundaries of protected speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Argue that the First Amendment's 'no law' clause should be interpreted almost literally, with minimal exceptions, and that any harm-based limitation erodes fundamental rights. They are structurally excluded from the operational logic of this reading.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, absolutist_speech_advocates, excluded,
    organized, generational, analytical, national).

% Analyze the evolution and application of the harm-limited reading, debating its philosophical underpinnings, practical implications, and consistency with constitutional principles. They provide critical commentary but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To reconcile the constitutional commitment to free expression with the imperative to protect individuals and groups from direct, demonstrable, and unconsented-to harm caused by speech, thereby enabling a more inclusive public sphere.
% TRANSFER_FUNCTION: Transfers the burden of enduring demonstrable harm from vulnerable individuals and groups to speakers whose expression causes that harm, by permitting legal restrictions on such speech.
% ABSENT_VOICES: Absolutist speech advocates are absent from the operational logic of this reading; they would argue that any restriction on speech, even harmful, is an unacceptable violation of fundamental rights and that the 'cure' of regulation is worse than the 'disease' of harmful speech.
% DISAPPEARANCE_RATIONALE: If the harm-limited reading vanished overnight, there would be no legal basis to restrict speech causing demonstrable harm. This would lead to a significant increase in incitement, harassment, defamation, and other forms of harmful expression, forcing vulnerable groups to withdraw from public discourse and potentially leading to increased social conflict and violence. Society would have to find other, likely less effective, means of protection.
% FOUNDING_PROBLEM: The core problem was how to interpret the First Amendment's broad protection of speech in a way that acknowledges the real-world impact of speech, particularly its capacity to inflict direct and severe harm on individuals and social order, without unduly chilling legitimate expression.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, as evidenced by ongoing legal challenges, social science research on the impact of online harassment and disinformation, and persistent public debate from diverse civil society groups, legal experts, and international human rights organizations (not solely from those who benefit from speech restrictions).
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.68) because it curtails the expressive freedom of speakers, imposing costs for speech deemed harmful. Suppression is also high (0.75) as it requires active legal and regulatory enforcement to identify, adjudicate, and penalize harmful speech. Theater ratio is low (0.20) because the debate and application of this principle are genuine, reflecting a live and contested area of law, not mere performance. Accessibility collapse is moderate (0.60) as alternatives to harmful speech (e.g., non-harmful expression, alternative communication channels) exist, but the option of unfettered harmful speech is curtailed. Resistance is high (0.70) from those whose speech is restricted, who often claim their rights are being violated.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable minorities, this constraint is a vital Rope or Scaffold, providing necessary protection and enabling their participation. From the perspective of speakers whose speech is restricted, it can feel like a Snare, an arbitrary curtailment of fundamental rights. The engine's per-seat classification will reflect these divergent experiences based on the structural roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable minorities and public safety advocates are the primary beneficiaries, as the constraint directly protects them from harm. Speakers of harmful speech are the targets, bearing the costs of restriction. Courts and regulators act as agenda-setters, defining and enforcing the boundaries. Absolutist speech advocates are excluded, as their core premise is incompatible with this reading's operational logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope acknowledges both the genuine coordination function (protecting vulnerable groups) and the asymmetric extraction (from speakers of harmful speech). This prevents mislabeling it as a pure Rope (ignoring the extraction) or a pure Snare (ignoring the coordination benefit for protected groups). The 'live' status of the founding problem further indicates that the mandate has not atrophied, though its application is constantly evolving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_definition_ambiguity,
    'What constitutes ''demonstrable unconsented-to harm'' in the context of speech, and how is it objectively measured?',
    'Development of clearer legal standards, empirical research on the causal links between speech and harm, and judicial consensus on evidentiary thresholds.',
    'If harm is defined too broadly or subjectively, the constraint''s effective suppression and extractiveness increase, potentially chilling legitimate speech. If defined too narrowly, it fails to protect vulnerable groups, weakening its coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demonstrable_harm_definition_ambiguity, conceptual, 'Ambiguity in defining and measuring ''demonstrable harm'' from speech.').

omega_variable(
    first_amendment_kernel_reading_identity,
    'Is this ''harm-limited'' reading a legitimate interpretation of the First Amendment''s original intent and evolving constitutional principles, or an overreach that fundamentally alters its core meaning?',
    'Continued judicial review, constitutional scholarship, and public discourse that either solidifies this reading''s place within the constitutional tradition or leads to its re-evaluation.',
    'If deemed an overreach, its legitimacy erodes, increasing resistance and potentially leading to legal challenges that could shift the balance back towards more absolutist interpretations. If solidified, it strengthens the legal basis for speech regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_amendment_kernel_reading_identity, conceptual, 'Whether the harm-limited reading is a legitimate or transformative interpretation of the First Amendment.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties, platform policies) or internalized (self-censorship due to fear of reprisal)?',
    'Post-regulation speech trajectory analysis: if self-censorship persists even after specific regulations are removed or softened, it suggests a significant internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as speakers carry the suppression with them. This makes the constraint harder to ''fix'' through legal reform alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for harmful speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1969, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1969, 0.18).
narrative_ontology:measurement(firs_tr_t1980, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1980, 0.19).
narrative_ontology:measurement(firs_tr_t1991, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1991, 0.19).
narrative_ontology:measurement(firs_tr_t2002, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2002, 0.2).
narrative_ontology:measurement(firs_tr_t2013, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(firs_be_t1969, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1969, 0.55).
narrative_ontology:measurement(firs_be_t1980, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(firs_be_t1991, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1991, 0.61).
narrative_ontology:measurement(firs_be_t2002, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2002, 0.64).
narrative_ontology:measurement(firs_be_t2013, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2013, 0.66).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1969, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1969, 0.6).
narrative_ontology:measurement(firs_su_t1980, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1980, 0.64).
narrative_ontology:measurement(firs_su_t1991, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1991, 0.68).
narrative_ontology:measurement(firs_su_t2002, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2002, 0.71).
narrative_ontology:measurement(firs_su_t2013, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2013, 0.73).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, online_content_moderation_policies).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, hate_speech_legislation).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, defamation_law).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'first_amendment_speech_protection' kernel. Its ε value differs significantly from the 'absolutist_reading' (lower extraction) and 'categorical_balancing_reading' (different extraction profile), necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
