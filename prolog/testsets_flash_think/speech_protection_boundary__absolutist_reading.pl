% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Absolutist Reading of Speech Protection (Brandenburg Standard)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'absolutist_reading' of the
 *   'speech_protection_boundary' kernel, which interprets the First Amendment
 *   as providing near-absolute protection for speech, with exceptions limited
 *   to direct incitement to imminent lawless action (the Brandenburg
 *   standard). This reading prioritizes free expression above most other
 *   concerns, leading to a broad protected set for speakers. The metrics
 *   reflect the high cost borne by minoritized communities and targets of
 *   hate speech, who experience significant harm as an externality of this
 *   broad protection, while the state's ability to regulate is highly
 *   suppressed. The claimed type 'rope' reflects the ideal of coordinating
 *   free expression, while the metrics capture the actual, often extractive,
 *   operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.65).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.75).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist Reading of Speech Protection (Brandenburg Standard)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '974e4037-7169-4fc8-9098-ab822c1ffbec').
narrative_ontology:cs_kernel_codification('974e4037-7169-4fc8-9098-ab822c1ffbec', fixed_text).
narrative_ontology:cs_authority_grounding('974e4037-7169-4fc8-9098-ab822c1ffbec', lineage).
narrative_ontology:cs_interpretation_layer_present('974e4037-7169-4fc8-9098-ab822c1ffbec').
narrative_ontology:cs_reading_relation('974e4037-7169-4fc8-9098-ab822c1ffbec', speech_protection_boundary__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('974e4037-7169-4fc8-9098-ab822c1ffbec', speech_protection_boundary__balancing_reading, forecloses).
narrative_ontology:cs_axiom('974e4037-7169-4fc8-9098-ab822c1ffbec', foundational, speech_is_presumptively_free).
narrative_ontology:cs_axiom_status(speech_is_presumptively_free, holdable).
narrative_ontology:cs_axiom_grounding('974e4037-7169-4fc8-9098-ab822c1ffbec', speech_is_presumptively_free, deontological).
narrative_ontology:cs_axiom('974e4037-7169-4fc8-9098-ab822c1ffbec', secondary, marketplace_of_ideas_truth_discovery).
narrative_ontology:cs_axiom_status(marketplace_of_ideas_truth_discovery, holdable).
narrative_ontology:cs_axiom_grounding('974e4037-7169-4fc8-9098-ab822c1ffbec', marketplace_of_ideas_truth_discovery, instrumental).
narrative_ontology:cs_reference_frame('974e4037-7169-4fc8-9098-ab822c1ffbec', post_brandenburg_jurisprudence).
narrative_ontology:cs_drift_state('974e4037-7169-4fc8-9098-ab822c1ffbec', contemporary_social_media_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('974e4037-7169-4fc8-9098-ab822c1ffbec', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, civil_liberties_advocates).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, targets_of_hate_speech).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, prior_restraint_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from broad protection for their expression, facing minimal legal repercussions for most forms of speech, even if offensive or controversial. Their ability to speak freely is largely unconstrained by the state.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, speakers, beneficiary,
    moderate, biographical, mobile, global).

% Actively defend and promote the absolutist interpretation of free speech, seeing it as essential for democratic discourse and individual liberty. Their mission aligns with the broad protection afforded by this standard.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, civil_liberties_advocates, beneficiary,
    organized, generational, analytical, national).

% Disproportionately bear the aggregate harm and externalized costs of broadly protected speech, including hate speech, harassment, and incitement that falls short of the Brandenburg standard. They have limited legal recourse to mitigate these harms.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_communities, payer,
    powerless, generational, trapped, national).

% Directly experience the psychological, social, and sometimes physical harms resulting from speech protected under this standard. Their ability to live free from harassment and discrimination is compromised, with little legal protection.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, targets_of_hate_speech, payer,
    powerless, immediate, trapped, local).

% Are often pressured to apply the Brandenburg standard (or a similar high bar) to content moderation, leading to challenges in balancing free expression with user safety. They benefit from reduced liability but face public backlash and operational costs from managing harmful content.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, social_media_platforms, agenda_setter,
    powerful, biographical, constrained, global).

% Are the primary interpreters and enforcers of the Brandenburg standard, consistently upholding a high bar for speech restriction. They shape the legal landscape for free expression through their rulings.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Are constrained in their ability to pass laws restricting speech due to the high bar set by the Brandenburg standard. They observe the harms but often lack the legal authority to address them directly without risking constitutional challenge.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, legislators, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, speakers).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a broad societal understanding of free expression, ensuring a wide range of ideas can be shared without prior restraint or fear of punishment, fostering open debate and preventing government censorship.
% TRANSFER_FUNCTION: Transfers the burden of managing harmful speech from the state (which is largely prevented from regulating) to individuals and communities, particularly minoritized groups who disproportionately bear the brunt of hate speech and incitement. It also transfers the cost of content moderation to private platforms.
% ABSENT_VOICES: Those advocating for stronger protections against hate speech, harassment, and disinformation, particularly minoritized communities whose safety and dignity are often compromised by the broad scope of protected speech. Their calls for greater regulation are largely excluded from the legal framework.
% DISAPPEARANCE_RATIONALE: If the Brandenburg standard vanished, the legal landscape for speech regulation would fundamentally shift. States and platforms would likely move to restrict more categories of speech, leading to a complex and fragmented regulatory environment, and potentially chilling legitimate expression while offering more protection to vulnerable groups. The balance of power between speakers and those harmed by speech would be dramatically altered.
% FOUNDING_PROBLEM: To prevent government censorship and ensure a robust public discourse, particularly in the wake of McCarthyism and concerns about overbroad restrictions on political speech that could stifle dissent.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and some legal scholars corroborate the ongoing need for strong protections against state censorship. However, human rights advocates and scholars of critical race theory, from outside the traditional civil liberties establishment, argue that the original problem has been largely superseded by new challenges (e.g., online hate speech), and the current standard exacerbates harm.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) is high because the broad protection for speech, while beneficial to speakers, imposes substantial costs and harms on vulnerable groups. Suppression (0.75) is high because the legal framework actively suppresses attempts to regulate speech, even when it causes significant social harm. The theater ratio is low (0.15) as the standard is genuinely and consistently applied by courts. Accessibility collapse is high for those seeking to restrict speech, as their legal options are severely limited. Resistance is moderate, reflecting ongoing advocacy and legal challenges from groups harmed by speech.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers and civil liberties advocates, this constraint functions as a robust rope, coordinating free expression. From the perspective of minoritized communities and targets of hate speech, the same constraint operates as a snare, enabling significant harm and suppressing their ability to seek redress. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and civil liberties advocates are clear beneficiaries, experiencing low directionality as the constraint subsidizes their expression. Minoritized communities and targets of hate speech are clear targets, experiencing high directionality as the constraint extracts from them by enabling harm. Social media platforms and courts act as agenda-setters, interpreting and enforcing the standard, with their directionality modulated by their specific roles and incentives.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutist_vs_harm_definition,
    'Is the Brandenburg standard''s definition of ''imminent lawless action'' sufficiently narrow to prevent all significant harm, or does it permit substantial aggregate harm to minoritized communities?',
    'Empirical studies on the impact of protected hate speech on vulnerable populations, and legal analysis of how ''imminence'' and ''incitement'' are applied in practice versus their theoretical scope.',
    'If the standard permits substantial aggregate harm, the constraint''s effective extractiveness on victims is higher than currently acknowledged, potentially reclassifying it as a snare from their seat. If the standard is found to effectively prevent all significant harm, the absolutist reading''s claim of minimal negative externality is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_vs_harm_definition, empirical, 'Whether the Brandenburg standard''s narrow harm exception adequately addresses real-world harms.').

omega_variable(
    state_vs_private_power_ambiguity,
    'Does the absolutist reading, primarily designed to limit state power, inadvertently empower private actors (e.g., social media platforms) to become de facto censors or amplifiers of harm without accountability?',
    'Analysis of platform content moderation policies and their impact, and legal scholarship on the ''state action doctrine'' in the digital age. This would involve examining whether platforms'' enforcement of the standard (or lack thereof) constitutes state-like behavior.',
    'If private platforms are found to wield state-like power over speech, the constraint''s effective suppression and extractiveness could be re-evaluated to account for this ''private censorship'' or ''private amplification of harm'' that the original standard did not anticipate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_vs_private_power_ambiguity, conceptual, 'The applicability of a state-centric speech protection standard to private digital platforms.').

omega_variable(
    absolutist_reading_vs_other_rights,
    'Does the absolutist reading of free speech inherently conflict with other fundamental rights, such as the right to equality, dignity, or freedom from discrimination, within a single constitutional framework?',
    'Deep conceptual analysis of constitutional coherence, examining whether a framework can simultaneously uphold an absolutist speech right and robust anti-discrimination rights without internal contradiction. This is a philosophical rather than empirical question.',
    'If an inherent conflict is established, it would highlight a fundamental tension within the constitutional order, potentially leading to a re-evaluation of the ''absolutist'' claim''s internal consistency or its compatibility with a broader rights framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_reading_vs_other_rights, conceptual, 'Conceptual compatibility of absolutist speech with other fundamental rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__absolutist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t6, speech_protection_boundary__absolutist_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(spee_tr_t12, speech_protection_boundary__absolutist_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(spee_tr_t18, speech_protection_boundary__absolutist_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement(spee_tr_t24, speech_protection_boundary__absolutist_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__absolutist_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__absolutist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(spee_be_t6, speech_protection_boundary__absolutist_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(spee_be_t12, speech_protection_boundary__absolutist_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(spee_be_t18, speech_protection_boundary__absolutist_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(spee_be_t24, speech_protection_boundary__absolutist_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__absolutist_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__absolutist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(spee_su_t6, speech_protection_boundary__absolutist_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(spee_su_t12, speech_protection_boundary__absolutist_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(spee_su_t18, speech_protection_boundary__absolutist_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(spee_su_t24, speech_protection_boundary__absolutist_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(spee_su_t30, speech_protection_boundary__absolutist_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, balancing_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, online_content_moderation_policies).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_protection_boundary' kernel. It represents the absolutist interpretation, which prioritizes free expression with minimal exceptions. It forecloses the 'harm_limited_reading' and 'balancing_reading' due to fundamental differences in their core premises regarding speech regulation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
