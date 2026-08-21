% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Absolutist Reading of Speech Harm Boundary
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint represents an 'absolutist' reading of speech protection,
 *   where the threshold for speech to be considered unprotected and thus
 *   subject to regulation is extremely high. This reading prioritizes speaker
 *   autonomy and the 'marketplace of ideas' above nearly all other concerns,
 *   including the harms that speech may inflict on individuals or groups. The
 *   constraint is claimed as a 'tangled_rope' because it genuinely
 *   coordinates a broad sphere of expression (benefiting speakers) but does
 *   so by extracting significant costs from those harmed by speech (victims),
 *   requiring active enforcement of its high threshold.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.85).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.1).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Absolutist Reading of Speech Harm Boundary").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '4d31af55-b700-44e8-a83f-a133e2dd39b8').
narrative_ontology:cs_kernel_codification('4d31af55-b700-44e8-a83f-a133e2dd39b8', fixed_text).
narrative_ontology:cs_authority_grounding('4d31af55-b700-44e8-a83f-a133e2dd39b8', lineage).
narrative_ontology:cs_interpretation_layer_present('4d31af55-b700-44e8-a83f-a133e2dd39b8').
narrative_ontology:cs_reading_relation('4d31af55-b700-44e8-a83f-a133e2dd39b8', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d31af55-b700-44e8-a83f-a133e2dd39b8', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('4d31af55-b700-44e8-a83f-a133e2dd39b8', foundational, speech_is_presumptively_free).
narrative_ontology:cs_axiom_status(speech_is_presumptively_free, holdable).
narrative_ontology:cs_axiom_grounding('4d31af55-b700-44e8-a83f-a133e2dd39b8', speech_is_presumptively_free, deontological).
narrative_ontology:cs_axiom('4d31af55-b700-44e8-a83f-a133e2dd39b8', foundational, marketplace_of_ideas_maximizes_truth).
narrative_ontology:cs_axiom_status(marketplace_of_ideas_maximizes_truth, holdable).
narrative_ontology:cs_axiom_grounding('4d31af55-b700-44e8-a83f-a133e2dd39b8', marketplace_of_ideas_maximizes_truth, empirically_contingent).
narrative_ontology:cs_reference_frame('4d31af55-b700-44e8-a83f-a133e2dd39b8', founding_era_anti_censorship_principle).
narrative_ontology:cs_drift_state('4d31af55-b700-44e8-a83f-a133e2dd39b8', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4d31af55-b700-44e8-a83f-a133e2dd39b8', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, free_speech_advocates).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, vulnerable_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a broad scope of protected speech, allowing expression with minimal fear of legal repercussions, even if that speech causes harm to others. Their autonomy is prioritized.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers, beneficiary,
    moderate, biographical, mobile, national).

% Bear the direct costs of speech that causes harm (e.g., emotional distress, reputational damage, incitement to violence) but falls outside the extremely narrow categories of unprotected speech. They have limited recourse.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech, payer,
    powerless, immediate, trapped, local).

% Actively defend the high threshold for unprotected speech, arguing that any broader restriction would chill legitimate expression. They shape legal interpretation and public discourse around speech rights.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, free_speech_advocates, agenda_setter,
    organized, generational, analytical, national).

% Disproportionately affected by harmful speech (e.g., hate speech, harassment) that is protected under this absolutist reading. Their identity makes them targets, and their ability to exit the public sphere is constrained.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, vulnerable_groups, payer,
    powerless, generational, identity_locked, national).

% Interpret and enforce the speech protections, consistently applying a high bar for speech to be deemed unprotected. They are bound by precedent but also shape its evolution.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, courts, agenda_setter,
    institutional, civilizational, constrained, national).

% Are constrained in their ability to pass laws restricting speech due to the high constitutional bar set by this reading. They observe the harms but often lack the power to address them through legislation without facing judicial challenge.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legislators, observer,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit high, boundary for protected speech, providing predictability for speakers and minimizing state interference in expression. It coordinates public discourse by prioritizing speaker autonomy.
% TRANSFER_FUNCTION: Transfers the burden of harm from speakers to the targets of speech, by setting an extremely high threshold for speech to be considered unprotected and thus actionable.
% ABSENT_VOICES: Those who advocate for a more robust protection against speech-induced harm, particularly vulnerable groups, are often marginalized in the legal and philosophical discourse that entrenches this absolutist reading. Their experiences of harm are often discounted or reframed as the 'cost of a free society'.
% DISAPPEARANCE_RATIONALE: If this absolutist reading vanished, the legal landscape around speech would immediately shift. Courts would likely adopt more balancing tests, legislatures would pass more restrictive speech laws, and the public would demand greater accountability for harmful expression. The current prioritization of speaker autonomy would be fundamentally altered.
% FOUNDING_PROBLEM: The constraint was built to prevent government censorship and ensure a robust marketplace of ideas, protecting dissent and promoting democratic deliberation.
% FOUNDING_PROBLEM_CORROBORATION: Free speech advocates and some legal scholars attest the problem is still live, citing ongoing threats to expression. Targets of harmful speech and other legal scholars argue that while censorship is a concern, the current reading has over-prioritized speaker autonomy at the expense of other fundamental rights, indicating the founding problem has shifted or been over-solved in one direction. Legislative debates and international human rights reports corroborate the ongoing contestation.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the cost of harm is largely borne by the targets of speech, with minimal legal recourse. Suppression (0.1) is low because the constraint's primary function is to prevent suppression of speech, not to enforce it; the active enforcement is directed at preventing restrictions on speech. Theater ratio (0.05) is low, indicating that the constraint's stated purpose (protecting speech) largely aligns with its operation, even if the consequences are contested. Accessibility collapse (0.2) is low because alternatives to this absolutist reading (e.g., harm-balancing approaches) are widely discussed and advocated, though not legally dominant. Resistance (0.7) is high, reflecting ongoing social and legal challenges from those advocating for greater protection against speech harms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers, this constraint is a 'rope' or even a 'mountain' of fundamental liberty, enabling robust public discourse. From the perspective of targets of harmful speech, it operates as a 'snare', trapping them in a system where their harms are legally unaddressable. The engine's classification as 'tangled_rope' reflects the hybrid nature: a genuine coordination of expression coupled with asymmetric extraction of harm from specific groups.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and free speech advocates are clear beneficiaries, experiencing low directionality as the constraint subsidizes their expressive freedom. Targets of harmful speech and vulnerable groups are clear victims, experiencing high directionality as they bear the costs of protected harmful speech. Courts and free speech advocates act as agenda-setters, actively shaping and enforcing this high threshold. Legislators are observers, often constrained by this reading from enacting broader speech regulations.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to protect free expression remains live, but its application in an absolutist manner has led to a situation where the costs are disproportionately borne by vulnerable groups. The classification as 'tangled_rope' prevents mislabeling it as a pure 'rope' (ignoring the victims) or a pure 'snare' (ignoring the genuine coordination of expression). The contestation over the 'founding_problem_status' highlights the drift: the original problem of censorship is still relevant, but the solution has created new problems of unaddressed harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unprotected_categories_scope,
    'Is the current set of unprotected speech categories (incitement, true threats, defamation, obscenity) truly exhaustive of speech that causes severe, direct harm, or are there other categories that should be unprotected?',
    'Empirical studies on the impact of currently protected speech on vulnerable populations, combined with legal and philosophical re-evaluation of the ''harm principle'' in speech contexts.',
    'If new categories of severely harmful speech are identified and deemed unprotected, the extractiveness from victims would decrease, and the constraint might shift towards a ''rope'' or ''scaffold'' for a more balanced coordination of speech and safety.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unprotected_categories_scope, empirical, 'Ambiguity regarding the completeness of unprotected speech categories.').

omega_variable(
    absolutism_vs_balancing_framing,
    'Is the absolutist reading of speech protection a necessary structural feature for robust democracy, or is it a conceptual choice that could be re-framed to allow for harm-balancing without chilling legitimate speech?',
    'Comparative legal analysis of democracies with different speech protection frameworks (e.g., those incorporating dignity or harm-balancing tests), assessing their democratic health and expressive vibrancy.',
    'If it''s a conceptual choice, a shift to a harm-balancing framework (e.g., the ''harm_balancing_reading'' sibling) would reallocate the costs of speech, potentially reducing extractiveness from victims and shifting the constraint towards a more balanced ''rope'' or ''scaffold''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutism_vs_balancing_framing, conceptual, 'Whether absolutism is a structural necessity or a conceptual choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1960, speech_harm_boundary__absolutist_reading, theater_ratio, 1960, 0.03).
narrative_ontology:measurement(spee_tr_t1980, speech_harm_boundary__absolutist_reading, theater_ratio, 1980, 0.04).
narrative_ontology:measurement(spee_tr_t2000, speech_harm_boundary__absolutist_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(spee_tr_t2024, speech_harm_boundary__absolutist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(spee_be_t1960, speech_harm_boundary__absolutist_reading, base_extractiveness, 1960, 0.75).
narrative_ontology:measurement(spee_be_t1980, speech_harm_boundary__absolutist_reading, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(spee_be_t2000, speech_harm_boundary__absolutist_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(spee_be_t2024, speech_harm_boundary__absolutist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1960, speech_harm_boundary__absolutist_reading, suppression_requirement, 1960, 0.08).
narrative_ontology:measurement(spee_su_t1980, speech_harm_boundary__absolutist_reading, suppression_requirement, 1980, 0.09).
narrative_ontology:measurement(spee_su_t2000, speech_harm_boundary__absolutist_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(spee_su_t2024, speech_harm_boundary__absolutist_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, online_content_moderation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
