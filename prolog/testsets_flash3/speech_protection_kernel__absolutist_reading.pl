% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Speech Protection (Absolutist Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'absolutist' reading of speech protection,
 *   where speech is protected near-categorically and listener harm is
 *   generally not a valid ground for restriction. This reading prioritizes
 *   speaker autonomy and a wide-open public forum, even at the cost of
 *   individual or group harm. It is one reading of the broader
 *   'speech_protection_kernel' and is characterized by a very low
 *   extractiveness from speakers but a non-zero, often substantial, cost
 *   borne by those harmed by protected speech. The claimed type is 'mountain'
 *   because its proponents often frame it as an irreducible principle of free
 *   society, a natural law of liberal democracy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.15).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.05).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, mountain).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Speech Protection (Absolutist Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:emerges_naturally(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, '903cfaaa-1af6-4004-aeb0-37aa75df4dcd').
narrative_ontology:cs_kernel_codification('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', fixed_text).
narrative_ontology:cs_authority_grounding('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', lineage).
narrative_ontology:cs_interpretation_layer_present('903cfaaa-1af6-4004-aeb0-37aa75df4dcd').
narrative_ontology:cs_reading_relation('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', foundational, speaker_autonomy_maximization).
narrative_ontology:cs_axiom_status(speaker_autonomy_maximization, holdable).
narrative_ontology:cs_axiom_grounding('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', speaker_autonomy_maximization, deontological).
narrative_ontology:cs_axiom('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', foundational, listener_harm_insufficient_for_restriction).
narrative_ontology:cs_axiom_status(listener_harm_insufficient_for_restriction, holdable).
narrative_ontology:cs_axiom_grounding('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', listener_harm_insufficient_for_restriction, conventional).
narrative_ontology:cs_reference_frame('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', unfettered_public_discourse).
narrative_ontology:cs_drift_state('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', contemporary_social_media_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('903cfaaa-1af6-4004-aeb0-37aa75df4dcd', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, listeners_harmed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or groups who wish to express themselves without fear of legal or social sanction, even if their speech is offensive or causes distress to others. They benefit from the broad protection this reading affords.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, speakers, beneficiary,
    moderate, biographical, mobile, national).

% Individuals or groups who experience direct harm (emotional, psychological, reputational) from speech that is protected under this absolutist reading. They bear the cost of unmitigated speech and have limited recourse.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, listeners_harmed, payer,
    powerless, immediate, trapped, local).

% Courts and legislative bodies tasked with interpreting and enforcing constitutional speech protections. Under this reading, their role is primarily to prevent restrictions on speech, rather than to balance speech against other harms.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, state_actors, agenda_setter,
    institutional, generational, constrained, national).

% Groups advocating for restrictions on certain types of speech (e.g., hate speech, incitement) due to its harmful effects. Their arguments are largely rejected by this reading, which prioritizes speaker autonomy.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, advocacy_groups_for_restricted_speech, excluded,
    organized, biographical, constrained, national).

% Academics and legal theorists who champion the absolutist interpretation of speech protection, arguing for minimal government intervention and maximum speaker freedom. They provide the intellectual grounding for this reading.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, legal_scholars_absolutist, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, predictable boundary for protected speech, minimizing uncertainty for speakers and reducing the state's power to censor based on content or perceived harm.
% TRANSFER_FUNCTION: Transfers the burden of harmful speech from the speaker to the listener, by prioritizing speaker autonomy over listener protection from distress or offense.
% ABSENT_VOICES: Victims of hate speech, defamation, or other harmful but protected expression are effectively silenced in the legal discourse, as their harm is deemed insufficient to justify restriction. Advocacy groups for such victims are structurally excluded from influencing the core interpretation.
% DISAPPEARANCE_RATIONALE: If this absolutist reading vanished, the legal landscape for speech would fundamentally shift. Courts would likely adopt more balancing tests, leading to increased restrictions on speech based on harm, and a significant re-evaluation of free expression principles.
% FOUNDING_PROBLEM: To prevent government censorship and ensure a robust public discourse by establishing a broad zone of protected expression.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and civil liberties organizations, even those who disagree with the absolutist interpretation, generally corroborate the historical problem of government overreach in speech regulation. The ongoing potential for censorship is widely acknowledged.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(speech_protection_kernel__absolutist_reading),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the constraint primarily protects speakers from state extraction, rather than extracting from them. However, it is not zero, as even absolutist readings acknowledge narrow categories of unprotected speech (e.g., true threats, incitement to violence) which can still 'extract' from speakers who cross these lines. Suppression is very low (0.05) as the core function is to prevent suppression. Theater ratio is zero as there's no performative maintenance; the principle is either upheld or not. Accessibility collapse is high (0.9) because alternatives to broad speech protection (e.g., content-based restrictions) are largely foreclosed. Resistance is low (0.1) from speakers, but higher from those harmed by speech, though this resistance is often ineffective against the absolutist framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers, this is a pure 'rope' or even 'mountain' that enables their expression. From the perspective of those harmed by speech, it can feel like a 'snare' that traps them in a cycle of abuse with no legal exit. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers are clear beneficiaries (d near 0.0) as the constraint maximizes their freedom. Listeners harmed by speech are payers (d near 1.0) as they bear the costs without recourse. State actors, while agenda-setters, are constrained by this reading to protect speech, making their d closer to 0.5. Advocacy groups for restricted speech are excluded, effectively making them targets of the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_principle,
    'Is the near-categorical protection of speech a genuine natural law of a free society, or a constructed legal principle that benefits identifiable agents (speakers) while imposing costs on others (harmed listeners)?',
    'Comparative legal analysis across diverse constitutional systems and historical periods: if similar protections emerge independently across different legal traditions, it supports natural law; if it correlates with specific political ideologies or power structures, it supports a constructed principle.',
    'If a constructed principle, the ''mountain'' claim is a false summit, and the constraint would reclassify as a ''tangled_rope'' or ''snare'' from the perspective of harmed listeners, reflecting the extraction of their safety/dignity for speaker autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_principle, conceptual, 'Ambiguity between natural law and constructed legal principle.').

omega_variable(
    scope_of_harm_definition,
    'How broadly should ''harm'' be defined in the context of speech? Does it include emotional distress, reputational damage, or only direct incitement to violence?',
    'Sociological and psychological research on the impact of different speech types, combined with jurisprudential evolution in tort law and human rights. A broader definition of harm would challenge the absolutist premise.',
    'If ''harm'' is defined more broadly, the absolutist reading''s core premise (listener harm is not grounds for restriction) is weakened, potentially shifting the constraint towards a ''harm_threshold_reading'' and increasing its effective extractiveness from harmed listeners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_harm_definition, empirical, 'Ambiguity in the definition and scope of ''harm'' from speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1940, speech_protection_kernel__absolutist_reading, theater_ratio, 1940, 0.0).
narrative_ontology:measurement(spee_tr_t1960, speech_protection_kernel__absolutist_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_kernel__absolutist_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__absolutist_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__absolutist_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(spee_be_t1940, speech_protection_kernel__absolutist_reading, base_extractiveness, 1940, 0.1).
narrative_ontology:measurement(spee_be_t1960, speech_protection_kernel__absolutist_reading, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(spee_be_t1980, speech_protection_kernel__absolutist_reading, base_extractiveness, 1980, 0.13).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__absolutist_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__absolutist_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1940, speech_protection_kernel__absolutist_reading, suppression_requirement, 1940, 0.05).
narrative_ontology:measurement(spee_su_t1960, speech_protection_kernel__absolutist_reading, suppression_requirement, 1960, 0.05).
narrative_ontology:measurement(spee_su_t1980, speech_protection_kernel__absolutist_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__absolutist_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__absolutist_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'speech_protection_kernel'. Each reading represents a distinct structural claim about the nature and limits of free expression, with different beneficiaries, victims, and classifications. This absolutist reading influences the others by setting a high bar for speech restriction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
