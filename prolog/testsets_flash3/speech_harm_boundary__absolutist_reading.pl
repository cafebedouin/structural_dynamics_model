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
 *   where the threshold for speech to be considered unprotected (and thus
 *   subject to legal restriction) is extremely high. This reading prioritizes
 *   speaker autonomy and the 'marketplace of ideas' over the potential harms
 *   caused by speech, leading to a narrow set of unprotected categories
 *   (e.g., incitement, true threats, defamation, obscenity). The high
 *   extractiveness reflects the significant costs borne by targets of harmful
 *   speech that falls below this high threshold. The claimed type is
 *   'tangled_rope' because it does provide a coordination function
 *   (predictability for speakers) but at a high, asymmetric cost to those
 *   harmed by speech.
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
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '29be558f-0425-4070-8df8-6d0abda1522b').
narrative_ontology:cs_kernel_codification('29be558f-0425-4070-8df8-6d0abda1522b', fixed_text).
narrative_ontology:cs_authority_grounding('29be558f-0425-4070-8df8-6d0abda1522b', lineage).
narrative_ontology:cs_interpretation_layer_present('29be558f-0425-4070-8df8-6d0abda1522b').
narrative_ontology:cs_reading_relation('29be558f-0425-4070-8df8-6d0abda1522b', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('29be558f-0425-4070-8df8-6d0abda1522b', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('29be558f-0425-4070-8df8-6d0abda1522b', foundational, speech_is_presumptively_free).
narrative_ontology:cs_axiom_status(speech_is_presumptively_free, holdable).
narrative_ontology:cs_axiom_grounding('29be558f-0425-4070-8df8-6d0abda1522b', speech_is_presumptively_free, deontological).
narrative_ontology:cs_axiom('29be558f-0425-4070-8df8-6d0abda1522b', foundational, harm_is_not_a_basis_for_restriction_unless_extreme).
narrative_ontology:cs_axiom_status(harm_is_not_a_basis_for_restriction_unless_extreme, holdable).
narrative_ontology:cs_axiom_grounding('29be558f-0425-4070-8df8-6d0abda1522b', harm_is_not_a_basis_for_restriction_unless_extreme, conventional).
narrative_ontology:cs_reference_frame('29be558f-0425-4070-8df8-6d0abda1522b', post_brandenburg_era).
narrative_ontology:cs_drift_state('29be558f-0425-4070-8df8-6d0abda1522b', contemporary_digital_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('29be558f-0425-4070-8df8-6d0abda1522b', '').
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

% Benefit from a broad scope of protected speech, allowing expression with minimal fear of legal repercussions for causing offense or distress, even when that speech causes significant harm to others. Their autonomy is prioritized.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers, beneficiary,
    moderate, biographical, mobile, national).

% Bear the direct costs of speech that causes harm (e.g., harassment, hate speech, emotional distress) but does not meet the extremely high threshold for unprotected categories. They have limited legal recourse and often face social pressure to tolerate such speech.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech, payer,
    powerless, immediate, trapped, local).

% Actively defend the broad scope of protected speech, arguing that any restriction, even for harm, risks chilling legitimate expression. They shape legal and public discourse around speech norms and thresholds.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, free_speech_advocates, agenda_setter,
    organized, generational, analytical, national).

% Disproportionately affected by harmful speech that targets their identity or characteristics. The high harm threshold means they often lack legal protection against speech that contributes to discrimination, marginalization, or incites violence against them, even if it doesn't meet the 'true threat' standard.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, vulnerable_groups, payer,
    powerless, generational, identity_locked, national).

% Analyze the legal and philosophical implications of the absolutist approach, debating its consistency with other constitutional values and its real-world impact on social cohesion and equality. They contribute to the intellectual contest over the kernel.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit high, threshold for what constitutes unprotected speech, providing predictability for speakers and minimizing state interference in expression. It coordinates the expectation that speech, even offensive, is generally tolerated.
% TRANSFER_FUNCTION: Transfers the burden of harm from speakers to the targets of speech, by setting an extremely high bar for legal intervention. It prioritizes speaker autonomy over the protection of individuals from speech-related harms.
% ABSENT_VOICES: Those who advocate for a more robust protection against speech-related harms, particularly for vulnerable groups, are often marginalized in the discourse, their concerns framed as attempts to 'censor' or 'restrict' free expression. Their perspectives are systematically downplayed in favor of speaker-centric arguments.
% DISAPPEARANCE_RATIONALE: If the absolutist reading vanished, the legal landscape around speech would immediately shift. Courts would likely adopt more balancing tests, leading to new categories of unprotected speech and increased litigation over harm. Speakers would face greater uncertainty, and targets of harmful speech would gain new avenues for redress.
% FOUNDING_PROBLEM: To prevent government censorship and ensure a robust marketplace of ideas, protecting even unpopular or offensive speech from state suppression.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historical analyses corroborate the founding problem of government overreach and censorship. However, the extent to which this problem remains the primary driver, versus the protection of certain forms of harmful speech, is contested by civil rights organizations and communication ethicists.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is high (0.85) because the absolutist reading effectively transfers the cost of speech-related harm from speakers to targets, who have little legal recourse. Suppression is low (0.1) because the constraint's persistence relies more on judicial precedent and ideological commitment than active coercion against speakers. Resistance is high (0.7) from groups advocating for greater protection against harmful speech. Accessibility collapse is low (0.2) as alternative legal frameworks (e.g., dignity-based, harm-balancing) are actively debated and pursued.
 *
 * PERSPECTIVAL GAP:
 *   Speakers experience this as a 'rope' or even a 'mountain' of fundamental liberty, while targets of harmful speech experience it as a 'snare' that traps them in a cycle of unaddressed harm. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and free speech advocates are clear beneficiaries, as their ability to express themselves is maximized. Targets of harmful speech and vulnerable groups are the primary victims, bearing the costs of speech that causes them distress or discrimination but remains protected. Legal scholars act as observers, analyzing the structural implications.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading prevents mislabeling coordination as pure extraction by acknowledging the genuine coordination function of providing a clear, high bar for speech protection. However, the high extractiveness and resistance suggest that the coordination function is increasingly overshadowed by the asymmetric transfer of harm, indicating a drift towards a more extractive classification for victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_definition_ambiguity,
    'How is ''harm'' defined in the context of speech, and does the legal definition align with the lived experience of targets?',
    'Empirical studies on the psychological and social impact of different categories of speech, combined with legal re-evaluation of harm thresholds.',
    'If the legal definition of harm is found to be too narrow relative to lived experience, the effective extractiveness for targets would be even higher, pushing the classification further towards a Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_ambiguity, empirical, 'Discrepancy between legal and experiential definitions of speech harm.').

omega_variable(
    chilling_effect_vs_actual_harm,
    'Does a high harm threshold genuinely prevent a ''chilling effect'' on legitimate speech, or does it primarily protect harmful speech while legitimate speech remains robust?',
    'Comparative legal analysis across jurisdictions with different harm thresholds, measuring rates of self-censorship versus rates of harmful speech incidents.',
    'If the chilling effect argument is found to be overstated, the justification for the high extractiveness would weaken, potentially reclassifying the constraint as a Snare even for speakers, as the coordination story would be revealed as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_vs_actual_harm, empirical, 'Trade-off between preventing chilling effects and mitigating actual harm.').

omega_variable(
    absolutist_vs_balancing_framing,
    'Is the ''absolutist'' framing of speech protection a genuine structural feature of the legal system, or a rhetorical device to resist any balancing of interests?',
    'Analysis of judicial opinions and legislative debates for explicit or implicit balancing tests, even when an absolutist outcome is reached. If balancing is consistently present but overridden, the framing is rhetorical.',
    'If the framing is primarily rhetorical, the constraint''s ''claimed_type'' as a Rope (implying genuine coordination) would be challenged, potentially reclassifying it as a Snare, as the coordination story would be revealed as cover for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_vs_balancing_framing, conceptual, 'Rhetorical vs. structural nature of absolutist speech protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_harm_boundary__absolutist_reading, theater_ratio, 1969, 0.03).
narrative_ontology:measurement(spee_tr_t1980, speech_harm_boundary__absolutist_reading, theater_ratio, 1980, 0.04).
narrative_ontology:measurement(spee_tr_t1990, speech_harm_boundary__absolutist_reading, theater_ratio, 1990, 0.04).
narrative_ontology:measurement(spee_tr_t2000, speech_harm_boundary__absolutist_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(spee_tr_t2010, speech_harm_boundary__absolutist_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(spee_tr_t2024, speech_harm_boundary__absolutist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_harm_boundary__absolutist_reading, base_extractiveness, 1969, 0.75).
narrative_ontology:measurement(spee_be_t1980, speech_harm_boundary__absolutist_reading, base_extractiveness, 1980, 0.78).
narrative_ontology:measurement(spee_be_t1990, speech_harm_boundary__absolutist_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(spee_be_t2000, speech_harm_boundary__absolutist_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(spee_be_t2010, speech_harm_boundary__absolutist_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(spee_be_t2024, speech_harm_boundary__absolutist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_harm_boundary__absolutist_reading, suppression_requirement, 1969, 0.08).
narrative_ontology:measurement(spee_su_t1980, speech_harm_boundary__absolutist_reading, suppression_requirement, 1980, 0.09).
narrative_ontology:measurement(spee_su_t1990, speech_harm_boundary__absolutist_reading, suppression_requirement, 1990, 0.09).
narrative_ontology:measurement(spee_su_t2000, speech_harm_boundary__absolutist_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(spee_su_t2010, speech_harm_boundary__absolutist_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(spee_su_t2024, speech_harm_boundary__absolutist_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, dignity_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, harm_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_harm_boundary' kernel. It is linked to the 'dignity_reading' and 'harm_balancing_reading' as sibling interpretations of the same core constitutional principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
