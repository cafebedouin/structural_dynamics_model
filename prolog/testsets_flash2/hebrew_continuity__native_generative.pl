% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Continuity: Native Generative Reading
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'native generative' reading of Hebrew
 *   continuity, which asserts that Hebrew truly lives only through the daily,
 *   intuitive use of native speakers. This reading prioritizes modern Israeli
 *   Hebrew and actively standardizes it, often at the expense of older,
 *   liturgical, or diaspora forms. The constraint is claimed as a 'tangled
 *   rope' because it genuinely coordinates a living language but also
 *   extracts linguistic authority and cultural legitimacy from non-native
 *   forms, requiring active enforcement to maintain its dominance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.68).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.75).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Continuity: Native Generative Reading").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '8ce392f9-df19-4ad8-aaa9-0fe13a653aa7').
narrative_ontology:cs_kernel_codification('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7', formalized).
narrative_ontology:cs_authority_grounding('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7', lineage).
narrative_ontology:cs_interpretation_layer_present('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7').
narrative_ontology:cs_reading_relation('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7', hebrew_continuity__bridge_pidginized, forecloses).
narrative_ontology:cs_axiom('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7', foundational, native_generative_use_is_life).
narrative_ontology:cs_axiom_status(native_generative_use_is_life, holdable).
narrative_ontology:cs_axiom_grounding('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7', native_generative_use_is_life, conventional).
narrative_ontology:cs_axiom('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7', secondary, linguistic_standardization_is_progress).
narrative_ontology:cs_axiom_status(linguistic_standardization_is_progress, holdable).
narrative_ontology:cs_axiom_grounding('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7', linguistic_standardization_is_progress, instrumental).
narrative_ontology:cs_reference_frame('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7', modern_israeli_hebrew_as_sole_standard).
narrative_ontology:cs_drift_state('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8ce392f9-df19-4ad8-aaa9-0fe13a653aa7', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_linguistic_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, native_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_liturgical_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, historical_hebrew_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, hebrew_language_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institutional body that codifies and promotes modern Hebrew, emphasizing its native, generative use. It defines 'correct' Hebrew based on contemporary usage and actively standardizes vocabulary and grammar, often marginalizing older forms.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_linguistic_academy, agenda_setter,
    institutional, generational, constrained, national).

% The community whose daily, intuitive use of Hebrew is validated and elevated as the standard. They benefit from the linguistic and cultural centrality of modern Hebrew, which is shaped by their generative practice.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, native_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% Communities for whom Hebrew is primarily a language of prayer, ritual, and ancient texts. Their forms of Hebrew are often deemed 'dead' or 'inauthentic' by the native-generative standard, leading to a devaluation of their linguistic heritage and a sense of exclusion from the 'living' language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_liturgical_communities, payer,
    powerless, generational, identity_locked, global).

% Academics who study the historical evolution of Hebrew across its various stages. They find their work on non-modern forms of Hebrew increasingly marginalized or devalued in contexts dominated by the native-generative paradigm, which prioritizes contemporary usage.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, historical_hebrew_scholars, payer,
    moderate, civilizational, constrained, global).

% Individuals learning modern Hebrew benefit from a standardized, actively used language with clear pedagogical resources. They are drawn into the native-generative paradigm as the 'correct' way to learn and speak Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, hebrew_language_learners, beneficiary,
    moderate, immediate, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, living standard for Hebrew, enabling clear communication and cultural cohesion among its native speakers, and providing a clear target for language learners.
% TRANSFER_FUNCTION: Transfers linguistic authority and cultural centrality from historical and liturgical forms of Hebrew to modern, natively spoken Hebrew, devaluing other forms and their associated communities.
% ABSENT_VOICES: Communities and scholars who prioritize the historical, liturgical, or pidginized forms of Hebrew are often excluded from the discourse that defines 'living' Hebrew. They would argue for a broader, more inclusive definition of Hebrew continuity.
% DISAPPEARANCE_RATIONALE: If the native-generative standard vanished, the linguistic landscape of Israel and the Jewish diaspora would undergo significant shifts. The authority of the Israeli Linguistic Academy would diminish, other forms of Hebrew might gain legitimacy, and the cultural hierarchy surrounding Hebrew would be profoundly altered.
% FOUNDING_PROBLEM: The problem of a 'dead' language, used only for ritual or scholarship, lacking a vibrant, daily, spoken life, and thus unable to serve as a modern national language.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli Linguistic Academy and native Hebrew speakers attest that the problem of language vitality is still live, citing the need for ongoing linguistic development and adaptation. While the language is clearly 'alive,' the ongoing effort to maintain its native, generative status against other forms is a continuous project.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because this reading devalues and marginalizes other forms of Hebrew, effectively extracting their linguistic legitimacy. Suppression is also high, as the Israeli Linguistic Academy actively enforces the native-generative standard through education, media, and cultural institutions, suppressing alternatives. Theater ratio is low because the project of language standardization and revitalization is a genuine, active effort, not merely performative. The metrics show a steady increase in extractiveness and suppression as the native-generative paradigm solidified its dominance over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of native Hebrew speakers and the Academy, this constraint is a necessary coordination mechanism for a vibrant national language. From the perspective of diaspora liturgical communities, it is an extractive force that diminishes their heritage and identity. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli Linguistic Academy and native Hebrew speakers are beneficiaries, as their linguistic practices and institutional authority are validated and promoted. Diaspora liturgical communities and historical Hebrew scholars are victims, as their forms of Hebrew are marginalized and their contributions to Hebrew continuity are devalued. Language learners are beneficiaries as they gain access to a standardized, living language.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_living_language,
    'Is ''living language'' exclusively defined by native, generative use, or can it encompass other forms of active, meaningful engagement (e.g., liturgical, scholarly, pidginized)?',
    'A shift in sociolinguistic consensus or institutional policy to formally recognize multiple valid forms of ''living'' Hebrew, or empirical studies demonstrating the functional vitality of non-native forms.',
    'If the definition broadens, the extractiveness and suppression of this constraint would decrease, potentially reclassifying it as a ''rope'' or even a ''scaffold'' if its purpose becomes transitional support for a pluralistic linguistic landscape. If the definition remains narrow, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Ambiguity in the conceptual definition of a ''living language'' and its impact on linguistic legitimacy.').

omega_variable(
    identity_lock_of_diaspora_communities,
    'To what extent are diaspora liturgical communities ''identity_locked'' into their forms of Hebrew, making exit from the constraint''s devaluation difficult?',
    'Qualitative sociological studies exploring the self-perception and linguistic practices of these communities, and their responses to the native-generative paradigm. Analysis of whether they actively resist or internalize the marginalization.',
    'If identity-lock is profound, the effective extraction from these communities is higher, as the cost of abandoning their linguistic heritage is existential. If they are more ''constrained'' or ''mobile'' in their linguistic identity, the effective extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_diaspora_communities, empirical, 'The degree to which linguistic identity binds diaspora communities to their forms of Hebrew, affecting their exit options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1948, hebrew_continuity__native_generative, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(hebr_tr_t1968, hebrew_continuity__native_generative, theater_ratio, 1968, 0.12).
narrative_ontology:measurement(hebr_tr_t1988, hebrew_continuity__native_generative, theater_ratio, 1988, 0.14).
narrative_ontology:measurement(hebr_tr_t2008, hebrew_continuity__native_generative, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_continuity__native_generative, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1948, hebrew_continuity__native_generative, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(hebr_be_t1968, hebrew_continuity__native_generative, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement(hebr_be_t1988, hebrew_continuity__native_generative, base_extractiveness, 1988, 0.62).
narrative_ontology:measurement(hebr_be_t2008, hebrew_continuity__native_generative, base_extractiveness, 2008, 0.66).
narrative_ontology:measurement(hebr_be_t2024, hebrew_continuity__native_generative, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1948, hebrew_continuity__native_generative, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(hebr_su_t1968, hebrew_continuity__native_generative, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(hebr_su_t1988, hebrew_continuity__native_generative, suppression_requirement, 1988, 0.68).
narrative_ontology:measurement(hebr_su_t2008, hebrew_continuity__native_generative, suppression_requirement, 2008, 0.72).
narrative_ontology:measurement(hebr_su_t2024, hebrew_continuity__native_generative, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_continuity' kernel. This 'native_generative' reading asserts that Hebrew lives only through native speaker intuition and daily generative use, contrasting with liturgical preservation and pidginized bridge language readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
