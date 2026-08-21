% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__native_generation_reading, []).

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
 *   constraint_id: hebrew_living_language__native_generation_reading
 *   human_readable: Hebrew as a Living Language: Native Generative Speech Reading
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story analyzes the 'native generation' reading of Hebrew
 *   as a living language, which posits that Hebrew's vitality depends on its
 *   use as a primary, generatively spoken vernacular by native speakers,
 *   rather than through liturgical recitation or literary production alone.
 *   This reading was central to the Zionist project of language revival and
 *   nation-building, leading to the active promotion of Hebrew and, often,
 *   the suppression of other Jewish vernaculars like Yiddish and Ladino. The
 *   constraint is claimed as a Tangled Rope because it genuinely coordinated
 *   a national identity and communication, but did so with significant,
 *   asymmetric extraction from speakers of other languages.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.65).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.7).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew as a Living Language: Native Generative Speech Reading").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '6d3ca8bb-b7d5-4bd9-9c64-55831c17168b').
narrative_ontology:cs_kernel_codification('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b', implicit).
narrative_ontology:cs_authority_grounding('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b', lineage).
narrative_ontology:cs_interpretation_layer_present('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b').
narrative_ontology:cs_reading_relation('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b', hebrew_living_language__liturgical_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b', hebrew_living_language__literary_revival_reading, forecloses).
narrative_ontology:cs_axiom('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b', foundational, generative_speech_is_life).
narrative_ontology:cs_axiom_status(generative_speech_is_life, holdable).
narrative_ontology:cs_axiom_grounding('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b', generative_speech_is_life, conventional).
narrative_ontology:cs_axiom('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b', secondary, diaspora_languages_are_dead).
narrative_ontology:cs_axiom_status(diaspora_languages_are_dead, holdable).
narrative_ontology:cs_axiom_grounding('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b', diaspora_languages_are_dead, conventional).
narrative_ontology:cs_reference_frame('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b', modern_national_vernacular).
narrative_ontology:cs_drift_state('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b', contemporary_multicultural_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6d3ca8bb-b7d5-4bd9-9c64-55831c17168b', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_language_revivalists).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, non_hebrew_vernacular_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and implements policies that prioritize Hebrew as the sole living vernacular, actively promoting native generative speech and discouraging other Jewish languages. Their identity is fused with the success of this project.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_language_revivalists, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefits from a unified national language as a cornerstone of national identity and cohesion. Provides funding and institutional support for Hebrew education and cultural programs, while implicitly or explicitly marginalizing other languages.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, israeli_state_institutions, beneficiary,
    institutional, generational, constrained, national).

% Historically faced social and institutional pressure to abandon Yiddish in favor of Hebrew. Their language was often framed as a 'diaspora' language, antithetical to the 'new' Hebrew culture. Many experienced a loss of their native tongue and cultural heritage.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speakers, payer,
    powerless, biographical, identity_locked, regional).

% Similar to Yiddish speakers, Ladino speakers experienced marginalization and pressure to adopt Hebrew, leading to a decline in the use of their traditional language. Their cultural identity was challenged by the singular focus on Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speakers, payer,
    powerless, biographical, identity_locked, regional).

% Any individual or community whose primary vernacular was not Hebrew (e.g., Arabic speakers, various immigrant groups) faced a linguistic hierarchy where Hebrew was privileged, impacting their access to services, education, and social mobility if they did not adopt it.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, non_hebrew_vernacular_speakers, payer,
    powerless, biographical, constrained, national).

% Observe the Israeli language policy with mixed feelings. Some embrace the revival of Hebrew as a national language, while others lament the suppression of other Jewish languages that were central to their own cultural heritage.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, diaspora_jewish_communities, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a unified, modern national language for the nascent Israeli state, enabling daily communication, cultural production, and administrative functions among a diverse immigrant population.
% TRANSFER_FUNCTION: Transfers linguistic dominance, cultural capital, and national identity from diverse Jewish vernaculars (like Yiddish and Ladino) to modern Hebrew, enforced through educational, social, and political pressures.
% ABSENT_VOICES: Speakers of Yiddish, Ladino, and other Jewish vernaculars were often marginalized or actively suppressed in the early stages of Hebrew revival. They would argue for linguistic pluralism and the recognition of their languages as equally 'living' and vital to Jewish heritage.
% DISAPPEARANCE_RATIONALE: If the constraint that Hebrew is 'living' only through native generative speech vanished, the linguistic landscape of Israel would fundamentally shift. Other Jewish languages might experience a resurgence, and the exclusive focus on Hebrew as the sole marker of national identity would be challenged, leading to a more pluralistic linguistic environment.
% FOUNDING_PROBLEM: The early Zionist movement sought to create a new, unified national identity for Jewish people, which required a common, modern language distinct from the languages of the diaspora.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Zionism and Israeli state-building corroborate the foundational problem of national unity and identity. Sociolinguists and cultural critics, from outside the immediate beneficiaries, attest that the problem of linguistic unity remains live, though its solutions and consequences are contested.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_living_language__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the emphasis on native generative speech led to the marginalization and decline of other Jewish languages, imposing a cost on their speakers. Suppression is also high (0.7) due to active institutional policies and social pressures that discouraged the use of non-Hebrew vernaculars. The theater ratio is low (0.1) because the project of creating a natively spoken Hebrew was a genuine, functional effort, not primarily performative. Accessibility collapse is moderate (0.4) as alternatives (other languages) were not entirely eliminated but were significantly constrained. Resistance is moderate (0.5) reflecting ongoing cultural and academic debates, as well as some continued use of other languages despite pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hebrew language revivalists and Israeli state institutions, this constraint was a necessary and beneficial 'Rope' for national cohesion and cultural renewal. From the perspective of Yiddish or Ladino speakers, it operated as a 'Snare,' extracting their linguistic heritage and imposing a new linguistic identity. The engine's classification will reflect this divergence based on the declared beneficiary/victim structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew language revivalists and Israeli state institutions are clear beneficiaries (low d) as they achieved their goal of a national language and solidified national identity. Speakers of Yiddish, Ladino, and other non-Hebrew vernaculars are victims (high d) as their languages were suppressed and they bore the costs of linguistic shift. Diaspora Jewish communities are observers, with varied perspectives depending on their own linguistic and cultural ties.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure 'Rope' (ignoring the extraction) or a pure 'Snare' (ignoring the genuine coordination function of nation-building). It acknowledges the dual nature of the constraint: a successful coordination of national identity that simultaneously involved significant linguistic and cultural extraction. The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that while the initial problem of national language is addressed, the constraint's persistence still shapes the linguistic reality, and its removal would cause significant reorganization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of other Jewish languages structural (institutional policies, educational systems) or internalized (social pressure, self-censorship)?',
    'Post-exit suppression trajectory: if suppression persists after the institutional mechanisms are removed (e.g., in diaspora communities), reclassify as partially internalized. Historical sociological studies could also differentiate.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests, as individuals carried the suppression with them. This would amplify the extractive nature for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-Hebrew vernaculars.').

omega_variable(
    linguistic_pluralism_impact,
    'Would a more pluralistic linguistic policy (e.g., state support for Yiddish/Ladino) have undermined the national identity coordination function, or would it have enriched it?',
    'Comparative analysis with other multilingual nation-states that successfully fostered national identity alongside linguistic diversity. Counterfactual historical analysis.',
    'If pluralism would not have undermined coordination, the ''necessity'' of suppression is weakened, shifting the constraint closer to a Snare. If it would have undermined, the coordination function is more robust, supporting the Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(linguistic_pluralism_impact, preference, 'Impact of linguistic pluralism on national identity coordination.').

omega_variable(
    kernel_reading_divergence,
    'Is the ''native_generation_reading'' truly distinct from the ''literary_revival_reading'' in its practical implications for language policy and suppression?',
    'Detailed historical analysis of policy implementation: did policies explicitly target daily speech over literary production, or were they conflated? Examination of educational curricula and cultural funding allocations.',
    'If the practical implications were largely conflated, the two readings might collapse into a single, broader ''modern Hebrew'' constraint, potentially altering the perceived extractiveness and victim set by diluting the specific focus on native speech.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Distinction between native generative speech and literary production as criteria for a ''living'' language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1880, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__native_generation_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_living_language__native_generation_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_living_language__native_generation_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__native_generation_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_living_language__native_generation_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__native_generation_reading, base_extractiveness, 1880, 0.4).
narrative_ontology:measurement(hebr_be_t1920, hebrew_living_language__native_generation_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(hebr_be_t1960, hebrew_living_language__native_generation_reading, base_extractiveness, 1960, 0.7).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__native_generation_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(hebr_be_t2020, hebrew_living_language__native_generation_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__native_generation_reading, suppression_requirement, 1880, 0.3).
narrative_ontology:measurement(hebr_su_t1920, hebrew_living_language__native_generation_reading, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(hebr_su_t1960, hebrew_living_language__native_generation_reading, suppression_requirement, 1960, 0.75).
narrative_ontology:measurement(hebr_su_t2000, hebrew_living_language__native_generation_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(hebr_su_t2020, hebrew_living_language__native_generation_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
