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
 *   constraint_id: hebrew_living_language__native_generation_reading
 *   human_readable: Hebrew as a Living Language: Native Generative Speech Reading
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'native generation' reading of Hebrew as a
 *   living language, which asserts that Hebrew's vitality depends on its use
 *   as a daily, generative vernacular by native speakers. This reading
 *   emerged during the Zionist project and led to the active promotion of
 *   modern Hebrew, often at the expense of other Jewish vernaculars like
 *   Yiddish and Ladino. The constraint is classified as a Tangled Rope
 *   because it genuinely coordinated a new national language while
 *   simultaneously extracting from and suppressing existing linguistic
 *   communities.
 *
 * KEY AGENTS:
 *   - hebrew_revivalists: Primary agenda-setter (organized/identity_locked) — actively promoted native Hebrew speech.
 *   - israeli_state_institutions: Beneficiary (institutional/constrained) — adopted Hebrew as official language, supported its spread.
 *   - yiddish_speakers: Primary victim (powerless/identity_locked) — faced pressure and suppression of their language.
 *   - ladino_speakers: Primary victim (powerless/identity_locked) — experienced similar pressures to adopt Hebrew.
 *   - non_hebrew_vernacular_speakers: Payer (moderate/constrained) — pressured to adopt Hebrew for social participation.
 *   - liturgical_scholars: Excluded (moderate/analytical) — emphasized liturgical continuity, marginalized by this reading.
 *   - literary_academics: Excluded (moderate/analytical) — focused on written literary revival, also sidelined.
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
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew as a Living Language: Native Generative Speech Reading").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, 'a22c48af-bc17-4c5e-9a6c-c9d50c288a01').
narrative_ontology:cs_kernel_codification('a22c48af-bc17-4c5e-9a6c-c9d50c288a01', formalized).
narrative_ontology:cs_authority_grounding('a22c48af-bc17-4c5e-9a6c-c9d50c288a01', lineage).
narrative_ontology:cs_interpretation_layer_present('a22c48af-bc17-4c5e-9a6c-c9d50c288a01').
narrative_ontology:cs_reading_relation('a22c48af-bc17-4c5e-9a6c-c9d50c288a01', hebrew_living_language__liturgical_continuity_reading, influences).
narrative_ontology:cs_reading_relation('a22c48af-bc17-4c5e-9a6c-c9d50c288a01', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('a22c48af-bc17-4c5e-9a6c-c9d50c288a01', foundational, daily_generative_speech_is_life).
narrative_ontology:cs_axiom_status(daily_generative_speech_is_life, holdable).
narrative_ontology:cs_axiom_grounding('a22c48af-bc17-4c5e-9a6c-c9d50c288a01', daily_generative_speech_is_life, conventional).
narrative_ontology:cs_axiom('a22c48af-bc17-4c5e-9a6c-c9d50c288a01', secondary, national_unity_requires_common_vernacular).
narrative_ontology:cs_axiom_status(national_unity_requires_common_vernacular, holdable).
narrative_ontology:cs_axiom_grounding('a22c48af-bc17-4c5e-9a6c-c9d50c288a01', national_unity_requires_common_vernacular, instrumental).
narrative_ontology:cs_reference_frame('a22c48af-bc17-4c5e-9a6c-c9d50c288a01', modern_hebrew_as_sole_vernacular).
narrative_ontology:cs_drift_state('a22c48af-bc17-4c5e-9a6c-c9d50c288a01', contemporary_multicultural_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a22c48af-bc17-4c5e-9a6c-c9d50c288a01', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_revivalists).
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

% Advocates and practitioners who actively promote Hebrew as a daily spoken language, establishing educational systems and social norms that prioritize native, generative speech over other forms of Hebrew use. They benefit from the constraint's success in establishing Hebrew as the primary vernacular.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_revivalists, agenda_setter,
    organized, generational, identity_locked, national).

% The state apparatus that adopted modern Hebrew as its official language, investing resources in its promotion and implicitly or explicitly discouraging other Jewish vernaculars. It benefits from the cultural and political cohesion a shared, 'living' national language provides.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, israeli_state_institutions, beneficiary,
    institutional, generational, constrained, national).

% Speakers of Yiddish who faced social pressure, institutional disincentives, and sometimes outright suppression of their language in favor of modern Hebrew. They bore the cost of language shift, often losing their native tongue or seeing its status diminish.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speakers, payer,
    powerless, biographical, identity_locked, local).

% Speakers of Ladino who experienced similar pressures to adopt Hebrew, leading to the decline of their traditional language. They paid the cost of cultural assimilation into the dominant Hebrew-speaking society.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speakers, payer,
    powerless, biographical, identity_locked, local).

% Any speakers of other non-Hebrew vernaculars (e.g., Arabic, Russian, Amharic) within Israel who are pressured to adopt Hebrew for full social and economic participation. They bear the cost of linguistic assimilation.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, non_hebrew_vernacular_speakers, payer,
    moderate, biographical, constrained, national).

% Scholars and religious leaders who emphasize the continuity of Hebrew through liturgical use and textual study, regardless of daily spoken fluency. Their perspective on Hebrew's 'liveness' is marginalized by the native-generation reading.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, liturgical_scholars, excluded,
    moderate, generational, analytical, global).

% Academics who focus on Hebrew's literary revival through written works, arguing that generative written competence is sufficient for a language to be 'living.' Their view is also sidelined by the emphasis on native daily speech.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, literary_academics, excluded,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, generative vernacular for a diverse immigrant population, enabling daily communication, cultural cohesion, and national identity formation in a new state.
% TRANSFER_FUNCTION: Transfers linguistic dominance and cultural capital from existing Jewish vernaculars (like Yiddish and Ladino) to modern Hebrew, requiring speakers of other languages to adopt Hebrew for full participation.
% ABSENT_VOICES: Speakers and proponents of other Jewish vernaculars (Yiddish, Ladino) who were actively suppressed or marginalized; they would argue for multilingualism or the inherent value of their own 'living' languages, but their voices were largely excluded from the dominant discourse of Hebrew revival.
% DISAPPEARANCE_RATIONALE: If the constraint of 'native generative speech' vanished, the linguistic landscape of Israel would immediately diversify. Other languages might gain prominence, and the unique cultural and political identity tied to modern Hebrew as a sole vernacular would be fundamentally altered, leading to a significant societal rearrangement.
% FOUNDING_PROBLEM: The problem of a Jewish people without a common, daily spoken language, scattered across diasporas with diverse vernaculars, hindering national unity and modern cultural expression.
% FOUNDING_PROBLEM_CORROBORATION: Hebrew revivalists and Israeli state institutions attest that the problem of national cohesion and a shared modern culture remains live, requiring a common, generative language. Sociolinguists and historians, from outside the benefiting parties, corroborate the historical problem but often contest the necessity of suppressing other languages to solve it.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high (0.65) because the constraint demanded a significant linguistic and cultural shift from many Jewish immigrants, effectively extracting their native vernaculars and replacing them with Hebrew. Suppression is high (0.70) due to active policies and social pressures discouraging other languages. Theater ratio is low (0.10) as the effort to establish Hebrew as a living language was largely genuine and functional, not performative. The initial lower values reflect the nascent stage of the revival, with extractiveness and suppression rising as the movement gained institutional power and then stabilizing once Hebrew was firmly established.
 *
 * PERSPECTIVAL GAP:
 *   Hebrew revivalists and Israeli state institutions would experience this as a necessary and beneficial coordination, fostering national identity and cultural unity. However, speakers of Yiddish, Ladino, and other vernaculars experienced it as a coercive extraction, forcing them to abandon or marginalize their heritage languages. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew revivalists and Israeli state institutions are beneficiaries, as the constraint directly serves their goals of national language building. Yiddish, Ladino, and other non-Hebrew vernacular speakers are victims, bearing the costs of linguistic suppression and cultural shift. Liturgical scholars and literary academics are excluded, as their alternative readings of Hebrew's 'liveness' are not accommodated by this constraint's definition.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the Hebrew revival as pure coordination (Rope) by highlighting the active suppression and extraction from other linguistic communities. It also avoids mislabeling it as pure extraction (Snare) by acknowledging the genuine coordination function of establishing a common national language. The 'tangled' nature captures the dual reality of a project that was both unifying and coercive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of other vernaculars structural (institutional policies, educational mandates) or internalized (social pressure, desire for assimilation)?',
    'Post-migration linguistic surveys and oral histories: if suppression persists after formal policies are relaxed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. If purely structural, policy changes would be more immediately effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-Hebrew vernaculars.').

omega_variable(
    necessity_of_suppression,
    'Was the suppression of other Jewish vernaculars a necessary condition for the successful revival of Hebrew as a daily spoken language, or could a multilingual approach have achieved similar national cohesion?',
    'Comparative historical analysis of other language revitalization movements that adopted multilingual policies, assessing their success in national identity formation and daily language use.',
    'If suppression was not necessary, the extractive component of this constraint is higher and less justifiable; if it was necessary, the coordination function is more tightly coupled to the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_suppression, conceptual, 'Whether the suppression of other languages was essential for Hebrew''s revival.').

omega_variable(
    strict_reachability_break_acknowledgment,
    'To what extent did the proponents of the native_generation_reading acknowledge the strict-reachability break from prior forms of Hebrew use (liturgical, literary) and the need for linguistic reconstruction?',
    'Analysis of primary texts from Hebrew revivalists and early Israeli linguistic policy documents for explicit statements on the discontinuity and reconstructive effort.',
    'If the break was fully acknowledged, it strengthens the claim of a new, generative language. If denied, it suggests a performative continuity claim masking a fundamental shift, increasing the theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reachability_break_acknowledgment, empirical, 'Acknowledgment of the linguistic break and reconstruction effort.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1880, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__native_generation_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_living_language__native_generation_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_living_language__native_generation_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_living_language__native_generation_reading, theater_ratio, 1940, 0.12).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_living_language__native_generation_reading, theater_ratio, 1960, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__native_generation_reading, base_extractiveness, 1880, 0.3).
narrative_ontology:measurement(hebr_be_t1900, hebrew_living_language__native_generation_reading, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement(hebr_be_t1920, hebrew_living_language__native_generation_reading, base_extractiveness, 1920, 0.6).
narrative_ontology:measurement(hebr_be_t1940, hebrew_living_language__native_generation_reading, base_extractiveness, 1940, 0.68).
narrative_ontology:measurement(hebr_be_t1960, hebrew_living_language__native_generation_reading, base_extractiveness, 1960, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__native_generation_reading, suppression_requirement, 1880, 0.2).
narrative_ontology:measurement(hebr_su_t1900, hebrew_living_language__native_generation_reading, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement(hebr_su_t1920, hebrew_living_language__native_generation_reading, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(hebr_su_t1940, hebrew_living_language__native_generation_reading, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(hebr_su_t1960, hebrew_living_language__native_generation_reading, suppression_requirement, 1960, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_living_language' kernel. Its emphasis on native generative speech directly influenced and often suppressed other understandings of Hebrew's vitality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
