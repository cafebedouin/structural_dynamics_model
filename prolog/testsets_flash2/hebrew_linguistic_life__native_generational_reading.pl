% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__native_generational_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Hebrew Linguistic Life: Native Generational Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines 'linguistic life' for Hebrew as requiring native,
 *   generational acquisition and use in all daily functions, including
 *   secular speech. This reading was central to the Hebrew revival movement
 *   and Zionist nationalism, which aimed to transform Hebrew from a
 *   liturgical and scholarly language into a modern vernacular. The
 *   constraint implies that Hebrew was 'dead' for centuries (70-1880 CE) and
 *   required active 'revival.' This process involved significant suppression
 *   of other Jewish languages, making it highly extractive for their
 *   speakers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.85).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.9).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, snare).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew Linguistic Life: Native Generational Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, '87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2').
narrative_ontology:cs_kernel_codification('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2', implicit).
narrative_ontology:cs_authority_grounding('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2', extraction).
narrative_ontology:cs_interpretation_layer_present('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2').
narrative_ontology:cs_reading_relation('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2', foundational, native_generational_acquisition_is_life).
narrative_ontology:cs_axiom_status(native_generational_acquisition_is_life, holdable).
narrative_ontology:cs_axiom_grounding('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2', native_generational_acquisition_is_life, conventional).
narrative_ontology:cs_axiom('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2', foundational, secular_mundane_use_is_life).
narrative_ontology:cs_axiom_status(secular_mundane_use_is_life, holdable).
narrative_ontology:cs_axiom_grounding('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2', secular_mundane_use_is_life, conventional).
narrative_ontology:cs_reference_frame('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2', modern_european_nation_state_linguistic_model).
narrative_ontology:cs_drift_state('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2', contemporary_multiculturalism_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('87e15c4e-1021-4a2f-9670-cf8a8ad3a7d2', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revival_movement_leaders).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, zionist_nationalist_ideologues).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jews).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, linguistic_diversity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promoted the exclusive use of Hebrew as a spoken vernacular, viewing it as essential for national identity and cultural renewal. They established schools, published materials, and exerted social pressure to abandon other Jewish languages.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revival_movement_leaders, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefited from the linguistic unification as it reinforced a singular national identity and facilitated the creation of a new, secular Israeli culture. They provided political and financial support for the revival efforts.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, zionist_nationalist_ideologues, beneficiary,
    powerful, generational, constrained, national).

% Were pressured, often coercively, to abandon Yiddish in favor of Hebrew. This resulted in the loss of their mother tongue, cultural heritage, and social marginalization within the nascent Israeli society. Their identity was deeply intertwined with Yiddish.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speakers, payer,
    powerless, biographical, identity_locked, regional).

% Similar to Yiddish speakers, they faced immense pressure to switch to Hebrew, leading to the decline of Ladino and the erosion of their distinct Sephardic cultural identity. Abandoning Ladino meant losing a direct link to their ancestral heritage.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speakers, payer,
    powerless, biographical, identity_locked, regional).

% Experienced similar linguistic displacement, being compelled to adopt Hebrew and often facing stigmatization for their Arabic linguistic and cultural background. This severed ties to centuries of shared heritage with Arab societies.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jews, payer,
    powerless, biographical, identity_locked, regional).

% Argued for the value of multilingualism and the preservation of all Jewish languages, but their voices were largely marginalized or actively suppressed by the dominant nationalist narrative. They were excluded from the core decision-making processes of the revival.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, linguistic_diversity_advocates, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a unified national language for a new Jewish state, facilitating communication, education, and cultural cohesion among diverse immigrant populations.
% TRANSFER_FUNCTION: Transferred linguistic dominance and cultural capital from existing Jewish vernaculars (Yiddish, Ladino, Judeo-Arabic) to Modern Hebrew, along with the associated social and political power.
% ABSENT_VOICES: The voices of those who wished to maintain their mother tongues (Yiddish, Ladino, Judeo-Arabic speakers) were largely absent from the dominant discourse, or actively silenced. Linguistic diversity advocates were also marginalized.
% DISAPPEARANCE_RATIONALE: If this definition of 'linguistic life' vanished, the historical narrative of Hebrew's 'death' and 'revival' would be fundamentally challenged. The justification for the suppression of other Jewish languages would collapse, leading to a re-evaluation of linguistic heritage and potentially a resurgence of interest in these languages.
% FOUNDING_PROBLEM: The perceived lack of a common, secular, spoken language for the Jewish people, essential for national self-determination and the creation of a modern, unified culture.
% FOUNDING_PROBLEM_CORROBORATION: The Hebrew revival movement leaders and Zionist ideologues attest that the problem of national linguistic unity was live and successfully addressed. However, linguistic historians and cultural anthropologists, from outside the benefiting parties, corroborate the problem's existence but contest the necessity and methods of the 'revival' at the expense of other languages.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__native_generational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__native_generational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the constraint demanded the abandonment of existing mother tongues and cultural practices for a new, imposed linguistic norm. Suppression is very high (0.9) due to the active social, educational, and political pressure exerted to enforce Hebrew exclusivity, often leading to the stigmatization of other languages. Theater ratio is low (0.05) as the movement was genuinely focused on functional linguistic shift, not mere performance. The historical measurements show a clear increase in extractiveness and suppression as the revival movement gained momentum and enforced its vision.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hebrew revivalists, this constraint was a necessary 'rope' for national rebirth. From the perspective of Yiddish or Ladino speakers, it was a 'snare' that coerced them into abandoning their heritage. The engine's classification will reflect the latter due to the high extractiveness and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew revival leaders and Zionist ideologues are clear beneficiaries, as the constraint directly served their ideological and nation-building goals. Speakers of other Jewish languages (Yiddish, Ladino, Judeo-Arabic) are victims, bearing the direct cost of linguistic and cultural displacement. Linguistic diversity advocates are excluded, their arguments against linguistic monoculture being ignored or suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to create a living, national language. While this goal was achieved, the methods employed, particularly the suppression of other languages, indicate that the 'coordination' function was deeply intertwined with 'extraction.' The high suppression and extractiveness prevent mislabeling this as a simple coordination problem; it was a coercive transformation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dormancy_vs_death,
    'Was Hebrew truly ''dead'' between 70 CE and 1880 CE, or was it in a state of dormancy/specialized use (liturgical, scholarly) that constitutes a different form of ''life''?',
    'Conceptual re-evaluation of ''linguistic life'' criteria, acknowledging diverse forms of language vitality beyond native vernacular use. Historical linguistic analysis of the continuous, albeit specialized, use of Hebrew during this period.',
    'If reclassified as dormant, the ''revival'' narrative loses its foundational premise, and the justification for suppressing other languages weakens. This would shift the constraint''s perceived necessity and potentially lower its extractiveness in retrospect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dormancy_vs_death, conceptual, 'Ambiguity in the definition of ''linguistic life'' for Hebrew.').

omega_variable(
    coercion_vs_choice,
    'To what extent was the shift to Hebrew a voluntary choice by individuals, versus a result of social, economic, and political coercion?',
    'Sociological studies of language shift in early Israeli society, including oral histories and archival research documenting pressures on non-Hebrew speakers. Analysis of institutional policies (e.g., in schools, public administration) that favored Hebrew.',
    'If coercion is found to be the dominant factor, the constraint''s suppression metric is further validated, reinforcing its classification as a snare. If significant voluntary adoption is demonstrated, it might suggest a more complex ''tangled rope'' dynamic, though still highly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_choice, empirical, 'The balance between voluntary adoption and coercive enforcement in the Hebrew revival.').

omega_variable(
    linguistic_diversity_cost,
    'Was the loss of linguistic diversity (Yiddish, Ladino, Judeo-Arabic) an unavoidable cost of Hebrew revival, or could a multilingual national identity have been fostered?',
    'Comparative studies of other nation-building projects that successfully maintained multilingualism. Counterfactual historical analysis exploring alternative policy choices and their potential outcomes.',
    'If a multilingual path was viable, the constraint''s victim set expands to include ''lost linguistic heritage'' as an avoidable cost, further solidifying its extractive nature and challenging the ''necessity'' argument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_diversity_cost, preference, 'Whether linguistic diversity was an unavoidable casualty of nation-building.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1880, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1920, 0.06).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1950, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1880, 0.4).
narrative_ontology:measurement(hebr_be_t1900, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(hebr_be_t1920, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1920, 0.75).
narrative_ontology:measurement(hebr_be_t1950, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1950, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1880, 0.3).
narrative_ontology:measurement(hebr_su_t1900, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(hebr_su_t1920, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1920, 0.8).
narrative_ontology:measurement(hebr_su_t1950, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1950, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_linguistic_life' kernel. It defines linguistic life as requiring native, generational acquisition and use in all daily functions, leading to the 'death' and 'revival' narrative. This reading directly influenced the 'hebrew_as_sole_national_language' constraint by providing its ideological justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
