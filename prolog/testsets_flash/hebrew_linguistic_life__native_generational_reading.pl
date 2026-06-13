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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Hebrew Linguistic Life: Native Generational Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines the 'life' of the Hebrew language as requiring
 *   native acquisition by children and use in all daily, secular functions.
 *   This 'native generational' reading was central to the Zionist project of
 *   Hebrew revival, asserting that Hebrew was 'dead' from 70 CE until its
 *   modern revival (circa 1880 CE) and necessitating active intervention to
 *   achieve 'life'. This definition implicitly (and often explicitly)
 *   devalued other forms of Hebrew use (e.g., liturgical) and other Jewish
 *   languages (e.g., Yiddish, Ladino), leading to their suppression in favor
 *   of modern Hebrew.
 *
 * KEY AGENTS:
 *   - hebrew_revivalists: Agenda setter (institutional/arbitrage) — actively promoted and enforced the native generational definition.
 *   - israeli_state: Beneficiary/Agenda setter (institutional/arbitrage) — institutionalized the native generational definition as official policy.
 *   - yiddish_speakers: Payer (powerless/identity_locked) — pressured to abandon their mother tongue for Hebrew.
 *   - ladino_speakers: Payer (powerless/identity_locked) — pressured to abandon their mother tongue for Hebrew.
 *   - arabic_speakers_in_palestine: Excluded (powerless/trapped) — their native language was marginalized by the Hebrew-centric linguistic project.
 *   - linguistic_scholars: Observer (analytical/analytical) — analyze the historical and sociological impacts of the revival.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.8).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.75).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew Linguistic Life: Native Generational Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, '076bbdf5-ae7a-4dc5-8a70-982f91669069').
narrative_ontology:cs_kernel_codification('076bbdf5-ae7a-4dc5-8a70-982f91669069', implicit).
narrative_ontology:cs_authority_grounding('076bbdf5-ae7a-4dc5-8a70-982f91669069', lineage).
narrative_ontology:cs_interpretation_layer_present('076bbdf5-ae7a-4dc5-8a70-982f91669069').
narrative_ontology:cs_reading_relation('076bbdf5-ae7a-4dc5-8a70-982f91669069', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('076bbdf5-ae7a-4dc5-8a70-982f91669069', hebrew_linguistic_life__marketplace_pidgin_reading, influences).
narrative_ontology:cs_axiom('076bbdf5-ae7a-4dc5-8a70-982f91669069', foundational, language_requires_native_child_acquisition).
narrative_ontology:cs_axiom_status(language_requires_native_child_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('076bbdf5-ae7a-4dc5-8a70-982f91669069', language_requires_native_child_acquisition, conventional).
narrative_ontology:cs_axiom('076bbdf5-ae7a-4dc5-8a70-982f91669069', foundational, language_requires_secular_daily_use).
narrative_ontology:cs_axiom_status(language_requires_secular_daily_use, holdable).
narrative_ontology:cs_axiom_grounding('076bbdf5-ae7a-4dc5-8a70-982f91669069', language_requires_secular_daily_use, conventional).
narrative_ontology:cs_reference_frame('076bbdf5-ae7a-4dc5-8a70-982f91669069', modern_national_linguistic_standard).
narrative_ontology:cs_drift_state('076bbdf5-ae7a-4dc5-8a70-982f91669069', contemporary_multicultural_discourse, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('076bbdf5-ae7a-4dc5-8a70-982f91669069', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revivalists).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, israeli_state).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, arabic_speakers_in_palestine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Intellectuals and activists who championed the revival of Hebrew as a spoken language, establishing schools, dictionaries, and social norms that prioritized its use over other languages. They actively enforced the 'native generational' definition of linguistic life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revivalists, agenda_setter,
    institutional, generational, arbitrage, national).

% The nascent and established Israeli state adopted Hebrew as its official language, promoting its use through education, military, and public institutions. This institutionalized the native generational reading, making it a de facto requirement for full participation in society.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Immigrants and existing communities whose primary language was Yiddish. They faced immense social pressure, educational policies, and cultural campaigns to abandon Yiddish in favor of Hebrew, often leading to intergenerational language loss and cultural rupture.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speakers, payer,
    powerless, biographical, identity_locked, national).

% Sephardic immigrants whose primary language was Ladino. Similar to Yiddish speakers, they experienced pressure to adopt Hebrew, leading to the decline of Ladino as a vibrant spoken language in Israel.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speakers, payer,
    powerless, biographical, identity_locked, national).

% The indigenous Arabic-speaking population whose language was marginalized and devalued by the Hebrew-centric linguistic project. Their linguistic reality was largely ignored or actively suppressed within the dominant national narrative, despite their historical presence.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, arabic_speakers_in_palestine, excluded,
    powerless, generational, trapped, national).

% Academics and researchers who study language revival, language death, and the sociolinguistics of Hebrew and other Jewish languages. They analyze the historical processes, policies, and social impacts of the Hebrew revival, often critiquing its coercive aspects.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, linguistic_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, israeli_state).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a common, unifying language for the diverse Jewish immigrant populations in Palestine/Israel, fostering a shared national identity and enabling daily communication across different cultural backgrounds.
% TRANSFER_FUNCTION: Transfers linguistic dominance and cultural capital from other Jewish languages (Yiddish, Ladino) and Arabic to modern Hebrew, consolidating national identity and political power around a single linguistic standard.
% ABSENT_VOICES: Speakers of other Jewish languages (Yiddish, Ladino) who resisted the pressure to abandon their mother tongues, and the indigenous Arabic-speaking population whose linguistic rights and presence were systematically marginalized. They would argue for linguistic pluralism and against the coercive aspects of the Hebrew revival.
% DISAPPEARANCE_RATIONALE: If this definition of linguistic life vanished, the historical narrative of Hebrew's 'death and rebirth' would be fundamentally altered. The legitimacy of policies that suppressed other languages would be undermined, potentially leading to a re-evaluation of linguistic diversity within Israeli society and a different understanding of Hebrew's historical continuity.
% FOUNDING_PROBLEM: The lack of a common spoken language among Jewish immigrants from diverse diasporic communities, hindering social cohesion and national identity formation in Palestine/Israel.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli state and many Zionist historians attest that the problem of linguistic fragmentation was real and that Hebrew revival was a necessary solution. Linguistic scholars, while acknowledging the problem, also corroborate that the 'native generational' definition was a specific choice that had significant, often coercive, consequences for other languages, rather than a purely natural process.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).

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
 *   The constraint is a Tangled Rope because it provided a genuine coordination function (a common language for a diverse immigrant population) but also involved significant asymmetric extraction and suppression. Extractiveness is high (0.8) due to the demand for complete linguistic shift, imposing high costs on speakers of other languages. Suppression is high (0.75) because the revival was actively enforced through education, social pressure, and state policy, leading to the marginalization and decline of other languages. Theater ratio is low (0.1) because the effort was genuinely focused on achieving the stated goal of a living, spoken language, not merely performance. Accessibility collapse is moderate (0.6) as alternatives (other languages) were not entirely eliminated but severely constrained. Resistance is moderate (0.7) as there was significant cultural and personal resistance to abandoning ancestral languages.
 *
 * PERSPECTIVAL GAP:
 *   Hebrew revivalists and the Israeli state experienced this as a necessary, beneficial coordination effort to forge a national identity and provide a common language. Speakers of other Jewish languages experienced it as a coercive, extractive process that demanded the abandonment of their cultural heritage. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew revivalists and the Israeli state are beneficiaries and agenda-setters, as they actively shaped and benefited from the linguistic homogeneity. Yiddish and Ladino speakers are victims, bearing the cost of language shift and cultural loss. Arabic speakers in Palestine were excluded, their linguistic reality largely ignored or actively suppressed by the dominant narrative. The constraint subsidized the creation of a new national identity at the expense of linguistic diversity.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as a pure Rope (ignoring the coercive aspects) or a pure Snare (ignoring the genuine coordination function of providing a common language). It highlights the dual nature of the constraint: a solution to a collective action problem (common language) that simultaneously extracted from and suppressed other linguistic communities. The 'mandate' of revival was arguably resolved by the mid-20th century, but the constraint's effects on linguistic diversity persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of linguistic vitality, or a constructed definition serving nationalist aims?',
    'Cross-cultural linguistic studies on language death and revival, independent of nationalist narratives.',
    'If constructed, the constraint''s extractiveness and suppression are higher, as it actively suppresses linguistic diversity for a political goal. If genuine, the extraction is a necessary cost of revival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''native_generational_reading'' of the ''hebrew_linguistic_life'' kernel. Sibling readings include ''liturgical_preservation_reading'' and ''marketplace_pidgin_reading''. This reading asserts a specific, high bar for ''aliveness'' that necessitates active intervention and language shift.').

omega_variable(
    suppression_of_linguistic_diversity,
    'To what extent was the decline of Yiddish and Ladino a natural linguistic shift, versus an actively enforced suppression by the Hebrew revival movement and the nascent Israeli state?',
    'Historical sociological research on language policy, educational curricula, and social pressures in early Zionist communities and the State of Israel.',
    'If suppression was primarily active and enforced, the constraint''s suppression metric is accurate. If it was largely a natural shift, the suppression metric is overstated, and the constraint is less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_linguistic_diversity, empirical, 'Assesses the balance between natural language shift and active suppression in the decline of other Jewish languages.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1880, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__native_generational_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hebr_tr_t10, hebrew_linguistic_life__native_generational_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__native_generational_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(hebr_tr_t30, hebrew_linguistic_life__native_generational_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(hebr_be_t10, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(hebr_be_t30, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hebr_su_t10, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(hebr_su_t20, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(hebr_su_t30, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_linguistic_life' kernel, focusing on native generational acquisition. It is linked to other readings that emphasize liturgical use or marketplace function, as they represent competing definitions of linguistic vitality for Hebrew.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
