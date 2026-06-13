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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Continuity via Native Generative Use
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story analyzes the 'native_generative' reading of Hebrew
 *   continuity, which posits that Hebrew truly lives only through the
 *   intuition and daily generative use of its native speakers. This reading,
 *   largely championed by modern Israeli linguistic institutions, has been
 *   instrumental in the successful revitalization of Hebrew as a spoken
 *   language. However, it simultaneously marginalizes other forms of Hebrew
 *   continuity, such as liturgical preservation or its use as a pidginized
 *   bridge language in the diaspora. The constraint operates as a Tangled
 *   Rope: it coordinates the development of modern Hebrew but extracts from
 *   communities whose engagement with Hebrew does not conform to the
 *   native-generative ideal.
 *
 * KEY AGENTS:
 *   - israeli_linguistic_academy: Primary agenda_setter (institutional/constrained)
 *   - native_hebrew_speakers: Primary beneficiary (organized/mobile)
 *   - diaspora_liturgical_communities: Primary payer (moderate/identity_locked)
 *   - non_native_hebrew_scholars: Secondary payer (moderate/constrained)
 *   - language_revitalization_theorists: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.65).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.7).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.65).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Continuity via Native Generative Use").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '27e416b5-1b4a-414c-8f56-75f495413764').
narrative_ontology:cs_kernel_codification('27e416b5-1b4a-414c-8f56-75f495413764', formalized).
narrative_ontology:cs_authority_grounding('27e416b5-1b4a-414c-8f56-75f495413764', lineage).
narrative_ontology:cs_interpretation_layer_present('27e416b5-1b4a-414c-8f56-75f495413764').
narrative_ontology:cs_reading_relation('27e416b5-1b4a-414c-8f56-75f495413764', hebrew_continuity__liturgical_preservation, influences).
narrative_ontology:cs_reading_relation('27e416b5-1b4a-414c-8f56-75f495413764', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('27e416b5-1b4a-414c-8f56-75f495413764', foundational, native_speaker_intuition_is_ultimate_arbiter).
narrative_ontology:cs_axiom_status(native_speaker_intuition_is_ultimate_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('27e416b5-1b4a-414c-8f56-75f495413764', native_speaker_intuition_is_ultimate_arbiter, conventional).
narrative_ontology:cs_axiom('27e416b5-1b4a-414c-8f56-75f495413764', foundational, daily_generative_use_defines_vitality).
narrative_ontology:cs_axiom_status(daily_generative_use_defines_vitality, holdable).
narrative_ontology:cs_axiom_grounding('27e416b5-1b4a-414c-8f56-75f495413764', daily_generative_use_defines_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('27e416b5-1b4a-414c-8f56-75f495413764', modern_spoken_hebrew_revitalization).
narrative_ontology:cs_drift_state('27e416b5-1b4a-414c-8f56-75f495413764', contemporary_diaspora_linguistic_pluralism, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('27e416b5-1b4a-414c-8f56-75f495413764', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_linguistic_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, native_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_liturgical_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, non_native_hebrew_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the standards for modern Hebrew, promoting lexical expansion and phonological standardization based on native speaker intuition. Benefits from the perceived vitality and 'authenticity' of modern Hebrew, which reinforces its authority.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_linguistic_academy, agenda_setter,
    institutional, generational, constrained, national).

% Their daily generative use and intuition are elevated as the ultimate arbiter of 'living' Hebrew. They benefit from the cultural and social capital associated with being native speakers of a 'revived' language, and their linguistic practices are validated.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, native_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% Their traditional, liturgical use of Hebrew is often devalued or deemed 'dead' by the native-generative standard. They bear the cost of linguistic marginalization and the pressure to conform to modern Israeli Hebrew, despite their deep historical connection to the language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_liturgical_communities, payer,
    moderate, generational, identity_locked, global).

% Their academic study and use of Hebrew, often focused on historical texts or non-native fluency, are implicitly or explicitly deemed less 'authentic' or 'living' than native generative use. They face pressure to adopt modern Hebrew norms or risk being seen as irrelevant to the language's 'true' vitality.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, non_native_hebrew_scholars, payer,
    moderate, biographical, constrained, global).

% Analyze the success and implications of the Hebrew revitalization project, including the social and linguistic costs of prioritizing native generative use over other forms of continuity. They can critique the constraint's mechanisms and outcomes.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, language_revitalization_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ongoing development and standardization of Hebrew as a modern, living language, ensuring a shared linguistic reference point for its native speakers and cultural institutions.
% TRANSFER_FUNCTION: Transfers linguistic authority and cultural capital from traditional, liturgical, or scholarly forms of Hebrew to modern, native generative use, effectively marginalizing non-native forms.
% ABSENT_VOICES: Historical linguists who emphasize the unbroken textual tradition of Hebrew, and communities for whom Hebrew is primarily a sacred language, are often excluded from the discourse on 'living' Hebrew. They would argue for a broader definition of linguistic vitality.
% DISAPPEARANCE_RATIONALE: If the constraint of 'native generative use' as the sole arbiter of Hebrew's life vanished, the linguistic landscape would diversify. Liturgical and scholarly forms would regain legitimacy, potentially leading to a more pluralistic understanding of Hebrew's continuity, but also possibly fragmenting the modern language's standardization efforts.
% FOUNDING_PROBLEM: The perceived 'death' of Hebrew as a spoken language, leading to its relegation to liturgical and academic contexts, threatening its role as a national language for a nascent state.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli Linguistic Academy and native Hebrew speakers attest that the problem of ensuring Hebrew's vitality as a modern language is still live, citing the need for continuous adaptation and expansion. Language revitalization theorists, from outside the benefiting parties, corroborate the historical problem but contest the current status, arguing the language is now robustly 'live' and the constraint has shifted to one of linguistic hegemony.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).

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
 *   The extractiveness (0.65) stems from the devaluation and marginalization of non-native forms of Hebrew, forcing other communities to either conform or accept their linguistic practices as 'dead.' Suppression (0.70) is high due to the institutional power of the Israeli linguistic academy in setting norms and the social pressure on non-native speakers to adopt modern Hebrew. The theater ratio is low (0.10) because the efforts to promote native generative use are genuinely functional in language revitalization, even if they come with extractive side effects. The historical measurements show a steady increase in extractiveness and suppression as the native-generative paradigm solidified its dominance over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Israeli Linguistic Academy and native Hebrew speakers, this constraint is a successful coordination mechanism that ensured Hebrew's survival and modernization. From the perspective of diaspora liturgical communities and non-native scholars, it is an extractive force that devalues their legitimate forms of engagement with Hebrew, creating a linguistic hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli Linguistic Academy and native Hebrew speakers are clear beneficiaries, as their linguistic practices and authority are elevated. Diaspora liturgical communities and non-native scholars are targets, bearing the costs of marginalization and devaluation. Their 'identity_locked' exit options for liturgical communities reflect the deep cultural and religious ties that make abandoning traditional Hebrew unthinkable, even as it is devalued.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to revive Hebrew as a spoken language. While this problem is largely 'live' in terms of ongoing development, the constraint has arguably shifted from pure revitalization to enforcing a specific, exclusionary definition of 'living' Hebrew. This analysis prevents mislabeling the constraint as a pure Rope (ignoring the extraction from non-native forms) or a pure Snare (ignoring the genuine coordination function of language standardization). The 'contested' status of the founding problem reflects this shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_living_language,
    'Is ''living language'' exclusively defined by native generative use, or can it encompass other forms of continuous, active engagement (e.g., liturgical, scholarly, pidginized)?',
    'Conceptual analysis and cross-linguistic comparison of language vitality metrics, particularly for languages with complex historical and diasporic contexts.',
    'If a broader definition is adopted, the constraint''s extractiveness and suppression of non-native forms would be re-evaluated as less justified, potentially reclassifying it as a Snare due to its narrow, exclusionary definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Ambiguity in the definition of ''living language'' and its implications for Hebrew''s continuity.').

omega_variable(
    native_generative_vs_constructed_kernel,
    'Is the emphasis on native generative use a natural outcome of language revitalization, or a constructed ideological stance that benefits specific linguistic and nationalistic agendas?',
    'Sociological and historical analysis of the language revitalization movement, examining the explicit and implicit goals of its key actors and institutions, and the power dynamics involved in linguistic standardization.',
    'If primarily a constructed stance, the constraint''s ''naturalness'' claim would be undermined, and its classification would lean more strongly towards a Snare or Tangled Rope, highlighting the active enforcement of a particular linguistic ideology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_generative_vs_constructed_kernel, empirical, 'Whether the native-generative focus is a natural linguistic process or an ideological construct.').

omega_variable(
    kernel_reading_impact_on_victims,
    'How would the adoption of a ''liturgical preservation'' or ''bridge pidginized'' reading of Hebrew continuity impact the perceived status and resources of diaspora liturgical communities and non-native scholars?',
    'Counterfactual analysis and comparative study of language policy in other diasporic contexts where multiple forms of linguistic continuity are officially recognized and supported.',
    'If alternative readings gained legitimacy, the victim status of these groups would diminish, and the current constraint''s extractiveness would be seen as a direct consequence of its exclusionary definition of ''living'' Hebrew.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_impact_on_victims, conceptual, 'Impact of alternative kernel readings on the victim groups of the native_generative reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(hebr_be_t1948, hebrew_continuity__native_generative, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(hebr_be_t1968, hebrew_continuity__native_generative, base_extractiveness, 1968, 0.5).
narrative_ontology:measurement(hebr_be_t1988, hebrew_continuity__native_generative, base_extractiveness, 1988, 0.58).
narrative_ontology:measurement(hebr_be_t2008, hebrew_continuity__native_generative, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement(hebr_be_t2024, hebrew_continuity__native_generative, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1948, hebrew_continuity__native_generative, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(hebr_su_t1968, hebrew_continuity__native_generative, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(hebr_su_t1988, hebrew_continuity__native_generative, suppression_requirement, 1988, 0.65).
narrative_ontology:measurement(hebr_su_t2008, hebrew_continuity__native_generative, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(hebr_su_t2024, hebrew_continuity__native_generative, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_continuity' kernel, focusing on native generative use. It is structurally distinct from the 'liturgical_preservation' and 'bridge_pidginized' readings, which represent alternative understandings of Hebrew's vitality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
