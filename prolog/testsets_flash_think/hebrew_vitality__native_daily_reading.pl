% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Hebrew Vitality: Native Daily Use Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint represents the 'native_daily_reading' of Hebrew vitality,
 *   asserting that only its use as a natively spoken, daily vernacular
 *   constitutes true 'life' for the language, while ritual recitation is
 *   merely 'preservation.' This perspective was central to the Zionist
 *   project of Hebrew revival, which actively promoted and institutionalized
 *   modern Hebrew as a spoken language, often at the expense of its
 *   traditional liturgical and scholarly roles. The constraint is claimed as
 *   a Rope by its proponents (a necessary coordination for nation-building)
 *   but operates with significant extraction and suppression from other
 *   perspectives.
 *
 * KEY AGENTS:
 *   - Zionist state-building project: Agenda setter, beneficiary (institutional/arbitrage)
 *   - Modern Hebrew speakers: Beneficiary (moderate/mobile)
 *   - Liturgical Hebrew tradition: Payer, victim (powerless/identity_locked)
 *   - Diaspora Jewish communities: Payer, victim (moderate/constrained)
 *   - Religious authorities: Payer (powerful/constrained)
 *   - Linguists and revitalization experts: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.6).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.75).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Hebrew Vitality: Native Daily Use Reading").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, '4a725022-a219-45fe-90ff-96c1c7659c9a').
narrative_ontology:cs_kernel_codification('4a725022-a219-45fe-90ff-96c1c7659c9a', formalized).
narrative_ontology:cs_authority_grounding('4a725022-a219-45fe-90ff-96c1c7659c9a', lineage).
narrative_ontology:cs_interpretation_layer_present('4a725022-a219-45fe-90ff-96c1c7659c9a').
narrative_ontology:cs_reading_relation('4a725022-a219-45fe-90ff-96c1c7659c9a', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('4a725022-a219-45fe-90ff-96c1c7659c9a', hebrew_vitality__hybrid_continuity_reading, forecloses).
narrative_ontology:cs_axiom('4a725022-a219-45fe-90ff-96c1c7659c9a', foundational, native_generation_is_sole_vitality).
narrative_ontology:cs_axiom_status(native_generation_is_sole_vitality, holdable).
narrative_ontology:cs_axiom_grounding('4a725022-a219-45fe-90ff-96c1c7659c9a', native_generation_is_sole_vitality, deontological).
narrative_ontology:cs_axiom('4a725022-a219-45fe-90ff-96c1c7659c9a', secondary, ritual_is_preservation_not_life).
narrative_ontology:cs_axiom_status(ritual_is_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('4a725022-a219-45fe-90ff-96c1c7659c9a', ritual_is_preservation_not_life, conventional).
narrative_ontology:cs_reference_frame('4a725022-a219-45fe-90ff-96c1c7659c9a', modern_hebrew_vernacular_state).
narrative_ontology:cs_drift_state('4a725022-a219-45fe-90ff-96c1c7659c9a', contemporary_globalized_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4a725022-a219-45fe-90ff-96c1c7659c9a', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, modern_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_hebrew_tradition).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, diaspora_jewish_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, religious_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promoted and institutionalized modern Hebrew as the sole living language for the nascent state, establishing educational systems, academies, and media to ensure its native, daily use. Benefits from a unified national identity and cultural distinctiveness.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, arbitrage, national).

% Are the direct beneficiaries of a living, functional vernacular language for daily life, education, and culture. They experience the language as natural and unconstrained, often unaware of the historical effort and suppression involved in its establishment.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, modern_hebrew_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Is marginalized and desacralized by the exclusive focus on native, daily use. Its role as a primary form of Hebrew vitality is denied, reducing its status to mere 'preservation' rather than 'life.' Its adherents are forced to accept a diminished role for their form of Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_hebrew_tradition, payer,
    powerless, civilizational, identity_locked, global).

% Often maintain Hebrew primarily through liturgical study and ritual, or as a secondary language. This reading's exclusive definition of vitality implicitly devalues their forms of Hebrew engagement, creating a linguistic and cultural distance from the Israeli center.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_jewish_communities, payer,
    moderate, generational, constrained, global).

% Historically held authority over Hebrew language and its meaning. This authority is challenged and diminished by the secular, nationalistic project of vernacularization, which redefines Hebrew's purpose and primary domain of use.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, religious_authorities, payer,
    powerful, generational, constrained, global).

% Analyze the processes of language death and revival, often providing theoretical frameworks that can either support or challenge the exclusive claims of this reading. They observe the social and political dynamics of language shift.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, linguists_and_revitalization_experts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a unified, living vernacular language for a modern nation-state, enabling daily communication, cultural production, and a distinct national identity, thereby solving the collective action problem of language choice for a diverse immigrant population.
% TRANSFER_FUNCTION: Transfers linguistic authority, cultural capital, and institutional resources from traditional liturgical and scholarly uses of Hebrew to its modern, secular, native, and daily spoken form. It also transfers the burden of lexical expansion and linguistic standardization to state-backed institutions.
% ABSENT_VOICES: Those who uphold the unbroken liturgical chain as the primary form of Hebrew vitality, or those who advocate for a more pluralistic definition of language life that includes both ritual and reconstructed vernacular forms. Their perspectives are marginalized by the exclusive 'native generation' axiom.
% DISAPPEARANCE_RATIONALE: If the constraint (that only native daily use constitutes vitality) vanished, the exclusive justification for the institutional and cultural structures supporting modern Hebrew's dominance would collapse. Liturgical and diaspora forms of Hebrew would regain their status as valid expressions of vitality, leading to a reorganization of linguistic policy, educational priorities, and cultural discourse around a more pluralistic understanding of Hebrew's life.
% FOUNDING_PROBLEM: The perceived lack of a living, daily spoken language for the nascent Zionist project, leading to a need for a modern national language distinct from the languages of exile and purely religious contexts to foster national unity and cultural independence.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historians, educators, and cultural figures attest to the historical and ongoing necessity of a living vernacular for national self-determination. Critics, including some religious scholars and diaspora intellectuals, contest the framing of liturgical use as 'not life,' arguing the founding problem was misdiagnosed or over-solved at the expense of other forms of Hebrew vitality.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.80 at interval end) reflects the high cost imposed on alternative forms of Hebrew vitality, particularly the devaluation of liturgical use and the cultural distance created for diaspora communities. Suppression (0.90) is severe, as the institutional efforts to establish modern Hebrew as the sole living language actively marginalized and resisted other forms of Hebrew engagement. The theater ratio is low (0.10) because the constraint is genuinely about fostering actual, lived language use, not performative maintenance of an atrophied function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Zionist agenda-setter, this constraint is a vital coordination mechanism for national self-determination and cultural renewal. From the perspective of the liturgical tradition and diaspora communities, it is an extractive imposition that desacralizes Hebrew and creates an artificial hierarchy of linguistic 'life.' The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project and modern Hebrew speakers are the primary beneficiaries, gaining a unified national language and identity. The liturgical Hebrew tradition, diaspora Jewish communities, and religious authorities are the victims, experiencing a loss of status, authority, and cultural relevance for their forms of Hebrew. The constraint subsidizes the creation of a new linguistic reality by extracting from existing ones.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (creating a living national language) is still considered 'live' by its proponents, preventing a clear mandatrophy resolution from their seat. However, the 'only native generation' clause is increasingly contested, suggesting a potential for mandatrophy if a more pluralistic view of vitality gains wider acceptance. The high extractiveness and suppression, despite the 'live' founding problem, indicate that the coordination function is deeply intertwined with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_definition_ambiguity,
    'Is native, daily generation the *only* valid criterion for language vitality, or do other forms, such as continuous liturgical use, also constitute ''life'' for a language?',
    'Conceptual re-evaluation of linguistic vitality by sociolinguists and cultural theorists, or a shift in community consensus regarding the value of diverse forms of language engagement.',
    'If liturgical use is recognized as a form of vitality, the constraint''s extractiveness and suppression would be re-evaluated downward, and its classification might shift from Tangled Rope towards a more benign type, or decompose into multiple, less extractive constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_definition_ambiguity, conceptual, 'Ambiguity in the definition of language vitality and ''life'' for Hebrew.').

omega_variable(
    necessity_of_suppression,
    'Was the active marginalization of liturgical Hebrew a necessary condition for the successful vernacularization of modern Hebrew, or an avoidable act of cultural desacralization?',
    'Comparative studies of other language revitalization efforts that adopted more pluralistic approaches, or historical counterfactual analysis exploring alternative paths for Hebrew revival.',
    'If suppression was not strictly necessary, the constraint''s high suppression metric would be seen as purely extractive, strengthening its Snare-like qualities. If deemed necessary, it would be viewed as a regrettable but unavoidable cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_suppression, empirical, 'Whether the suppression of alternative Hebrew uses was a necessary cost or an extractive act.').

omega_variable(
    diaspora_cultural_alienation,
    'To what extent has the exclusive focus on native Israeli Hebrew contributed to a sense of cultural alienation or linguistic disempowerment among diaspora Jewish communities?',
    'Sociological surveys and qualitative studies among diaspora communities, assessing their engagement with Hebrew and their perceptions of its ''vitality'' in their contexts.',
    'Strong evidence of alienation would increase the perceived victimhood of diaspora communities, amplifying the constraint''s effective extraction from their seat. Weak evidence would suggest their ''payer'' role is less severe than currently assessed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_cultural_alienation, empirical, 'Impact of native Hebrew exclusivity on diaspora communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1900, 1970).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1900, hebrew_vitality__native_daily_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(hebr_tr_t1910, hebrew_vitality__native_daily_reading, theater_ratio, 1910, 0.1).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__native_daily_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(hebr_tr_t1930, hebrew_vitality__native_daily_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_vitality__native_daily_reading, theater_ratio, 1940, 0.1).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_vitality__native_daily_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_vitality__native_daily_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(hebr_tr_t1970, hebrew_vitality__native_daily_reading, theater_ratio, 1970, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1900, hebrew_vitality__native_daily_reading, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement(hebr_be_t1910, hebrew_vitality__native_daily_reading, base_extractiveness, 1910, 0.5).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__native_daily_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(hebr_be_t1930, hebrew_vitality__native_daily_reading, base_extractiveness, 1930, 0.6).
narrative_ontology:measurement(hebr_be_t1940, hebrew_vitality__native_daily_reading, base_extractiveness, 1940, 0.65).
narrative_ontology:measurement(hebr_be_t1950, hebrew_vitality__native_daily_reading, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(hebr_be_t1960, hebrew_vitality__native_daily_reading, base_extractiveness, 1960, 0.75).
narrative_ontology:measurement(hebr_be_t1970, hebrew_vitality__native_daily_reading, base_extractiveness, 1970, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1900, hebrew_vitality__native_daily_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(hebr_su_t1910, hebrew_vitality__native_daily_reading, suppression_requirement, 1910, 0.58).
narrative_ontology:measurement(hebr_su_t1920, hebrew_vitality__native_daily_reading, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(hebr_su_t1930, hebrew_vitality__native_daily_reading, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement(hebr_su_t1940, hebrew_vitality__native_daily_reading, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(hebr_su_t1950, hebrew_vitality__native_daily_reading, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(hebr_su_t1960, hebrew_vitality__native_daily_reading, suppression_requirement, 1960, 0.85).
narrative_ontology:measurement(hebr_su_t1970, hebrew_vitality__native_daily_reading, suppression_requirement, 1970, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
