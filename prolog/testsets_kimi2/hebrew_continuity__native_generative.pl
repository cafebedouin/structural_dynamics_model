% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Continuity via Native Generative Use
 *   domain: sociolinguistic/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the native_generative reading of the
 *   hebrew_continuity kernel: the claim that Hebrew survives as a living
 *   language exclusively through the intuitive, daily generative use of
 *   native child speakers. Emerging from the Zionist language revival, the
 *   constraint justifies massive institutional reconstructionâlexical
 *   expansion, phonological standardization, and state educational
 *   enforcementâwhile defining liturgical and diasporic Hebrew as
 *   non-generative or 'dead.' It coordinates the collective-action problem of
 *   nation-building language shift but simultaneously extracts legitimacy and
 *   resources from liturgical-only communities.
 *
 * KEY AGENTS:
 *   - modern_hebrew_speakers: Primary beneficiary (organized/identity_locked) â receive prestige and institutional support
 *   - revival_institutions: Agenda setter (institutional/arbitrage) â administers standardization and enforces the native-speaker criterion
 *   - liturgical_only_communities: Primary payer (moderate/constrained) â bear delegitimization and marginalization
 *   - diaspora_hebrew_users: Excluded seat (moderate/constrained) â rendered linguistically invisible
 *   - sociolinguistic_observers: Analytical observer (analytical/analytical) â documents the revival and its hierarchies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.62).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.58).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Continuity via Native Generative Use").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistic/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, 'a910d1ee-b7c1-4229-bf4b-14a55208561b').
narrative_ontology:cs_kernel_codification('a910d1ee-b7c1-4229-bf4b-14a55208561b', implicit).
narrative_ontology:cs_authority_grounding('a910d1ee-b7c1-4229-bf4b-14a55208561b', practice).
narrative_ontology:cs_interpretation_layer_present('a910d1ee-b7c1-4229-bf4b-14a55208561b').
narrative_ontology:cs_reading_relation('a910d1ee-b7c1-4229-bf4b-14a55208561b', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('a910d1ee-b7c1-4229-bf4b-14a55208561b', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('a910d1ee-b7c1-4229-bf4b-14a55208561b', foundational, hebrew_continuity_requires_native_child_speakers).
narrative_ontology:cs_axiom_status(hebrew_continuity_requires_native_child_speakers, holdable).
narrative_ontology:cs_axiom_grounding('a910d1ee-b7c1-4229-bf4b-14a55208561b', hebrew_continuity_requires_native_child_speakers, empirically_contingent).
narrative_ontology:cs_axiom('a910d1ee-b7c1-4229-bf4b-14a55208561b', foundational, phonological_standardization_legitimate_reconstruction).
narrative_ontology:cs_axiom_status(phonological_standardization_legitimate_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('a910d1ee-b7c1-4229-bf4b-14a55208561b', phonological_standardization_legitimate_reconstruction, instrumental).
narrative_ontology:cs_reference_frame('a910d1ee-b7c1-4229-bf4b-14a55208561b', native_speaker_speech_community).
narrative_ontology:cs_drift_state('a910d1ee-b7c1-4229-bf4b-14a55208561b', contemporary_hebrew_diaspora_contact, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a910d1ee-b7c1-4229-bf4b-14a55208561b', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, modern_hebrew_speakers).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, revival_institutions).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_only_communities).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, native_speaker_essentialism).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, language_vitality_through_child_acquisition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their children's intuitive daily speech is treated as the sole proof of Hebrew's vitality. They receive institutional support, educational prestige, and the legitimation of their linguistic identity as the authentic Hebrew. Exit is identity-locked because their national self-concept is constituted through native monolingual Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, modern_hebrew_speakers, beneficiary,
    organized, biographical, identity_locked, national).

% Language academies, state curriculum boards, and military language units that expand the lexicon, standardize phonology, and enforce the native-speaker criterion through schooling and official discourse. They set the rules for what counts as living Hebrew and could theoretically pivot to a pluralistic model, though at high political cost.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, revival_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Communities whose Hebrew is confined to prayer, Talmud study, and ritual recitation. Their Hebrew is classified as non-generative and therefore 'dead' within the dominant framework, which excludes their practice from state recognition, educational funding, and public legitimacy while appropriating their historical continuity claims.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, liturgical_only_communities, payer,
    moderate, generational, constrained, national).

% Diaspora Jews who use Hebrew for religious study, cultural events, or limited interpersonal contact but lack native fluency. They are rendered linguistically invisible by the native-speaker exclusivity and are absent from Israeli language-planning forums.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_hebrew_users, excluded,
    moderate, biographical, constrained, global).

% Researchers of language endangerment and revival who document the Hebrew resurgence. Some affirm the native-speaker model as empirically necessary; others note that the same model creates hierarchies of authenticity that marginalize liturgical and diasporic forms.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, sociolinguistic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__native_generative, diffuse).
narrative_ontology:fixing_cost_class(hebrew_continuity__native_generative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of reviving a daily spoken Hebrew after centuries of non-native use by establishing clear generative norms, expanding the lexicon for modern life, and creating a self-sustaining speech community.
% TRANSFER_FUNCTION: Moves cultural prestige, state educational resources, and historical legitimacy from liturgical and diasporic Hebrew toward modern native-speaking communities and the standardization institutions that administer them.
% ABSENT_VOICES: Liturgical-only communities and diaspora non-native users are structurally absent from language-planning discourse; they would contest the native-speaker exclusivity if seated in curriculum and academy deliberations.
% DISAPPEARANCE_RATIONALE: If the native-speaker exclusivity vanished, state curricula would reallocate resources toward liturgical Hebrew, diaspora varieties would gain legitimacy, and the nation-building narrative would lose a central ideological anchor. Modern Hebrew would continue to exist but would no longer monopolize the label of 'living Hebrew.'
% FOUNDING_PROBLEM: Hebrew had ceased to function as a daily spoken language among Jews and was at risk of disappearing as a shared lingua franca; the Zionist settlement project required a modern unifying tongue.
% FOUNDING_PROBLEM_CORROBORATION: Non-Zionist Jewish historians and sociolinguists outside the Israeli state apparatus attest to the pre-revival decline of everyday Hebrew, though they dispute that native generative use is the sole legitimate continuity mechanism; some postcolonial scholars argue the 'death' narrative was constructed to justify nation-building extraction.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the native-speaker exclusivity systematically transfers legitimacy and resources away from liturgical and diasporic forms. Suppression (0.58) reflects active institutional enforcement via schooling, standardization, and public discourse that frames non-native Hebrew as deficient. Theater ratio (0.35) captures the increasing performative dimension of native-speaker policing as Hebrew has become demographically secure. Accessibility collapse (0.55) is moderate: liturgical Hebrew is conceptually marginalized but persists as a lived practice. Resistance (0.50) reflects ongoing contestation from ultra-Orthodox communities, diaspora critics, and postcolonial linguists.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (revival institutions) experiences the constraint as necessary coordination for national survival; the payer seat (liturgical communities) experiences the same structure as cultural erasure. The beneficiary seat (modern speakers) experiences linguistic pride and material support, while the excluded seat (diaspora users) experiences silence. The engine computes these divergences from the structural dataâthe modern speaker's identity-locked exit amplifies their subsidy, while the liturgical community's constrained exit amplifies their extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Modern Hebrew speakers and revival institutions are structural beneficiaries of the native-speaker criterion: it validates their linguistic form and channels resources to them (low d, damped effective extraction). Liturgical-only communities are structural targets: the constraint explicitly defines their Hebrew out of existence as a living language, and their exit is constrained by religious identity and communal boundaries (high d, amplified effective extraction). Diaspora users are excluded from the conversation entirely, experiencing the constraint as pure external definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the revival as pure extraction (snare) by acknowledging the genuine coordination problem it solvedâHebrew had ceased to be a daily spoken languageâand the real collective-action structure of creating new native norms. Conversely, it prevents mislabeling it as pure coordination (rope) by registering the identifiable victim set (liturgical communities) and the active enforcement required to maintain the exclusivity claim against competing continuity narratives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_generative_reading_position,
    'How would the constraint''s classification change if the liturgical_preservation or bridge_pidginized reading of the Hebrew continuity kernel were adopted instead?',
    'Comparative analysis of sibling constraint stories in the hebrew_continuity family, examining shifts in beneficiary/victim sets and epsilon values.',
    'Would reassign victim status from liturgical communities to modern standardization institutions or diaspora contact speakers; would reduce extractiveness if the reading abandoned exclusivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_generative_reading_position, conceptual, 'Reading-position ambiguity within the Hebrew continuity kernel').

omega_variable(
    coordination_extraction_separability,
    'Could the coordination function of Hebrew revivalâcreating a modern spoken lingua francaâhave been achieved without delegitimizing liturgical and diasporic Hebrew?',
    'Counterfactual historical analysis of bilingual or diglossic revival models that parallel modern and liturgical registers without hierarchical ranking.',
    'If separable, the current constraint is a tangled rope where extraction exceeds coordination cost; if inseparable, the marginalization of liturgical Hebrew was the necessary price of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable in the revival').

omega_variable(
    suppression_mechanism_liturgical,
    'Is the marginalization of liturgical Hebrew structural (institutional defunding and exclusion from state curricula) or internalized (communities accepting the ''dead language'' narrative)?',
    'Post-exit trajectory analysis: if liturgical communities recover legitimacy when state narratives shift, suppression is structural; if marginalization persists absent state enforcement, it is internalized.',
    'Internalized suppression would raise effective extraction beyond the structural measure, as the target community carries the constraint after external barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_liturgical, empirical, 'Structural vs internalized suppression mechanism for liturgical communities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_native_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebrew_native_tr_t20, hebrew_continuity__native_generative, theater_ratio, 20, 0.2).
narrative_ontology:measurement(hebrew_native_tr_t40, hebrew_continuity__native_generative, theater_ratio, 40, 0.28).
narrative_ontology:measurement(hebrew_native_tr_t60, hebrew_continuity__native_generative, theater_ratio, 60, 0.32).
narrative_ontology:measurement(hebrew_native_tr_t80, hebrew_continuity__native_generative, theater_ratio, 80, 0.34).
narrative_ontology:measurement(hebrew_native_tr_t100, hebrew_continuity__native_generative, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(hebrew_native_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hebrew_native_be_t20, hebrew_continuity__native_generative, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(hebrew_native_be_t40, hebrew_continuity__native_generative, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(hebrew_native_be_t60, hebrew_continuity__native_generative, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(hebrew_native_be_t80, hebrew_continuity__native_generative, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(hebrew_native_be_t100, hebrew_continuity__native_generative, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_native_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(hebrew_native_su_t20, hebrew_continuity__native_generative, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(hebrew_native_su_t40, hebrew_continuity__native_generative, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(hebrew_native_su_t60, hebrew_continuity__native_generative, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(hebrew_native_su_t80, hebrew_continuity__native_generative, suppression_requirement, 80, 0.62).
narrative_ontology:measurement(hebrew_native_su_t100, hebrew_continuity__native_generative, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one member of the hebrew_continuity family. The label 'Hebrew continuity' conflates three structurally distinct claims: native generative continuity (this story), liturgical preservation continuity, and bridge pidginized continuity. Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as sibling readings under the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
