% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Hebrew Native Generation Vitality Criterion
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the native_generation_reading of the
 *   hebrew_living_language kernel: the claim that Hebrew achieved living
 *   status only when native speakers began producing daily speech
 *   generatively rather than through memorized liturgical recitation. This
 *   reading was institutionalized by Zionist language planners from the 1880s
 *   through the mid-20th century and served as the ideological backbone for
 *   suppressing competing Jewish vernaculars (Yiddish, Ladino, Judeo-Arabic)
 *   in schools and public life. The structural delta includes moderate
 *   extractiveness, an identifiable victim set of non-Hebrew vernacular
 *   speakers, and an acknowledged 'strict-reachability break' requiring heavy
 *   reconstruction.
 *
 * KEY AGENTS:
 *   - zionist_language_institutions: Primary agenda-setter (institutional/arbitrage) â defines and enforces the native-generation criterion
 *   - hebrew_native_generations: Primary beneficiary (moderate/constrained) â inherits linguistic capital from the revival
 *   - yiddish_speakers: Primary target (powerless/identity_locked) â bears the cost of language suppression
 *   - ladino_speakers: Primary target (powerless/identity_locked) â bears the cost of language suppression
 *   - mizrahi_vernacular_communities: Secondary target (powerless/constrained) â excluded from discourse and pressured to assimilate
 *   - sociolinguistic_observers: Analytical observer (analytical/analytical) â sees the full structural asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.55).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.68).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew Native Generation Vitality Criterion").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '0648a3cb-a328-444b-a6d0-d89118836637').
narrative_ontology:cs_kernel_codification('0648a3cb-a328-444b-a6d0-d89118836637', formalized).
narrative_ontology:cs_authority_grounding('0648a3cb-a328-444b-a6d0-d89118836637', expertise).
narrative_ontology:cs_interpretation_layer_present('0648a3cb-a328-444b-a6d0-d89118836637').
narrative_ontology:cs_reading_relation('0648a3cb-a328-444b-a6d0-d89118836637', hebrew_living_language__liturgical_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('0648a3cb-a328-444b-a6d0-d89118836637', hebrew_living_language__literary_revival_reading, forecloses).
narrative_ontology:cs_axiom('0648a3cb-a328-444b-a6d0-d89118836637', foundational, native_generative_speech_required).
narrative_ontology:cs_axiom_status(native_generative_speech_required, holdable).
narrative_ontology:cs_axiom_grounding('0648a3cb-a328-444b-a6d0-d89118836637', native_generative_speech_required, empirically_contingent).
narrative_ontology:cs_axiom('0648a3cb-a328-444b-a6d0-d89118836637', foundational, memorized_performance_excluded).
narrative_ontology:cs_axiom_status(memorized_performance_excluded, holdable).
narrative_ontology:cs_axiom_grounding('0648a3cb-a328-444b-a6d0-d89118836637', memorized_performance_excluded, conventional).
narrative_ontology:cs_reference_frame('0648a3cb-a328-444b-a6d0-d89118836637', native_generative_speech_framework).
narrative_ontology:cs_drift_state('0648a3cb-a328-444b-a6d0-d89118836637', post_revival_consensus, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0648a3cb-a328-444b-a6d0-d89118836637', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, zionist_language_institutions).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_native_generations).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, mizrahi_vernacular_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Language planning bodies from the Hebrew Language Committee to the Academy of the Hebrew Language that codified modern Hebrew, enforced its use in schools and public institutions, and policed the boundary between 'living' native speech and 'dead' liturgical recitation. They defined the criterion that only native generative speech counts as linguistic vitality, authorizing the marginalization of competing Jewish vernaculars.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, zionist_language_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Children raised as native Hebrew speakers from the early twentieth century onward, who inherited a fully modernized mother tongue. They benefit from the linguistic capital and national belonging the revival created, though they did not choose the cost imposed on prior vernacular communities.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_native_generations, beneficiary,
    moderate, biographical, constrained, national).

% Ashkenazi Jewish communities for whom Yiddish was the daily vernacular. Their children were channeled into Hebrew-medium schools where Yiddish was stigmatized as a diaspora language. They bore the cost of language shift and cultural erasure.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speakers, payer,
    powerless, biographical, identity_locked, national).

% Sephardic Jewish communities whose daily language was Ladino. They faced institutional pressure to abandon Ladino in favor of Hebrew, losing intergenerational transmission and cultural heritage.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speakers, payer,
    powerless, biographical, identity_locked, national).

% Middle Eastern and North African Jewish communities speaking Judeo-Arabic and other regional vernaculars. They were marginalized in the Hebrew-revival narrative and pressured to assimilate linguistically, with their mother tongues excluded from public and educational spaces.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, mizrahi_vernacular_communities, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, mizrahi_vernacular_communities, excluded).

% Linguists and historians studying language revitalization and nationalism. They observe that Hebrew revival is a unique case of reversed language shift, but note the heavy planning and suppression required, questioning whether the native-generation criterion captures the full structural cost.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, sociolinguistic_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of creating a unified national vernacular for a multilingual immigrant population by establishing a single mother tongue capable of serving modern state functions, commerce, and interethnic communication.
% TRANSFER_FUNCTION: Moves linguistic dominance and intergenerational transmission from diaspora vernaculars (Yiddish, Ladino, Judeo-Arabic) to Hebrew; moves educational resources, public prestige, and state legitimacy from heritage languages to the revived national language.
% ABSENT_VOICES: Yiddish cultural institutions, Ladino heritage organizations, and Mizrahi cultural movements that would argue for multilingual maintenance or equal public status for diaspora vernaculars. They were structurally absent from early planning commissions and later marginalized in state institutions.
% DISAPPEARANCE_RATIONALE: If the native-generation criterion vanished and liturgical or literary continuity were accepted as sufficient for 'living' status, educational priorities would shift, diaspora vernaculars might retain greater institutional support, and the symbolic hierarchy justifying Hebrew monolingualism would weaken.
% FOUNDING_PROBLEM: The Jewish diaspora lacked a shared modern vernacular for national life; liturgical Hebrew was not transmitted as a mother tongue in the home, and the multilingual diaspora spoke mutually unintelligible languages.
% FOUNDING_PROBLEM_CORROBORATION: Zionist language planners attest the problem was the absence of a modern Hebrew speech community. Comparative sociolinguists and historians of Jewish diaspora cultures corroborate the multilingual reality but dispute that the absence of a native modern Hebrew vernacular constituted a crisis requiring the suppression of existing Jewish languages; Yiddish and Ladino scholars attest the vitality of those vernaculars before the revival project.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.55) because the suppression served a genuine nation-building coordination function while still imposing asymmetric costs on specific communities. Suppression is higher (0.68) because the policy required active institutional enforcement through schooling, public stigma, and exclusion of other languages from state spaces. Theater_ratio is moderate (0.42): much early revival was functional language building, but a significant portion became performative nationalism enforcing monolingual identity. Accessibility_collapse (0.60) reflects the closure of Yiddish and Ladino schooling and media. Resistance (0.58) captures sustained opposition from Yiddishist circles and ultra-Orthodox communities.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (language institutions) experiences the constraint as successful coordination and scientific modernization. The payer seats (diaspora vernacular communities) experience it as cultural extraction and forced assimilation. The beneficiary seat (Hebrew native generations) occupies an ambivalent position: they receive linguistic capital but had no voice in the cost imposed on their parents. The observer seat sees the full asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Language institutions and native Hebrew generations are structural beneficiaries (low d). Diaspora vernacular communities are structural targets (high d), especially those with identity-locked exit (Yiddish and Ladino speakers whose cultural identity was fused with the suppressed language). The substantial scope differential between the institutional agenda-setter (national) and the powerless vernacular communities (national but identity-locked) amplifies effective extraction for the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine coordination function of creating a shared national language for a multilingual population. It prevents mislabeling it as pure coordination (rope) by naming the asymmetric cost borne by specific vernacular communities and the active enforcement required to suppress alternatives. The temporal measurements show extraction rising with institutional capacity, then stabilizing as native transmission became self-sustaining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is Hebrew''s living status dependent exclusively on native generative speech, or do liturgical continuity and literary revival constitute alternative sufficient conditions?',
    'Comparative analysis of the three kernel readings against sociolinguistic vitality metrics; evaluation of whether the native-generation reading forecloses its siblings logically or merely politically.',
    'If the liturgical or literary readings are valid, the constraint''s victim set shrinks and its extractiveness drops; if native-generation is the only valid reading, the suppression of diaspora vernaculars appears as a harder necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel ambiguity: which reading of Hebrew vitality is structurally correct').

omega_variable(
    reconstruction_vs_nativeness,
    'Does the massive planned reconstruction of Hebrew (neologisms, grammatical regulation, institutional planning) constitute a break that invalidates the ''native generative speech'' criterion, or is native acquisition sufficient to overcome the engineered origin?',
    'Historical sociolinguistic analysis comparing Hebrew revival to other language revivals (Irish, Welsh) where native transmission coexists with heavy planning.',
    'If reconstruction undermines the nativeness claim, the constraint is more theatrical/performative and less genuinely coordinative; if nativeness absorbs the construction, the constraint remains a tangled rope with real coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_vs_nativeness, empirical, 'Whether planned reconstruction invalidates native-generation authenticity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__native_generation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hebr_tr_t10, hebrew_living_language__native_generation_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(hebr_tr_t20, hebrew_living_language__native_generation_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(hebr_tr_t30, hebrew_living_language__native_generation_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(hebr_tr_t40, hebrew_living_language__native_generation_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(hebr_tr_t50, hebrew_living_language__native_generation_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(hebr_tr_t60, hebrew_living_language__native_generation_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(hebr_tr_t70, hebrew_living_language__native_generation_reading, theater_ratio, 70, 0.38).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__native_generation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hebr_be_t10, hebrew_living_language__native_generation_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(hebr_be_t20, hebrew_living_language__native_generation_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(hebr_be_t30, hebrew_living_language__native_generation_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(hebr_be_t40, hebrew_living_language__native_generation_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(hebr_be_t50, hebrew_living_language__native_generation_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(hebr_be_t60, hebrew_living_language__native_generation_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(hebr_be_t70, hebrew_living_language__native_generation_reading, base_extractiveness, 70, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_living_language__native_generation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(hebr_su_t10, hebrew_living_language__native_generation_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(hebr_su_t20, hebrew_living_language__native_generation_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(hebr_su_t30, hebrew_living_language__native_generation_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(hebr_su_t40, hebrew_living_language__native_generation_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(hebr_su_t50, hebrew_living_language__native_generation_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(hebr_su_t60, hebrew_living_language__native_generation_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(hebr_su_t70, hebrew_living_language__native_generation_reading, suppression_requirement, 70, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is the native_generation_reading of the hebrew_living_language kernel. The colloquial claim 'Hebrew is a living language' decomposes into three structurally distinct readings: this one (native daily generative speech required), literary_revival_reading (written generative competence suffices), and liturgical_continuity_reading (unbroken recitation suffices). Each reading has a different epsilon, different victim/beneficiary structures, and different empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
