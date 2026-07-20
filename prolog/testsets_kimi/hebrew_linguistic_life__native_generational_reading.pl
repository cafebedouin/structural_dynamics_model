% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Hebrew Native-Generational Vitality Criterion
 *   domain: sociolinguistic/political
 *
 * SUMMARY:
 *   This constraint instantiates the native_generational_reading of the
 *   hebrew_linguistic_life kernel. It defines a language as alive only when
 *   children acquire it as a mother tongue and employ it for all daily
 *   functions including secular mundane speech. This reading underwrote the
 *   Zionist revival of Hebrew from a dormant sacred and literary language (c.
 *   70-1880 CE) into a modern national vernacular, and it justified the
 *   active suppression of competing Jewish languagesâprincipally Yiddish
 *   and Ladinoâwhose speakers were coerced or socially pressured into
 *   abandoning their mother tongues. The constraint is not a natural law but
 *   a normative criterion that functions as both a genuine coordination
 *   mechanism (unifying a nation around a shared tongue) and an asymmetric
 *   extraction mechanism (destroying diaspora linguistic diversity).
 *
 * KEY AGENTS:
 *   - zionist_language_institutions (agenda_setter / institutional / arbitrage) â administers the language policy and enforces Hebrew-medium standards
 *   - zionist_political_elite (beneficiary / powerful / mobile) â captures political legitimacy from a unified Hebrew-speaking citizenry
 *   - yiddish_and_ladino_speakers (payer / powerless / identity_locked) â bear the cost of mother-tongue abandonment and cultural erasure
 *   - traditional_religious_communities (excluded / moderate / identity_locked) â excluded from the discourse because their liturgical-preservation framing contradicts the secular-vernacular mandate
 *   - sociolinguistic_observers (observer / analytical) â document the coercion and measure the shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.78).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.82).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew Native-Generational Vitality Criterion").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistic/political").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, 'eb385e51-514a-48af-b07f-151ca446a72e').
narrative_ontology:cs_kernel_codification('eb385e51-514a-48af-b07f-151ca446a72e', formalized).
narrative_ontology:cs_authority_grounding('eb385e51-514a-48af-b07f-151ca446a72e', lineage).
narrative_ontology:cs_interpretation_layer_present('eb385e51-514a-48af-b07f-151ca446a72e').
narrative_ontology:cs_reading_relation('eb385e51-514a-48af-b07f-151ca446a72e', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb385e51-514a-48af-b07f-151ca446a72e', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('eb385e51-514a-48af-b07f-151ca446a72e', foundational, native_acquisition_necessary_for_linguistic_life).
narrative_ontology:cs_axiom_status(native_acquisition_necessary_for_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('eb385e51-514a-48af-b07f-151ca446a72e', native_acquisition_necessary_for_linguistic_life, conventional).
narrative_ontology:cs_axiom('eb385e51-514a-48af-b07f-151ca446a72e', foundational, secular_mundane_use_required).
narrative_ontology:cs_axiom_status(secular_mundane_use_required, holdable).
narrative_ontology:cs_axiom_grounding('eb385e51-514a-48af-b07f-151ca446a72e', secular_mundane_use_required, conventional).
narrative_ontology:cs_reference_frame('eb385e51-514a-48af-b07f-151ca446a72e', national_vernacular_mother_tongue).
narrative_ontology:cs_drift_state('eb385e51-514a-48af-b07f-151ca446a72e', contemporary_globalized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb385e51-514a-48af-b07f-151ca446a72e', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, zionist_political_elite).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_and_ladino_speakers).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, zionist_nation_state_legitimacy).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, monolingual_national_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates language policy bodies, school curricula, and public signage mandates. Drafts dictionaries, regulates neologisms, and oversees Hebrew-language media. Receives state budget allocations and prestige from running the national language infrastructure.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, zionist_language_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Draws political legitimacy from a unified Hebrew-speaking citizenry. Uses the native-generational standard to justify nation-building budgets, immigration absorption policies, and the marginalization of competing diaspora identities. Does not administer language academies directly but sets national priorities that fund them.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, zionist_political_elite, beneficiary,
    powerful, generational, mobile, national).

% Arrived in Palestine and Israel with Yiddish or Ladino as mother tongues. Found their languages excluded from schools, media, and public employment. Their children were channeled into Hebrew-only education. Continuing to speak these languages at home or in public drew social stigma and was framed as anti-Zionist or primitive. Many experienced the loss as cultural amputation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_and_ladino_speakers, payer,
    powerless, biographical, identity_locked, national).

% Maintain that Hebrew belongs to sacred study and prayer, and that Yiddish is the appropriate vernacular for daily life. Their schools resisted Hebrew-only mandates. They were largely excluded from the nation-building discourse and portrayed as obstacles to progress.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, traditional_religious_communities, excluded,
    moderate, generational, identity_locked, global).

% Document language shift, measure intergenerational transmission rates, and analyze the political economy of the Hebrew revival. They observe the tension between the official monolingual narrative and the multilingual reality of immigrant communities.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, sociolinguistic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a unified national speech community around a single mother tongue, replacing diaspora multilingualism with a shared vernacular for governance, education, and daily life.
% TRANSFER_FUNCTION: Moves linguistic practice, educational investment, and cultural prestige from diaspora languages (Yiddish, Ladino, Judeo-Arabic) to modern Hebrew, and transfers the cost of language abandonment and identity reconstruction onto immigrant and diaspora communities.
% ABSENT_VOICES: Yiddish and Ladino literary communities, non-Zionist religious scholars who maintained Hebrew as exclusively sacred, and indigenous Arabic-speaking populations were structurally excluded from the discourse that defined linguistic legitimacy; their objections were treated as anti-national or archaic.
% DISAPPEARANCE_RATIONALE: If the native-generational criterion vanished as the dominant framework, Israeli language policy would shift toward multilingual recognition, diaspora Jewish languages might regain institutional support, and the ideological link between national identity and a single mother tongue would weaken â the sociolinguistic order would rearrange.
% FOUNDING_PROBLEM: Jewish diaspora communities spoke dozens of distinct languages, lacking a common modern vernacular for political mobilization, state-building, or territorial concentration; Hebrew existed only as a sacred and literary language.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historians and sociolinguists attest the diaspora lacked a shared secular tongue; Yiddishist and non-Zionist Jewish historians attest that Yiddish and Ladino functioned adequately as diaspora vernaculars and that the 'problem' was constructed by nationalists seeking homogeneity. Academic sociolinguistics outside the Zionist tradition corroborates the multilingual functionality of pre-revival Jewish life.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because the constraint extracts entire linguistic identities and cultural repertoires from Yiddish and Ladino speakers, transferring their communicative labor and filial transmission to Hebrew. Suppression is higher (0.82) because the constraint required active state and social enforcementâschool exclusions, media bans, employment discrimination, and public stigmaâto collapse alternatives. Theater ratio is moderate-high (0.48): much enforcement became performative nationalism (purity campaigns, anti-Yiddish theatrical slogans, symbolic legislation) after the basic coordination goal of creating native speakers was largely achieved by the 1960s. Accessibility collapse is high (0.76) because once the state apparatus was in place, maintaining Yiddish or Ladino as a mother tongue became structurally nearly impossible. Resistance is moderate (0.42): Yiddishists and religious communities mounted significant early resistance, but it was largely overcome by state power.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as successful nation-building coordination: a miraculous revival of a dead tongue and the creation of a unified political community. The payer seat experiences the same structure as cultural dispossession and coerced identity reconstruction. The engine computes this divergence from the same structural data; the authored claim (tangled_rope) asserts that both perceptions are structurally anchored and neither is mere error.
 *
 * DIRECTIONALITY LOGIC:
 *   The zionist_language_institutions and zionist_political_elite sit at the beneficiary end of the directionality spectrum: they collect prestige, budget, and political consolidation from the constraint. Their exit options (arbitrage, mobile) reflect that they could change the policy without personal cost. The yiddish_and_ladino_speakers sit at the full-target end: they pay the transfer directly through mother-tongue abandonment, and their exit is identity_locked because the language is constitutive of their cultural self-concept. Traditional_religious_communities are excluded rather than targeted, but their exclusion is the structural condition that allows the constraint to present itself as unanimous.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâa lack of shared modern vernacular for state-buildingâwas substantially solved by the 1960s, when a full generation of native Hebrew speakers had matured. Yet the constraint persisted: diaspora languages continued to be stigmatized, multilingual education remained suppressed, and the native-generational criterion was used to delegitimize newer diasporic arrivals (e.g., Russian- or Amharic-speaking immigrants). This persistence beyond function is a mandatrophy signal. Classifying the constraint as tangled_rope rather than rope prevents mislabeling the post-solution enforcement as benign coordination; classifying it as tangled_rope rather than snare acknowledges that the initial coordination problem was genuine and the extraction was layered onto that real function rather than pure cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_acquisition_normative_or_descriptive,
    'Is native generational acquisition a necessary descriptive condition for all languages universally, or a normative stipulation particular to the Zionist nation-building project?',
    'Comparative sociolinguistic analysis of language revival cases (Irish, Welsh, Maori) to test whether the native-generational criterion predicts functional vitality or merely national legitimacy.',
    'If descriptive-universal, the constraint has genuine coordination content; if normative-particular, the coordination function is cover for cultural homogenization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_acquisition_normative_or_descriptive, conceptual, 'Whether the native-generational criterion is empirical or ideological').

omega_variable(
    coercion_mechanism_in_revival,
    'Was the abandonment of Yiddish and Ladino primarily driven by state coercion and social sanction, or by voluntary prestige-driven shift toward Hebrew?',
    'Archival analysis of mandatory schooling records, employment discrimination patterns, and oral histories of immigrant language choices in the Yishuv and early State.',
    'A coercion-dominant profile raises suppression and extractiveness, supporting a snare or tangled-rope classification; a prestige-dominant profile lowers both, pushing toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_in_revival, empirical, 'Coercion versus prestige in language shift').

omega_variable(
    reading_contest_ambiguity,
    'Does the native-generational reading foreclose its siblings logically, or do they coexist as incommensurable framings within the same polity?',
    'Analysis of whether Israeli institutional frameworks (education, law, religion) can simultaneously accommodate liturgical-preservation and marketplace-pidgin criteria without logical contradiction.',
    'If foreclosing, the constraint functions as an exclusive epistemic gate; if coexisting, it is one position among many in a distributed contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_ambiguity, conceptual, 'Logical relationship between kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_native_gen_tr_t0, hebrew_linguistic_life__native_generational_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hebrew_native_gen_tr_t10, hebrew_linguistic_life__native_generational_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(hebrew_native_gen_tr_t20, hebrew_linguistic_life__native_generational_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(hebrew_native_gen_tr_t30, hebrew_linguistic_life__native_generational_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(hebrew_native_gen_tr_t40, hebrew_linguistic_life__native_generational_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(hebrew_native_gen_tr_t50, hebrew_linguistic_life__native_generational_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement(hebrew_native_gen_tr_t60, hebrew_linguistic_life__native_generational_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(hebrew_native_gen_tr_t70, hebrew_linguistic_life__native_generational_reading, theater_ratio, 70, 0.45).
narrative_ontology:measurement(hebrew_native_gen_tr_t80, hebrew_linguistic_life__native_generational_reading, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(hebrew_native_gen_be_t0, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(hebrew_native_gen_be_t10, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(hebrew_native_gen_be_t20, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(hebrew_native_gen_be_t30, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(hebrew_native_gen_be_t40, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(hebrew_native_gen_be_t50, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 50, 0.8).
narrative_ontology:measurement(hebrew_native_gen_be_t60, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(hebrew_native_gen_be_t70, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 70, 0.72).
narrative_ontology:measurement(hebrew_native_gen_be_t80, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 80, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_native_gen_su_t0, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hebrew_native_gen_su_t10, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(hebrew_native_gen_su_t20, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(hebrew_native_gen_su_t30, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(hebrew_native_gen_su_t40, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(hebrew_native_gen_su_t50, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 50, 0.82).
narrative_ontology:measurement(hebrew_native_gen_su_t60, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(hebrew_native_gen_su_t70, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 70, 0.58).
narrative_ontology:measurement(hebrew_native_gen_su_t80, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 80, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one reading of the hebrew_linguistic_life kernel. Its siblings (liturgical_preservation_reading and marketplace_pidgin_reading) are separate constraints with distinct epsilon values and stakeholder surfaces. Decomposition follows the epsilon-invariance principle: the label 'Hebrew linguistic life' conflates structurally distinct claims about what counts as language vitality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
