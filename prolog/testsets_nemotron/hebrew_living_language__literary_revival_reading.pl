% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Hebrew as Living Language Through Haskalah Literary Production
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the literary-revival reading of the
 *   hebrew_living_language kernel: the claim that Hebrew remained a 'living
 *   language' through the Haskalah period (c. 1780-1880) and beyond,
 *   sustained by elite literary production — journals, novels, poetry,
 *   scientific translations — despite having no native speakers in daily
 *   life. The written chain was unbroken; generative competence existed in
 *   the literary sphere. This reading is Mountain-like in its metrics:
 *   extremely low extractiveness (elite practice, no coercion), negligible
 *   suppression, partial continuity (the written tradition never died), and
 *   ambiguous strict-reachability (no native speakers until Ben-Yehuda era).
 *   The beneficiaries are the maskilim intellectuals and the Hebrew literary
 *   networks they built, who gained cultural authority from this frame. No
 *   victim set exists — the constraint did not extract from or suppress
 *   alternative Hebrew constituencies. The kernel contest centers on whether
 *   'living language' can be predicated of literary-only maintenance.
 *
 * KEY AGENTS:
 *   - maskilim_intellectuals: Primary beneficiaries (institutional/biographical) — produced the literary corpus, gained cultural authority
 *   - hebrew_literary_networks: Beneficiary institutions (institutional/generational) — journals, societies, publishing houses that sustained written Hebrew
 *   - traditional_scholars: Excluded/alternative constituency (organized/biographical) — maintained liturgical Hebrew, not part of literary-revival frame
 *   - native_generation_advocates: Later constituency (institutional/biographical) — Ben-Yehuda circle, claimed Hebrew 'became living' only with native speech
 *   - analytical_observer: Observer (analytical/civilizational) — sees full kernel structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.05).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.08).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, mountain).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew as Living Language Through Haskalah Literary Production").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:emerges_naturally(hebrew_living_language__literary_revival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '2c573d2c-429a-4039-89c0-83bbf3b33067').
narrative_ontology:cs_kernel_codification('2c573d2c-429a-4039-89c0-83bbf3b33067', distributed).
narrative_ontology:cs_authority_grounding('2c573d2c-429a-4039-89c0-83bbf3b33067', distributed).
narrative_ontology:cs_reading_relation('2c573d2c-429a-4039-89c0-83bbf3b33067', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c573d2c-429a-4039-89c0-83bbf3b33067', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('2c573d2c-429a-4039-89c0-83bbf3b33067', foundational, literary_production_suffices_for_living_language).
narrative_ontology:cs_axiom_status(literary_production_suffices_for_living_language, holdable).
narrative_ontology:cs_axiom_grounding('2c573d2c-429a-4039-89c0-83bbf3b33067', literary_production_suffices_for_living_language, conventional).
narrative_ontology:cs_axiom('2c573d2c-429a-4039-89c0-83bbf3b33067', secondary, written_generative_competence_is_language_vitality).
narrative_ontology:cs_axiom_status(written_generative_competence_is_language_vitality, holdable).
narrative_ontology:cs_axiom_grounding('2c573d2c-429a-4039-89c0-83bbf3b33067', written_generative_competence_is_language_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('2c573d2c-429a-4039-89c0-83bbf3b33067', haskalah_literary_continuity).
narrative_ontology:cs_drift_state('2c573d2c-429a-4039-89c0-83bbf3b33067', native_generation_onset, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2c573d2c-429a-4039-89c0-83bbf3b33067', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, maskilim_intellectuals).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_literary_networks).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, written_competence_preserves_language_vitality).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, literary_generation_sustains_transmission_chain).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, haskalah_hebrew_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enlightenment Hebrew writers (Mendelssohn, Wessely, Luzzatto, Mapu, Smolenskin, etc.) who produced the modern Hebrew literary corpus — journals (Ha-Me'assef, Ha-Shahar), novels (Ahavat Zion), poetry, scientific translations. They wrote Hebrew by choice, not necessity; their exit options were German, Russian, Yiddish, French. They gained cultural authority, institutional recognition, and intellectual community from the literary-revival frame. No coercion bound them to Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, maskilim_intellectuals, beneficiary,
    institutional, biographical, arbitrage, continental).

% The journals, literary societies (e.g., Shoharei Ha-Tov Ve-Ha-Yosher), publishing houses, and correspondence networks that sustained written Hebrew across Eastern and Central Europe. These institutions collected subscriptions, distributed texts, and provided the infrastructure for literary production. They benefited from the literary-revival frame as it justified their existence and attracted funding. Exit was possible (shift to Yiddish or European-language publishing) but would dissolve the network's specific identity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_literary_networks, beneficiary,
    institutional, generational, mobile, continental).

% Rabbinic scholars and yeshiva networks maintaining liturgical Hebrew and Aramaic textual study. They were not part of the Haskalah literary project; their Hebrew was ritual-textual, not literary-generative. They would object to the claim that literary Hebrew is the 'living' form, but they were structurally excluded from the maskilim's literary sphere. Their identity was fused to liturgical continuity; exit from that frame meant religious rupture.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, traditional_scholars, excluded,
    organized, generational, identity_locked, continental).

% The Ben-Yehuda circle and early Zionist Hebrew speakers (1880s onward) who claimed Hebrew only became 'living' with native daily speech. They used the literary corpus as raw material but treated literary-revival as insufficient for 'living language' status. They observed the literary-revival constraint from outside — it preceded them and provided vocabulary, but they did not participate in it as a coordination mechanism.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, native_generation_advocates, observer,
    institutional, biographical, analytical, regional).

% The comparative linguist or historian of language revival who sees the full kernel structure: three readings of 'hebrew_living_language,' each with Mountain-like metrics in its own domain, mutually ambiguous on the predicate 'living.' This seat computes the structural relationships across readings.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Created a shared modern Hebrew literary language capable of expressing Enlightenment thought, scientific concepts, and nationalist sentiment — solving the coordination problem of how a dispersed intellectual community could communicate in Hebrew without a spoken vernacular.
% TRANSFER_FUNCTION: Moves cultural authority and intellectual legitimacy from traditional rabbinic elites to maskilim literary networks, via the production and circulation of modern Hebrew texts. No material resource transfer; the transfer is symbolic capital.
% ABSENT_VOICES: Traditional scholars (liturgical continuity constituency) and the future native-speaking generation (not yet existent) — the former excluded by the maskilim's secular frame, the latter not yet born. Yiddish-speaking masses were not addressed by literary Hebrew and had no voice in its construction.
% DISAPPEARANCE_RATIONALE: If the literary-revival constraint vanished overnight (no Haskalah Hebrew journals, novels, translations), the modern Hebrew lexical and grammatical corpus would not exist — Ben-Yehuda's spoken revival would lack its written foundation. The native-generation reading depends on the literary-revival reading's output. The world rearranges: no literary corpus, no spoken revival in its historical form.
% FOUNDING_PROBLEM: How to express modern European thought (science, philosophy, nationalism) in Hebrew — a language with no native speakers, no modern vocabulary, and no secular literary tradition — so that Jewish intellectuals could participate in Enlightenment discourse without abandoning Hebrew.
% FOUNDING_PROBLEM_CORROBORATION: Maskilim writings explicitly state this problem (Mendelssohn's Bi'ur project, Wessely's Divrei Shalom Ve-Emet, Smolenskin's editorials). External corroboration: non-Jewish Hebraists (e.g., Gesenius, Geiger) document the vocabulary gap. No non-beneficiary source disputes the founding problem's reality — even traditional scholars acknowledged Hebrew lacked modern vocabulary, though they opposed the secular solution.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_living_language__literary_revival_reading),
    narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics reflect the structural reality of elite literary production: extractiveness ~0.05 because participation was voluntary, limited to a small intellectual class, and involved no resource extraction from non-participants; suppression ~0.08 because the literary sphere coexisted with (did not suppress) liturgical Hebrew and Yiddish vernaculars; theater_ratio ~0.12 because the literary project was genuinely generative — new vocabulary, genres, scientific register — not performative maintenance; accessibility_collapse ~0.25 because alternatives (Yiddish, liturgical Hebrew, European languages) remained fully accessible and competed for the same intellectuals; resistance ~0.15 because the literary-revival frame faced intellectual critique (from traditionalists and assimilationists) but no structural enforcement was needed or applied.
 *
 * PERSPECTIVAL GAP:
 *   The maskilim and literary networks experienced this as genuine coordination — they built a shared written language de novo. Traditional scholars experienced it as a rival frame for Hebrew's legitimacy (liturgical continuity vs. literary innovation). Native-generation advocates later experienced it as a predecessor that provided corpus but not spoken vitality. The analytical observer sees three readings of one kernel, each Mountain-like in its own metrics but mutually ambiguous on the predicate 'living.'
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (maskilim, literary networks) are institutional actors with biographical-to-generational horizons and arbitrage-grade exit (they wrote in German, Russian, Yiddish too). No payers/victims exist — the constraint did not impose costs on non-participants. Directionality for beneficiaries is near 0.0 (full beneficiary); for excluded traditional scholars, directionality is irrelevant (not governed by this constraint). The engine will compute per-seat types from this structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem (creating a modern Hebrew literary language capable of expressing Enlightenment thought) was live throughout the period and the arrangement solved it. The arrangement did not persist past its function — it evolved into the native-generation phase. The literary-revival constraint was transitional in function (scaffold-like) but Mountain in metrics because it emerged from voluntary intellectual coordination, not enforced structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Does the ''living language'' predicate apply to a language sustained solely by elite literary production without native daily speech, or does ''living'' require generative native speakers?',
    'Comparative analysis of language vitality metrics across revival cases (Hebrew, Cornish, Manx, Wampanoag) distinguishing literary-maintenance from intergenerational transmission.',
    'If ''living'' requires native generation, this reading''s Mountain claim collapses — the constraint becomes a claim about literary practice, not language vitality. If literary production suffices, the Mountain holds for this reading but the kernel itself fragments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel ''hebrew_living_language'' admits a literary-only reading as structurally Mountain, or whether the predicate ''living'' structurally forecloses non-native readings.').

omega_variable(
    beneficiary_extraction_coupling,
    'Do the maskilim and Hebrew literary networks benefit from the literary-revival constraint in a way that constitutes extraction from other potential Hebrew constituencies (traditional scholars, later native speakers)?',
    'Resource-flow analysis of Haskalah-era Hebrew publishing, educational appointments, and institutional recognition — who gained professional standing, funding, or authority from the literary-revival frame?',
    'If beneficiaries captured institutional resources that would otherwise flow to other Hebrew constituencies, the constraint shows False Summit Mountain signature — natural-law framing masking rent capture. If benefits were purely literary-cultural with no resource diversion, Mountain holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_extraction_coupling, empirical, 'Whether the declared beneficiaries extracted rents from the literary-revival frame at the expense of other Hebrew constituencies.').

omega_variable(
    reading_relations_structural_delta,
    'What is the structural relationship between this literary-revival reading and the liturgical-continuity and native-generation readings?',
    'Trace institutional succession: did Haskalah literary networks displace liturgical authorities? Did native-generation advocates treat literary-revival as predecessor or rival? Map resource flows and authority claims across the three readings.',
    'If literary-revival forecloses liturgical-continuity (by claiming the ''living'' mantle), or influences native-generation (by providing the corpus native speakers revived), the kernel''s internal structure determines which readings can coexist in a single framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_structural_delta, conceptual, 'Structural relationship of this reading to sibling readings of the hebrew_living_language kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1780, 1940).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1780, hebrew_living_language__literary_revival_reading, theater_ratio, 1780, 0.08).
narrative_ontology:measurement(hebr_tr_t1820, hebrew_living_language__literary_revival_reading, theater_ratio, 1820, 0.1).
narrative_ontology:measurement(hebr_tr_t1860, hebrew_living_language__literary_revival_reading, theater_ratio, 1860, 0.12).
narrative_ontology:measurement(hebr_tr_t1890, hebrew_living_language__literary_revival_reading, theater_ratio, 1890, 0.12).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_living_language__literary_revival_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_living_language__literary_revival_reading, theater_ratio, 1940, 0.12).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1780, hebrew_living_language__literary_revival_reading, base_extractiveness, 1780, 0.03).
narrative_ontology:measurement(hebr_be_t1820, hebrew_living_language__literary_revival_reading, base_extractiveness, 1820, 0.04).
narrative_ontology:measurement(hebr_be_t1860, hebrew_living_language__literary_revival_reading, base_extractiveness, 1860, 0.05).
narrative_ontology:measurement(hebr_be_t1890, hebrew_living_language__literary_revival_reading, base_extractiveness, 1890, 0.05).
narrative_ontology:measurement(hebr_be_t1920, hebrew_living_language__literary_revival_reading, base_extractiveness, 1920, 0.05).
narrative_ontology:measurement(hebr_be_t1940, hebrew_living_language__literary_revival_reading, base_extractiveness, 1940, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1780, hebrew_living_language__literary_revival_reading, suppression_requirement, 1780, 0.05).
narrative_ontology:measurement(hebr_su_t1820, hebrew_living_language__literary_revival_reading, suppression_requirement, 1820, 0.07).
narrative_ontology:measurement(hebr_su_t1860, hebrew_living_language__literary_revival_reading, suppression_requirement, 1860, 0.08).
narrative_ontology:measurement(hebr_su_t1890, hebrew_living_language__literary_revival_reading, suppression_requirement, 1890, 0.08).
narrative_ontology:measurement(hebr_su_t1920, hebrew_living_language__literary_revival_reading, suppression_requirement, 1920, 0.08).
narrative_ontology:measurement(hebr_su_t1940, hebrew_living_language__literary_revival_reading, suppression_requirement, 1940, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__literary_revival_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hebrew_living_language kernel. The literary-revival reading (this story) claims Mountain metrics for elite literary production. The liturgical-continuity reading claims Mountain for ritual transmission. The native-generation reading claims Scaffold/Rope for the spoken revival. The three readings differ on the predicate 'living' and its structural requirements — each has its own ε, beneficiaries, and type. They form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
