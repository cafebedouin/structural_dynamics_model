% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Hebrew Living Language — Native Generation Criterion
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The native_generation_reading asserts that Hebrew becomes a living
 *   language only when native speakers produce daily speech generatively —
 *   not through liturgical recitation or literary production alone. This
 *   reading, championed by Eliezer Ben-Yehuda and the Zionist revival
 *   project, treats the emergence of native Hebrew speakers in Palestine
 *   (late 19th–mid 20th century) as the definitive moment Hebrew became
 *   'living.' The constraint operates as a tangled rope: it coordinates a
 *   linguistically diverse immigrant population (Yiddish, Ladino,
 *   Judeo-Arabic, Russian, Polish speakers) around a single vernacular,
 *   enabling shared civic life, education, and defense — but it achieves this
 *   through active suppression of the immigrants' heritage vernaculars
 *   (Yiddish theater bans, 'speak Hebrew' campaigns, marginalization of
 *   non-Hebrew press). The victims are the speakers of those vernaculars
 *   whose intergenerational transmission was disrupted. The reading's ε
 *   (0.45) reflects moderate but sustained extraction: the coordination
 *   function is genuine (a common language was functionally necessary), but
 *   the asymmetric cost falls on specific communities whose languages were
 *   treated as obstacles rather than resources.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.45).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.6).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew Living Language — Native Generation Criterion").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, 'bf716992-b064-4313-b1d3-e1c95586f03c').
narrative_ontology:cs_kernel_codification('bf716992-b064-4313-b1d3-e1c95586f03c', formalized).
narrative_ontology:cs_authority_grounding('bf716992-b064-4313-b1d3-e1c95586f03c', lineage).
narrative_ontology:cs_interpretation_layer_present('bf716992-b064-4313-b1d3-e1c95586f03c').
narrative_ontology:cs_reading_relation('bf716992-b064-4313-b1d3-e1c95586f03c', hebrew_living_language__liturgical_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('bf716992-b064-4313-b1d3-e1c95586f03c', hebrew_living_language__literary_revival_reading, forecloses).
narrative_ontology:cs_axiom('bf716992-b064-4313-b1d3-e1c95586f03c', foundational, native_speech_generativity_defines_living_language).
narrative_ontology:cs_axiom_status(native_speech_generativity_defines_living_language, holdable).
narrative_ontology:cs_axiom_grounding('bf716992-b064-4313-b1d3-e1c95586f03c', native_speech_generativity_defines_living_language, empirically_contingent).
narrative_ontology:cs_reference_frame('bf716992-b064-4313-b1d3-e1c95586f03c', native_speech_criterion).
narrative_ontology:cs_drift_state('bf716992-b064-4313-b1d3-e1c95586f03c', post_statehood, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bf716992-b064-4313-b1d3-e1c95586f03c', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, zionist_revival_project).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, new_yishuv_immigrants).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_educators).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, judeo_arabic_speakers).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, native_speech_generativity_defines_living_language).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, vernacular_revival_possible).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored and enforced the Hebrew revival through institutions (Va'ad HaLashon, later Academy of Hebrew Language), education system, and public campaigns. Gains national cohesion, a distinct national language, and symbolic sovereignty. Can redirect language policy at will; exit is irrelevant as it controls the constraint.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, zionist_revival_project, agenda_setter,
    institutional, generational, arbitrage, national).

% Linguistically diverse immigrants (Yiddish, Russian, Polish, German, Arabic speakers) who gain a common spoken language for daily life, employment, and civic participation. They benefit from coordination but pay the cost of heritage language loss. Exit means emigration or cultural marginalization; constrained by economic and ideological commitment to the Yishuv.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, new_yishuv_immigrants, beneficiary,
    organized, biographical, constrained, national).

% Teachers, linguists, curriculum developers who gain professional status, employment, and institutional authority from Hebrew's institutionalization. Their careers depend on the constraint's persistence. Exit means professional retraining; constrained by specialized investment in Hebrew pedagogy.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_educators, beneficiary,
    moderate, biographical, constrained, national).

% Ashkenazi immigrants and their descendants for whom Yiddish was the primary vernacular and carrier of cultural, political (Bundist, socialist), and religious identity. Subject to 'Ivri, daber ivrit' campaigns, Yiddish theater bans (1920s–1950s), press restrictions, and educational exclusion. Identity_locked: Yiddish was fused with anti-assimilationist Jewish identity; abandoning it meant not just language shift but ideological rupture. Intergenerational transmission collapsed within two generations in Palestine/Israel.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speakers, payer,
    organized, generational, identity_locked, national).

% Sephardi immigrants from the Ottoman Balkans and Levant for whom Ladino (Judeo-Spanish) was the vernacular. Less politically organized than Yiddishists; marginalized by both Ashkenazi-dominated Yishuv institutions and the Hebrew revival's Ashkenazi-normative pronunciation and lexicon. Exit_options constrained: some maintained Ladino in home settings for a generation, but institutional support was absent; transmission collapsed faster than Yiddish due to smaller population and lower prestige.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speakers, payer,
    moderate, generational, constrained, national).

% Mizrahi immigrants from Arab countries (Iraq, Yemen, Morocco, etc.) for whom Judeo-Arabic varieties were vernaculars. Arrived mostly post-1948 into a Hebrew-dominant state; their languages were treated as 'dialects' or 'corrupted Arabic' rather than distinct Jewish languages. No institutional support for maintenance; rapid shift to Hebrew enforced by schooling, military service, and economic integration. Exit_options constrained: emigration not viable; cultural marginalization within Israeli society.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, judeo_arabic_speakers, payer,
    moderate, generational, constrained, national).

% Bundists, Yiddishist intellectuals, cultural autonomists who argued for Yiddish as the Jewish national language and opposed Hebrew's exclusivity. Structurally excluded from language policy decisions; their institutions (Yiddish schools, press, theater) were suppressed or denied state funding. Trapped: they could not exit the constraint's jurisdiction (British Mandate, then Israel) and their identity was bound to the suppressed language.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_cultural_activists, excluded,
    organized, generational, trapped, national).

% Ultra-Orthodox (Haredi) communities who viewed Hebrew as a sacred language (lashon hakodesh) unfit for mundane daily use, and Zionism as heretical. Excluded from the revival project's framing; their objection (secularization of the holy tongue) was structurally inadmissible within the secular-nationalist framework. Identity_locked: their entire religious identity requires maintaining Yiddish as the vernacular and Hebrew as sacred; exit from this frame is doctrinal apostasy.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, anti_zionist_religious_communities, excluded,
    organized, generational, identity_locked, national).

% External analysts (e.g., Joshua Fishman, Ghil'ad Zuckermann, Nancy Dorian) who study Hebrew revival as a case study in language revitalization. They see the full structure: coordination function, extraction from minority vernaculars, identity-lock dynamics, and mandatrophy. Their seat computes the per-seat classifications the engine produces.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, sociolinguists_revitalization_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creating a shared spoken vernacular for a linguistically diverse immigrant population (Yiddish, Ladino, Judeo-Arabic, Russian, Polish, German speakers) with no common language, enabling civic life, education, defense, and nation-building in the Yishuv and early Israel.
% TRANSFER_FUNCTION: Moves linguistic capital, institutional recognition, educational resources, media access, and public sphere participation from Yiddish, Ladino, Judeo-Arabic, and other heritage vernaculars to Hebrew. The transfer is enforced through school systems, public employment requirements, media licensing, and social pressure campaigns ('Ivri, daber ivrit').
% ABSENT_VOICES: Yiddish cultural activists (Bundists, Yiddishists), anti-Zionist Haredi communities, Sephardi/Mizrahi intellectuals who advocated for Judeo-Arabic or Ladino recognition, and Palestinian Arabic speakers whose language was simultaneously marginalized. These voices were structurally excluded from the language policy arena by the Zionist project's institutional monopoly and the security/militarization logic that treated linguistic unity as existential.
% DISAPPEARANCE_RATIONALE: If the native-generation criterion and its enforcement machinery vanished overnight, Hebrew would likely remain dominant due to its current entrenchment (millions of native speakers, state infrastructure), but the linguistic landscape would reorganize: Yiddish, Ladino, and Judeo-Arabic might have persisted as community vernaculars alongside Hebrew (as in the Catalan/Spanish or Welsh/English models), Arabic would have stronger institutional standing, and the ideological link between Hebrew monolingualism and Jewish national authenticity would dissolve.
% FOUNDING_PROBLEM: How to create a common spoken language for a linguistically diverse immigrant population with no shared vernacular, in a context of nation-building under existential threat, where linguistic unity was seen as a prerequisite for collective survival.
% FOUNDING_PROBLEM_CORROBORATION: The general problem of creating a shared vernacular for polyglot communities is corroborated by language revitalization scholars outside the Zionist project — notably Joshua Fishman (reversing language shift framework), Nancy Dorian (language death/contracting communities), and contemporary revitalization practitioners (Māori, Welsh, Hawaiian, Sami). They attest that the 'Babel problem' of immigrant or minority populations lacking a common spoken language is a real, recurring coordination challenge. However, they also document that suppression of heritage languages is NOT a necessary solution (cf. Catalan, Basque, Māori models), corroborating that the specific extraction component of this constraint outlived its coordination justification.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__native_generation_reading_tests).
:- end_tests(hebrew_living_language__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the revival project diverted cultural, educational, and institutional resources from existing vernaculars to Hebrew, but the coordination benefit (a shared spoken language for a polyglot immigrant society) is real and substantial. Suppression (0.6) is high: the constraint's persistence depended on active enforcement — school systems, media control, public campaigns, and social pressure — not merely participant preference. Theater ratio (0.3) reflects performative elements: the revival narrative ('Hebrew was dead and we resurrected it') obscures the continuous liturgical and literary use that never ceased. Accessibility collapse (0.7) is high: once Hebrew became the language of instruction, governance, and daily life, alternatives for full civic participation effectively vanished. Resistance (0.5) was significant: Yiddishist movements (Bund, cultural autonomists), ultra-Orthodox anti-Zionists, and Sephardi/Mizrahi intellectuals actively contested the suppression, but were overborne by state-building imperatives.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (Zionist project), the constraint is a rope: genuine coordination solving the 'Babel problem' of immigrant polyglossia. From the payer seats (Yiddish/Ladino/Judeo-Arabic speakers), it is a snare: their languages were suppressed to clear space for Hebrew, and the coordination story is cover for nation-building extraction. The engine computes this divergence from the structural data — the declared beneficiaries and victims, their power levels (institutional vs. organized/moderate), and exit options (arbitrage vs. constrained/identity_locked). The claimed_type (tangled_rope) reflects the author's judgment that both the coordination function and the asymmetric extraction are structurally real and inseparable in this constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The zionist_revival_project (agenda_setter) sits at the beneficiary end (d ~0.15): it authored the constraint, controls the Academy of Hebrew Language, and gains national cohesion from Hebrew's success. New_yishuv_immigrants (beneficiary) also benefit (d ~0.3): they gain a common language for economic and social integration, though they pay the cost of abandoning heritage languages. Hebrew_educators (beneficiary) gain professional status and institutional roles (d ~0.25). Yiddish_speakers, ladino_speakers, judeo_arabic_speakers (payers) sit at the target end (d ~0.85): they bear the intergenerational transmission loss, cultural marginalization, and explicit suppression campaigns. Their exit_options were 'constrained' (emigration possible but costly) to 'identity_locked' (Yiddish as core of Bundist/Orthodox identity). The strict-reachability break is acknowledged: the gap between liturgical Hebrew and generative native speech was bridged only through deliberate reconstruction (neologisms, grammar standardization), not organic continuity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — creating a shared vernacular for a linguistically diverse immigrant population — was live at the start (late 19th century) and remains live for language revitalization projects globally (corroborated by Fishman's reversing language shift framework). However, the specific constraint (Hebrew as exclusive vernacular enforced through suppression of other Jewish languages) has outlived its founding justification: Hebrew is now securely established with millions of native speakers; the suppression machinery (bans, campaigns) is no longer necessary for Hebrew's vitality but persists in attenuated form (e.g., academic Hebrew requirements marginalizing Arabic, Yiddish cultural funding disparities). This is mandatrophy: the mandate (revive Hebrew) has succeeded, but the extraction machinery (suppress competitors) persists. The six-questions capture this: founding_problem_status = live (the general problem persists), but the specific constraint's extraction component is no longer justified by the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vernacular_suppression_necessity,
    'Was the suppression of Yiddish, Ladino, and Judeo-Arabic structurally necessary for Hebrew''s revival as a native spoken language, or was it ideological overreach by the Zionist project?',
    'Counterfactual comparison with other language revivals (e.g., Māori, Welsh, Catalan) where minority vernaculars coexisted with the revived language; historical analysis of ''Ivri, daber ivrit'' campaign necessity vs. political choice.',
    'If suppression was unnecessary, the constraint''s extraction component is pure ideological imposition rather than coordination cost, shifting classification toward snare. If necessary, the extraction is the price of coordination, supporting tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_suppression_necessity, empirical, 'Whether vernacular suppression was a coordination necessity or ideological extraction.').

omega_variable(
    living_language_definition_contestation,
    'Is the definition of ''living language'' as requiring native generative speech the only linguistically valid criterion, or does the liturgical/literary continuity criterion constitute a legitimate alternative definition?',
    'Disciplinary consensus survey in sociolinguistics and language revitalization studies; analysis of UNESCO language vitality criteria and Ethnologue classification practices.',
    'If the native-speech criterion is merely one contested definition among others, the kernel''s contestation is definitional rather than structural — the constraint''s claimed_type may be a framing effect. If the criterion is disciplinarily dominant, the reading''s axiomatic claim holds empirical weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_language_definition_contestation, conceptual, 'Whether the native-generation definition of ''living language'' is structurally privileged or contestable.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (native_generation_reading) of the contested kernel hebrew_living_language. Sibling readings: liturgical_continuity_reading, literary_revival_reading. What structural elements do the readings disagree on?',
    'Compare the three readings'' victim sets, beneficiary sets, and claimed coordination functions. The native_generation_reading identifies Yiddish/Ladino speakers as victims and requires active suppression; the liturgical_reading identifies no victims (continuity is costless); the literary_reading identifies maskilic intellectuals as beneficiaries without a clear victim set.',
    'Clarifies that ε differs across readings because they describe different constraints: native_generation_reading = extraction via suppression; liturgical_reading = near-zero extraction; literary_reading = coordination without extraction. Confirms ε-invariance requires separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: this reading''s structural delta vs. sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_native_gen_tr_t0, hebrew_living_language__native_generation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebrew_native_gen_tr_t20, hebrew_living_language__native_generation_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(hebrew_native_gen_tr_t40, hebrew_living_language__native_generation_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(hebrew_native_gen_tr_t60, hebrew_living_language__native_generation_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(hebrew_native_gen_tr_t80, hebrew_living_language__native_generation_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement(hebrew_native_gen_tr_t100, hebrew_living_language__native_generation_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(hebrew_native_gen_be_t0, hebrew_living_language__native_generation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebrew_native_gen_be_t20, hebrew_living_language__native_generation_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(hebrew_native_gen_be_t40, hebrew_living_language__native_generation_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(hebrew_native_gen_be_t60, hebrew_living_language__native_generation_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(hebrew_native_gen_be_t80, hebrew_living_language__native_generation_reading, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(hebrew_native_gen_be_t100, hebrew_living_language__native_generation_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_native_gen_su_t0, hebrew_living_language__native_generation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(hebrew_native_gen_su_t20, hebrew_living_language__native_generation_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(hebrew_native_gen_su_t40, hebrew_living_language__native_generation_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(hebrew_native_gen_su_t60, hebrew_living_language__native_generation_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(hebrew_native_gen_su_t80, hebrew_living_language__native_generation_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(hebrew_native_gen_su_t100, hebrew_living_language__native_generation_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__native_generation_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_academy_standardization).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, yiddish_suppression_policy).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, arabic_language_status_israel).

% DUAL FORMULATION NOTE:
% This story is one member of the hebrew_living_language constraint family. The kernel decomposes into three readings with distinct ε values: liturgical_continuity_reading (ε ≈ 0.05, mountain), literary_revival_reading (ε ≈ 0.15, rope), native_generation_reading (ε = 0.45, tangled_rope). The ε-invariance principle requires separate stories because the referent ('Hebrew as living language') is measured differently by each reading: liturgical continuity measures textual transmission continuity; literary revival measures written generative competence; native generation measures native speaker communities. These are different constraints linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_living_language__native_generation_reading, institutional, 0.15).
constraint_indexing:directionality_override(hebrew_living_language__native_generation_reading, organized, 0.3).
constraint_indexing:directionality_override(hebrew_living_language__native_generation_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
