% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Native Generational Acquisition as Sole Criterion for Hebrew Language Life
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The native-generational reading defines language life exclusively as
 *   intergenerational transmission of a language as mother tongue for all
 *   daily functions — secular, mundane, and sacred. Applied to Hebrew, this
 *   reading declares Hebrew 'dead' from ~70 CE to ~1880 CE (liturgical use
 *   alone insufficient) and treats the revival as a deliberate, enforced
 *   reconstruction. The criterion was weaponized: by defining Jewish
 *   linguistic authenticity solely through Hebrew native acquisition, it
 *   rendered Yiddish, Ladino, Judeo-Arabic, and other Jewish languages 'dead'
 *   or 'exile languages' regardless of their millions of native speakers. The
 *   constraint is the criterion itself — the definitional boundary that
 *   legitimized suppression. Beneficiaries (revivalists, Zionist
 *   institutions, Israeli state) gained a unifying national language; victims
 *   (Yiddish/Ladino/Judeo-Arabic speakers) lost intergenerational
 *   transmission under state-enforced monolingualism. The constraint is a
 *   tangled rope: it solves a genuine coordination problem (common language
 *   for a polyglot immigrant society) but does so through asymmetric
 *   extraction (eliminating linguistic diversity).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.78).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.82).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Native Generational Acquisition as Sole Criterion for Hebrew Language Life").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, '159aa19a-7880-46aa-9940-e619b167bba7').
narrative_ontology:cs_kernel_codification('159aa19a-7880-46aa-9940-e619b167bba7', formalized).
narrative_ontology:cs_authority_grounding('159aa19a-7880-46aa-9940-e619b167bba7', extraction).
narrative_ontology:cs_interpretation_layer_present('159aa19a-7880-46aa-9940-e619b167bba7').
narrative_ontology:cs_reading_relation('159aa19a-7880-46aa-9940-e619b167bba7', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('159aa19a-7880-46aa-9940-e619b167bba7', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('159aa19a-7880-46aa-9940-e619b167bba7', foundational, native_generational_acquisition_necessary_for_language_life).
narrative_ontology:cs_axiom_status(native_generational_acquisition_necessary_for_language_life, holdable).
narrative_ontology:cs_axiom_grounding('159aa19a-7880-46aa-9940-e619b167bba7', native_generational_acquisition_necessary_for_language_life, empirically_contingent).
narrative_ontology:cs_axiom('159aa19a-7880-46aa-9940-e619b167bba7', foundational, secular_mundane_use_required_for_language_vitality).
narrative_ontology:cs_axiom_status(secular_mundane_use_required_for_language_vitality, holdable).
narrative_ontology:cs_axiom_grounding('159aa19a-7880-46aa-9940-e619b167bba7', secular_mundane_use_required_for_language_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('159aa19a-7880-46aa-9940-e619b167bba7', hebrew_as_sacred_only).
narrative_ontology:cs_drift_state('159aa19a-7880-46aa-9940-e619b167bba7', post_revival_establishment, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('159aa19a-7880-46aa-9940-e619b167bba7', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revivalists).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, zionist_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, israeli_state).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, judeo_arabic_speakers).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, national_language_unification_requires_vernacular_exclusivity).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, language_revival_is_possible_through_deliberate_policy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ideological entrepreneurs (Ben-Yehuda, Second Aliyah activists) who defined the criterion, built the school system, pressured families to speak Hebrew at home, and treated Yiddish/Ladino as 'exile languages' to be abandoned. They controlled the narrative of what counted as authentic Jewish linguistic life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revivalists, agenda_setter,
    organized, generational, arbitrage, global).

% Jewish National Fund, Histadrut, Jewish Agency — gained a unifying national language that enabled state-building, military coordination, and civic integration. The criterion served their nation-building project; they funded and enforced Hebrew-only education and public life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, zionist_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Post-1948, the state institutionalized the criterion through compulsory Hebrew education, Hebrew-only broadcasting, military service in Hebrew, and suppression of Yiddish theater/press (1949-1951 ban). The state is both administrator and primary beneficiary of the linguistic monopoly.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, israeli_state, beneficiary).

% Eastern European Jewish immigrants and their children for whom Yiddish was mother tongue and daily language. Coerced through school systems, workplace pressure, social stigma, and explicit ideology ('Yiddish is the language of exile') to abandon intergenerational transmission. Exit meant losing communal identity, religious continuity, and cultural heritage.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speakers, payer,
    organized, biographical, identity_locked, national).

% Sephardi/Mizrahi immigrants from Ottoman lands for whom Ladino (Judeo-Spanish) was mother tongue. Faced similar coercion through Hebrew-only schools and media, compounded by ethnic marginalization within the Zionist project. Their linguistic heritage had no institutional defenders in the new state.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speakers, payer,
    moderate, biographical, identity_locked, national).

% Jewish immigrants from Arab countries (Iraq, Yemen, Morocco, etc.) whose mother tongues were Judeo-Arabic varieties. Subjected to the same Hebrew-only regime; their languages were stigmatized as 'Arabic' and thus doubly suspect. Loss was near-total within one generation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, judeo_arabic_speakers, payer,
    moderate, biographical, identity_locked, national).

% Haredi/ultra-Orthodox communities (Neturei Karta, Satmar, Old Yishuv) who opposed the secularization of Hebrew and maintained Yiddish as daily vernacular. Excluded from the revivalist consensus; their objection that Hebrew should remain sacred-only was overridden by state power. They persist as a minority maintaining Yiddish but under constant pressure.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, religious_anti_zionists, excluded,
    organized, generational, constrained, national).

% Secular Yiddishists (Bundists, cultural autonomists) who sought to maintain Yiddish as a modern Jewish national language alongside Hebrew. Marginalized, censored (Yiddish theater ban 1949-1951), and denied state resources. Their vision of bilingual Jewish national life was foreclosed by the native-generational criterion.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_cultural_activists, excluded,
    moderate, biographical, constrained, national).

% Scholars (Fishman, Harshav, Spolsky, Wexler) who analyze the revival as a case study in language planning, noting both the unprecedented success of creating a native-speaking population and the cost in linguistic diversity. They see the full structure but hold no leverage over it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, sociolinguistic_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Created a shared vernacular for a linguistically diverse immigrant population, enabling civic integration, military command, public education, and democratic discourse in a single language — solving the coordination problem of building a functioning polity from dozens of mother tongues.
% TRANSFER_FUNCTION: Moves intergenerational linguistic transmission from heritage languages (Yiddish, Ladino, Judeo-Arabic) to Hebrew, enforced through state education, media monopoly, and ideological pressure. The transfer is from families/communities to the state's linguistic monopoly.
% ABSENT_VOICES: Yiddish/Ladino/Judeo-Arabic speakers themselves — especially the first generation of immigrants who lost their languages — were not consulted about the criterion. Their objection would have been that a living language does not require the extinction of others. They were structurally excluded by the same ideology that defined them as 'exile remnants.'
% DISAPPEARANCE_RATIONALE: If the native-generational criterion vanished overnight, Hebrew would likely remain dominant through inertia, but Yiddish, Ladino, and Judeo-Arabic revival movements would gain legitimacy and state support. Multilingual education models would emerge. The ideological mandate for Hebrew exclusivity would collapse, reorganizing Israeli linguistic policy toward pluralism.
% FOUNDING_PROBLEM: The Yishuv (pre-state Jewish community) lacked a common spoken language: immigrants spoke Yiddish, Ladino, Judeo-Arabic, Russian, Polish, German, etc. No language served for daily coordination across communities. The revivalists posed native-generational Hebrew as the only solution that would create a 'normal' nation.
% FOUNDING_PROBLEM_CORROBORATION: The coordination problem is attested by external observers (British Mandate administrators, early sociologists like Ruppin) who documented the linguistic chaos. But the claim that ONLY native-generational Hebrew could solve it — requiring suppression of all alternatives — is contested by historians (Brenner, Shavit) and linguists (Fishman, Wexler) who note that multilingual federations (Switzerland, Belgium) solve coordination without linguistic monopoly. The beneficiaries (Zionist institutions) attest the problem remains live; external scholars attest it was always solvable without extraction.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the criterion extracts linguistic heritage from communities that had maintained it for centuries, transferring cultural capital to the state's chosen language. Suppression (0.82) is very high because the constraint's persistence depended on active enforcement: school bans on Yiddish, theater censorship, media monopoly, ideological shaming. Theater ratio (0.45) reflects that the coordination function (national unification) is real but increasingly performative — Hebrew would likely remain dominant without active suppression of alternatives. Accessibility collapse (0.88) is near-total for victims: once the criterion is accepted as the definition of 'language life,' alternatives become conceptually impossible (a 'dead language' cannot be revived by the same logic). Resistance (0.55) is moderate: significant cultural resistance existed (Yiddishists, Haredim, Sephardi intellectuals) but was largely overcome by state power.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (revivalists, state), the constraint appears as a rope: genuine coordination achieving a miraculous revival. From the payer seats (Yiddish/Ladino speakers), it appears as a snare: the coordination story is cover for extraction — their languages were viable and the suppression was unnecessary for the coordination function. The engine computes this divergence from the structural data: same constraint, different effective extraction (χ) per seat due to directionality (d) and exit_options (identity_locked vs arbitrage).
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew revivalists and Zionist institutions are structural beneficiaries (d ~ 0.15): they defined the criterion, control its enforcement, and collect the coordination benefits. Israeli state is both agenda-setter and beneficiary (d ~ 0.10): it administers the constraint and gains civic/military cohesion. Yiddish/Ladino/Judeo-Arabic speakers are structural targets (d ~ 0.90): identity_locked exit means they cannot leave the constraint without losing communal identity; the constraint extracts their linguistic heritage. Religious anti-Zionists and Yiddish activists are excluded (d ~ 0.75): they oppose the constraint but are locked in by communal identity and state power. Sociolinguistic observers are analytical (d = 0.5): symmetric costs/benefits as analysts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (linguistic coordination for a polyglot immigrant society) was real but the solution (total Hebrew monopoly via native-generational criterion) was not the only solution. Multilingual models existed. The mandate has outlived its founding function: Hebrew is now securely established as majority native language (90%+), yet the suppression machinery persists (e.g., 2024 nation-state law enshrining Hebrew exclusivity, ongoing marginalization of Arabic, no state support for heritage language education). The constraint persists by inertia and ideological commitment — a classic mandatrophy signature. The theater_ratio rise after 1948 (0.40→0.50) tracks the shift from necessary coordination to performative maintenance of monopoly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the native_generational_reading a distinct constraint from the kernel''s other readings, or a measurement variant of a single constraint?',
    'Apply the ε-invariance test: if the liturgical_preservation_reading and marketplace_pidgin_reading yield structurally different ε values, beneficiary/victim sets, and enforcement requirements, they are distinct constraints linked by network.affects_constraints.',
    'If distinct, each reading gets its own classification (this one: tangled_rope; liturgical may be mountain/rope; pidgin may be rope). If conflated, the engine would average across incommensurable structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: this constraint is one reading of the hebrew_linguistic_life kernel; sibling readings are separate constraint stories.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of Yiddish/Ladino/Judeo-Arabic primarily structural (state bans, school policy) or internalized (speakers accepting ''exile language'' stigma, voluntarily shifting)?',
    'Post-exit suppression trajectory: track heritage language maintenance in communities that emigrated to non-Hebrew-dominant environments (e.g., US, France, Argentina). If transmission persists there but collapsed in Israel, suppression was primarily structural. If transmission collapses everywhere, internalized stigma played larger role.',
    'If primarily structural, the constraint''s effective suppression is higher and the extraction more clearly coercive. If substantially internalized, the constraint operates partly through identity capture — victims enforce it on themselves — which raises the theater_ratio and suggests piton-like dynamics in later phases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for heritage language loss.').

omega_variable(
    coordination_extraction_necessity,
    'Did the coordination function (creating a common language for the Yishuv/Israel) structurally REQUIRE the total suppression of other Jewish languages, or was multilingual coexistence feasible?',
    'Counterfactual comparison: examine multilingual state-building cases (Switzerland, Belgium, Singapore, India) where coordination was achieved without linguistic monopoly. Assess whether Israel''s specific conditions (ongoing conflict, rapid immigration, socialist ideology) made monopoly structurally necessary.',
    'If multilingual coexistence was feasible, the extraction was gratuitous — the tangled_rope''s coordination function could have been achieved with lower extraction, making the constraint closer to snare. If monopoly was structurally necessary, the extraction is the price of coordination, supporting tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_necessity, conceptual, 'Whether the coordination function necessitated the asymmetric extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1880, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1948, 0.4).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1960, 0.5).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1980, 0.48).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_linguistic_life__native_generational_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_linguistic_life__native_generational_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(hebr_be_t1900, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1900, 0.25).
narrative_ontology:measurement(hebr_be_t1920, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(hebr_be_t1948, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1948, 0.72).
narrative_ontology:measurement(hebr_be_t1960, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1960, 0.85).
narrative_ontology:measurement(hebr_be_t1980, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1980, 0.82).
narrative_ontology:measurement(hebr_be_t2000, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(hebr_be_t2024, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1880, 0.2).
narrative_ontology:measurement(hebr_su_t1900, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(hebr_su_t1920, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement(hebr_su_t1948, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1948, 0.88).
narrative_ontology:measurement(hebr_su_t1960, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1960, 0.9).
narrative_ontology:measurement(hebr_su_t1980, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(hebr_su_t2000, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(hebr_su_t2024, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__native_generational_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, israeli_nation_state_law_2018).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, yiddish_cultural_suppression_1949_1951).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, arabic_language_status_israel).

% DUAL FORMULATION NOTE:
% This constraint (native_generational_reading) and its siblings (liturgical_preservation_reading, marketplace_pidgin_reading) form a constraint family decomposing the kernel 'hebrew_linguistic_life'. The ε values differ substantially: liturgical_reading ε ≈ 0.05 (minimal extraction, voluntary participation), pidgin_reading ε ≈ 0.15 (coordination with low suppression), native_generational_reading ε = 0.78 (high extraction, active suppression). The native_generational_reading structurally influences the others by monopolizing state resources and legitimacy, foreclosing their institutional realization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__native_generational_reading, organized, 0.15).
constraint_indexing:directionality_override(hebrew_linguistic_life__native_generational_reading, institutional, 0.1).
constraint_indexing:directionality_override(hebrew_linguistic_life__native_generational_reading, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
