% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Native Daily Hebrew as Sole Vitality Criterion
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint story captures the 'native daily reading' of Hebrew
 *   vitality: the claim that Hebrew is only 'alive' when it serves as a
 *   native vernacular for daily life, and that liturgical, scholarly, or
 *   diaspora uses count as 'preservation, not life.' This reading was
 *   constructed by the Zionist revival project (late 19th century onward) and
 *   institutionalized through the Hebrew education system, the Academy of the
 *   Hebrew Language, and state policy. It extracts legitimacy from
 *   traditional custodians of Hebrew (religious communities, diaspora
 *   scholars) by redefining their relationship to the language as
 *   inauthentic. The constraint operates as a tangled rope: it solves a
 *   genuine coordination problem (creating a shared vernacular for
 *   state-building) while asymmetrically extracting from communities whose
 *   Hebrew was never 'dead' but differently alive.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.42).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.68).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Native Daily Hebrew as Sole Vitality Criterion").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, 'a3786b31-df51-457a-b818-2d0b13dea9a3').
narrative_ontology:cs_kernel_codification('a3786b31-df51-457a-b818-2d0b13dea9a3', distributed).
narrative_ontology:cs_authority_grounding('a3786b31-df51-457a-b818-2d0b13dea9a3', lineage).
narrative_ontology:cs_interpretation_layer_present('a3786b31-df51-457a-b818-2d0b13dea9a3').
narrative_ontology:cs_reading_relation('a3786b31-df51-457a-b818-2d0b13dea9a3', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('a3786b31-df51-457a-b818-2d0b13dea9a3', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('a3786b31-df51-457a-b818-2d0b13dea9a3', foundational, vernacular_nativity_as_vitality_necessary_condition).
narrative_ontology:cs_axiom_status(vernacular_nativity_as_vitality_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('a3786b31-df51-457a-b818-2d0b13dea9a3', vernacular_nativity_as_vitality_necessary_condition, conventional).
narrative_ontology:cs_axiom('a3786b31-df51-457a-b818-2d0b13dea9a3', foundational, liturgical_usage_as_preservation_not_life).
narrative_ontology:cs_axiom_status(liturgical_usage_as_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('a3786b31-df51-457a-b818-2d0b13dea9a3', liturgical_usage_as_preservation_not_life, conventional).
narrative_ontology:cs_reference_frame('a3786b31-df51-457a-b818-2d0b13dea9a3', pre_revival_hebrew_as_sacred_only).
narrative_ontology:cs_drift_state('a3786b31-df51-457a-b818-2d0b13dea9a3', contemporary_israeli_hebrew_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a3786b31-df51-457a-b818-2d0b13dea9a3', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, hebrew_education_establishment).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, secular_nationalist_intelligentsia).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_tradition_continuity).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, traditional_religious_communities).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, diaspora_hebrew_scholarship).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, vernacular_reconstruction_as_nation_building).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, living_language_as_sovereignty_marker).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drove Hebrew revival as a national infrastructure project: mandated Hebrew in schools, administration, military, and public life. Invested massive institutional resources in lexical expansion (Academy of the Hebrew Language), teacher training, and suppression of competing linguistic frameworks (Yiddish, Ladino, liturgical Hebrew as primary). Gains legitimacy and nation-building coherence from Hebrew's status as a living vernacular.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, zionist_state_building_project, beneficiary).

% Controls Hebrew language pedagogy, curriculum, and certification. Benefits from state funding, professional prestige, and institutional monopoly on language gatekeeping. Their authority derives from being the certified transmitters of 'correct' Hebrew. Exit is constrained because their professional identity is fused with the revival project's success.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, hebrew_education_establishment, beneficiary,
    organized, biographical, constrained, national).

% Cultural producers (writers, journalists, academics) for whom Hebrew vernacular is both medium and credential. Their cultural capital depends on Hebrew's vitality as a modern expressive instrument. They have more exit mobility than the education establishment but their audience and recognition structure is Hebrew-bound.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, secular_nationalist_intelligentsia, beneficiary,
    organized, biographical, mobile, national).

% Communities for whom Hebrew's primary valence is sacred text, prayer, and study. The revival's secularization and semantic shift (biblical words repurposed for modern concepts: 'electricity', 'democracy', 'tank') constitutes desacralization. They pay by having their liturgical language become a secular national instrument. Exit is identity-locked: abandoning Hebrew would fracture religious continuity, but staying means accepting its transformation.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_tradition_continuity, payer,
    organized, generational, identity_locked, global).

% Haredi and other traditional communities who maintained Hebrew as loshn-koydesh (holy tongue) while using Yiddish or other vernaculars. The revival imposed a Hebrew they did not choose, repurposed their sacred lexicon, and marginalized their linguistic ecology. They are trapped: the state operates in Hebrew, military service requires Hebrew, civic participation requires Hebrew — but their relationship to the language is fundamentally alienated.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, traditional_religious_communities, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, traditional_religious_communities, excluded).

% Scholars and educators outside Israel who maintained Hebrew as a literary and scholarly medium. The Israeli revival's claim to monopoly on 'authentic' Hebrew vitality marginalizes diaspora Hebrew creativity, redefines normative usage around Israeli norms, and extracts recognition capital. Their exit is constrained: they can continue working but lose institutional recognition and audience access.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_hebrew_scholarship, payer,
    moderate, biographical, constrained, global).

% Communities from Arabic-speaking lands with their own Hebrew liturgical traditions (different pronunciation, piyyut traditions, religious poetry). The revival's Ashkenazi-normative pronunciation and secular lexicon erased their Hebrew traditions. They were excluded from defining the revived language's shape while being compelled to adopt it.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, mizrahi_jewish_communities, excluded,
    moderate, generational, constrained, national).

% Sociolinguists, historians of language revival, and comparative revival scholars who study Hebrew as a case study. They observe the constraint's operation without being organized by it. Their analytical exit is unrestricted.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, linguistic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared vernacular for a territorially concentrated population building state institutions, enabling unified education, administration, military command, and civic discourse where none existed previously.
% TRANSFER_FUNCTION: Moves linguistic authority, lexical ownership, and cultural capital from traditional religious custodians (who held Hebrew as sacred trust) to a secular state-building project that repurposes the language as a national infrastructure. Transfers the cost of lexical innovation and pedagogical enforcement onto communities whose primary relationship to Hebrew was liturgical.
% ABSENT_VOICES: Pre-revival Hebrew speakers in the Yishuv who used Hebrew as a lingua franca but not a native vernacular; Jewish communities in the Islamic world whose Hebrew traditions were distinct from both Ashkenazi liturgical and secular Zionist norms; Palestinian Arabic speakers whose language was displaced in the same territorial space. These voices are structurally absent from the 'revival' narrative because the constraint defines vitality in a way that excludes them by definition.
% DISAPPEARANCE_RATIONALE: If the 'native generation only' criterion vanished overnight, Hebrew's status as a national language would not collapse — it has millions of native speakers and full institutional infrastructure. But the *moral hierarchy* that delegitimizes liturgical Hebrew, diaspora Hebrew, and non-native speakers would dissolve. Religious communities could reclaim Hebrew as sacred without being told their usage is 'preservation not life'. Diaspora Hebrew creativity would gain legitimacy. The constraint's disappearance rearranges the *evaluation framework*, not the language's existence.
% FOUNDING_PROBLEM: How to create a shared spoken language for a population arriving from dozens of linguistic backgrounds, in a territory where no common vernacular existed, to serve as the operating system for a new state.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Zionist leadership's own documents (Ben-Gurion, Jabotinsky, Szold) and by contemporary linguists (Harshav, Fellman) who note the absence of a spoken Hebrew base. The *status* as 'live' is corroborated by Israeli linguists (e.g., Ghil'ad Zuckermann's 'revivalistics' framework) who argue the revival is ongoing and incomplete, and by the Academy of the Hebrew Language's continued lexical expansion mandate. No corroboration from outside the beneficiary set for the claim that the problem *requires* the 'native generation only' vitality criterion — that criterion is the constraint itself, not the founding problem's necessary solution.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).
:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the constraint's coordination function (shared vernacular for state institutions) is real and substantial, but the 'only native generation counts' criterion extracts recognition and authority from liturgical and diaspora Hebrew users. Suppression (0.68) is high: the constraint requires active enforcement through education monopoly, state language policy, and the delegitimization of alternative Hebrew ecologies. Theater ratio (0.28) reflects that the coordination function is genuine but increasingly performative — Hebrew's vitality is now self-sustaining, yet the criterion persists as a boundary marker. Accessibility collapse (0.62) is moderately high: once you accept 'native daily use = vitality,' alternative frameworks (liturgical continuity, diaspora creativity) become nearly unintelligible as 'vitality.' Resistance (0.58) is significant: religious communities, diaspora scholars, and revisionist linguists have contested the criterion continuously.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (Zionist project), the constraint is a successful rope: it solved a coordination problem with manageable overhead. From the payer seats (liturgical tradition, traditional communities), it is a snare: their language was taken and redefined without consent, and the 'vitality' criterion delegitimizes their continuing relationship to Hebrew. The observer seat sees a tangled rope — genuine coordination with asymmetric extraction. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project is the primary beneficiary (d ≈ 0.15): it gains a nation-building infrastructure, legitimacy, and a uniqueness claim. The Hebrew education establishment and secular intelligentsia are secondary beneficiaries (d ≈ 0.25–0.35): they gain professional authority and cultural capital. Liturgical tradition continuity is the primary victim (d ≈ 0.85): its sacred language is repurposed, its authority displaced, and its usage framed as 'preservation.' Traditional religious communities (d ≈ 0.8) are trapped — they must use the language but on alien terms. Diaspora scholarship (d ≈ 0.6) is constrained — they participate but on Israeli-normative terms. Mizrahi communities (d ≈ 0.7) are excluded from authorship of the revived language.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a shared vernacular for state-building) remains live in the sense that Hebrew continues to need lexical expansion for new domains (technology, science, law). But the *criterion* 'only native generation constitutes vitality' has outlived its founding function: Hebrew is now natively spoken by millions; the criterion now serves to police boundaries (who counts as a legitimate Hebrew speaker, what counts as legitimate Hebrew usage) rather than to drive revival. This is mandatrophy: the mandate (revival) succeeded, but the constraint (the vitality criterion) persists and extracts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_naturalness,
    'Is ''native daily generation'' a structurally necessary criterion for language vitality, or a constructed boundary that serves the Zionist state-building project''s legitimacy?',
    'Comparative analysis of other language revivals (Welsh, Māori, Irish) — do they adopt the same criterion, or do they recognize liturgical/scholarly continuity as vitality? If the criterion is unique to Hebrew revival, it is more likely constructed.',
    'If constructed, the constraint is a false summit candidate (claims mountain-like naturalness for a criterion that serves identifiable beneficiaries). If structurally necessary, the extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_criterion_naturalness, conceptual, 'Whether the vitality criterion is a natural law of sociolinguistics or a political construction.').

omega_variable(
    liturgical_substrate_necessity,
    'Was the liturgical continuity of Hebrew a necessary substrate for the vernacular revival, or was the revival a de novo construction that could have used any lexical base?',
    'Historical linguistics: analyze the proportion of revived Hebrew''s core grammar and lexicon that derives directly from liturgical/textual Hebrew vs. neologisms. If the substrate was necessary, the liturgical reading''s claim to partial vitality gains structural weight.',
    'If the substrate was necessary, the native_daily_reading''s extraction from liturgical tradition is higher (it builds on what it then delegitimizes). If not necessary, the extraction is lower but the coordination claim is weaker.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_substrate_necessity, empirical, 'Whether the revival depended on the liturgical tradition it now frames as ''preservation not life''.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative Hebrew ecologies (liturgical, diaspora, Mizrahi) primarily structural (state policy, education monopoly) or internalized (communities accepting the ''native only'' criterion as self-evident)?',
    'Post-exit trajectory analysis: when communities gain institutional autonomy (e.g., Haredi education systems, diaspora Hebrew programs), do they adopt the ''native only'' criterion or develop alternative vitality frameworks?',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the criterion has been absorbed into the self-understanding of its victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of alternative Hebrew vitalities.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''Hebrew vitality'' admit a single coherent framing, or do the three readings (native_daily, liturgical, hybrid_continuity) represent genuinely distinct kernels that have been forced into a single label?',
    'Test whether each reading''s ε, beneficiary/victim structure, and coordination function are stable under its own operationalization. If they decompose into separate constraints with different types, the kernel label is a conflation.',
    'If the kernel fractures, this reading is one of several independent constraints, not a reading of a shared kernel. The network.affects_constraints links would become the primary structural relationship.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel_id ''hebrew_vitality'' names one contested commitment or a conflation of distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1881, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1881, hebrew_vitality__native_daily_reading, theater_ratio, 1881, 0.05).
narrative_ontology:measurement(hebr_tr_t1904, hebrew_vitality__native_daily_reading, theater_ratio, 1904, 0.12).
narrative_ontology:measurement(hebr_tr_t1917, hebrew_vitality__native_daily_reading, theater_ratio, 1917, 0.18).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_vitality__native_daily_reading, theater_ratio, 1948, 0.35).
narrative_ontology:measurement(hebr_tr_t1967, hebrew_vitality__native_daily_reading, theater_ratio, 1967, 0.38).
narrative_ontology:measurement(hebr_tr_t1990, hebrew_vitality__native_daily_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_vitality__native_daily_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1881, hebrew_vitality__native_daily_reading, base_extractiveness, 1881, 0.15).
narrative_ontology:measurement(hebr_be_t1904, hebrew_vitality__native_daily_reading, base_extractiveness, 1904, 0.22).
narrative_ontology:measurement(hebr_be_t1917, hebrew_vitality__native_daily_reading, base_extractiveness, 1917, 0.31).
narrative_ontology:measurement(hebr_be_t1948, hebrew_vitality__native_daily_reading, base_extractiveness, 1948, 0.48).
narrative_ontology:measurement(hebr_be_t1967, hebrew_vitality__native_daily_reading, base_extractiveness, 1967, 0.52).
narrative_ontology:measurement(hebr_be_t1990, hebrew_vitality__native_daily_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(hebr_be_t2024, hebrew_vitality__native_daily_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1881, hebrew_vitality__native_daily_reading, suppression_requirement, 1881, 0.25).
narrative_ontology:measurement(hebr_su_t1904, hebrew_vitality__native_daily_reading, suppression_requirement, 1904, 0.38).
narrative_ontology:measurement(hebr_su_t1917, hebrew_vitality__native_daily_reading, suppression_requirement, 1917, 0.52).
narrative_ontology:measurement(hebr_su_t1948, hebrew_vitality__native_daily_reading, suppression_requirement, 1948, 0.78).
narrative_ontology:measurement(hebr_su_t1967, hebrew_vitality__native_daily_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(hebr_su_t1990, hebrew_vitality__native_daily_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(hebr_su_t2024, hebrew_vitality__native_daily_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__native_daily_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_academy_lexical_authority).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, israeli_language_education_policy).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, diaspora_hebrew_institutional_recognition).

% DUAL FORMULATION NOTE:
% This constraint (native_daily_reading) and its two siblings (liturgical_reading, hybrid_continuity_reading) form a constraint family decomposing the 'Hebrew vitality' kernel. Each reading has distinct ε, beneficiaries, victims, and claimed types. The native_daily_reading (this story) claims tangled_rope with moderate ε=0.42, beneficiaries=Zionist project, victims=liturgical tradition. The liturgical_reading would claim mountain (ε≈0) with no beneficiaries/victims. The hybrid_continuity_reading would claim rope (ε≈0.15) with beneficiaries=both revival and tradition, victims=minimal. The ε values differ by a wide margin — they are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_vitality__native_daily_reading, organized, 0.75).
constraint_indexing:directionality_override(hebrew_vitality__native_daily_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
