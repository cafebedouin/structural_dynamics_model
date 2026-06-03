% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Hebrew Linguistic Life: The Liturgical Preservation Reading
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel 'Hebrew
 *   linguistic life.' The liturgical_preservation_reading claims that a
 *   language is alive when its sacred texts are continuously recited,
 *   studied, and transmitted in unbroken chain regardless of whether any
 *   community uses it as a vernacular medium. This reading emerged from
 *   post-exilic Jewish theology and persists in Orthodox institutional
 *   frameworks. It directly contradicts the native_generational_reading (a
 *   language is alive only when children acquire it as mother tongue) and
 *   challenges the marketplace_pidgin_reading (a language is alive when it
 *   functions as inter-communal coordination medium). The three readings are
 *   not empirical disagreements about observable facts but competing
 *   commitments about what criterion constitutes 'linguistic life.' The
 *   liturgical reading denies that Hebrew ever died — it claims continuous
 *   transmission through sacred study — and therefore frames the Ben-Yehuda
 *   revival project not as resurrection but as desecration. The sacred
 *   tradition (the continuous chain of liturgical recitation and rabbinic
 *   interpretation) becomes the victim set, threatened by vernacular
 *   corruption. Simultaneously, the constraint coordinates genuine
 *   transmission of religious meaning and cultural identity across diaspora
 *   communities, making it a tangled rope rather than pure snare. The
 *   extractiveness measurement trajectory shows rising extraction pressure as
 *   vernacular Hebrew expanded (1880–1948) — the liturgical standard became
 *   increasingly a gatekeeping mechanism as it competed with a living
 *   language — and then moderate decline (1948–2026) as the state legitimized
 *   both criteria and allowed them to coexist in institutional practice.
 *
 * KEY AGENTS:
 *   - Rabbinic Interpretive Tradition: Primary beneficiary (institutional/arbitrage) — controls epistemic authority and social legitimacy through monopoly on authoritative textual interpretation
 *   - Orthodox Jewish Institutions: Secondary beneficiary (institutional/constrained) — depend on liturgical continuity for institutional reproduction and communal coherence
 *   - Sacred Textual Integrity: Primary victim (powerless/trapped) — abstract commitment to preserving canonical form; suffers from definitional instability about what counts as preservation
 *   - Vernacular Hebrew Communities: Secondary victim (moderate/identity_locked) — speakers are identity-locked to Hebrew but structurally excluded from legitimacy criteria that define the language as 'alive'
 *   - Modern Hebrew Revivalists: Organized antagonist (organized/constrained) — working to expand Hebrew functionality while delegitimized by the constraint's frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.38).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.62).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Linguistic Life: The Liturgical Preservation Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, 'b6f93526-a233-4181-909b-02370a3c1ebd').
narrative_ontology:cs_kernel_codification('b6f93526-a233-4181-909b-02370a3c1ebd', fixed_text).
narrative_ontology:cs_authority_grounding('b6f93526-a233-4181-909b-02370a3c1ebd', lineage).
narrative_ontology:cs_interpretation_layer_present('b6f93526-a233-4181-909b-02370a3c1ebd').
narrative_ontology:cs_reading_relation('b6f93526-a233-4181-909b-02370a3c1ebd', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('b6f93526-a233-4181-909b-02370a3c1ebd', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('b6f93526-a233-4181-909b-02370a3c1ebd', foundational, textual_continuity_sufficient_for_life).
narrative_ontology:cs_axiom_status(textual_continuity_sufficient_for_life, holdable).
narrative_ontology:cs_axiom_grounding('b6f93526-a233-4181-909b-02370a3c1ebd', textual_continuity_sufficient_for_life, theological).
narrative_ontology:cs_axiom('b6f93526-a233-4181-909b-02370a3c1ebd', foundational, hebrew_never_died_only_transformed).
narrative_ontology:cs_axiom_status(hebrew_never_died_only_transformed, holdable).
narrative_ontology:cs_axiom_grounding('b6f93526-a233-4181-909b-02370a3c1ebd', hebrew_never_died_only_transformed, deontological).
narrative_ontology:cs_reference_frame('b6f93526-a233-4181-909b-02370a3c1ebd', continuous_rabbinic_textual_transmission).
narrative_ontology:cs_drift_state('b6f93526-a233-4181-909b-02370a3c1ebd', contemporary_post_1948_israeli_vernacularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b6f93526-a233-4181-909b-02370a3c1ebd', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_interpretive_tradition).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, orthodox_jewish_institutions).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_textual_integrity).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, vernacular_hebrew_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VERNACULAR HEBREW SPEAKER (SNARE) — Identity-locked to Hebrew as mother tongue and national language, yet structurally barred from full participation in the constraint's legitimacy criteria. A Sabra child speaks Hebrew daily for all functions (the native_generational_reading's criterion), but this very vernacular use is read by the liturgical preservation reading as a degradation or desecration of the sacred language. The speaker is trapped between two incompatible definitions of linguistic life: theirs is alive by one criterion (native use) but dead or corrupted by another (non-liturgical). Cannot exit without abandoning identity; the constraint extracts the cost of this definitional instability without offering coherence.
constraint_indexing:constraint_classification(hebrew_linguistic_life__liturgical_preservation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: JEWISH DIASPORA COMMUNITY (TANGLED ROPE) — Experiences genuine coordination function: the liturgical continuity provides intergenerational cohesion, identity stability, and transmission mechanism for religious and cultural meaning. Hebrew prayerbooks enable worship across linguistic boundaries. Yet the same mechanism extracts a cost: diaspora members who don't read Classical Hebrew fluently bear a barrier to full participatory access. The liturgical standard creates both belonging and exclusion. Constrained exit: leaving the tradition requires abandoning religious identity and community ties, not merely changing linguistic practice.
constraint_indexing:constraint_classification(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RABBINIC INTERPRETIVE TRADITION (ROPE) — Primary beneficiary (institutional/arbitrage). The liturgical preservation criterion is the institutional mechanism through which rabbinic authority is reproduced. As long as the sacred texts are studied continuously in the tradition of commentary and interpretation, the rabbinic class retains epistemic and social authority. The constraint solves a coordination problem from the tradition's perspective: maintaining textual continuity across diaspora, ensuring that authoritative interpretation remains anchored in the canonical texts. The tradition experiences the constraint as pure coordination — the liturgical chain IS their institutional function. Net beneficiary.
constraint_indexing:constraint_classification(hebrew_linguistic_life__liturgical_preservation_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODERN HEBREW REVIVALIST MOVEMENT (TANGLED ROPE) — Organized actors (Ben-Yehuda, labor Zionists, kibbutz educators) coordinating the creation of a living vernacular. The liturgical preservation reading frames their project as desecration rather than revival — they are not resurrecting a dead language (the reading denies the language ever died) but corrupting a sacred one. The movement benefits from access to Hebrew literacy infrastructure and lexical depth but faces delegitimation from the institutional constraint. Constrained exit: cannot simply adopt another national language without abandoning Zionist ideology; must work within the linguistic field even as the field's canonical definition excludes them.
constraint_indexing:constraint_classification(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MODERN JEWISH STATE INSTITUTIONS (PITON) — The Israeli state apparatus maintains the liturgical preservation reading as an ideological backdrop even while operationally committed to vernacular Hebrew standardization. The state schools teach Modern Hebrew while reciting classical liturgical Hebrew in morning prayers; the state declares Hebrew revival while the constraint definition denies revival occurred. The theater_ratio is high: performing liturgical continuity while building vernacular institutions. The original function (preserving religious authority through unbroken textual transmission) has atrophied; the institution persists through inertia and ideological accommodation.
constraint_indexing:constraint_classification(hebrew_linguistic_life__liturgical_preservation_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LINGUISTIC ESSENCE VIEW (MOUNTAIN) — At the civilizational/universal scope, this perspective reads the liturgical preservation criterion as capturing a fundamental truth about language: the only language that truly persists is one whose texts are recited and studied in unbroken chain, because that is the material mechanism through which linguistic form is preserved independent of social change. This is presented as a natural law of how languages persist. However, the structural data (beneficiaries, active enforcement, suppression of vernacular criteria) reveals this as a false summit — the 'essence of linguistic life' is a normative claim about what should count as life, not a descriptive claim about linguistic persistence mechanisms.
constraint_indexing:constraint_classification(hebrew_linguistic_life__liturgical_preservation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_linguistic_life__liturgical_preservation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_linguistic_life__liturgical_preservation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, TR),
    TR >= 0.70.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The liturgical preservation reading benefits the rabbinic tradition and Orthodox institutions by granting them monopoly over what counts as authentic Hebrew and legitimate linguistic authority. However, extraction is not total because: (a) the constraint genuinely coordinates intergenerational transmission of religious meaning; (b) the native_generational_reading has gained social and state legitimacy, limiting the liturgical reading's extractive reach; (c) vernacular speakers now vastly outnumber liturgically-trained speakers, reducing the extraction mechanism's enforcement capacity. The ε trajectory shows rising extraction during the revivalist era (when the liturgical standard competed most sharply with vernacular emergence) and declining extraction as state institutions absorbed both criteria. Suppression (0.62): Moderate-high. The constraint suppresses alternative definitions of linguistic life by framing them as degradation or ignorance ('the language is already alive through liturgy — your project is desecration, not revival'). This blocks consideration of the marketplace_pidgin criterion (functionality) and directly contradicts the native_generational criterion (mother-tongue acquisition). The suppression is structural (embedded in institutional authority) and internalized (many non-Orthodox Jews accept the liturgical criterion as a measure of cultural authenticity even while speaking vernacular Hebrew daily). Theater ratio (0.68): High. The modern Israeli state enacts liturgical continuity performatively — morning prayers in schools, Hebrew language law naming classical texts as foundational — while operationally building a vernacular system. The constraint's theatrical content has declined from its peak (1948) as the state normalized both criteria, but remains substantial because the ritual performance of 'linguistic continuity' serves legitimacy functions separate from the actual linguistic transmission mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a stark perspectival gap between the beneficiaries (rabbinic tradition sees rope: pure coordination) and the victims (vernacular speakers see snare: identity-locked exclusion). The beneficiary perspective experiences the constraint as solving the legitimate problem of maintaining religious and cultural continuity across diaspora separation. The victim perspective experiences it as an impossible dual bind: Hebrew is alive only if you study sacred texts in a language you don't speak functionally; if you use Hebrew for daily life, you are corrupting it. The piton perspective (state institutions) is crucial: the state has absorbed both the liturgical and native-generational criteria, maintaining the liturgical reading as ideological backdrop while operationally committing to vernacular expansion. This dual commitment degrades the constraint's original function (preserving rabbinic authority) into performance. The analytical observer risks seeing a natural law ('languages need sacred texts to survive') but the structural data reveals this as a false summit: the criterion is a normative claim grounded in rabbinic institutional interests, not a descriptive fact about how languages persist.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic tradition experiences low d (0.15–0.20) because they are net beneficiaries with arbitrage options — they can maintain their interpretive authority regardless of how many people speak Hebrew vernacularly, and they retain access to textual study whether or not it becomes a popular practice. The vernacular speakers experience high d (0.75–0.85) because they are victims with identity-locked exit: they are constitutionally bound to Hebrew identity but structurally excluded from the constraint's legitimacy criteria. A secular Israeli Sabra child speaks Hebrew for all daily functions (satisfying the native_generational criterion) but is read as linguistically ignorant or corrupting by the liturgical standard — they cannot exit without abandoning their linguistic and national identity, yet the identity they possess is delegitimated by the constraint. The diaspora community experiences moderate d (0.55–0.65) because the constraint both coordinates genuine transmission (beneficiary function) and extracts barriers to participation (victim cost) — they benefit from the liturgical continuity but at the cost of dependency on trained specialists for access to the canonical texts.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING EXEMPLAR: The mandatrophy in this constraint is resolved by recognizing that all three readings are held by different stakeholders with different structural relationships. The liturgical reading is not 'wrong' — it genuinely describes a mechanism through which Hebrew cultural and religious identity persisted across diaspora and persecution. The native_generational reading is not 'wrong' — it genuinely describes the mechanism through which Hebrew became a living spoken language and national medium. The marketplace_pidgin reading is not 'wrong' — it describes how Hebrew functions as a lingua franca in multilingual Israeli contexts. But the three readings are incompatible as definitions of a single criterion ('linguistic life'). They cannot all be simultaneously true of the same language at the same moment in the same institution. The constraint resolves the mandatrophy by showing that the kernel itself (what makes a language alive?) is under-determined by language structure. The Hebrew language objectively satisfies all three criteria. But because the criteria are grounded in competing institutional and value commitments (rabbinic authority, national identity, functional communication), different communities will declare which criterion is authoritative. The liturgical_preservation_reading is a tangled rope because it genuinely coordinates meaningful transmission (rope function) while also extracting institutional authority and delegitimating competing criteria (extraction function). Its classification remains stable only because the state apparatus now absorbs both the liturgical and native-generational criteria, preventing either reading from achieving total dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_continuity_vs_functional_vitality,
    'Does a language count as ''alive'' when its canonical texts are continuously studied by a small scholarly/clerical class but are not functionally used by any community for daily communication?',
    'Comparative analysis: Latin (continuously transmitted in Church and academia, zero functional vernacular speakers) vs. Hebrew (continuously transmitted in liturgy, now with millions of functional speakers) vs. ancient Egyptian (transmission broken, now reconstructed from texts). Do these form a coherent category or structurally distinct cases?',
    'If textual continuity alone suffices: liturgical preservation reading is robust. If functional vitality is also required: the reading misclassifies linguistic death as life. The native_generational_reading becomes definitionally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_continuity_vs_functional_vitality, conceptual, 'Whether textual continuity alone constitutes linguistic life').

omega_variable(
    desecration_vs_evolution_framing,
    'Is the transition from liturgical-only to vernacular-functional Hebrew best understood as desecration of the sacred (liturgical reading frame) or as natural linguistic evolution and democratization (native_generational reading frame)?',
    'Historical analysis of Hebrew speakers'' own self-descriptions 1880-1920: did revivalists frame their project as restoration vs. innovation vs. desecration? Analysis of rabbinic responses: authentic resistance grounded in liturgical theology vs. institutional defense of interpretive monopoly?',
    'If desecration frame is accurate: the liturgical preservation reading''s victim set (sacred tradition) is correctly identified. If evolution frame is accurate: the victim set is actually the modernization of the language, and the liturgical reading represents institutional resistance to social change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(desecration_vs_evolution_framing, empirical, 'Framing the transition as desecration vs. evolution').

omega_variable(
    competing_readings_kernel_structure,
    'Do the three readings (liturgical_preservation, native_generational, marketplace_pidgin) represent genuinely incompatible definitions of linguistic life, or do they describe complementary dimensions of a single linguistic system?',
    'Formal analysis: can a language satisfy all three criteria simultaneously? (Hebrew arguably does: liturgically preserved + natively acquired + functionally coordinating). If yes, the three readings should coexist; if no, they foreclose each other. Sociological analysis: do communities actually stake competing truth-claims about which reading is correct, or do they simply emphasize different criteria for different purposes?',
    'If incompatible: the readings are in genuine logical conflict, and at least one reading will be empirically falsified or epistemically displaced. If complementary: the three readings form a perspectival system, and the kernel itself is under-determined by language structure alone (under-determination rooted in competing value commitments, not epistemic uncertainty).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_readings_kernel_structure, conceptual, 'Whether the three readings are incompatible or complementary').

omega_variable(
    authority_erosion_in_liturgical_transmission,
    'Is the ability of non-specialists (secular Israeli speakers, non-Orthodox Jews, linguists, students) to study and interpret Hebrew sacred texts currently eroding the gatekeeping authority of the rabbinic tradition, or is the tradition maintaining interpretive monopoly despite broader textual access?',
    'Institutional analysis: what fraction of Hebrew biblical scholarship is conducted outside rabbinic institutions (universities, secular academies, non-Orthodox seminaries)? Do these alternative interpretations carry weight in Jewish communal decision-making, or are they epistemically marginalized?',
    'If authority is eroding: the suppression gate will eventually fall, and the constraint will degrade from tangled_rope toward rope (extraction mechanism weakens). If authority persists: the constraint remains stable even with broad textual access, because the rabbinic frame controls what counts as legitimate interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_erosion_in_liturgical_transmission, empirical, 'Whether rabbinic interpretive monopoly is erosion under broader textual access').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_liturg_theater_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(heb_liturg_theater_t1880, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1880, 0.68).
narrative_ontology:measurement(heb_liturg_theater_t1948, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1948, 0.72).
narrative_ontology:measurement(heb_liturg_theater_t2026, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2026, 0.68).

% Extraction over time
narrative_ontology:measurement(heb_liturg_extract_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(heb_liturg_extract_t1880, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement(heb_liturg_extract_t1948, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(heb_liturg_extract_t2026, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(heb_liturg_suppress_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(heb_liturg_suppress_t1880, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1880, 0.7).
narrative_ontology:measurement(heb_liturg_suppress_t1948, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(heb_liturg_suppress_t2026, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, ben_yehuda_revival_desecration_frame).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_jewish_institutional_reproduction).

% DUAL FORMULATION NOTE:
% The contested kernel hebrew_linguistic_life decomposes into three structurally distinct constraint stories (liturgical_preservation_reading, native_generational_reading, marketplace_pidgin_reading) because each reading instantiates a different criterion for 'linguistic life' that produces different ε values, different victim/beneficiary sets, and different classifications. The liturgical_preservation_reading has ε=0.38 (tangled_rope); the native_generational_reading has ε=0.52 (snare, from the liturgical_preservation reading's perspective); the marketplace_pidgin_reading has ε=0.25 (rope). These are not the same constraint viewed from different angles — they are competing institutional frameworks making incompatible truth claims about what criterion constitutes linguistic aliveness. All three must be linked in the network to show the full structure of the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__liturgical_preservation_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
