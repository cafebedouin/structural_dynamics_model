% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Living Language Status — Literary Continuity Reading
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   The literary-continuity reading of living language status defines a
 *   language as alive if it serves as a productive medium for new literary
 *   and intellectual work, regardless of whether native speakers transmit it
 *   generationally. This reading emerged during the Haskalah (Jewish
 *   Enlightenment, 18th–19th century) as maskilim (Hebrew intellectuals)
 *   revived Hebrew literature to claim cultural authority and validate
 *   Hebrew's status as a modern language. Haskalah periodicals, poetry, and
 *   philosophical works demonstrated that Hebrew could express contemporary
 *   ideas — a claim essential because Hebrew had been primarily a liturgical
 *   and scholarly language for over 2,000 years, with no native speaker
 *   community. The constraint is a Tangled Rope: it genuinely coordinates
 *   around a real problem (how to establish and legitimize a language's
 *   status when spoken transmission has been broken) while simultaneously
 *   extracting value from those excluded by the definition (illiterate
 *   speakers, oral tradition bearers, religious communities whose liturgical
 *   usage was reframed as 'preservation' rather than 'vitality'). The
 *   maskilim benefited from a definition that placed cultural authority in
 *   their hands — literary and intellectual work, which they controlled —
 *   rather than in mass adoption or native speaker transmission, which they
 *   could not achieve in the short term. This reading coexists with two
 *   sibling readings: the liturgical-preservation reading (language is living
 *   through sacred text and ritual continuity) and the native-generation
 *   reading (language is living only through mother-tongue transmission).
 *   Each reading reflects a different party's structural position and grounds
 *   legitimacy in different mechanisms of transmission.
 *
 * KEY AGENTS:
 *   - Maskilim and Secular Intellectuals: Institutional beneficiaries (institutional/arbitrage) — define language vitality through literary production they control; establish cultural authority without mass adoption
 *   - Illiterate and Non-Literary Speakers: Primary victims (powerless/trapped) — excluded from vitality definition; their living usage is rendered invisible or devalued
 *   - Oral Tradition Bearers and Communal Speakers: Secondary victims (moderate/constrained) — benefit from language continuation but face extraction: oral fluency is subordinated to literary metrics
 *   - Religious Communities and Liturgical Practitioners: Organized stakeholders (organized/constrained) — maintain language through liturgy; face subordination: their transmission is labeled 'preservation' not 'vitality'
 *   - Language Revitalization and Education Movements: Organized agents (organized/mobile) — see the literary-continuity criterion as temporary; building native acquisition pathways that render this definition obsolete
 *   - Academic Linguistics Institutions: Institutional inertia (institutional/arbitrage) — maintain prestige association with Haskalah definitions despite broader recognition that spoken transmission is primary vitality metric
 *   - Analytical Observer: Civilizational scale (analytical/analytical) — risks naturalizing a contingent Haskalah choice as an immutable property of language classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.28).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.42).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Living Language Status — Literary Continuity Reading").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(living_language_status__literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, '0886c506-0180-45a8-acc0-a08fd3851728').
narrative_ontology:cs_kernel_codification('0886c506-0180-45a8-acc0-a08fd3851728', fixed_text).
narrative_ontology:cs_authority_grounding('0886c506-0180-45a8-acc0-a08fd3851728', extraction).
narrative_ontology:cs_interpretation_layer_present('0886c506-0180-45a8-acc0-a08fd3851728').
narrative_ontology:cs_reading_relation('0886c506-0180-45a8-acc0-a08fd3851728', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('0886c506-0180-45a8-acc0-a08fd3851728', living_language_status__native_generation_reading, influences).
narrative_ontology:cs_axiom('0886c506-0180-45a8-acc0-a08fd3851728', foundational, literary_productivity_sufficient_for_vitality).
narrative_ontology:cs_axiom_status(literary_productivity_sufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('0886c506-0180-45a8-acc0-a08fd3851728', literary_productivity_sufficient_for_vitality, instrumental).
narrative_ontology:cs_axiom('0886c506-0180-45a8-acc0-a08fd3851728', secondary, literacy_as_primary_evidence_of_continuity).
narrative_ontology:cs_axiom_status(literacy_as_primary_evidence_of_continuity, holdable).
narrative_ontology:cs_axiom_grounding('0886c506-0180-45a8-acc0-a08fd3851728', literacy_as_primary_evidence_of_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('0886c506-0180-45a8-acc0-a08fd3851728', haskalah_literary_modernity).
narrative_ontology:cs_drift_state('0886c506-0180-45a8-acc0-a08fd3851728', contemporary_multilevel_vitality_metrics, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('0886c506-0180-45a8-acc0-a08fd3851728', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_intellectuals).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, secular_literati).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, hegemonic_cultural_authority).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_speakers).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, non_literary_communities).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, oral_tradition_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ILLITERATE/NON-LITERARY SPEAKER (SNARE) — Excluded from the definition of language vitality because they do not participate in literary production. Bears the extraction of being rendered invisible in the vitality metric while their actual living usage is devalued. No exit: cannot suddenly become literate elites to prove language vitality. Maximum extraction.
constraint_indexing:constraint_classification(living_language_status__literary_continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORAL TRADITION BEARER (TANGLED ROPE) — Benefits from language continuation and literary innovations that validate the language's status, but extraction occurs: their own fluency and usage patterns are devalued unless they participate in literary production. Constrained exit: can attempt to enter literary circles but faces high barriers (literacy, cultural capital, elite gatekeeping).
constraint_indexing:constraint_classification(living_language_status__literary_continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MASKILIM AND SECULAR INTELLECTUALS (ROPE) — Primary beneficiaries. Define language vitality through literary production, which they control. Coordinate around Haskalah periodicals and modern Hebrew literature as proof of status. Pure coordination function for this group: establishing cultural authority and legitimacy. Arbitrage option: can switch between languages or contexts to maintain status.
constraint_indexing:constraint_classification(living_language_status__literary_continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RELIGIOUS COMMUNITIES (TANGLED ROPE) — Organized groups who maintain the language through liturgical use and transmission. Benefit from literary validation (Haskalah provides intellectual prestige), but face extraction: their own liturgical practices are deemed 'preservation' rather than 'vitality,' subordinated to secular literary metrics. Constrained exit: religious practice is identity-bound; cannot easily switch to other languages for liturgy without identity rupture.
constraint_indexing:constraint_classification(living_language_status__literary_continuity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LANGUAGE REVITALIZATION MOVEMENTS (SCAFFOLD) — Organized agents (educators, revival activists, state language policy makers) who see the literary-continuity reading as a temporary framing with a sunset. Modern movements like immersion education, digital native communities, and grassroots revitalization create paths to native speaker transmission that render the literary-continuity definition obsolete. Mobile exit: these movements can redirect resources toward native acquisition if the constraint loses authority. Theater: moderate — some performative institutional validation of 'revitalization' claims.
constraint_indexing:constraint_classification(living_language_status__literary_continuity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC LINGUISTICS (PITON) — The literary-continuity criterion persists in academic and cultural institutions as a degraded classification system. Modern linguistics recognizes multiple vitality metrics (UNESCO, Ethnologue) that go far beyond literary production, but the prestige associated with Haskalah-era definitions remains institutionalized through inertia. The academic ritual of citing Hebrew literature as proof of vitality continues despite broader recognition that spoken transmission is the actual vitality criterion. Theater ratio high: much institutional performance around 'literary heritage' while functional definitions have shifted.
constraint_indexing:constraint_classification(living_language_status__literary_continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scale, one might frame this as an inherent feature of language classification: literary production IS inherently more documentable and preservable than oral usage, so languages with literary traditions inevitably appear more 'alive' to observers. This naturalizes the literary criterion as an immutable property of how we measure language status. However, false summit: the criterion is a contingent choice made by the Haskalah intellectuals, not a law of linguistics.
constraint_indexing:constraint_classification(living_language_status__literary_continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(living_language_status__literary_continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(living_language_status__literary_continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(living_language_status__literary_continuity_reading, TR),
    TR >= 0.70.

:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-Low. The literary-continuity reading produces real coordination — it solved a genuine problem for Hebrew speakers: how to establish legitimate cultural status without native transmission. The beneficiaries (maskilim) provided genuine intellectual work (periodicals, literature, philosophy) that sustained the language as a living medium of thought. However, extraction is moderate because the definition simultaneously excludes non-literary speakers and subordinates oral/liturgical transmission. The measured value reflects that this is a real coordination mechanism (not pure extraction) but with asymmetric benefit. The value increases slightly over the interval (0.18→0.32) as the literary-continuity reading becomes more institutionalized and alternative metrics (native transmission, liturgical continuity) become more marginalized. Suppression (0.42): Moderate. Alternative definitions of vitality are suppressed but not eliminated. Liturgical practitioners maintain their own transmission (not fully suppressed), and oral communities continue speaking despite not being counted in the literary vitality metric. The suppression is primarily epistemic: a hierarchy of what counts as evidence of vitality. Suppression remains stable across the interval because the literary definition becomes institutionalized while alternatives persist in parallel. Theater Ratio (0.58): Moderate-High. The literary-continuity reading involves substantial performative content: the Haskalah was partly a legitimacy project (establishing Hebrew's modern status) alongside genuine intellectual production. The publication of periodicals and literature served both functional (expression of contemporary thought) and performative (demonstration that Hebrew can be modern) purposes. Theater increases slightly over the interval as the literary definition becomes more institutionalized and assumes a ritualized quality in academic and cultural discourse.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a sharp perspectival gap between beneficiaries and victims. The maskilim see Rope: they are solving a genuine coordination problem (establishing linguistic legitimacy through literature) and experience the constraint as purely functional. Religious communities see Tangled Rope: they benefit from the language's elevated status but face extraction because their own liturgical practices are subordinated to secular literary metrics. Illiterate or non-literary speakers see Snare: they are rendered invisible and their living usage devalued. Revitalization movements see Scaffold: the literary criterion is temporary, and modern native-acquisition approaches will make it obsolete. Academic institutions see Piton: the literary definition persists through inertia despite broader recognition of other vitality metrics. The analytical observer from the civilizational scale risks seeing Mountain (naturalizing the literary criterion as an immutable property of language), but the false summit detector identifies this as a contingent Haskalah choice, not a law of linguistics. The gap reveals that 'language vitality' is not an objective fact but a social definition with distributive consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value derives from the agent's structural position relative to the constraint. Beneficiaries (maskilim/intellectuals) with arbitrage options experience low or negative effective extraction — they control the definition and benefit from it. Victims (illiterate speakers, oral bearers) with no exit experience maximum extraction. Religious communities with organized but constrained exit experience moderate extraction — they can maintain liturgical practice but face subordination of that practice to secular literary metrics. Revitalization movements with organized and mobile exit experience lower extraction — they have agency and alternatives (native acquisition pathways). Academic institutions with arbitrage options experience low extraction in the form of prestige-maintenance benefits. The piton classification reflects high theater ratio (performative institutional maintenance of an outdated definition) rather than high extraction. The false summit classification at civilizational scale reflects that this constraint's apparent universality is actually a contingent observational choice by historically-specific actors (maskilim intellectuals).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literary_production_as_vitality_definition,
    'Is literary productivity a necessary and sufficient criterion for language vitality, or is it one contingent metric among many?',
    'Historical comparison: contrast the Haskalah definition with modern UNESCO vitality criteria (intergenerational transmission, language domains, speaker attitudes). Examine whether languages with high literary production but low native speaker transmission are classified as vital by contemporary standards.',
    'If literary production is contingent and non-necessary: this constraint collapses toward Rope (pure coordination) and the false summit detector fires. If literary production is accepted as sufficient even without native transmission: tangled_rope classification holds; the constraint persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literary_production_as_vitality_definition, conceptual, 'Whether literary productivity is necessary/sufficient for vitality or contingent metric').

omega_variable(
    maskilim_gatekeeping_mechanism,
    'To what degree does the literary-continuity reading function as a mechanism for maskilim cultural gatekeeping, and to what degree is it a genuine coordination solution to the preservation problem?',
    'Archival analysis of Haskalah periodicals and Hebrew literature production: who authored, who had access, who was excluded? Comparative analysis with liturgical and oral transmission patterns that persisted in parallel. Examine whether literary production was the primary mechanism maintaining Hebrew status or whether it was a legitimacy narrative built atop existing liturgical transmission.',
    'If primarily gatekeeping (high maskilim exclusivity, low alternative transmission): constraint reclassifies as Snare from broader perspective. If genuine coordination: Tangled Rope classification holds but requires acknowledging the asymmetry more explicitly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maskilim_gatekeeping_mechanism, empirical, 'Degree to which literary-continuity reading functions as maskilim gatekeeping vs genuine coordination').

omega_variable(
    kernel_reading_logical_relationship,
    'What is the logical relationship between this reading and the native_generation_reading? Do they represent genuinely distinct definitions of vitality that can coexist, or does one logically foreclose the other?',
    'Analyze whether a language can simultaneously satisfy both criteria (literary vitality AND native generational transmission). Examine historical cases: Hebrew itself (literary vitality without native transmission until 20th century), Icelandic (literary vitality WITH native transmission), Latin (literary vitality without native transmission). Identify whether rejecting this reading requires accepting native transmission as necessary, or whether both criteria can be held by different observational frameworks.',
    'If they foreclose each other: reading_relations should use ''forecloses'' (rare). If they coexist as different parties'' readings: ''coexists_with'' (expected). If this reading structurally undercuts the native reading''s conditions: ''influences''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_logical_relationship, conceptual, 'Logical relationship between literary_continuity and native_generation readings of living_language_status').

omega_variable(
    observer_dependent_vitality_metrics,
    'Does the Haskalah definition of vitality reflect genuine structural properties of Hebrew''s status, or does it reflect the observational position of the maskilim (urban, literate, intellectuals) who could not access or measure rural, oral, or liturgical transmission patterns?',
    'Historical reconstruction: examine 18th-19th century infrastructure for documenting vs measuring: (a) Haskalah literary production (highly visible, publishable, archivable); (b) liturgical Hebrew usage (visible but not modern-prose literature); (c) colloquial Hebrew or Yiddish-Hebrew code-switching in non-literate communities (largely invisible to literate observers). Assess whether the literary definition emerged from actual measurement capacity vs ideological preference.',
    'If observer-dependent: the constraint instantiates what the prompt calls ''false summit detection'' — naturalizing a contingent observation position as a universal vitality criterion. Supports reclassification as Snare (extraction via observation bias) or toward revealing the Tangled Rope''s asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_dependent_vitality_metrics, empirical, 'Whether vitality definition reflects structural properties or observational position of maskilim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 1750, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(llslcr_theater_1760, living_language_status__literary_continuity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(llslcr_theater_1810, living_language_status__literary_continuity_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(llslcr_theater_1860, living_language_status__literary_continuity_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(llslcr_extractiveness_1760, living_language_status__literary_continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(llslcr_extractiveness_1810, living_language_status__literary_continuity_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(llslcr_extractiveness_1860, living_language_status__literary_continuity_reading, base_extractiveness, 100, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(llslcr_suppression_1760, living_language_status__literary_continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(llslcr_suppression_1810, living_language_status__literary_continuity_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(llslcr_suppression_1860, living_language_status__literary_continuity_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, hebrew_revitalization_state_enforcement).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, linguistic_gatekeeping_elite_definition).

% DUAL FORMULATION NOTE:
% The living_language_status kernel decomposes into three constraint stories (three readings), each with structurally distinct epsilon and beneficiary/victim profiles. This story (literary_continuity_reading) has epsilon=0.28 (moderate coordination with moderate extraction). The sibling readings will have different epsilon values reflecting different mechanisms of transmission (liturgical_preservation ≈ 0.12, rope; native_generation ≈ 0.35, tangled_rope with different beneficiaries). The three stories are linked bidirectionally via network.affects_constraints because the acceptance or dominance of one reading structurally affects the others' legitimacy and institutional standing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__literary_continuity_reading, institutional, 0.25).
constraint_indexing:directionality_override(living_language_status__literary_continuity_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
