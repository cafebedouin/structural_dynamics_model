% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Hebrew as Living Language via Haskalah Literary Generativity
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the literary-revival reading of the contested
 *   'Hebrew living language' kernel: the claim that Hebrew remained a living
 *   language through the Haskalah period (roughly 1770s–1880s) because
 *   maskilic writers produced genuinely generative literary Hebrew — new
 *   sentences, new registers, new secular content — even though almost no one
 *   spoke Hebrew as a daily vernacular. This reading treats written
 *   generative competence as sufficient evidence of life, independent of oral
 *   daily use. It sits between two sibling readings: the
 *   liturgical_continuity_reading, which locates Hebrew's life in unbroken
 *   ritual recitation regardless of generativity, and the
 *   native_generation_reading, which insists life requires native daily
 *   speech production, treating literary output (however generative) as
 *   insufficient. The three readings share the historical kernel — Hebrew's
 *   textual chain was never fully broken — but disagree on what property of
 *   that chain constitutes 'life.'
 *
 * KEY AGENTS:
 *   - haskalah_literary_circle: primary agenda-setter and beneficiary — produces the literary corpus that is the evidentiary basis of this reading
 *   - hebrew_periodical_readership: secondary beneficiary — consumes and normalizes the literary register without speaking it
 *   - zionist_cultural_nationalists: downstream beneficiary — inherits and redeploys the corpus as continuity evidence for the later native-speech revival
 *   - traditional_yiddish_vernacular_speakers: excluded — the actual daily-language majority, structurally outside this literate debate
 *   - linguistic_historians: analytical observer — assesses the boundary conditions of 'living language' comparatively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.08).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.05).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew as Living Language via Haskalah Literary Generativity").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '40a6f73b-619e-42db-9790-377a70352bed').
narrative_ontology:cs_kernel_codification('40a6f73b-619e-42db-9790-377a70352bed', distributed).
narrative_ontology:cs_authority_grounding('40a6f73b-619e-42db-9790-377a70352bed', practice).
narrative_ontology:cs_reading_relation('40a6f73b-619e-42db-9790-377a70352bed', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('40a6f73b-619e-42db-9790-377a70352bed', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('40a6f73b-619e-42db-9790-377a70352bed', foundational, generative_literary_output_constitutes_vitality).
narrative_ontology:cs_axiom_status(generative_literary_output_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('40a6f73b-619e-42db-9790-377a70352bed', generative_literary_output_constitutes_vitality, conventional).
narrative_ontology:cs_axiom('40a6f73b-619e-42db-9790-377a70352bed', secondary, daily_oral_use_not_necessary_for_life_status).
narrative_ontology:cs_axiom_status(daily_oral_use_not_necessary_for_life_status, holdable).
narrative_ontology:cs_axiom_grounding('40a6f73b-619e-42db-9790-377a70352bed', daily_oral_use_not_necessary_for_life_status, conventional).
narrative_ontology:cs_reference_frame('40a6f73b-619e-42db-9790-377a70352bed', maskilic_literary_generativity_standard).
narrative_ontology:cs_drift_state('40a6f73b-619e-42db-9790-377a70352bed', post_native_revival_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('40a6f73b-619e-42db-9790-377a70352bed', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_literary_circle).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_periodical_readership).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, zionist_cultural_nationalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maskilim writers across late 18th and 19th century Central and Eastern Europe compose original poetry, novels, essays, and periodicals in Hebrew, demonstrating that the language can generate new sentences never spoken or recited before, on secular subjects — nature, romance, politics, science — outside the liturgical register. They set the terms of what counts as evidence for Hebrew's vitality (generative literary output) because they are the ones producing it, and they benefit from that framing being accepted since it validates their own project.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_literary_circle, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, haskalah_literary_circle, beneficiary).

% Educated Jewish readers across Europe consume Haskalah-era Hebrew journals, novels, and poetry as their primary access to secular Hebrew culture. They gain a shared literary vernacular for modern ideas without needing to speak Hebrew daily; their engagement is entirely through reading and occasional composition, never conversational fluency.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_periodical_readership, beneficiary,
    moderate, biographical, mobile, continental).

% Later nationalist movements draw on the Haskalah's demonstrated literary corpus as proof that Hebrew was never truly dead and could be built upon, citing this reading to legitimate subsequent revival projects. They benefit from the literary-revival reading being treated as sufficient continuity, since it shortens the distance their own project has to bridge.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, zionist_cultural_nationalists, beneficiary,
    organized, generational, mobile, continental).

% The vast majority of Ashkenazi Jews in this period speak Yiddish as their daily vernacular and have no stake in whether Hebrew literary output counts as 'living language' — the debate occurs entirely within a literate elite and does not touch their linguistic life. If asked, some would object that calling Hebrew 'alive' obscures that it was not anyone's mother tongue or street language at the time.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, traditional_yiddish_vernacular_speakers, excluded,
    powerless, biographical, constrained, regional).

% Scholars of language revitalization assess whether written generative competence without a native speech community satisfies criteria for a 'living language,' comparing Hebrew's Haskalah-era status to other literary-only or liturgical-only language states (Latin, Sanskrit, Classical Arabic) to test the boundary of the concept.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__literary_revival_reading, haskalah_literary_circle).
narrative_ontology:fixing_cost_class(hebrew_living_language__literary_revival_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared literary register and a body of demonstrably generative, non-liturgical Hebrew text that later readers, writers, and revivalists can build on — establishing that the language's grammar and lexicon remain productive for expressing genuinely new content, not merely reproducible for fixed liturgical formulas.
% TRANSFER_FUNCTION: Moves cultural and symbolic capital from the wider vernacular-speaking Jewish population (whose actual daily language is Yiddish or the local vernacular) to a literate maskilic elite who produce and control the Hebrew literary corpus; no material extraction occurs, but interpretive authority over what counts as 'Hebrew being alive' concentrates in this circle.
% ABSENT_VOICES: Native Yiddish speakers and traditional liturgical communities are not consulted on whether this counts as the language living — the claim is adjudicated entirely among literate producers and later nationalist interpreters, and ordinary daily speakers of other languages who read no Hebrew literature have no voice in the debate at all.
% DISAPPEARANCE_RATIONALE: If Haskalah literary production had never existed, the world plausibly rearranges for later revivalists (Ben-Yehuda and the native-generation project would have had a thinner, more purely liturgical textual base to draw modern vocabulary and stylistic register from) but arguably stays similar for the broader Jewish population, whose vernacular life was unaffected either way. Whether the literary corpus was load-bearing for the later native-speech revival, or merely one convenient precedent among several, is exactly the disputed question this reading takes a side on.
% FOUNDING_PROBLEM: The Haskalah sought to demonstrate that Hebrew could serve as a modern, secular vehicle for Enlightenment ideas — science, philosophy, romantic literature, political commentary — proving the language was not confined to prayer and could be an instrument of contemporary Jewish cultural and intellectual life.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Hebrew linguists and historians of the Haskalah (writing well outside the movement and with no stake in its self-image) generally treat the literary-generativity demonstration as historically accomplished and closed — modern spoken Israeli Hebrew is now the dominant evidentiary basis for the language's vitality, making the narrower Haskalah-era literary claim a historical staging point rather than a live present-tense argument. No corroborating voice from outside the maskilic tradition and its nationalist inheritors treats the literary-only claim as still needing to be established today.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, contested).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).
:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.08) because this is genuinely elite cultural production with no coercive machinery, no rent extraction, and no identifiable victim class paying a cost for the arrangement — the closest thing to a cost is the very diffuse symbolic marginalization of non-literate vernacular speakers, which does not rise to victimhood. Suppression is low (0.05): no one is coerced into accepting the literary-revival framing, and maskilic writers actively competed for readership and legitimacy against traditionalist and assimilationist alternatives. Theater ratio is modest and rising slightly (0.10 to 0.16) reflecting the genuine but partial nature of the achievement — celebratory maskilic self-narration inflates the claim's significance somewhat, but the underlying literary production is real and substantial, not performative cover for something else. Accessibility collapse is moderate (0.3): once one accepts written generativity as the standard, the alternative framings (liturgical continuity, native speech) are not thereby foreclosed for others to hold — they simply address a different question. Resistance is low-moderate (0.2): the main resistance is intellectual and retrospective (scholars debating the sufficiency criterion), not political suppression of a rival account.
 *
 * DIRECTIONALITY LOGIC:
 *   The maskilic literary circle is the clear structural beneficiary and agenda-setter: they define what counts as evidence for the claim they are simultaneously producing. Readers benefit passively by gaining access to a modern secular register. Zionist cultural nationalists benefit downstream by inheriting a usable literary precedent. No group bears a cost through this arrangement in the way a victim would — the excluded vernacular-speaking majority is excluded from the CONVERSATION, not extracted FROM by it, which is why no victims array is authored and the constraint reads as rope rather than tangled_rope or snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proving Hebrew could carry secular Enlightenment content) is genealogically dead — later native-speech revival superseded it as the live question about Hebrew's vitality. But mandatrophy does not apply cleanly here because this reading was never institutionally enforced as an ongoing mandate; it is a historical episode whose evidentiary claim (written generativity happened) remains true regardless of whether the underlying problem is still live. The disappearance_verdict is authored 'contested' rather than 'world_unchanged' precisely because historians dispute whether the literary corpus was causally load-bearing for the subsequent native revival or merely one convenient precedent — this is the omega-worthy uncertainty, not a mandatrophy case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generativity_sufficient_for_life,
    'Is written generative competence (producing genuinely new, non-formulaic sentences) sufficient on its own for a language to count as ''living,'' or does life require a native daily-speech community regardless of literary productivity?',
    'Comparative typological analysis against other languages with literary-only generative traditions but no native speech community (e.g., Classical Arabic''s use in modern literary and journalistic contexts, Sanskrit''s philosophical composition tradition) to see whether linguists'' ''living language'' judgments track generativity or native acquisition.',
    'If generativity is treated as sufficient, this reading stands as an independently valid account of Hebrew''s vitality during the Haskalah, distinct from and not dependent on the later native-speech revival. If native daily speech is treated as necessary, this reading is downgraded to describing a necessary precursor or contributing factor rather than living-language status itself, collapsing toward the native_generation_reading''s framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generativity_sufficient_for_life, conceptual, 'Whether literary generativity alone satisfies ''living language'' criteria.').

omega_variable(
    haskalah_corpus_causal_load,
    'Was the Haskalah literary corpus causally necessary for the later native-speech revival (Ben-Yehuda era), or would the revival have succeeded on liturgical and scriptural Hebrew alone without this intervening literary phase?',
    'Historical-linguistic analysis of the vocabulary, syntax, and stylistic sources actually drawn upon by early modern Hebrew revivalists and coiners — tracing what proportion of revived Hebrew''s modern register derives from Haskalah literary innovation versus direct biblical/rabbinic sourcing versus outright neologism.',
    'High causal load would support treating this reading as a genuine load-bearing stage of continuity feeding into the native_generation_reading; low causal load would support treating it as a parallel, largely independent elite phenomenon that happened not to be necessary for the eventual outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(haskalah_corpus_causal_load, empirical, 'Whether Haskalah literary Hebrew was a necessary precursor to native revival or a separable episode.').

omega_variable(
    elite_versus_population_vitality,
    'Can a language''s ''life'' be meaningfully assessed at the level of a small literate elite, or does the concept of living language inherently require reference to a broader population''s daily practice?',
    'Conceptual analysis drawing on sociolinguistic definitions of language vitality (e.g., UNESCO/Fishman GIDS-style frameworks) applied retroactively to the Haskalah case, examining whether such frameworks are population-scale by definitional necessity.',
    'If vitality is inherently population-scale, this reading''s claim is better redescribed as ''the Hebrew literary register was alive'' rather than ''Hebrew was alive'' — a significant narrowing of scope with implications for how strongly this reading can claim its title.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_versus_population_vitality, conceptual, 'Whether living-language status can attach to elite literary practice independent of population-scale usage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__literary_revival_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t20, hebrew_living_language__literary_revival_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(hebr_tr_t40, hebrew_living_language__literary_revival_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(hebr_tr_t60, hebrew_living_language__literary_revival_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(hebr_tr_t80, hebrew_living_language__literary_revival_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement(hebr_tr_t100, hebrew_living_language__literary_revival_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__literary_revival_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(hebr_be_t20, hebrew_living_language__literary_revival_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(hebr_be_t40, hebrew_living_language__literary_revival_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(hebr_be_t60, hebrew_living_language__literary_revival_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(hebr_be_t80, hebrew_living_language__literary_revival_reading, base_extractiveness, 80, 0.08).
narrative_ontology:measurement(hebr_be_t100, hebrew_living_language__literary_revival_reading, base_extractiveness, 100, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__literary_revival_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__literary_revival_reading, 0.05).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial claim 'Hebrew was a living language across its diaspora history' per the ε-invariance principle. Each reading fixes a different sufficient condition for 'living': literary generativity (this story), liturgical/textual continuity (sibling), or native daily speech production (sibling). ε differs sharply across the three: this reading and the liturgical reading both author very low extraction (elite/devotional practice, no victims), while the native_generation_reading is expected to carry a different profile given the coercive elements of the actual 20th-century Hebrew revival campaign (suppression of Yiddish, ideological pressure on immigrants). All three link to each other via affects_constraints since they share the same historical kernel and compete for interpretive authority over the same underlying fact pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
