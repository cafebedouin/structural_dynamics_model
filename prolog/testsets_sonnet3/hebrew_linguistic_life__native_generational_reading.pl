% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Native-Generational Standard for Hebrew's Linguistic Vitality (Ben-Yehuda Revival Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates the native-generational reading of the Hebrew
 *   linguistic-life kernel: a language is alive only when children acquire it
 *   as mother tongue and use it for all daily secular functions. Under this
 *   reading, Hebrew was genuinely dead as a spoken vernacular from roughly 70
 *   CE to 1880 CE — surviving only in liturgy, study, and literary
 *   composition — and its 'revival' beginning with Eliezer Ben-Yehuda and the
 *   first Hebrew-speaking households in Jerusalem was a real, unprecedented
 *   act of deliberate re-vernacularization, not merely a re-description of
 *   continuous use. The reading's own victim set follows directly from its
 *   own criterion: because the standard treats total secular-domain
 *   replacement as the measure of success, it structurally required immigrant
 *   children to stop transmitting Yiddish, Ladino, and Mizrahi vernaculars as
 *   mother tongues, since a household could not simultaneously be raising
 *   Hebrew-native children and preserving the parent generation's home
 *   languages at full strength. This is a different constraint from the
 *   liturgical-preservation reading (which would find Hebrew never dead) and
 *   the marketplace-pidgin reading (which would find Hebrew's earlier
 *   trade/prayer-community uses already sufficient); each of those is
 *   authored as its own sibling story with its own epsilon.
 *
 * KEY AGENTS:
 *   - zionist_nation_building_project: agenda_setter (institutional/arbitrage) — sets and enforces the native-generational criterion
 *   - sabra_native_hebrew_speakers: beneficiary (organized/mobile) — the living proof-population the standard requires
 *   - yiddish_speaking_immigrants: payer (moderate/constrained) — bear coerced language shift under active suppression campaigns
 *   - ladino_speaking_immigrants: payer (powerless/trapped) — vernacular heritage discontinued within one to two generations
 *   - sociolinguists_and_historians: observer (analytical) — document both the revival's genuine achievement and its coercive costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.58).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.63).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Native-Generational Standard for Hebrew's Linguistic Vitality (Ben-Yehuda Revival Reading)").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, '37b8e90d-6020-4bc2-8e04-8015b075d917').
narrative_ontology:cs_kernel_codification('37b8e90d-6020-4bc2-8e04-8015b075d917', distributed).
narrative_ontology:cs_authority_grounding('37b8e90d-6020-4bc2-8e04-8015b075d917', practice).
narrative_ontology:cs_interpretation_layer_present('37b8e90d-6020-4bc2-8e04-8015b075d917').
narrative_ontology:cs_reading_relation('37b8e90d-6020-4bc2-8e04-8015b075d917', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('37b8e90d-6020-4bc2-8e04-8015b075d917', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('37b8e90d-6020-4bc2-8e04-8015b075d917', foundational, native_transmission_is_the_sole_life_criterion).
narrative_ontology:cs_axiom_status(native_transmission_is_the_sole_life_criterion, holdable).
narrative_ontology:cs_axiom_grounding('37b8e90d-6020-4bc2-8e04-8015b075d917', native_transmission_is_the_sole_life_criterion, conventional).
narrative_ontology:cs_axiom('37b8e90d-6020-4bc2-8e04-8015b075d917', foundational, total_domain_secular_use_required_for_vitality).
narrative_ontology:cs_axiom_status(total_domain_secular_use_required_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('37b8e90d-6020-4bc2-8e04-8015b075d917', total_domain_secular_use_required_for_vitality, conventional).
narrative_ontology:cs_reference_frame('37b8e90d-6020-4bc2-8e04-8015b075d917', pre_revival_liturgical_only_hebrew).
narrative_ontology:cs_drift_state('37b8e90d-6020-4bc2-8e04-8015b075d917', post_1948_state_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('37b8e90d-6020-4bc2-8e04-8015b075d917', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, zionist_nation_building_project).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, sabra_native_hebrew_speakers).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_immigrants).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speaking_immigrants).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, mizrahi_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, diaspora_linguistic_heritage).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, vernacularization_thesis).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, nation_state_monolingual_ideal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the criterion that a living language requires native child acquisition and total domain coverage, and builds schools, youth movements, and language committees (the Va'ad ha-Lashon) around enforcing this standard. Collects legitimacy for the settlement project from having 'revived' an ancient tongue as the national vernacular; the standard justifies suppressing rival home languages among new immigrants.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, zionist_nation_building_project, agenda_setter,
    institutional, generational, arbitrage, national).

% The first generations of children raised speaking Hebrew as a mother tongue in Ottoman and Mandate Palestine. Their existence is the empirical proof the reading requires; they inherit social prestige, native fluency advantage in state institutions, and cultural authority as the living embodiment of the revival, at no direct cost to themselves.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, sabra_native_hebrew_speakers, beneficiary,
    organized, generational, mobile, national).

% Codifies vocabulary, grammar, and pronunciation standards under the premise that Hebrew's status as a living mother tongue must be actively engineered and defended. Its continued institutional relevance and funding depend on the native-generational standard remaining the accepted definition of linguistic life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_language_academy, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, hebrew_language_academy, agenda_setter).

% Arrive with Yiddish as a fully functioning mother tongue covering all domains of daily life, including secular speech, humor, and commerce. Under the native-generational standard applied coercively by Hebraist institutions, their language is reclassified as an obstacle to national revival; children are schooled exclusively in Hebrew, Yiddish press and theater are suppressed by language-defense squads (Gdud Meginei ha-Safa), and intergenerational transmission is deliberately broken within a generation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_immigrants, payer,
    moderate, biographical, constrained, national).

% Sephardi immigrant communities whose Judeo-Spanish vernacular meets every criterion of the native-generational standard in its country of origin, yet is treated as a diasporic relic to be shed on arrival. Elders retain Ladino; children are absorbed into Hebrew schooling with no institutional support for maintaining Ladino as a home language, accelerating its decline within one to two generations.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speaking_immigrants, payer,
    powerless, biographical, trapped, national).

% Jewish immigrants from Arabic-, Persian-, and other vernacular-speaking communities whose native tongues are functionally alive by the same standard applied to Hebrew, but are socially and institutionally coded as markers of backwardness relative to the revived national language, producing pressure toward rapid Hebraization and loss of home-language transmission.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, mizrahi_vernacular_speakers, payer,
    powerless, biographical, constrained, national).

% The accumulated body of diaspora vernaculars (Yiddish, Ladino, Judeo-Arabic dialects) that the native-generational standard implicitly devalues as non-national. Not an agent itself, but the record against which the revival's costs are measured; no institutional seat exists to advocate for its preservation within the nation-building framework.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, diaspora_linguistic_heritage, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__native_generational_reading, diaspora_linguistic_heritage).

% Study the Hebrew revival as the paradigm case of successful language revitalization by the native-generational criterion, while increasingly documenting the coercive suppression of Yiddish, Ladino, and Mizrahi vernaculars that the same revival required. Their analysis is the primary source that surfaces the victim set outside the beneficiary institutions' own telling.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, sociolinguists_and_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, unambiguous test for whether a language counts as 'alive' — native child acquisition plus total-domain use — which let the Zionist project mobilize disparate immigrant populations around one national vernacular and coordinate schooling, administration, and cultural production in a shared tongue rather than fragmenting across dozens of home languages.
% TRANSFER_FUNCTION: Moves linguistic capital, intergenerational transmission, and cultural prestige away from diaspora vernaculars (Yiddish, Ladino, Judeo-Arabic dialects) and concentrates it in Hebrew and its native-speaking cohort and the institutions that codify it, converting immigrant children's mother-tongue inheritance into Hebrew fluency at the cost of their parents' languages.
% ABSENT_VOICES: Yiddishist cultural movements, Ladino cultural preservation advocates, and Mizrahi community elders who argued their vernaculars already satisfied any reasonable definition of linguistic life were largely excluded from the institutions (the Va'ad ha-Lashon, the school system) that set the native-generational standard as policy; their objections survive mainly in memoir and later academic reconstruction, not in the founding institutional record.
% DISAPPEARANCE_RATIONALE: If the native-generational standard were abandoned as the criterion for linguistic life, Hebrew's revival would no longer be treated as sociolinguistically exceptional or as the necessary marker of national authenticity — this would reopen space for Yiddish, Ladino, and Mizrahi vernaculars to be recognized as never having been 'dead' by an equally valid coordination or liturgical standard, and would undercut the ideological framing that only full vernacular replacement constitutes successful revival.
% FOUNDING_PROBLEM: Zionist settlers in late Ottoman Palestine arrived speaking dozens of mutually unintelligible home languages; no shared vernacular existed to unify a claimed national community, and Hebrew existed only as a liturgical and literary language without native speakers, so a criterion was needed to define what a genuine national revival would require and to justify the deliberate replacement project.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and the Hebrew Language Academy attest the founding problem (linguistic fragmentation) was real and that native-generational revival was the only adequate solution. Independent sociolinguists and Yiddishist/Sephardi cultural historians attest the founding problem was real but the native-generational solution required coercive suppression of viable alternative vernaculars that were not, by any neutral standard, linguistically dead — corroboration from outside the beneficiary institutions supports the contested status.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.58 by 1948) reflects the reading's own coordination achievement — a genuinely dead vernacular restored to native-speaker status is a real public good for national coordination — layered with a substantial, non-trivial cost imposed on diaspora vernacular speakers who did not choose to abandon Yiddish, Ladino, or Mizrahi home languages but were structurally pressured to under the same standard. Suppression rises sharply from 1895 to 1923 (0.38 to 0.58) tracking the institutionalization of Hebrew-only schooling and organized anti-Yiddish enforcement (language-defense squads disrupting Yiddish theater and press), then plateaus as the shift becomes normalized rather than actively contested. Theater ratio stays low throughout (0.10 to 0.22) because the coordination function was substantially real, not primarily performative — actual native transmission did occur at scale.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist nation-building project and the resulting native-Hebrew-speaking cohort sit near the beneficiary end: they receive the coordination good (a shared national vernacular) and the legitimacy of having achieved what the reading defines as a linguistic resurrection, with mobile or arbitrage-grade exit options since the standard's costs do not fall on them. Yiddish, Ladino, and Mizrahi-vernacular immigrant populations sit near the target end: the reading's success criterion is defined in a way that necessarily discounts their mother tongues as obstacles rather than as already-living languages by an equally coherent standard, and their exit options range from constrained (economic and social integration pressure) to trapped (no institutional path existed to maintain both).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — linguistic fragmentation among a settler population needing a shared vernacular — was genuinely live at founding and substantially resolved by 1948 (Hebrew functioned as a native tongue for a growing generation). The constraint is not mandatrophic in the classic sense of an empty shell maintained by inertia; it is better read as tangled_rope precisely because the coordination function was real AND the extraction from diaspora vernacular communities was real and structurally necessary to the standard's own definition of success, not an accidental side effect. Classifying it as tangled_rope rather than pure rope prevents mislabeling the coercive suppression of Yiddish and Ladino as costless coordination, while classifying it as tangled_rope rather than pure snare prevents erasing the genuine, unprecedented sociolinguistic achievement the native-generational criterion captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dormancy_period_reality,
    'Was Hebrew genuinely without native speakers for the full 70-1880 CE span, or did small unbroken pockets of vernacular use persist (e.g., among some Yemenite or isolated communities) that would undercut the clean dead-then-revived narrative this reading depends on?',
    'Historical-linguistic review of documented vernacular Hebrew use across the diaspora during the claimed dormancy period, distinguishing liturgical/literary use from genuine native-speaker transmission.',
    'If unbroken native pockets existed, this reading''s dead-language premise weakens and it converges partially toward the liturgical_preservation_reading; if the dormancy was total, the native-generational reading''s dramatic revival claim is fully supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_period_reality, empirical, 'Whether the 70-1880 CE dormancy period this reading depends on was total or partial.').

omega_variable(
    coercion_versus_voluntary_shift,
    'How much of the Yiddish/Ladino/Mizrahi language shift was coerced by active institutional suppression (language-defense squads, Hebrew-only schooling mandates) versus voluntarily chosen by immigrants seeking social and economic integration into the new national project?',
    'Comparative analysis of communities with and without direct exposure to organized Hebraist enforcement campaigns, controlling for economic integration incentives.',
    'If shift was substantially voluntary, the victim framing weakens and the constraint moves toward rope; if substantially coerced, the tangled_rope classification with a real victim set is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_versus_voluntary_shift, empirical, 'Structural coercion versus voluntary assimilation as the mechanism of vernacular language loss.').

omega_variable(
    kernel_reading_incommensurability,
    'Is the disagreement between this reading, the liturgical_preservation_reading, and the marketplace_pidgin_reading resolvable by better evidence, or does it reflect genuinely incommensurable definitions of what ''linguistic life'' means that no additional data could settle?',
    'None available in principle if the disagreement is definitional rather than empirical; the most that can be done is documenting each reading''s internal coherence and tracing which institutional actors have historically privileged which definition.',
    'If incommensurable, all three readings persist as permanently coexisting constraints rather than converging toward one true account of Hebrew''s linguistic history.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three kernel readings of linguistic life are empirically adjudicable or definitionally incommensurable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1881, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1881, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1881, 0.1).
narrative_ontology:measurement(hebr_tr_t1895, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1895, 0.14).
narrative_ontology:measurement(hebr_tr_t1909, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1909, 0.17).
narrative_ontology:measurement(hebr_tr_t1923, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1923, 0.19).
narrative_ontology:measurement(hebr_tr_t1936, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1936, 0.21).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1948, 0.22).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1881, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1881, 0.3).
narrative_ontology:measurement(hebr_be_t1895, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1895, 0.4).
narrative_ontology:measurement(hebr_be_t1909, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1909, 0.48).
narrative_ontology:measurement(hebr_be_t1923, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1923, 0.53).
narrative_ontology:measurement(hebr_be_t1936, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1936, 0.56).
narrative_ontology:measurement(hebr_be_t1948, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1948, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1881, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1881, 0.25).
narrative_ontology:measurement(hebr_su_t1895, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1895, 0.38).
narrative_ontology:measurement(hebr_su_t1909, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1909, 0.5).
narrative_ontology:measurement(hebr_su_t1923, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1923, 0.58).
narrative_ontology:measurement(hebr_su_t1936, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1936, 0.61).
narrative_ontology:measurement(hebr_su_t1948, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1948, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__native_generational_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hebrew_linguistic_life kernel, decomposed per the ε-invariance principle: this reading (native_generational_reading) authors substantial extraction (0.58) driven by coerced diaspora-vernacular displacement; the liturgical_preservation_reading would author near-zero extraction since it treats Hebrew as never having been dead; the marketplace_pidgin_reading would author low-to-moderate extraction with a different beneficiary set (inter-communal trade/prayer interlocutors rather than the nation-building project). All three are linked via affects_constraints rather than merged into one story, since each reading commits to a different account of the dormancy period and a different victim/beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
