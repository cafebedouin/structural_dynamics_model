% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Literary-Productivity Criterion for Living Language Status (Haskalah Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates the literary-continuity reading of the 'living
 *   language' kernel: a language counts as living if it remains a productive
 *   medium for new literary and intellectual work, independent of whether it
 *   is anyone's native tongue in daily life. The Haskalah periodicals
 *   (Hame'assef and its successors) and the subsequent modern Hebrew literary
 *   revival are cited as proof that Hebrew was never dead, only dormant as a
 *   vernacular while remaining vital as a literary and intellectual
 *   instrument. This reading structurally benefits the maskilim and secular
 *   Hebrew intellectuals who produced that literature — it grants them
 *   cultural authority to speak for Hebrew's future — while it structurally
 *   excludes illiterate Hebrew-adjacent speakers, oral/liturgical users, and
 *   non-literary vernacular (Yiddish, Ladino) communities from counting as
 *   evidence of vitality at all. This is a distinct constraint from the
 *   liturgical-preservation reading (which would count precisely the
 *   populations this reading excludes) and from the native-generation reading
 *   (which would treat this reading's own evidentiary base — literary
 *   production without mass daily speech — as insufficient, even as
 *   corpse-preservation in fancier dress). Per the ε-invariance principle,
 *   each reading is authored here as its own constraint with its own ε; this
 *   file does not attempt to average or reconcile across the three.
 *
 * KEY AGENTS:
 *   - maskilim_literary_circles: Primary beneficiary/agenda_setter (organized/arbitrage) — sets and embodies the criterion
 *   - secular_hebrew_intellectuals: Beneficiary (organized/mobile) — gains cultural-nationalist standing
 *   - illiterate_hebrew_speakers: Primary excluded/payer (powerless/trapped) — real linguistic relationship rendered invisible
 *   - non_literary_vernacular_communities: Payer (powerless/trapped) — vitality defined against their primary language practice
 *   - linguistic_historians: Analytical observer (analytical/analytical) — evaluates the criterion's selection effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.32).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.28).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Literary-Productivity Criterion for Living Language Status (Haskalah Reading)").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, '229ca547-3890-4377-98d4-781189d2a8aa').
narrative_ontology:cs_kernel_codification('229ca547-3890-4377-98d4-781189d2a8aa', distributed).
narrative_ontology:cs_authority_grounding('229ca547-3890-4377-98d4-781189d2a8aa', practice).
narrative_ontology:cs_interpretation_layer_present('229ca547-3890-4377-98d4-781189d2a8aa').
narrative_ontology:cs_reading_relation('229ca547-3890-4377-98d4-781189d2a8aa', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('229ca547-3890-4377-98d4-781189d2a8aa', living_language_status__native_generation_reading, influences).
narrative_ontology:cs_axiom('229ca547-3890-4377-98d4-781189d2a8aa', foundational, literary_productivity_sufficient_for_vitality).
narrative_ontology:cs_axiom_status(literary_productivity_sufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('229ca547-3890-4377-98d4-781189d2a8aa', literary_productivity_sufficient_for_vitality, conventional).
narrative_ontology:cs_axiom('229ca547-3890-4377-98d4-781189d2a8aa', foundational, native_speaker_status_not_necessary_for_life).
narrative_ontology:cs_axiom_status(native_speaker_status_not_necessary_for_life, holdable).
narrative_ontology:cs_axiom_grounding('229ca547-3890-4377-98d4-781189d2a8aa', native_speaker_status_not_necessary_for_life, conventional).
narrative_ontology:cs_reference_frame('229ca547-3890-4377-98d4-781189d2a8aa', haskalah_periodical_emergence).
narrative_ontology:cs_drift_state('229ca547-3890-4377-98d4-781189d2a8aa', post_vernacular_revival_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('229ca547-3890-4377-98d4-781189d2a8aa', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_literary_circles).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, secular_hebrew_intellectuals).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, haskalah_periodical_editors).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_hebrew_speakers).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, non_literary_vernacular_communities).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, traditional_yeshiva_scholars_outside_literary_networks).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, hebrew_cultural_continuity_thesis).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, literary_production_as_vitality_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit and write for Haskalah periodicals (Hame'assef and successors), setting the standard by which Hebrew's 'life' is judged — productive literary and intellectual output. They gain cultural authority and a claim to speak for Hebrew's future without needing to demonstrate that ordinary Jews speak it at home. Their exit option is genuinely broad: they move between Hebrew, Yiddish, German, and other vernaculars as audience and prestige dictate.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_literary_circles, agenda_setter,
    organized, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, maskilim_literary_circles, beneficiary).

% Poets, essayists, and early Hebrew novelists who gain standing as custodians of a 'living' national-cultural language distinct from religious Hebrew. This status feeds directly into later Zionist cultural nationalism and secures them positions as arbiters of what counts as legitimate modern Hebrew.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, secular_hebrew_intellectuals, beneficiary,
    organized, generational, mobile, continental).

% Control which submissions count as serious literary Hebrew, effectively administering the criterion in practice. They can and do change editorial standards; their institutional survival depends on periodical subscriptions from a narrow literate elite, not from the broader Hebrew-adjacent population.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, haskalah_periodical_editors, agenda_setter,
    moderate, biographical, mobile, continental).

% Jews who use Hebrew liturgically or in daily prayer but cannot read or produce literary Hebrew prose or poetry. Under the literary-continuity criterion their relationship to the language counts for nothing in the vitality assessment — their transmission of Hebrew phrases, blessings, and oral fragments is invisible to the definition even though it is a real form of linguistic continuity.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_hebrew_speakers, excluded,
    powerless, biographical, trapped, regional).

% Communities whose primary living relationship to Jewish languages runs through Yiddish or Ladino vernacular speech rather than Hebrew literary production. The literary-continuity criterion implicitly devalues their linguistic vitality by defining 'life' in a way their languages and practices do not satisfy on Hebrew's behalf, and stakes their communities' claim to authenticity on an elite metric they never participate in producing.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, non_literary_vernacular_communities, payer,
    powerless, generational, trapped, regional).

% Scholars deeply engaged with Hebrew as a language of religious study and responsa but outside secular literary networks and periodicals. Their extensive engagement with Hebrew is reclassified as 'preservation' rather than 'vitality' under this criterion, costing them standing in debates about Hebrew's future even though their textual production in Hebrew is substantial.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, traditional_yeshiva_scholars_outside_literary_networks, payer,
    moderate, generational, constrained, regional).

% Study the Haskalah's periodical output and subsequent modern Hebrew literature as evidence in debates over language death, revival, and diglossia. They can name the criterion's selection effects without being party to the underlying contest for cultural authority.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, checkable standard — sustained literary and intellectual production — by which a language's vitality can be assessed across generations, coordinating a scattered maskilim intelligentsia around a common project (periodicals, prose, poetry) rather than leaving 'is Hebrew alive' as an unanswerable impressionistic question.
% TRANSFER_FUNCTION: Moves cultural authority and the recognized right to define Hebrew's future toward the literate, publishing intelligentsia, and away from illiterate or non-literary speakers and communities whose relationship to Hebrew runs through liturgy, oral fragments, or non-Hebrew vernaculars.
% ABSENT_VOICES: Illiterate Hebrew-adjacent speakers and non-literary vernacular (Yiddish/Ladino) communities have no periodical, no seat in the editorial rooms, and no vote in what counts as 'productive' — they would object that their oral and liturgical relationship to Hebrew is real continuity, not corpse-preservation, but they are not in the room where the criterion is set.
% DISAPPEARANCE_RATIONALE: If the literary-continuity criterion vanished, the maskilim's specific claim to cultural authority over Hebrew's future would lose its evidentiary anchor (no more pointing to periodicals as proof of life), and Zionist cultural-nationalist narratives built on it would need a different foundation. But the underlying literary corpus (Haskalah periodicals, modern Hebrew literature) would still exist as a historical fact — only the classificatory use of that corpus as proof of 'vitality' against rival criteria would disappear. Maskilim descendants and literary historians dispute how much would actually change versus rival readings absorbing the same corpus into a different framework.
% FOUNDING_PROBLEM: Enlightenment-era Jewish intellectuals needed to argue that Hebrew was not a dead liturgical relic but a viable vehicle for modern secular thought, science, and literature, in order to justify producing and reading Hebrew-language periodicals, novels, and essays alongside (or instead of) Yiddish and European languages.
% FOUNDING_PROBLEM_CORROBORATION: Modern Hebrew literary historians and linguists outside the original Haskalah movement (e.g., scholars of language revival and diglossia) corroborate that literary production was a genuine and historically significant form of linguistic vitality; but sociolinguists studying language death and revitalization from outside the maskilim tradition dispute that literary output alone settles vitality, noting that native transmission and community-wide daily use are treated by other frameworks as necessary conditions the literary criterion was constructed partly to sidestep.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, contested).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.32) because the primary transfer is symbolic and reputational — cultural authority and the right to define Hebrew's trajectory — rather than material extraction; this is elite coordination around a definitional criterion, not rent extraction from a captive population. Suppression is moderate-low (0.28): no one is coerced into accepting the literary-continuity criterion, but its adoption by cultural-nationalist institutions (and eventually the Zionist movement) gave it outsized influence over which linguistic practices counted as legitimate evidence, closing off alternative framings for populations without periodical access. Theater ratio is modest (0.22) and rises slowly — some genealogical mythmaking accretes as the criterion gets retrofitted into nationalist historiography, but the underlying literary production is real and substantial, not performative. Accessibility collapse (0.45) is moderate: illiterate and non-literary speakers could in principle still assert alternative vitality criteria (and did, via the sibling readings), so the collapse is partial, not near-total.
 *
 * PERSPECTIVAL GAP:
 *   From the maskilim/editor seat, the criterion is a genuine coordination achievement: it gives a scattered, minority intelligentsia a shared, falsifiable standard for language vitality, avoiding both premature declarations of Hebrew's death and unfalsifiable claims of vitality. From the excluded seats, the same criterion operates as a quiet reallocation of who gets to say what Hebrew is for — a reallocation that happens to track literacy and institutional access rather than any speaker's lived relationship to the language.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim and secular intellectuals sit near the beneficiary end of directionality: they wrote the criterion, administer its application through periodical editorial control, and collect the cultural authority it confers, with arbitrage-grade exit across languages and audiences. Illiterate speakers and non-literary vernacular communities sit near the target end: trapped in regional, oral/vernacular linguistic practice, they bear the cost of having their own continuity with Hebrew (or Yiddish/Ladino) reclassified as non-evidence, with no periodical or editorial seat to contest the framing from. Traditional yeshiva scholars occupy an intermediate position — substantial Hebrew textual production, but outside the literary networks the criterion privileges, so their extensive engagement is downgraded to 'preservation.'
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — proving Hebrew could serve modern secular literary and intellectual purposes, against the assumption that a liturgical-only language was inert — was substantially live during the Haskalah period and remains at least partially live wherever modern Hebrew literature's legitimacy is still contested. Because literary Hebrew production continued and eventually fed a genuine vernacular revival (Ben-Yehuda and after), the criterion did not become a pure zombie mandate the way some Piton candidates do; but its continued use to adjudicate vitality questions for OTHER endangered or liturgical languages, long after Hebrew's own revival made the criterion's original evidentiary function moot for Hebrew specifically, is the contested residue this story flags via the founding_problem mismatch fields.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literary_output_vs_mass_vitality,
    'Does sustained literary/intellectual production by a small elite constitute genuine evidence of a language''s vitality, or does it measure only the vitality of a literate subculture while leaving the broader population''s relationship to the language unaddressed?',
    'Compare trajectories of languages with strong literary production but declining vernacular use (e.g., classical/liturgical Hebrew pre-revival, Latin post-medieval) against languages with strong vernacular use but minimal formal literary output, to see which populations of the language survive institutional pressure over centuries.',
    'If literary production reliably predicts eventual vernacular revival (as it arguably did for Hebrew), the criterion has genuine predictive/coordinative value beyond elite self-interest. If it does not reliably predict revival, the criterion functions mainly as retrospective legitimation for the class that already produces the literature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literary_output_vs_mass_vitality, empirical, 'Whether literary productivity is a reliable proxy for language vitality or a self-serving proxy for elite cultural production.').

omega_variable(
    committer_kernel_naturalness,
    'Is ''living language status'' a natural kind with one correct criterion that this reading approximates, or is it an irreducibly contested political/cultural category where each reading (literary, liturgical, native-generation) reflects a different community''s stake in what counts as Hebrew''s continuity, with no framework-independent fact of the matter?',
    'This is not resolvable by further empirical study of Hebrew alone — it requires either (a) a general linguistic theory of language death/revival that all three reading-communities would accept as authoritative, which does not currently exist in a form accepted across sociolinguistics, religious studies, and nationalism studies, or (b) acceptance that the kernel is genuinely contested and each reading is a distinct, defensible constraint.',
    'If a unifying framework existed, one reading might legitimately foreclose the others rather than merely coexisting with them. Absent such a framework, all three readings remain live, mutually non-foreclosing positions held by different communities (maskilim/secular nationalists, traditionalist/liturgical communities, and vernacular-transmission linguists respectively).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_naturalness, conceptual, 'Whether the living-language kernel has a framework-independent resolution or is irreducibly perspectival across the three sibling readings.').

omega_variable(
    retrospective_zionist_appropriation,
    'How much of the literary-continuity criterion''s later authority derives from its genuine explanatory power for the Hebrew case specifically, versus its retrospective appropriation by Zionist cultural nationalism as a template applied to justify claims about Hebrew''s status that serve state-building purposes beyond the original Haskalah project?',
    'Trace citation and invocation patterns of the Haskalah periodicals as evidence across 19th-century maskilim writing versus 20th-century Zionist cultural-nationalist historiography, looking for divergence in how the evidence is framed and what conclusions it is asked to support.',
    'If the criterion was substantially repurposed by a later movement with different goals than the original Haskalah intelligentsia, the founding_problem_status of ''live'' vs. ''dead'' bifurcates by era, and the beneficiary set should be understood as extending well beyond the original maskilim to later Zionist cultural institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retrospective_zionist_appropriation, empirical, 'Whether the criterion''s later authority is native to the Haskalah project or substantially inherited/repurposed by later cultural-nationalist movements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__literary_continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(livi_tr_t20, living_language_status__literary_continuity_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(livi_tr_t40, living_language_status__literary_continuity_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(livi_tr_t60, living_language_status__literary_continuity_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(livi_tr_t80, living_language_status__literary_continuity_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(livi_tr_t100, living_language_status__literary_continuity_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__literary_continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(livi_be_t20, living_language_status__literary_continuity_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(livi_be_t40, living_language_status__literary_continuity_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(livi_be_t60, living_language_status__literary_continuity_reading, base_extractiveness, 60, 0.3).
narrative_ontology:measurement(livi_be_t80, living_language_status__literary_continuity_reading, base_extractiveness, 80, 0.31).
narrative_ontology:measurement(livi_be_t100, living_language_status__literary_continuity_reading, base_extractiveness, 100, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(living_language_status__literary_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__literary_continuity_reading, 0.1).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'is Hebrew (or any language) living' per the ε-invariance principle: literary_continuity_reading (this file, low ε, elite literary-coordination beneficiary structure), liturgical_preservation_reading (would count liturgical/ritual transmission as sufficient, different beneficiary set centered on religious authorities), and native_generation_reading (would require intergenerational mother-tongue transmission, treating literary-only continuity as insufficient — potentially the strictest and most victim-inclusive of the three readings, since it would count communities excluded here). Each reading is authored with independent ε, stakeholders, and classification; they are linked here rather than merged because measuring 'living language' by literary output versus liturgical use versus native transmission yields structurally different ε values and different victim/beneficiary sets — exactly the decomposition trigger the ε-invariance principle requires.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
