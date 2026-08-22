% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hebrew Vitality — Hybrid Continuity Reading (substrate-plus-reconstruction)
 *   domain: sociolinguistics/historical_linguistics/jewish_studies
 *
 * SUMMARY:
 *   This story authors the hybrid_continuity reading of the hebrew_vitality
 *   kernel: the claim that roughly seventeen centuries of Hebrew liturgical
 *   preservation supplied a necessary but not sufficient substrate for the
 *   language's 19th-20th century vernacular revival, which additionally
 *   required deliberate reconstruction (lexical coinage, child-first
 *   acquisition, institutional schooling, state formation) as an independent
 *   causal input. This is a low-extraction analytical synthesis rather than
 *   an operative constraint — it settles no resources and binds no
 *   institution's behavior; its 'cost' is purely interpretive, redistributing
 *   explanatory credit between two rival lineages (liturgical custodians and
 *   revival-movement institutions) that each prefer a reading crediting their
 *   own contribution as sufficient on its own. Two sibling constraints exist
 *   for the other readings of this kernel: liturgical_reading (ritual
 *   preservation alone constitutes vitality) and native_daily_reading (only
 *   native generation constitutes vitality; ritual recitation is
 *   preservation, not life). This story's ε is deliberately low and stable
 *   because a reframing move that tries to resolve a contest analytically,
 *   rather than adjudicating it in favor of either extractive lineage, should
 *   not carry the extraction profile of either partisan reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.08).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.05).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality — Hybrid Continuity Reading (substrate-plus-reconstruction)").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/historical_linguistics/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '7e4b4ffc-9882-446b-81a7-d6a4d3f30f83').
narrative_ontology:cs_kernel_codification('7e4b4ffc-9882-446b-81a7-d6a4d3f30f83', distributed).
narrative_ontology:cs_authority_grounding('7e4b4ffc-9882-446b-81a7-d6a4d3f30f83', distributed).
narrative_ontology:cs_reading_relation('7e4b4ffc-9882-446b-81a7-d6a4d3f30f83', hebrew_vitality__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('7e4b4ffc-9882-446b-81a7-d6a4d3f30f83', hebrew_vitality__native_daily_reading, influences).
narrative_ontology:cs_axiom('7e4b4ffc-9882-446b-81a7-d6a4d3f30f83', foundational, substrate_necessary_but_not_sufficient).
narrative_ontology:cs_axiom_status(substrate_necessary_but_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('7e4b4ffc-9882-446b-81a7-d6a4d3f30f83', substrate_necessary_but_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('7e4b4ffc-9882-446b-81a7-d6a4d3f30f83', foundational, reconstruction_necessary_but_not_sufficient).
narrative_ontology:cs_axiom_status(reconstruction_necessary_but_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('7e4b4ffc-9882-446b-81a7-d6a4d3f30f83', reconstruction_necessary_but_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('7e4b4ffc-9882-446b-81a7-d6a4d3f30f83', secondary, vitality_requires_dual_causal_components).
narrative_ontology:cs_axiom_status(vitality_requires_dual_causal_components, holdable).
narrative_ontology:cs_axiom_grounding('7e4b4ffc-9882-446b-81a7-d6a4d3f30f83', vitality_requires_dual_causal_components, empirically_contingent).
narrative_ontology:cs_created_at('7e4b4ffc-9882-446b-81a7-d6a4d3f30f83', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, language_revitalization_scholars).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, modern_hebrew_speech_community).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, necessary_but_insufficient_liturgical_role).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, dual_mechanism_revival_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sociolinguists studying Hebrew revival as a case study for other endangered-language projects. They benefit from a synthesis reading that gives them a portable causal model (substrate preservation plus deliberate reconstruction) applicable to other revitalization efforts. They set the terms of the academic debate by publishing the comparative framework, but they neither control liturgical practice nor speak the language natively as a birthright.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, language_revitalization_scholars, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, language_revitalization_scholars, agenda_setter).

% Native and near-native speakers of Modern Hebrew in Israel today. Under this reading, their living language is explained as the product of both an inherited liturgical substrate (vocabulary, script, textual corpus) and a distinct, deliberate 19th-20th century reconstruction effort (Ben-Yehuda-era coinage, child acquisition, institutional schooling). They neither lose nor gain anything material from which historical narrative wins; the reading simply describes how their language came to exist.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, modern_hebrew_speech_community, beneficiary,
    organized, generational, mobile, national).

% Rabbinic and liturgical authorities who maintained Hebrew through unbroken ritual recitation across the diaspora for roughly two millennia. This reading credits their preservation as necessary substrate but explicitly denies it was sufficient for vitality — a partial validation that some custodians read as a demotion of ritual continuity to mere raw material for a secularizing project they did not choose.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_tradition_custodians, excluded,
    institutional, civilizational, constrained, global).

% Institutional heirs of the Ben-Yehuda-era revival project (language academies, Zionist educational institutions) who credit deliberate reconstruction as the decisive causal factor. This reading grants their contribution necessity but pairs it symmetrically with the liturgical substrate, which some in this camp read as understating the singularity of the reconstruction achievement.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, revival_movement_descendants, excluded,
    organized, generational, constrained, national).

% Researchers and practitioners working on other endangered or dormant languages (Cornish, Wampanoag, Hawaiian) who watch the Hebrew case to extract transferable lessons. They observe the contest between readings without a direct stake in which one is declared correct for Hebrew specifically, but the hybrid reading is the one most exportable to their own projects since most other cases lack Hebrew's degree of liturgical substrate.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, comparative_language_revitalization_field, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves an apparent contradiction in the historical record — how a language with millennia of ritual-only use became a native vernacular within roughly two generations — by decomposing the outcome into two jointly necessary, individually insufficient causal components: preserved substrate (vocabulary, script, textual corpus, prestige) and deliberate reconstruction (coinage, child transmission, schooling, state formation). This lets scholars and language-planners model the mechanism rather than adjudicate a single origin story.
% TRANSFER_FUNCTION: This reading does not move resources, money, or coercive power between parties. It reallocates explanatory credit: it moves some causal weight away from an exclusive liturgical-continuity narrative and some away from an exclusive native-generation narrative, redistributing it into a joint-necessity structure. The only thing that moves is interpretive authority over a historical causal claim.
% ABSENT_VOICES: Practicing rabbinic authorities who might object that reducing liturgical practice to 'necessary substrate' understates its status as living religious language use, not dormant raw material, are not represented as first-person sources in the academic synthesis literature — they are cited through historical record rather than consulted as adjudicating parties. Ben-Yehuda-tradition institutional descendants who might object that pairing reconstruction symmetrically with substrate understates the singularity of deliberate revival are similarly absent from the framing conversation, which is conducted mostly among linguists and historians rather than the two rival lineages themselves.
% DISAPPEARANCE_RATIONALE: This is an analytical reading of a historical causal question, not an operative institution. If the hybrid-continuity framing vanished from the scholarly literature overnight, Modern Hebrew would keep being spoken exactly as it is; no liturgical practice, school curriculum, or state institution depends on this specific synthesis for its continued operation. Other readings of the same kernel (liturgical, native-daily) would continue to compete for explanatory primacy, as they did before this synthesis was proposed and would after.
% FOUNDING_PROBLEM: Explain how Modern Hebrew achieved native vernacular vitality despite roughly 1,700 years without a continuous native-speaking generation, when comparative sociolinguistics generally treats loss of intergenerational native transmission as fatal to vitality.
% FOUNDING_PROBLEM_CORROBORATION: Comparative language-revitalization researchers working on unrelated cases (Cornish, Hawaiian, Wampanoag) independently corroborate that neither substrate preservation alone nor reconstruction effort alone has reproduced Hebrew-scale outcomes elsewhere, which is external evidence for the joint-necessity claim from a community with no stake in which Hebrew-internal lineage gets narrative credit. Historical demographic and school-enrollment records from the Yishuv period, compiled independently of both the rabbinic and revivalist institutional traditions, provide additional non-partisan corroboration for the timing and mechanism of the reconstruction component.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).
:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits near the floor (0.06-0.08) because no party is coerced into anything by this reading; it is a historiographical synthesis consumed by researchers and, secondarily, by the speech community as a description of their own language's history. Suppression is near-zero (0.05) — no alternative reading is blocked from circulation; liturgical_reading and native_daily_reading continue to be argued in parallel. Theater ratio stays low (0.10 at interval end) because the synthesis performs genuine explanatory work rather than substituting performance for function; the modest rise reflects increasing citation of the synthesis as settled consensus in textbooks, a mild Goodhart risk where the hybrid frame starts to be invoked as authoritative shorthand rather than argued afresh.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries here are diffuse and non-extractive: scholars gain a portable causal model, and the modern speech community gains an accurate account of its own linguistic ancestry, but neither collects rents from the other lineages. There are no victims in the schema sense (no payer role authored) because this reading does not transfer resources or coerce compliance — it reallocates explanatory credit, which is why liturgical_tradition_custodians and revival_movement_descendants are marked excluded rather than payer: they are underrepresented as first-person parties to the framing conversation, not extracted from materially.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy question properly posed here since the constraint is analytical rather than institutional — nothing was founded to solve a problem and then outlived its function in the operative sense. The founding_problem/status fields are answered at the level of the historiographical puzzle (why did Hebrew revive when comparable substrate-only cases did not) rather than at the level of an institution that could decay. The founding problem remains live: comparative revitalization science still needs the joint-necessity mechanism to explain why Hebrew succeeded where other substrate-rich, reconstruction-poor or reconstruction-rich, substrate-poor cases have not replicated the outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_as_genuine_synthesis_or_third_partisan_claim,
    'Is the hybrid_continuity_reading a neutral analytical resolution of the liturgical vs. native-daily contest, or is it itself a third partisan position (favoring a secular-linguistics historiography that structurally favors the revival-movement''s institutional descendants by making reconstruction causally indispensable, even while crediting liturgical substrate)?',
    'Track whether liturgical_tradition_custodians and revival_movement_descendants, when directly consulted rather than read through the historical record, accept the hybrid framing as fair or reject it as their own reading demoted. Absence of that direct consultation in the existing corpus (documented under absent_voices) means this remains unresolved.',
    'If the hybrid reading is itself partisan (favoring a professional-linguistics historiographical authority over both religious-liturgical and nationalist-revivalist authority), its claimed low ε and diffuse beneficiary structure would need revision — the language_revitalization_scholars seat would look more like a beneficiary with a stake in a particular meta-narrative than a neutral analytical observer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_as_genuine_synthesis_or_third_partisan_claim, conceptual, 'Whether the synthesis reading is neutral or itself a third partisan claim favoring academic historiographical authority.').

omega_variable(
    necessity_sufficiency_boundary_empirical_test,
    'Is the joint-necessity claim (substrate insufficient alone, reconstruction insufficient alone, both jointly necessary) empirically falsifiable against comparative cases, or is it under-determined by the available historical record for Hebrew specifically?',
    'Comparative analysis across other liturgical-substrate languages that did NOT undergo deliberate reconstruction (to test whether substrate alone is ever sufficient) and reconstruction efforts for languages with NO liturgical substrate (to test whether reconstruction alone is ever sufficient), holding community size and political conditions as controls where possible.',
    'If either component turns out sufficient alone in some comparable case, the hybrid reading''s central joint-necessity claim would be falsified for the general mechanism, though it might still hold for the Hebrew-specific instance; this would shift the vindicated_propositions status from empirically supported to Hebrew-idiosyncratic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_sufficiency_boundary_empirical_test, empirical, 'Whether joint necessity is a general mechanism or a Hebrew-specific historical accident, given absence of a clean comparative test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t28, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 28, 0.06).
narrative_ontology:measurement(hebr_tr_t56, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 56, 0.08).
narrative_ontology:measurement(hebr_tr_t84, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 84, 0.09).
narrative_ontology:measurement(hebr_tr_t112, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 112, 0.1).
narrative_ontology:measurement(hebr_tr_t140, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 140, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(hebr_be_t28, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 28, 0.06).
narrative_ontology:measurement(hebr_be_t56, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 56, 0.07).
narrative_ontology:measurement(hebr_be_t84, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 84, 0.08).
narrative_ontology:measurement(hebr_be_t112, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 112, 0.08).
narrative_ontology:measurement(hebr_be_t140, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 140, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__hybrid_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__hybrid_continuity_reading, 0.02).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, native_daily_reading).

% DUAL FORMULATION NOTE:
% This story is the hybrid_continuity_reading member of the hebrew_vitality kernel family, alongside liturgical_reading and native_daily_reading. Where those two readings each claim the kernel outright (unbroken ritual use IS vitality; only native generation IS vitality), this reading denies sufficiency to either and asserts a dual necessary-component structure. It does not foreclose either sibling logically — a liturgical custodian or a revival-movement descendant can still coherently hold their own reading as primary — but it exerts structural influence on both by supplying the comparative-linguistics field's preferred explanatory model, which shapes how each lineage's contribution gets weighted in textbooks, museum narratives, and revitalization-policy transfer to other endangered languages.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
