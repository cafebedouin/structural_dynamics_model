% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__marketplace_pidgin_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Marketplace Pidgin Definition of Hebrew Linguistic Life
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint story captures the 'marketplace pidgin reading' of Hebrew
 *   linguistic life — the claim that Hebrew remained alive through continuous
 *   use as a modified Medieval Hebrew pidgin in Jerusalem markets pre-1880,
 *   functioning as an inter-communal coordination medium regardless of native
 *   speaker status or sacred function. The reading emerged in late
 *   19th-century Zionist historiography as a counter to both the 'dead
 *   language' trope and the traditionalist liturgical-only view. It carries a
 *   genuine coordination function (operational vitality criterion for
 *   sociolinguistics) but also extracts legitimacy from the liturgical
 *   preservation reading (traditional Jewish self-understanding) and the
 *   native generational reading (structuralist linguistics), particularly
 *   when institutionalized in Israeli academia and public memory. The
 *   constraint's extraction peaked at state-founding (1948) when the
 *   'continuous adaptation' narrative became official historiography, then
 *   partially declined as critical scholarship emerged, but persists in
 *   modified form.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.42).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.38).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Marketplace Pidgin Definition of Hebrew Linguistic Life").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__marketplace_pidgin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, 'fccd76c3-c708-4652-a6e7-89aea0be65f2').
narrative_ontology:cs_kernel_codification('fccd76c3-c708-4652-a6e7-89aea0be65f2', distributed).
narrative_ontology:cs_authority_grounding('fccd76c3-c708-4652-a6e7-89aea0be65f2', practice).
narrative_ontology:cs_interpretation_layer_present('fccd76c3-c708-4652-a6e7-89aea0be65f2').
narrative_ontology:cs_reading_relation('fccd76c3-c708-4652-a6e7-89aea0be65f2', hebrew_linguistic_life__liturgical_preservation_reading, influences).
narrative_ontology:cs_reading_relation('fccd76c3-c708-4652-a6e7-89aea0be65f2', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_axiom('fccd76c3-c708-4652-a6e7-89aea0be65f2', foundational, vernacular_function_suffices_for_vitality).
narrative_ontology:cs_axiom_status(vernacular_function_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('fccd76c3-c708-4652-a6e7-89aea0be65f2', vernacular_function_suffices_for_vitality, empirically_contingent).
narrative_ontology:cs_axiom('fccd76c3-c708-4652-a6e7-89aea0be65f2', foundational, continuous_adaptation_not_revival).
narrative_ontology:cs_axiom_status(continuous_adaptation_not_revival, holdable).
narrative_ontology:cs_axiom_grounding('fccd76c3-c708-4652-a6e7-89aea0be65f2', continuous_adaptation_not_revival, empirically_contingent).
narrative_ontology:cs_reference_frame('fccd76c3-c708-4652-a6e7-89aea0be65f2', pre_1880_jerusalem_market_ecology).
narrative_ontology:cs_drift_state('fccd76c3-c708-4652-a6e7-89aea0be65f2', zionist_revival_narrative_codification, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fccd76c3-c708-4652-a6e7-89aea0be65f2', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, zionist_nationalist_historians).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, functionalist_sociolinguists).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, traditionalist_liturgical_scholars).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, structuralist_generational_linguists).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, palestinian_arabic_market_speakers).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, vernacular_function_suffices_for_vitality).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, continuous_adaptation_not_revival).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and institutionalize the narrative that Hebrew never died but continued as a living market language, legitimizing the revival project as continuation rather than resurrection. Control academic appointments, funding, and public commemoration. Can shift frameworks but invested in this reading's legitimacy.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, zionist_nationalist_historians, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain a clear, empirically grounded criterion for language vitality (inter-communal coordination) that avoids the problematic native-speaker requirement. Their research programs and theoretical frameworks benefit from the marketplace reading's operationalizability. Can exit to other vitality frameworks but this one anchors their Hebrew work.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, functionalist_sociolinguists, beneficiary,
    organized, biographical, mobile, global).

% Hold the liturgical preservation reading as the authentic Jewish self-understanding of Hebrew's life. The marketplace reading marginalizes their framework by treating liturgical continuity as insufficient for 'true' vitality. Constrained by institutional power of Zionist historiography and by their own community's internal debates.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, traditionalist_liturgical_scholars, payer,
    organized, generational, constrained, global).

% Maintain that mother-tongue acquisition is the gold standard for language life. The marketplace reading's 'regardless of native speaker status' directly undermines their theoretical position. Can publish in other frameworks but face exclusion from Hebrew-specific discourse dominated by the marketplace reading.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, structuralist_generational_linguists, payer,
    organized, biographical, mobile, global).

% Were the actual Arabic-speaking majority in the Jerusalem market ecology where Hebrew pidgin functioned. Their multilingual market practices are erased when the pidgin is framed as 'Hebrew's continuous life' rather than as a contact variety in an Arabic-dominated space. No institutional voice in the scholarly contest.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, palestinian_arabic_market_speakers, excluded,
    powerless, biographical, trapped, local).

% Analyze the kernel contest from outside the nationalist/religious stakes. See all three readings as live positions with different empirical and normative commitments. Not constrained by any single framework's legitimacy requirements.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, contemporary_sociolinguistic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a sociolinguistically operational criterion for language vitality that distinguishes 'living inter-communal use' from 'mere liturgical preservation' without requiring native speaker communities — enabling classification of contact varieties, pidgins, and liturgical languages on a single functional continuum.
% TRANSFER_FUNCTION: Moves definitional authority and legitimacy from the liturgical preservation reading (traditional Jewish self-understanding) and the native generational reading (structuralist linguistics) to the functionalist marketplace reading, which then authorizes the 'continuous adaptation' narrative of Hebrew history.
% ABSENT_VOICES: Palestinian Arabic market speakers whose multilingual practices constituted the actual ecology of the Jerusalem pidgin; Sephardic and Mizrahi traditional scholars whose liturgical Hebrew was the high variety in that ecology; Yiddish-speaking Ashkenazi scholars for whom the marketplace Hebrew was a marginal contact variety, not a living language.
% DISAPPEARANCE_RATIONALE: If the marketplace reading vanished, the 'continuous adaptation' narrative of Hebrew history would lose its primary empirical anchor. Zionist historiography would revert to 'revival of a dead language' framing; functionalist sociolinguistics would lose its flagship case for vitality without native speakers; the liturgical and generational readings would regain uncontested ground in their respective domains.
% FOUNDING_PROBLEM: Late 19th-century Zionist historiography needed to counter the antisemitic trope that Hebrew was a 'dead language' and Jews a 'nation without a language,' while also countering traditionalist insistence that Hebrew's only legitimate life was liturgical. The marketplace reading solved this by finding a vernacular Hebrew that was neither fully liturgical nor fully native-spoken.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by Zionist historians themselves (Ben-Yehuda, Klausner, later Fellman) and by contemporary scholars of Zionist historiography (e.g., Katznelson, Shavit). Traditionalist scholars corroborate that the 'dead language' trope was the external threat but deny the marketplace reading's empirical adequacy. Palestinian historians (e.g., Tamari, Khalidi) corroborate the Arabic-dominated market ecology but were not consulted in the founding debate.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).
:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the reading's dual nature: genuine sociolinguistic coordination value (low extraction) plus nationalist narrative capture (high extraction). Suppression (0.38) peaked at 1948 when state institutions enforced the narrative through education and commemoration, then declined but persists in curriculum and funding structures. Theater ratio (0.28) indicates the empirical market pidgin evidence is real but selectively framed — the pidgin existed but its characterization as 'Hebrew's continuous life' rather than 'Arabic-market contact variety' is the performative layer. Accessibility collapse (0.45) is moderate: alternative readings persist in specialized domains but are marginalized in mainstream Israeli discourse. Resistance (0.55) is substantial from all three victim groups, each with different power and exit profiles.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences this as a genuine empirical correction to antisemitic and traditionalist distortions — the marketplace pidgin is real, documented, and functionally vital. The payer seats experience it as an ideological imposition that redefines their legitimate domains (liturgical continuity, native-speaker criterion) out of existence. The excluded seat experiences it as epistemic violence — their actual multilingual market practices are appropriated as 'Hebrew's life.' The observer seat sees a real empirical phenomenon (market pidgin) framed by a contest over legitimacy. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist nationalist historians are the agenda-setters (d near 0.0 — they benefit from and control the constraint). Functionalist sociolinguists are beneficiaries (d ~0.2 — they gain theoretical utility without bearing nationalist costs). Traditionalist liturgical scholars and structuralist generational linguists are payers (d ~0.7-0.8 — their frameworks are actively marginalized, though they retain some institutional footholds). Palestinian Arabic market speakers are excluded (d ~0.9 — their erasure is structural, not incidental). Contemporary observers sit at analytical distance (d=0.5). The derivation follows from beneficiary/victim declarations plus exit options: institutional actors with arbitrage exit (historians) get low d; organized but constrained actors (traditionalists) get high d; powerless trapped actors (Palestinian speakers) get highest d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (countering 'dead language' trope while bypassing liturgical-only traditionalism) was live in 1880-1948. Post-1948, with Hebrew as a native-spoken national language, the marketplace reading's original coordinating function is partially obsolete — but it persists because it legitimizes the state's origin story. This is mandatrophy: the mandate (countering the dead-language trope) has been superseded by facts on the ground (millions of native speakers), but the constraint persists because it now serves a new extraction function (legitimizing the 'continuous adaptation' narrative against Palestinian claims and internal critical scholarship). The reading does not declare mandatrophy_resolved because the constraint still performs work for the agenda-setters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_underdetermination,
    'Does the ''hebrew_linguistic_life'' kernel admit a single coherent framing, or do the three readings constitute fundamentally different kernels (vitality-as-function, vitality-as-transmission, vitality-as-acquisition) that only appear to contest because of the shared label?',
    'Test whether the three readings'' operational criteria can be simultaneously satisfied by a single language state, or whether they identify mutually exclusive conditions. If mutually exclusive, they are rival framings of one kernel; if compatible, they measure different dimensions.',
    'If the kernel fragments into multiple kernels, this reading''s extraction from the others is illusory — they never shared a referent. The contest would be a category error. If unitary kernel, the extraction is real and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three readings contest one kernel or occupy different kernels sharing a label.').

omega_variable(
    market_pidgin_empirical_adequacy,
    'Was the Jerusalem market variety a modified Medieval Hebrew pidgin functioning as Hebrew''s continuous life, or was it an Arabic-dominated contact variety with Hebrew lexical elements — a ''Hebrew pidgin'' only in retrospect?',
    'Detailed sociolinguistic reconstruction of pre-1880 Jerusalem market language using traveler accounts, merchant records, and comparative pidgin/creole studies. Determine whether Hebrew was the matrix language or a lexical contributor to an Arabic-based pidgin.',
    'If the latter, the marketplace reading''s empirical anchor collapses — the ''continuous Hebrew life'' claim becomes a retrospective projection. Extraction would be reclassified as primarily narrative fabrication rather than selective framing of real evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_pidgin_empirical_adequacy, empirical, 'Whether the historical market variety supports the reading''s core empirical claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of the liturgical and generational readings structural (institutional gatekeeping, funding, curriculum) or internalized (scholars in those traditions adopting the marketplace reading''s premises as ''modern'' and ''scientific'')?',
    'Track citation patterns, hiring data, and self-identification of scholars in the three traditions over time. Post-1990s, if traditionalist and generational scholars increasingly cite the marketplace reading as the default, internalization is confirmed.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint reproduces itself through the victims'' own epistemic adoption. This would increase the constraint''s extractiveness score for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of rival readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1880, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hlv_mpr_tr_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(hlv_mpr_tr_t1910, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1910, 0.18).
narrative_ontology:measurement(hlv_mpr_tr_t1948, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1948, 0.42).
narrative_ontology:measurement(hlv_mpr_tr_t1967, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(hlv_mpr_tr_t1990, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(hlv_mpr_tr_t2024, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(hlv_mpr_be_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(hlv_mpr_be_t1910, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1910, 0.28).
narrative_ontology:measurement(hlv_mpr_be_t1948, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1948, 0.52).
narrative_ontology:measurement(hlv_mpr_be_t1967, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1967, 0.48).
narrative_ontology:measurement(hlv_mpr_be_t1990, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(hlv_mpr_be_t2024, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hlv_mpr_su_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1880, 0.1).
narrative_ontology:measurement(hlv_mpr_su_t1910, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1910, 0.22).
narrative_ontology:measurement(hlv_mpr_su_t1948, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(hlv_mpr_su_t1967, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(hlv_mpr_su_t1990, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(hlv_mpr_su_t2024, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__marketplace_pidgin_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, zionist_historiography_origin_narrative).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, palestinian_nakba_counter_narrative).

% DUAL FORMULATION NOTE:
% This constraint is one member of the hebrew_linguistic_life constraint family (kernel_id: hebrew_linguistic_life). The three readings decompose the colloquial label 'Hebrew is a living language' into structurally distinct claims with different ε values: liturgical_preservation_reading (ε≈0.15, mountain-like), marketplace_pidgin_reading (ε≈0.42, tangled_rope), native_generational_reading (ε≈0.25, rope). They are linked by network.affects_constraints. The marketplace reading's ε is higher because it carries nationalist narrative weight; the liturgical reading's ε is lower because it makes no vernacular claim; the generational reading's ε is moderate because it sets a clear but exclusionary standard.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__marketplace_pidgin_reading, institutional, 0.05).
constraint_indexing:directionality_override(hebrew_linguistic_life__marketplace_pidgin_reading, organized, 0.75).
constraint_indexing:directionality_override(hebrew_linguistic_life__marketplace_pidgin_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
