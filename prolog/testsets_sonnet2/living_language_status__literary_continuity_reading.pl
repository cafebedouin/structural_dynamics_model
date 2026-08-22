% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Literary-Productivity Standard of Hebrew Vitality (Haskalah Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested 'living language'
 *   kernel: the literary-continuity criterion advanced by Haskalah maskilim
 *   and later secular Hebrew writers, which holds that a language is alive if
 *   it remains a productive medium for new literary and intellectual work,
 *   irrespective of whether it is anyone's mother tongue. Under this
 *   criterion, Haskalah periodicals (Hame'assef and successors) and the
 *   modern Hebrew literary revival (Mapu, Smolenskin, Y.L. Gordon, and later
 *   Bialik-era writers before native transmission took hold) serve as the
 *   evidentiary base for vitality. This is one of three sibling readings of
 *   the same kernel — the liturgical-preservation reading and the
 *   native-generation reading are separate constraint stories with different
 *   beneficiary/victim structures and different ε values, linked here via
 *   network edges, not folded into this one.
 *
 * KEY AGENTS:
 *   - maskilim_literary_intelligentsia: agenda_setter/beneficiary (organized/arbitrage) — defines and profits from the standard
 *   - secular_hebrew_writers: beneficiary (moderate/mobile) — gains cultural authority from the standard's acceptance
 *   - illiterate_yiddish_speaking_masses: payer (powerless/trapped) — excluded from the vitality definition entirely
 *   - non_literary_traditional_communities: payer (powerless/trapped) — their liturgical relationship is demoted to 'mere preservation'
 *   - rabbinic_authorities: excluded (institutional/constrained) — competing definitional authority shut out of the frame
 *   - cultural_historians: observer (analytical/analytical) — reconstructs the contest after the fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.28).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.32).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Literary-Productivity Standard of Hebrew Vitality (Haskalah Reading)").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, 'f9c72c8d-16c2-46ba-a302-947ba59f4f16').
narrative_ontology:cs_kernel_codification('f9c72c8d-16c2-46ba-a302-947ba59f4f16', distributed).
narrative_ontology:cs_authority_grounding('f9c72c8d-16c2-46ba-a302-947ba59f4f16', practice).
narrative_ontology:cs_interpretation_layer_present('f9c72c8d-16c2-46ba-a302-947ba59f4f16').
narrative_ontology:cs_reading_relation('f9c72c8d-16c2-46ba-a302-947ba59f4f16', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9c72c8d-16c2-46ba-a302-947ba59f4f16', living_language_status__native_generation_reading, influences).
narrative_ontology:cs_axiom('f9c72c8d-16c2-46ba-a302-947ba59f4f16', foundational, literary_productivity_sufficient_for_vitality).
narrative_ontology:cs_axiom_status(literary_productivity_sufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('f9c72c8d-16c2-46ba-a302-947ba59f4f16', literary_productivity_sufficient_for_vitality, conventional).
narrative_ontology:cs_axiom('f9c72c8d-16c2-46ba-a302-947ba59f4f16', secondary, native_speaker_status_irrelevant_to_vitality).
narrative_ontology:cs_axiom_status(native_speaker_status_irrelevant_to_vitality, holdable).
narrative_ontology:cs_axiom_grounding('f9c72c8d-16c2-46ba-a302-947ba59f4f16', native_speaker_status_irrelevant_to_vitality, conventional).
narrative_ontology:cs_reference_frame('f9c72c8d-16c2-46ba-a302-947ba59f4f16', haskalah_literary_revival_standard).
narrative_ontology:cs_drift_state('f9c72c8d-16c2-46ba-a302-947ba59f4f16', post_ben_yehuda_native_revival_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9c72c8d-16c2-46ba-a302-947ba59f4f16', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_literary_intelligentsia).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, secular_hebrew_writers).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_yiddish_speaking_masses).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, non_literary_traditional_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit and write for Haskalah periodicals (Hame'assef, later Hashiloach and others), publish poetry, philosophy, and journalism in a deliberately revived literary Hebrew. They set the criterion by which vitality is judged — productive literary output — a criterion under which their own activity is definitionally what counts as life. They can move between Hebrew, Yiddish, German, and Russian as needed; their prestige rests on Hebrew's status as a living intellectual medium.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_literary_intelligentsia, agenda_setter,
    organized, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, maskilim_literary_intelligentsia, beneficiary).

% Novelists, essayists, and poets (Mapu, Smolenskin, Y.L. Gordon) who gain readership, reputation, and a claim to national cultural leadership specifically because Hebrew is asserted to be a living literary vehicle rather than a fossilized liturgical register. Their careers and the legitimacy of a modern Hebrew national culture project depend on this reading being accepted.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, secular_hebrew_writers, beneficiary,
    moderate, biographical, mobile, continental).

% The everyday Jewish population of Eastern Europe speaks Yiddish as a mother tongue and has no access to Hebrew periodicals, which require literacy in a register few outside maskilim circles command. Under the literary-continuity standard, their daily linguistic life counts for nothing toward the vitality question; their exclusion from the literate public is what lets the maskilim claim cultural leadership over a 'living' national language most of the nation cannot read.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_yiddish_speaking_masses, payer,
    powerless, biographical, trapped, regional).

% Hasidic and traditionalist communities that use Hebrew extensively in prayer, study, and ritual but do not produce or consume the secular literary output the maskilim count as evidence of vitality. Their intensive liturgical relationship with Hebrew is reclassified, under this reading, as mere preservation rather than life — a demotion imposed without their participation in defining the standard.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, non_literary_traditional_communities, payer,
    powerless, generational, trapped, regional).

% Traditional religious leadership whose authority rests on Hebrew's sacred, liturgical function. They are not consulted in the maskilim's construction of the literary-productivity criterion and often actively oppose Haskalah periodicals as secularizing threats; their competing claim to define what counts as Hebrew's vitality is excluded from the literary reading's frame entirely.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, rabbinic_authorities, excluded,
    institutional, civilizational, constrained, regional).

% Later scholars of Jewish nationalism and language revival who reconstruct the Haskalah's role in the Hebrew revival narrative, evaluating competing standards of linguistic vitality without a stake in any single one.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a class of literate secular intellectuals around a shared literary field — periodicals, novels, criticism — that requires common technical vocabulary, genre conventions, and a readership large enough to sustain publication; this is a genuine coordination problem for anyone trying to write and be read in a revived literary register.
% TRANSFER_FUNCTION: Moves cultural authority and the power to define national linguistic vitality from traditional liturgical and vernacular-speaking communities to a literate secular intelligentsia, without any material transfer of goods — the transfer is definitional and reputational: who gets to say the language is alive, and on what terms.
% ABSENT_VOICES: The Yiddish-speaking masses and pietist communities whose Hebrew relationship is liturgical or wholly absent are never asked whether literary productivity is the right measure of vitality; rabbinic authorities who would insist liturgical continuity is sufficient (or that secular literary use is itself a corruption) are structurally outside the maskilim's periodical culture and its readership.
% DISAPPEARANCE_RATIONALE: If the literary-continuity standard vanished, the maskilim's specific claim to cultural leadership over the national language question would lose its evidentiary basis, and the field would default to either the liturgical-preservation reading or the emerging native-speaker standard — both already live alternatives. Whether 'the world' rearranges depends on which of the three readings you ask: the maskilim's cultural capital rearranges sharply; ordinary religious and vernacular life continues essentially unchanged, since it never depended on this standard in the first place.
% FOUNDING_PROBLEM: Enlightenment-era Jewish intellectuals needed to argue that Hebrew was not a dead, purely ritual language but a legitimate vehicle for modern secular thought, science, and literature — a claim required both to justify their own literary project and to counter both religious traditionalists (who wanted Hebrew confined to liturgy) and assimilationists (who thought Jews should simply adopt majority vernaculars).
% FOUNDING_PROBLEM_CORROBORATION: Later Zionist cultural historians and linguists outside the Haskalah's own circle (and largely sympathetic to the eventual native-speaker revival in Palestine) corroborate that the literary-continuity claim was real and historically consequential, but many also argue it was a transitional elite phenomenon superseded by the Ben-Yehuda-era native-speaker project; rabbinic critics of the era, from outside the maskilim's own ranks, corroborate that the literary standard was contested precisely because it displaced liturgical authority rather than supplementing it.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, contested).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.28) because this is elite coordination, not resource seizure — the maskilim are not extracting material wealth from the excluded groups. What is extracted is definitional authority: control over what counts as a 'living' national language, a status good with real downstream consequences for cultural leadership and, eventually, national political claims. Suppression (0.32) is moderate: the excluded groups are not actively silenced by this specific standard, but the literacy requirement itself constitutes a structural barrier they cannot cross without institutional investment they were never offered. Theater ratio is low (0.20) and rises only slightly over the interval — the literary output is genuinely produced, not performative; the periodicals are real intellectual work, not a hollow gesture.
 *
 * DIRECTIONALITY LOGIC:
 *   The maskilim and secular writers are declared beneficiaries because the standard is one they authored and that elevates their own activity to definitional status — this yields low d, near the subsidy end. The illiterate masses and traditionalist communities are declared victims not because anything is extracted FROM them materially, but because the standard is constructed such that their extensive relationship with Hebrew (liturgical, oral, ritual) counts for nothing under it; this is a directional cost even without material transfer, which is why the tangled_rope frame (genuine coordination function + asymmetric standing-cost) fits better than a pure rope reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The literary-continuity standard's founding problem — proving Hebrew could be a modern secular medium, against both traditionalist confinement to liturgy and assimilationist abandonment — was genuinely live during the Haskalah period. By the time of Eliezer Ben-Yehuda's native-speaker revival project in Palestine, the problem had substantially shifted: the question was no longer whether Hebrew COULD support new literature, but whether it could become anyone's mother tongue. Applying the literary-continuity standard as though it still settled the 'is Hebrew alive' question after native transmission became live elsewhere would be a mandatrophy — using a Haskalah-era answer to a question the field had already moved past. The founding_problem_status is marked contested rather than dead because, for the specific claim about literary productivity as sufficient evidence of vitality, later Hebrew literary culture (post-1948 Israeli literature) continued to matter as a marker of the language's health independent of native-speaker status, so the standard retains partial ongoing relevance alongside the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literary_output_vs_mass_vitality_gap,
    'Does sustained literary/intellectual production in a language, absent a native-speaking population, constitute genuine linguistic vitality, or is it a specialized register maintained by and for a small literate elite regardless of the language''s broader social status?',
    'Comparative sociolinguistic study of other elite-literary-only language situations (classical Latin humanism, Sanskrit pandit literature) tracking whether literary productivity without demographic base predicts eventual native-speaker revival, permanent elite-register status, or extinction of the literary tradition.',
    'If literary productivity reliably predicts or enables eventual demographic revival, the literary_continuity_reading gains structural credibility as an early-stage vitality indicator; if it does not, the reading functions primarily as elite status-conferral disconnected from the language''s actual survival prospects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literary_output_vs_mass_vitality_gap, conceptual, 'Whether literary productivity is evidence of vitality or a self-serving elite standard decoupled from mass language survival.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the underlying kernel (''a language is living if...'') best modeled as one contested predicate with three candidate fillers, or are ''living,'' ''preserved,'' and ''revived'' actually three different properties that the shared English word ''living'' conflates?',
    'Historical linguistic analysis of whether contemporaries (maskilim, rabbis, Zionist revivalists) treated their disagreement as a dispute over the SAME question or recognized themselves as answering different questions under one label.',
    'If contemporaries treated it as one contested question, the kernel/reading structure with forecloses/coexists_with/influences relations is the right model. If they were talking past each other about genuinely different properties, the ''kernel'' framing itself may be an artifact of retrospective labeling rather than a real object of contemporary dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three readings genuinely contest one kernel or retrospectively conflate three distinct linguistic properties.').

omega_variable(
    elite_beneficiary_persistence,
    'Does the maskilim/secular-writer beneficiary class''s advantage under this standard persist into the present Hebrew literary establishment, or was it a transitional formation dissolved once native transmission (post-1881 Palestine) made the literary and native-speaker standards converge?',
    'Trace institutional continuity between Haskalah-era literary institutions (periodicals, publishing houses) and contemporary Israeli literary establishment; assess whether cultural capital transferred across the transition or was displaced by a new native-speaker-based elite.',
    'Persistence would support treating this as an ongoing tangled_rope; dissolution would support treating the literary_continuity_reading as historically bounded and largely superseded once the native_generation_reading became empirically satisfied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_beneficiary_persistence, empirical, 'Whether the Haskalah-era beneficiary class''s advantage persisted or was absorbed into a later native-speaker-based establishment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 1783, 1917).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1783, living_language_status__literary_continuity_reading, theater_ratio, 1783, 0.12).
narrative_ontology:measurement(livi_tr_t1810, living_language_status__literary_continuity_reading, theater_ratio, 1810, 0.14).
narrative_ontology:measurement(livi_tr_t1840, living_language_status__literary_continuity_reading, theater_ratio, 1840, 0.16).
narrative_ontology:measurement(livi_tr_t1870, living_language_status__literary_continuity_reading, theater_ratio, 1870, 0.18).
narrative_ontology:measurement(livi_tr_t1900, living_language_status__literary_continuity_reading, theater_ratio, 1900, 0.19).
narrative_ontology:measurement(livi_tr_t1917, living_language_status__literary_continuity_reading, theater_ratio, 1917, 0.2).

% Extraction over time
narrative_ontology:measurement(livi_be_t1783, living_language_status__literary_continuity_reading, base_extractiveness, 1783, 0.18).
narrative_ontology:measurement(livi_be_t1810, living_language_status__literary_continuity_reading, base_extractiveness, 1810, 0.2).
narrative_ontology:measurement(livi_be_t1840, living_language_status__literary_continuity_reading, base_extractiveness, 1840, 0.23).
narrative_ontology:measurement(livi_be_t1870, living_language_status__literary_continuity_reading, base_extractiveness, 1870, 0.25).
narrative_ontology:measurement(livi_be_t1900, living_language_status__literary_continuity_reading, base_extractiveness, 1900, 0.27).
narrative_ontology:measurement(livi_be_t1917, living_language_status__literary_continuity_reading, base_extractiveness, 1917, 0.28).

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
% This story is one of three sibling readings of the living_language_status kernel, decomposed per the ε-invariance principle: literary_continuity_reading (this file, low ε, elite literary coordination), liturgical_preservation_reading (separate file, different beneficiary/victim structure centered on religious authority), and native_generation_reading (separate file, likely different ε given its exclusionary force against both literary and liturgical claims). Each carries its own extractiveness, beneficiaries, victims, and classification. The literary_continuity_reading historically preceded and partially enabled conditions for the native_generation_reading (Haskalah literary revival supplied vocabulary and prestige later drawn on by Ben-Yehuda's native-speaker project), which is why this file's affects_constraints edge points toward native_generation_reading as a downstream-influenced sibling, alongside the liturgical_preservation_reading it directly displaces in cultural authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
