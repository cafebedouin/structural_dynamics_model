% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__functional_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__functional_equivalence_reading, []).

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
 *   constraint_id: kjv_text_1611__functional_equivalence_reading
 *   human_readable: Functional Equivalence Reading of the KJV Kernel — Complementary Translation Ecology
 *   domain: religious_studies/textual_criticism/theology
 *
 * SUMMARY:
 *   This story instantiates the functional-equivalence reading of the KJV
 *   kernel: the claim that the 1611 King James Version and modern English
 *   translations occupy complementary, non-competing roles — the KJV for
 *   literary and historical value, modern versions for comprehension and
 *   updated scholarship — rather than one text holding sole legitimate
 *   authority. This is a distinct constraint from the exclusive-inspiration
 *   reading (which treats the KJV as the only valid English text and would
 *   classify as a tangled_rope or snare with KJV-only institutions as
 *   concentrated beneficiaries) and from the revisable-translation reading
 *   (which treats the KJV as an improvable draft superseded by better
 *   manuscript evidence, shifting authority toward textual-critical
 *   expertise). All three readings share the same underlying kernel — the
 *   1611 text and its claim to authority — but instantiate structurally
 *   different constraints with different beneficiary sets, different
 *   extraction profiles, and different enforcement postures. This reading has
 *   the lowest suppression and extraction of the three because it does not
 *   require any single text to hold gate-keeping power.
 *
 * KEY AGENTS:
 *   - lay_bible_readers: primary beneficiaries (powerless/mobile) — free translation choice by purpose
 *   - translation_committees: agenda-setters (institutional/mobile) — produce competing, non-exclusive translations
 *   - biblical_scholars: beneficiaries and secondary agenda-setters (organized/arbitrage) — use both KJV and modern texts for different scholarly purposes
 *   - kjv_only_movement_adherents: excluded voice (moderate/identity_locked) — object that pluralism erases a truth claim
 *   - textual_critics: analytical observers — document the kernel's competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__functional_equivalence_reading, 0.22).
domain_priors:suppression_score(kjv_text_1611__functional_equivalence_reading, 0.15).
domain_priors:theater_ratio(kjv_text_1611__functional_equivalence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__functional_equivalence_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__functional_equivalence_reading, "Functional Equivalence Reading of the KJV Kernel — Complementary Translation Ecology").
narrative_ontology:topic_domain(kjv_text_1611__functional_equivalence_reading, "religious_studies/textual_criticism/theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__functional_equivalence_reading, '94c1d0d0-86eb-464c-850b-4dc0e322ff6f').
narrative_ontology:cs_kernel_codification('94c1d0d0-86eb-464c-850b-4dc0e322ff6f', fixed_text).
narrative_ontology:cs_authority_grounding('94c1d0d0-86eb-464c-850b-4dc0e322ff6f', distributed).
narrative_ontology:cs_reading_relation('94c1d0d0-86eb-464c-850b-4dc0e322ff6f', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('94c1d0d0-86eb-464c-850b-4dc0e322ff6f', kjv_text_1611__revisable_translation_reading, coexists_with).
narrative_ontology:cs_axiom('94c1d0d0-86eb-464c-850b-4dc0e322ff6f', foundational, translations_serve_differentiated_nonexclusive_purposes).
narrative_ontology:cs_axiom_status(translations_serve_differentiated_nonexclusive_purposes, holdable).
narrative_ontology:cs_axiom_grounding('94c1d0d0-86eb-464c-850b-4dc0e322ff6f', translations_serve_differentiated_nonexclusive_purposes, instrumental).
narrative_ontology:cs_axiom('94c1d0d0-86eb-464c-850b-4dc0e322ff6f', foundational, no_single_english_text_holds_exclusive_inspired_authority).
narrative_ontology:cs_axiom_status(no_single_english_text_holds_exclusive_inspired_authority, holdable).
narrative_ontology:cs_axiom_grounding('94c1d0d0-86eb-464c-850b-4dc0e322ff6f', no_single_english_text_holds_exclusive_inspired_authority, conventional).
narrative_ontology:cs_reference_frame('94c1d0d0-86eb-464c-850b-4dc0e322ff6f', post_critical_scholarship_translation_ecology).
narrative_ontology:cs_drift_state('94c1d0d0-86eb-464c-850b-4dc0e322ff6f', contemporary_digital_bible_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('94c1d0d0-86eb-464c-850b-4dc0e322ff6f', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__functional_equivalence_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, lay_bible_readers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, biblical_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, translation_committees).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, literature_and_liturgy_communities).
narrative_ontology:constraint_vindicates(kjv_text_1611__functional_equivalence_reading, translation_pluralism_is_theologically_permissible).
narrative_ontology:constraint_vindicates(kjv_text_1611__functional_equivalence_reading, clarity_and_literary_value_are_independent_goods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chooses among KJV, NIV, ESV, NRSV and others depending on purpose — devotional reading, study, memorization, public recitation. No translation is imposed as the only legitimate text; the reader moves freely between them without doctrinal penalty.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, lay_bible_readers, beneficiary,
    powerless, biographical, mobile, global).

% Uses KJV for historical and literary-critical study (its influence on English literature, its manuscript tradition) and modern translations for exegesis grounded in updated manuscript evidence and lexicography. Produces new translations and commentary; benefits from having multiple textual witnesses in circulation rather than a single frozen text.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, biblical_scholars, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__functional_equivalence_reading, biblical_scholars, agenda_setter).

% Denominational and interdenominational bodies that produce and revise translations (NIV, ESV, NRSV, NLT, etc.), each pitched at a different register — formal equivalence, dynamic equivalence, readability. They compete for adoption, not by suppressing rivals but by serving distinct use cases; no committee claims sole legitimate authority over the English text.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, translation_committees, agenda_setter,
    institutional, generational, mobile, global).

% Preserves the KJV for liturgical cadence, literary allusion, and cultural inheritance (weddings, funerals, canonical English prose) while using modern translations for teaching and comprehension. Values both texts for different, non-competing reasons.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, literature_and_liturgy_communities, beneficiary,
    moderate, civilizational, mobile, global).

% Holds that only the KJV is the inspired English text and regards this pluralist reading as a capitulation that erodes doctrinal certainty. Their objection is not represented within the functional-equivalence framework itself — the reading structurally treats their exclusivity claim as one preference among several rather than as a truth claim requiring adjudication.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, kjv_only_movement_adherents, excluded,
    moderate, generational, identity_locked, national).

% Studies manuscript families (Textus Receptus vs. critical text traditions) and translation philosophy comparatively, without a stake in which translation wins broad adoption. Documents how each translation reading construes the underlying kernel differently.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, textual_critics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the real problem of serving heterogeneous purposes — literary/historical study, liturgical continuity, devotional clarity, scholarly exegesis — with one text family instead of forcing all uses through a single translation ill-suited to most of them.
% TRANSFER_FUNCTION: Moves authority away from any single institution or text and distributes it across translation committees, publishers, and reading communities; the corresponding cost is coordination overhead — no single canonical reference point, more interpretive labor per user to select an appropriate translation.
% ABSENT_VOICES: KJV-only adherents are structurally outside this reading's framework: the pluralist stance does not engage their inerrancy claim on its own terms but treats it as one use-preference among several, which is precisely what they object to.
% DISAPPEARANCE_RATIONALE: If this reading of the kernel disappeared, some communities would simply continue behaving as functional pluralists without naming it (world_unchanged for them); others currently restrained only by this reading's legitimating cover would revert toward exclusivist or revisionist framings with real institutional consequences (contested rather than a clean rearrangement).
% FOUNDING_PROBLEM: By the mid-20th century, advances in manuscript discovery (Dead Sea Scrolls, papyri) and linguistic scholarship, combined with declining comprehensibility of Early Modern English for ordinary readers, created pressure to reconcile continued reverence for the KJV's literary/historical status with the practical need for texts modern readers could understand and scholars could critically ground.
% FOUNDING_PROBLEM_CORROBORATION: Attested by linguists and literacy researchers (comprehension studies on archaic English), by textual critics documenting manuscript evidence unavailable in 1611, and by denominational bodies outside any single translation's publishing interest who continue to authorize multiple translations for different liturgical uses.
narrative_ontology:disappearance_verdict(kjv_text_1611__functional_equivalence_reading, contested).
narrative_ontology:founding_problem_status(kjv_text_1611__functional_equivalence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__functional_equivalence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__functional_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__functional_equivalence_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__functional_equivalence_reading_tests).
:- end_tests(kjv_text_1611__functional_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) and declining slightly over the interval because no institution captures rents by gatekeeping access to 'the' authoritative English Bible — authority is distributed across multiple committees and publishers whose competing products serve different needs. Suppression is low (0.15) because exit is genuinely available: a reader dissatisfied with one translation simply picks up another without doctrinal penalty from this reading's own framework (though KJV-only communities impose penalties from outside it). Theater ratio is modest (0.20) reflecting genuine scholarly and liturgical function rather than performative maintenance. Accessibility collapse is low (0.25) — alternatives to any single translation remain fully visible and actively used, which is the defining structural feature of this reading relative to its siblings.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (translation committees) this looks like healthy market coordination among complementary products. From the excluded seat (KJV-only adherents) the same structure looks like an erasure of a truth claim through procedural pluralism — the 'coexistence' framing is itself experienced as suppression of their exclusivist premise, even though no material extraction targets them.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay readers, scholars, and literary/liturgical communities are beneficiaries because the pluralist structure serves their actual, differentiated needs without cost concentrated on any of them. Translation committees are agenda-setters but compete rather than extract — their revenue and legitimacy depend on serving a use case well, not on suppressing rivals. KJV-only adherents are the one group structurally disadvantaged by this reading, but not as economic victims — their loss is doctrinal standing: this reading treats their exclusivity claim as preference rather than as binding truth, which is a real cost to them even though no material extraction occurs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling KJV's literary/historical value with modern comprehension needs and manuscript scholarship) remains live and is corroborated by parties outside any single translation's commercial interest (linguists, textual critics, ecumenical bodies). This blocks a mandatrophy read: the arrangement is not a vestigial structure defending a dead problem, it is an ongoing coordination solution to a persisting differentiation-of-use problem. The reading would only qualify as mandatrophic if the underlying divergence in reader needs and manuscript scholarship disappeared while committees kept producing competing translations out of institutional inertia — no evidence of that here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the functional-equivalence reading a stable resolution of the KJV authority kernel, or a temporary truce that will collapse back toward either exclusive-inspiration or revisable-translation framings under doctrinal or scholarly pressure?',
    'Track denominational statements and seminary curricula over multiple decades: sustained multi-translation endorsement across traditions would support stability; a swing toward mandating single translations (in either direction) would indicate the reading is unstable equilibrium rather than settled resolution.',
    'If unstable, this reading''s low-extraction profile is a snapshot of a contested moment rather than a durable structural fact, and the sibling readings should be weighted as live successor states rather than merely coexisting alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the functional-equivalence reading is a stable resolution or a transitional truce between the exclusivist and revisionist readings.').

omega_variable(
    excluded_voice_corroboration_asymmetry,
    'Does the structural exclusion of KJV-only adherents from this reading''s framework constitute suppression (their truth claim is silenced by definitional fiat) or accurate non-adjudication (the reading simply declines to settle an inerrancy question it is not equipped to settle)?',
    'Compare this reading''s treatment of the exclusivist claim to how it treats other contested theological claims it also declines to adjudicate — consistent non-adjudication across contested claims supports the second reading; selective non-adjudication targeting only the exclusivist claim would support the first.',
    'If suppression, this reading''s low suppression score (0.15) undercounts a real cost borne by KJV-only communities, whose loss is doctrinal rather than material and therefore invisible to material extraction metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_voice_corroboration_asymmetry, conceptual, 'Whether excluding the exclusivist reading from adjudication is suppression or neutral non-adjudication.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__functional_equivalence_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t1950, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(kjv__tr_t1965, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1965, 0.24).
narrative_ontology:measurement(kjv__tr_t1980, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(kjv__tr_t1995, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1995, 0.21).
narrative_ontology:measurement(kjv__tr_t2010, kjv_text_1611__functional_equivalence_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(kjv__tr_t2025, kjv_text_1611__functional_equivalence_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(kjv__be_t1950, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(kjv__be_t1965, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(kjv__be_t1980, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(kjv__be_t1995, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1995, 0.23).
narrative_ontology:measurement(kjv__be_t2010, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 2010, 0.22).
narrative_ontology:measurement(kjv__be_t2025, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 2025, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kjv_text_1611__functional_equivalence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__functional_equivalence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__functional_equivalence_reading, 0.1).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kjv_text_1611 kernel. exclusive_inspiration_reading treats the KJV as sole inspired authority (high extraction/suppression, concentrated beneficiaries in KJV-only institutions). revisable_translation_reading treats the KJV as an improvable draft superseded by manuscript scholarship (authority shifts toward textual-critical expertise; moderate extraction as legacy publishers lose ground). functional_equivalence_reading (this story) decentralizes authority across multiple co-existing translations, producing the lowest extraction and suppression of the three but the highest coordination overhead. All three share one kernel — the 1611 text and its authority claim — and diverge in claimed_type, beneficiary/victim structure, and metrics; per the ε-invariance principle they are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
