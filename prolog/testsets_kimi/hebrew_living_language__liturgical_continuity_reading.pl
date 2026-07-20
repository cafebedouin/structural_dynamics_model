% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew Liturgical Continuity Reading
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical_continuity reading of the
 *   hebrew_living_language kernel: the claim that Hebrew remains a living
 *   language through unbroken liturgical recitation and textual study across
 *   Jewish diaspora communities. Unlike the native_generation reading (which
 *   holds that Hebrew became living only with Modern Hebrew native speech) or
 *   the literary_revival reading (which privileges Haskalah written
 *   competence), this reading asserts that memorized recitation and sacred
 *   study suffice for linguistic continuity. The structural profile is
 *   low-extraction, voluntary, and identity-coordinated.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: Primary beneficiary (organized/global/identity_locked) â gains communal continuity and shared liturgical practice
 *   - rabbinic_transmitter_class: Agenda-setter (organized/global/identity_locked) â administers liturgical norms and textual interpretation
 *   - modern_linguistic_observers: Analytical observer (analytical/global) â disputes the liveness criterion from empirical linguistics
 *   - modern_hebrew_speakers: Secondary beneficiary (organized/national/mobile) â inherits and partly overshadows the liturgical continuity frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.1).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew Liturgical Continuity Reading").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '647d91df-3e0a-420c-b6b0-d44cb49fc211').
narrative_ontology:cs_kernel_codification('647d91df-3e0a-420c-b6b0-d44cb49fc211', fixed_text).
narrative_ontology:cs_authority_grounding('647d91df-3e0a-420c-b6b0-d44cb49fc211', lineage).
narrative_ontology:cs_interpretation_layer_present('647d91df-3e0a-420c-b6b0-d44cb49fc211').
narrative_ontology:cs_reading_relation('647d91df-3e0a-420c-b6b0-d44cb49fc211', hebrew_living_language__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('647d91df-3e0a-420c-b6b0-d44cb49fc211', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_axiom('647d91df-3e0a-420c-b6b0-d44cb49fc211', foundational, liturgical_continuity_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(liturgical_continuity_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('647d91df-3e0a-420c-b6b0-d44cb49fc211', liturgical_continuity_constitutes_linguistic_life, conventional).
narrative_ontology:cs_axiom('647d91df-3e0a-420c-b6b0-d44cb49fc211', foundational, memorized_recitation_preserves_generative_capacity).
narrative_ontology:cs_axiom_status(memorized_recitation_preserves_generative_capacity, holdable).
narrative_ontology:cs_axiom_grounding('647d91df-3e0a-420c-b6b0-d44cb49fc211', memorized_recitation_preserves_generative_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('647d91df-3e0a-420c-b6b0-d44cb49fc211', diaspora_liturgical_continuity).
narrative_ontology:cs_drift_state('647d91df-3e0a-420c-b6b0-d44cb49fc211', modern_hebrew_vernacular_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('647d91df-3e0a-420c-b6b0-d44cb49fc211', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, modern_hebrew_speakers).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, hebrew_unbroken_continuity_thesis).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, liturgical_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain collective identity, religious practice, and textual continuity across dispersed geographies through shared Hebrew liturgy and Torah study; participation is voluntary but deeply identity-bound, with exit requiring communal and religious rupture.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).

% Preserves and transmits liturgical Hebrew norms, interprets sacred texts, and authoritatively maintains the claim that unbroken recitation constitutes continuous linguistic life; frames adherence as religious obligation and departure as communal loss.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, rabbinic_transmitter_class, agenda_setter,
    organized, generational, identity_locked, global).

% Classify languages by native-speaker and daily-generative-use criteria; many dispute that memorized liturgical recitation suffices for 'living language' status, creating an external epistemic challenge to the continuity claim.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, modern_linguistic_observers, observer,
    analytical, biographical, analytical, global).

% Inherit the lexical and symbolic continuity preserved by centuries of liturgical maintenance; their native generative competence exists alongside and partly overshadows the liturgical continuity frame, altering its social significance.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, modern_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared sacred language and textual corpus across geographically dispersed Jewish communities, enabling common liturgical practice and rabbinic discourse without requiring political unification, native daily speech, or a shared secular language.
% TRANSFER_FUNCTION: Moves communal time, educational investment, and identity commitment from diaspora Jewish communities into Hebrew textual and liturgical competence; transfers symbolic capital of 'unbroken tradition' to the communities and interpretive class that maintain it.
% ABSENT_VOICES: Secular Jews who abandoned liturgical practice and linguists who classify Hebrew as 'revived' rather than 'continuously living' are largely absent from the traditional liturgical framework's self-accounting; their exclusion shapes the unanimity appearance of the continuity claim.
% DISAPPEARANCE_RATIONALE: If the liturgical continuity arrangement vanished, diaspora Jewish communities would lose their primary shared sacred language frame; prayer and study would fragment into vernaculars, the claim of unbroken Hebrew life would dissolve, and communal boundaries would reorganize around other cohesion mechanisms.
% FOUNDING_PROBLEM: Jewish communities in diaspora needed to maintain religious cohesion, textual access, and collective identity across vast geographic distances without a shared secular language, political state, or native speech community.
% FOUNDING_PROBLEM_CORROBORATION: Jewish historians and sociologists outside the rabbinic beneficiary set document the diaspora survival problem; secular Zionist historians corroborate the problem but attest it was addressed through state-building and native-language revival rather than liturgical continuity alone.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).
:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15 at interval end) because the constraint operates through voluntary symbolic participation without material rent transfer. Suppression is minimal (0.08) because persistence relies on identity-based social expectation rather than coercion. Theater_ratio is low (0.10) because liturgical recitation remains functionally central to worship rather than performed for external legitimation. accessibility_collapse (0.35) reflects that vernacular alternatives exist but carry high identity cost; resistance (0.05) is negligible because the practice is internally motivated. The flat measurement trajectories confirm stable, low-extraction operation over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (rabbinic transmitter class) experiences the constraint as sacred obligation and genealogical continuity; beneficiary seats experience it as identity maintenance and communal belonging; the analytical observer seat experiences it as an empirical claim about linguistic status subject to falsification by native-speaker criteria. The divergence is epistemic and definitional rather than extractive â no seat bears significant cost, and the engine computes low effective extraction across all positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive directionality: diaspora_jewish_communities and modern_hebrew_speakers both sit near the beneficiary end (low d). The rabbinic_transmitter_class, while not formally declared a beneficiary, derives low d from its structural subsidy via prestige and authority continuity. Modern_linguistic_observers sit at symmetric analytical distance (d â 0.5). No victim group is declared, yielding no high-d target seats â consistent with the rope classification and the voluntary-participation structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification prevents mislabeling: the constraint is not a mountain because liturgical practice is a constructed social arrangement, not a natural law; it is not a piton because the liturgical function remains genuinely performed and identity-sustaining rather than theatrically maintained; it is not a snare because participation is voluntary and no victim set is identifiable. The low theater_ratio and stable base_extractiveness confirm functional rather than inertial persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_vs_native_liveness_criterion,
    'Does liturgical recitation without native daily speech constitute linguistic ''life,'' or is native generative competence required?',
    'Comparative linguistic analysis of Hebrew''s structural evolution under liturgical preservation versus native revival, and historiographic consensus on whether ''living'' is a categorical or scalar predicate.',
    'If native competence is required, the liturgical continuity reading would be reclassified as a false summit (mountain-like claim masking constructed coordination); if liturgical sufficiency holds, the reading remains a low-extraction rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vs_native_liveness_criterion, conceptual, 'Contested definition of language liveness underlying the kernel').

omega_variable(
    participation_voluntariness,
    'Is participation in liturgical Hebrew genuinely voluntary, or does communal identity fusion constitute soft coercion that exceeds the surface metrics?',
    'Sociological measurement of exit rates and post-exit community attachment among Jews who abandon liturgical practice; comparison with other identity-bound language maintenance regimes.',
    'If identity fusion creates structural lock-in, effective suppression and extractiveness are higher than surface metrics suggest, potentially shifting classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_voluntariness, empirical, 'Whether identity-bound participation is voluntary or coerced').

omega_variable(
    kernel_reading_position,
    'This constraint is the liturgical_continuity reading of the hebrew_living_language kernel; how would classification change if the literary_revival or native_generation reading were adopted instead?',
    'Cross-reading comparison: the literary_revival reading would raise extractiveness (Haskalah intellectual class as concentrated beneficiary) and shift coordination type; the native_generation reading would treat the liturgical mechanism as a dead or superseded arrangement (piton or scaffold candidate).',
    'Sibling readings alter the beneficiary structure, victim set, and temporal boundaries of the constraint; this reading''s low extraction depends on accepting liturgical continuity as non-extractive symbolic preservation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel reading sibling structural differences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hebr_tr_t10, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(hebr_tr_t20, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(hebr_tr_t30, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hebr_be_t10, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(hebr_be_t20, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(hebr_be_t30, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 30, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(hebr_su_t10, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 10, 0.06).
narrative_ontology:measurement(hebr_su_t20, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 20, 0.07).
narrative_ontology:measurement(hebr_su_t30, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 30, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hebrew_living_language kernel, decomposed from the colloquial label 'Hebrew is a living language' per the epsilon-invariance principle. The liturgical continuity reading isolates the claim that unbroken liturgical recitation suffices for linguistic life; the literary revival reading isolates Haskalah generative written competence; the native generation reading isolates native daily speech as the necessary criterion. Each has distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
