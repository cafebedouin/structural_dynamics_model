% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia: Islamic Sovereignty Reading
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint represents the 'Islamic Sovereignty' reading of the Hagia
 *   Sophia's status, asserting its legitimacy from the 1453 Ottoman conquest
 *   and continuous Islamic endowment (waqf), making it sovereign Islamic
 *   worship space under Turkish state authority. This reading was formally
 *   re-established by executive decree in 2020, reversing its 1934 museum
 *   status. It is a Tangled Rope because it coordinates a national-religious
 *   identity while extracting costs from non-Muslim visitors, international
 *   heritage bodies, and secularist Turks through active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.75).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia: Islamic Sovereignty Reading").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, '6c6b4d34-7a7e-4916-8a80-bf0bf99a918a').
narrative_ontology:cs_kernel_codification('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a', fixed_text).
narrative_ontology:cs_authority_grounding('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a', lineage).
narrative_ontology:cs_interpretation_layer_present('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a').
narrative_ontology:cs_reading_relation('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_reading_relation('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a', foundational, conquest_establishes_perpetual_waqf_sovereignty).
narrative_ontology:cs_axiom_status(conquest_establishes_perpetual_waqf_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a', conquest_establishes_perpetual_waqf_sovereignty, theological).
narrative_ontology:cs_axiom('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a', foundational, national_sovereignty_overrides_international_heritage_claims).
narrative_ontology:cs_axiom_status(national_sovereignty_overrides_international_heritage_claims, holdable).
narrative_ontology:cs_axiom_grounding('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a', national_sovereignty_overrides_international_heritage_claims, conventional).
narrative_ontology:cs_reference_frame('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a', ottoman_conquest_waqf_status).
narrative_ontology:cs_drift_state('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a', contemporary_international_heritage_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6c6b4d34-7a7e-4916-8a80-bf0bf99a918a', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolically).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is moderate-high, reflecting the political consolidation and religious identity signal gained by the Turkish state and its constituency, balanced against the diplomatic friction and cultural costs imposed. Suppression (0.75) is high due to the active legal and political enforcement required to maintain this status against international and internal opposition. The theater ratio (0.20) is low, as the conversion is a genuinely functional act of religious and political reassertion, not merely performative maintenance. The metrics reflect the post-2020 reality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the AKP and its supporters, this is a legitimate act of national and religious self-determination, a restoration of historical rights. From the perspective of non-Muslim visitors, UNESCO, and secularist Turks, it is an act of exclusion and a violation of universal heritage principles. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The AKP political coalition and the Turkish Islamic constituency are clear beneficiaries, gaining political and religious affirmation (low directionality). The broader Sunni Ummah benefits symbolically. Non-Muslim visitors, UNESCO, and secularist Turks are targets, bearing costs of access restrictions, denied jurisdiction, and ideological defeat (high directionality). The constraint subsidizes the beneficiaries while extracting from the targets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    waqf_legitimacy_ambiguity,
    'Is the 1453 waqf (Islamic endowment) a perpetually binding legal instrument that overrides subsequent national and international legal frameworks, or is its authority subject to modern legal and heritage norms?',
    'International legal arbitration or a definitive ruling by a globally recognized court on the precedence of historical religious endowments versus contemporary national and international law.',
    'If perpetually binding, the Islamic sovereignty reading gains stronger legal grounding, potentially reclassifying aspects of its enforcement as Mountain-like. If subject to modern norms, its legal basis weakens, reinforcing its classification as a constructed, actively enforced constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(waqf_legitimacy_ambiguity, conceptual, 'Ambiguity of historical waqf legitimacy in modern legal context.').

omega_variable(
    political_vs_religious_motivation,
    'To what extent was the 2020 conversion primarily a political act to consolidate domestic support, versus a genuine religious imperative to restore worship space?',
    'Analysis of internal government communications, electoral outcomes post-conversion, and independent sociological studies of religious sentiment versus political opportunism.',
    'If primarily political, the extractiveness metric is more accurately attributed to political rent-seeking. If primarily religious, the coordination function for the Islamic constituency is stronger, potentially shifting the balance within the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_religious_motivation, empirical, 'Distinguishing political from religious drivers of the conversion.').

omega_variable(
    international_law_enforceability,
    'What is the actual enforceability of UNESCO''s ''universal heritage'' claims against a sovereign nation''s assertion of religious and national sovereignty over a site within its borders?',
    'Precedent from other cases where UNESCO status conflicted with national sovereignty, or a test case brought before an international court with enforcement powers.',
    'If UNESCO''s claims are largely unenforceable, the ''excluded'' status of the UNESCO regime is reinforced, and the Turkish state''s suppression of these claims is more effective. If enforceable, the constraint faces greater external resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_enforceability, empirical, 'Enforceability of international heritage law against national sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 2020, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(hagi_tr_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2022, 0.22).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(hagi_be_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2022, 0.67).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(hagi_su_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2022, 0.73).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hagia_sophia_substrate' kernel. Its Islamic sovereignty claim directly impacts the viability and legitimacy of the universal heritage and orthodox restitution readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
