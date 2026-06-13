% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hebrew Vitality: Hybrid Continuity Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint represents an analytical reading of Hebrew's
 *   revitalization, positing that liturgical preservation provided a
 *   necessary substrate, but active reconstruction was also required for its
 *   vernacular revival. It is a conceptual framework, not an actionable
 *   constraint, hence its low extractiveness and mountain-like properties. It
 *   synthesizes elements from both the 'liturgical preservation' and 'native
 *   generation' perspectives, arguing for a more complex, hybrid model of
 *   continuity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.1).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.05).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, mountain).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality: Hybrid Continuity Reading").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:emerges_naturally(hebrew_vitality__hybrid_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '009fd246-aee5-462d-b1e3-280d8eaf6fc3').
narrative_ontology:cs_kernel_codification('009fd246-aee5-462d-b1e3-280d8eaf6fc3', distributed).
narrative_ontology:cs_authority_grounding('009fd246-aee5-462d-b1e3-280d8eaf6fc3', expertise).
narrative_ontology:cs_interpretation_layer_present('009fd246-aee5-462d-b1e3-280d8eaf6fc3').
narrative_ontology:cs_reading_relation('009fd246-aee5-462d-b1e3-280d8eaf6fc3', hebrew_vitality__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('009fd246-aee5-462d-b1e3-280d8eaf6fc3', hebrew_vitality__native_daily_reading, influences).
narrative_ontology:cs_axiom('009fd246-aee5-462d-b1e3-280d8eaf6fc3', foundational, vitality_requires_both_substrate_and_reconstruction).
narrative_ontology:cs_axiom_status(vitality_requires_both_substrate_and_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('009fd246-aee5-462d-b1e3-280d8eaf6fc3', vitality_requires_both_substrate_and_reconstruction, empirically_contingent).
narrative_ontology:cs_axiom('009fd246-aee5-462d-b1e3-280d8eaf6fc3', secondary, liturgical_use_is_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(liturgical_use_is_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('009fd246-aee5-462d-b1e3-280d8eaf6fc3', liturgical_use_is_necessary_but_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('009fd246-aee5-462d-b1e3-280d8eaf6fc3', complex_historical_process).
narrative_ontology:cs_drift_state('009fd246-aee5-462d-b1e3-280d8eaf6fc3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('009fd246-aee5-462d-b1e3-280d8eaf6fc3', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, language_revitalization_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, modern_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, liturgical_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars analyze the historical and sociological conditions for language revival, seeking to understand the interplay between preservation and active use. This reading provides a framework for their analysis.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, language_revitalization_scholars, observer,
    analytical, generational, analytical, global).

% These communities maintained Hebrew through religious practice for centuries. While not directly 'paying' for this constraint, their historical role is acknowledged as a necessary substrate, even if insufficient for full vitality. Their identity is deeply tied to this continuity.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_communities, payer,
    organized, generational, identity_locked, global).

% These are individuals who use Hebrew as a living, vernacular language. They benefit from the historical continuity provided by liturgical use, which enabled the later reconstruction efforts that led to modern Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, modern_hebrew_speakers, beneficiary,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an analytical framework for understanding the complex, multi-faceted process of language revitalization, coordinating historical data with linguistic theory to explain how a 'dead' language became 'living' again.
% TRANSFER_FUNCTION: This analytical constraint primarily transfers understanding and explanatory power to scholars and practitioners of language revitalization, rather than material resources.
% ABSENT_VOICES: Extremist purists who might argue for a single, 'pure' path to vitality (either purely liturgical or purely native generation) are implicitly excluded by this hybrid reading, which emphasizes a complex interplay.
% DISAPPEARANCE_RATIONALE: The historical facts of Hebrew's revitalization would remain, but the conceptual framework for understanding them as a hybrid process would be lost, potentially leading to less nuanced approaches in other language revitalization efforts.
% FOUNDING_PROBLEM: The problem of understanding how a language, primarily preserved in religious texts and rituals, could transition to a vibrant, spoken vernacular, challenging simplistic notions of language death and rebirth.
% FOUNDING_PROBLEM_CORROBORATION: Linguists, historians, and sociologists of language, independent of any specific religious or nationalistic agenda, corroborate the complexity of Hebrew's revival and the need for a nuanced understanding beyond single-factor explanations.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_vitality__hybrid_continuity_reading),
    narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   As an analytical synthesis, this constraint has very low extractiveness and suppression; it describes a historical process rather than imposing a rule. Its 'mountain' classification reflects its status as a robust explanatory model that accounts for observed phenomena. The 'beneficiaries' are primarily scholars who gain explanatory power. The metrics are stable over time as this is a historical interpretation, not a dynamic process.
 *
 * PERSPECTIVAL GAP:
 *   This reading attempts to bridge the gap between those who emphasize liturgical continuity and those who emphasize modern reconstruction. It argues that both perspectives are partially correct but insufficient on their own, thus offering a more comprehensive understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Language revitalization scholars are the primary beneficiaries, as this reading provides a powerful analytical tool. Liturgical communities and modern Hebrew speakers are not 'targets' of this constraint but rather historical actors whose roles are explained by it. Their 'payer' and 'beneficiary' roles here reflect their structural position within the historical process this reading describes, not an active extraction by the constraint itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    analytical_vs_actionable,
    'Is this reading primarily an analytical framework for understanding historical processes, or does it imply actionable constraints for contemporary language revitalization efforts?',
    'Analysis of how this reading is applied in practice: if it primarily informs academic discourse, it remains analytical; if it directly shapes policy or funding for revitalization, it becomes actionable.',
    'If actionable, its extractiveness and suppression could rise if it imposes specific, costly requirements on revitalization programs, potentially shifting its classification from Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(analytical_vs_actionable, conceptual, 'Distinction between descriptive analysis and prescriptive action.').

omega_variable(
    liturgical_vs_reconstruction_weighting,
    'What is the precise weighting or relative importance of liturgical preservation versus active reconstruction in achieving Hebrew''s modern vitality?',
    'Further historical and linguistic research, potentially involving counterfactual analysis or comparative studies with other language revitalization efforts.',
    'A stronger emphasis on one factor over the other could align this reading more closely with either the ''liturgical_reading'' or ''native_daily_reading'' siblings, potentially altering its distinctiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vs_reconstruction_weighting, empirical, 'Relative contribution of different factors to language vitality.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''reading'' of the ''hebrew_vitality'' kernel, or is it a meta-analysis that transcends the kernel''s original contest?',
    'Examination of its proponents'' claims: if it directly engages and attempts to resolve the core tension of the kernel, it''s a reading; if it reframes the entire debate from an external perspective, it''s a meta-analysis.',
    'If a meta-analysis, its relationship to the other readings would be one of ''commentary_on'' rather than ''coexists_with'' or ''influences'', altering the network structure and the nature of the contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifies the epistemic status of this reading relative to the core kernel debate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 1800, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1800, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1800, 0.0).
narrative_ontology:measurement(hebr_tr_t1850, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1850, 0.0).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1950, 0.0).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2020, 0.0).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1800, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1800, 0.1).
narrative_ontology:measurement(hebr_be_t1850, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1850, 0.1).
narrative_ontology:measurement(hebr_be_t1900, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(hebr_be_t1950, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(hebr_be_t2000, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(hebr_be_t2020, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2020, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1800, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(hebr_su_t1850, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1850, 0.05).
narrative_ontology:measurement(hebr_su_t1900, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement(hebr_su_t1950, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(hebr_su_t2000, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(hebr_su_t2020, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2020, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
