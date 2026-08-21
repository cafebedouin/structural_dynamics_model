% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid pragmatic' reading of the
 *   Manifesto, a pivotal document in a religious institution's history. This
 *   reading views the Manifesto as a strategic institutional adaptation,
 *   where prophetic authority was deployed to manage an exogenous legal
 *   crisis. The goal was to preserve core theological commitments through
 *   scope ambiguity, allowing for federal compliance without explicit
 *   doctrinal reversal. This reading acknowledges both external pressure and
 *   internal agency, positioning the institutional leadership as navigating a
 *   complex, contested terrain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.65).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.7).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'cb6ad448-75a5-427e-af74-d9b6c29e6335').
narrative_ontology:cs_kernel_codification('cb6ad448-75a5-427e-af74-d9b6c29e6335', formalized).
narrative_ontology:cs_authority_grounding('cb6ad448-75a5-427e-af74-d9b6c29e6335', lineage).
narrative_ontology:cs_interpretation_layer_present('cb6ad448-75a5-427e-af74-d9b6c29e6335').
narrative_ontology:cs_reading_relation('cb6ad448-75a5-427e-af74-d9b6c29e6335', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb6ad448-75a5-427e-af74-d9b6c29e6335', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('cb6ad448-75a5-427e-af74-d9b6c29e6335', foundational, prophetic_adaptation_preserves_covenant).
narrative_ontology:cs_axiom_status(prophetic_adaptation_preserves_covenant, holdable).
narrative_ontology:cs_axiom_grounding('cb6ad448-75a5-427e-af74-d9b6c29e6335', prophetic_adaptation_preserves_covenant, theological).
narrative_ontology:cs_axiom('cb6ad448-75a5-427e-af74-d9b6c29e6335', secondary, institutional_survival_enables_future_revelation).
narrative_ontology:cs_axiom_status(institutional_survival_enables_future_revelation, holdable).
narrative_ontology:cs_axiom_grounding('cb6ad448-75a5-427e-af74-d9b6c29e6335', institutional_survival_enables_future_revelation, instrumental).
narrative_ontology:cs_reference_frame('cb6ad448-75a5-427e-af74-d9b6c29e6335', divinely_guided_institutional_pragmatism).
narrative_ontology:cs_drift_state('cb6ad448-75a5-427e-af74-d9b6c29e6335', contemporary_pluralistic_society, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('cb6ad448-75a5-427e-af74-d9b6c29e6335', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_purists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Navigates the tension between federal law and theological doctrine, deploying prophetic authority to manage crisis while preserving institutional integrity and core commitments. Benefits from maintaining both compliance and doctrinal flexibility.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Bear the interpretive uncertainty and legitimacy ambiguity arising from the Manifesto. They are expected to reconcile the shift with their faith, often experiencing cognitive dissonance or a sense of betrayal, but remain committed due to identity fusion with the institution.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    moderate, biographical, identity_locked, local).

% Struggle to reconcile the Manifesto with prior, deeply held theological commitments. They perceive a compromise of divine law for worldly expediency, but their options are limited to internal dissent or painful exit from a core identity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_purists, payer,
    powerless, generational, constrained, regional).

% Exerted external pressure that precipitated the Manifesto. Its role is now to monitor compliance with federal law, but it does not directly interpret the theological implications of the institutional shift.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional adaptation to exogenous legal pressure, allowing the institution to continue operating within federal law while preserving a framework for its core theological commitments, albeit with interpretive flexibility.
% TRANSFER_FUNCTION: Transfers the burden of reconciling conflicting mandates (federal law vs. theological doctrine) from institutional leadership to individual members, who must navigate interpretive ambiguity and maintain loyalty.
% ABSENT_VOICES: Those who left the institution due to the Manifesto's perceived compromise of doctrine are absent. They would argue that the 'adaptation' was a capitulation that sacrificed truth for institutional survival, and that the current arrangement lacks true legitimacy.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its interpretive framework vanished, the institution would face immediate legal challenges, internal schism over doctrinal purity, and a crisis of authority, forcing a fundamental reorganization of its structure and theological claims.
% FOUNDING_PROBLEM: The institution faced an existential crisis: federal legal mandates directly contradicted a core theological practice, threatening its legal status and continued existence.
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership attests the problem is live, as the need to balance legal compliance with doctrinal integrity is ongoing. External legal scholars and historians corroborate the historical pressure but view the 'solution' as a pragmatic compromise rather than a divine mandate.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate because while the leadership gains flexibility, the rank-and-file members bear the cost of interpretive uncertainty and cognitive dissonance. Suppression (0.70) is significant, as internal dissent is managed through appeals to authority and loyalty, and exit is difficult due to identity-lock. The theater ratio (0.40) reflects the performative aspect of maintaining a narrative of divine guidance while pragmatically adapting to external pressures. The claimed type is 'tangled_rope' because it serves a genuine coordination function (institutional survival) but involves asymmetric extraction from members.
 *
 * PERSPECTIVAL GAP:
 *   Institutional leadership experiences this as a necessary, divinely guided adaptation, preserving the institution. Rank-and-file members, however, experience it as a source of interpretive strain and a demand for loyalty that overrides clear doctrinal consistency. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is the primary beneficiary (d=0.0-0.2) as they maintain authority and institutional continuity. Rank-and-file members and doctrinal purists are victims (d=0.7-0.9) as they bear the costs of ambiguity and compromise. The federal government is an external force, not directly a stakeholder in the internal constraint's operation, but its pressure is the exogenous driver.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a 'snare' by acknowledging the genuine coordination problem (institutional survival under legal threat). However, it also prevents mislabeling it as a 'rope' by highlighting the significant, asymmetric extraction from members and the active suppression of dissent. The 'tangled_rope' classification captures the hybrid nature of both coordination and extraction, with the mandate evolving from crisis management to ongoing interpretive control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_ambiguity_resolution,
    'To what extent is the ''scope ambiguity'' a deliberate, functional mechanism for adaptation versus an unresolved internal contradiction?',
    'Analysis of internal institutional communications and theological discourse over time: does the ambiguity persist as a stable interpretive strategy, or do attempts to clarify it lead to internal conflict and schism?',
    'If deliberate and functional, the constraint''s coordination aspect is stronger. If an unresolved contradiction, the extractiveness from members (bearing the cognitive load) is higher, pushing it closer to a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_ambiguity_resolution, conceptual, 'Nature of the Manifesto''s scope ambiguity.').

omega_variable(
    identity_lock_strength,
    'How strong is the identity-lock mechanism for rank-and-file members, and what proportion of their commitment is due to genuine belief versus social/familial pressure?',
    'Longitudinal studies of ex-members'' post-exit experiences, and surveys of current members'' reasons for adherence, differentiating between theological conviction and social embeddedness.',
    'If identity-lock is primarily social/familial, the suppression metric is effectively higher, as exit costs are non-theological. If primarily genuine belief, the constraint is more robustly a tangled rope, as members are coordinated by shared conviction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Strength and nature of identity-lock for members.').

omega_variable(
    reading_framing_underdetermination,
    'Is the ''hybrid pragmatic'' framing the most defensible interpretation, or would an ''exogenous override'' or ''endogenous reinterpretation'' framing better capture the constraint''s true nature?',
    'Comparative analysis of historical evidence, theological texts, and institutional actions against the core premises of each reading. Which framing best accounts for the observed outcomes and internal dynamics?',
    'If the exogenous override reading is more accurate, the constraint''s suppression is higher and its internal legitimacy lower. If the endogenous reinterpretation reading is more accurate, the constraint''s extractiveness is lower, as members are genuinely coordinated by perceived divine will. This would shift the classification towards a snare or a rope, respectively.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Alternative framings of the Manifesto''s meaning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_commitment_legitimacy' kernel. It is linked to sibling readings 'marriage_commitment_legitimacy__exogenous_override_reading' and 'marriage_commitment_legitimacy__endogenous_reinterpretation_reading' via cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
