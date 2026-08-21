% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command (Coercion Visibility Reading)
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story analyzes the 'divine marriage command' kernel from
 *   the 'coercion visibility' reading, which interprets the 1890 Manifesto as
 *   an acknowledged response to federal coercion, with theological legitimacy
 *   derived from institutional survival. This reading closes the M-set gap by
 *   admitting exogenous pressure as a valid input for doctrinal shift, a
 *   position that carries potential legitimacy crises for a divinely-grounded
 *   authority. The constraint is claimed as a Tangled Rope, reflecting its
 *   coordination function (institutional survival) intertwined with
 *   asymmetric extraction (from members adhering to prior doctrine) and
 *   active enforcement (both internal and external).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.45).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.6).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command (Coercion Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, 'a8dc4116-14a5-486c-9793-17274287c53d').
narrative_ontology:cs_kernel_codification('a8dc4116-14a5-486c-9793-17274287c53d', formalized).
narrative_ontology:cs_authority_grounding('a8dc4116-14a5-486c-9793-17274287c53d', lineage).
narrative_ontology:cs_interpretation_layer_present('a8dc4116-14a5-486c-9793-17274287c53d').
narrative_ontology:cs_reading_relation('a8dc4116-14a5-486c-9793-17274287c53d', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8dc4116-14a5-486c-9793-17274287c53d', divine_marriage_command__substitutionist_reading, influences).
narrative_ontology:cs_axiom('a8dc4116-14a5-486c-9793-17274287c53d', foundational, institutional_survival_as_theological_imperative).
narrative_ontology:cs_axiom_status(institutional_survival_as_theological_imperative, holdable).
narrative_ontology:cs_axiom_grounding('a8dc4116-14a5-486c-9793-17274287c53d', institutional_survival_as_theological_imperative, theological).
narrative_ontology:cs_axiom('a8dc4116-14a5-486c-9793-17274287c53d', secondary, exogenous_pressure_as_divine_instrument).
narrative_ontology:cs_axiom_status(exogenous_pressure_as_divine_instrument, holdable).
narrative_ontology:cs_axiom_grounding('a8dc4116-14a5-486c-9793-17274287c53d', exogenous_pressure_as_divine_instrument, theological).
narrative_ontology:cs_reference_frame('a8dc4116-14a5-486c-9793-17274287c53d', divine_command_unimpeded_by_secular_power).
narrative_ontology:cs_drift_state('a8dc4116-14a5-486c-9793-17274287c53d', post_manifesto_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a8dc4116-14a5-486c-9793-17274287c53d', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, institutional_church_leadership).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, members_adhering_to_prior_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that issued the Manifesto, acknowledging it as a response to federal coercion. Benefits from the institutional survival and legal recognition that resulted from the doctrinal shift. Bears the burden of maintaining theological coherence amidst the acknowledged external pressure.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, institutional_church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Members who had committed to or believed in the prior doctrine of plural marriage. They bear the cost of conforming to the new, federally-mandated practice, often experiencing social and spiritual dislocation. Their identity is deeply intertwined with the church, making exit extremely difficult.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, members_adhering_to_prior_doctrine, payer,
    powerless, biographical, identity_locked, local).

% Exerted coercive pressure (legal, economic, military) that led to the Manifesto. Benefits from the church's conformity to federal law and social norms. Does not directly participate in the theological interpretation but its actions are acknowledged as a primary driver of the doctrinal shift.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Academics and internal theologians who analyze the historical and doctrinal implications of the Manifesto, particularly the role of coercion in shaping religious doctrine. Their work often highlights the tension between divine command and institutional pragmatism.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, theological_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the church's legal and social integration into the broader federal system, preventing institutional dissolution and allowing for continued practice of other religious tenets.
% TRANSFER_FUNCTION: Transfers the practice of plural marriage from a divinely sanctioned command to a suspended or superseded doctrine, from individual members to the institutional survival of the church, under pressure from the federal government.
% ABSENT_VOICES: Those who left the church rather than abandon plural marriage, or those who continued the practice in defiance of the Manifesto, are absent from the official narrative. They would argue that divine command cannot be superseded by secular coercion.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its theological justification vanished, the church's relationship with the federal government would be fundamentally destabilized, potentially leading to renewed legal conflict or institutional fragmentation. The current social and legal structure of the church depends on this historical accommodation.
% FOUNDING_PROBLEM: The institutional church faced existential threats (disincorporation, confiscation of property, imprisonment of leaders) from the federal government due to its practice of plural marriage.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court decisions, and contemporary sociological analyses corroborate that the existential threat from the federal government has largely passed, and the church has achieved legal and social integration. While some internal theological debates persist, the immediate 'founding problem' of institutional survival under federal coercion is no longer live.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).
:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, as the primary 'extraction' is the abandonment of a core practice by some members for the benefit of institutional survival. Suppression (0.60) is significant, reflecting the federal government's coercive power and the church's internal enforcement of the new norm. Theater ratio (0.20) is low, as the Manifesto was a genuine, albeit coerced, shift, not merely performative. The historical measurements show a gradual decrease in extractiveness and suppression as the church integrated into mainstream society and the immediate threat subsided.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the institutional church leadership, the Manifesto was a necessary, divinely guided act of prudence to preserve the institution. From the perspective of members adhering to prior doctrine, it was a painful extraction, a betrayal of divine command under duress. The federal government views it as a successful enforcement of secular law. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional church leadership is a beneficiary (d near 0.0) as the constraint ensured its survival. Members adhering to prior doctrine are targets (d near 1.0) as they bore the direct cost of doctrinal change. The federal government, while an external enforcer, also benefits from the church's compliance. Theological scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (institutional survival under federal coercion) is largely 'dead' as the immediate threat has passed. However, the constraint persists, and its theological justification continues to shape doctrine and member experience. This indicates a potential for mandatrophy, where the original coordination function has atrophied but the constraint remains due to institutional inertia and the ongoing theological work required to maintain its legitimacy. The 'dead' founding problem combined with 'world_rearranges' disappearance verdict signals this dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_as_divine_will,
    'To what extent can external coercion be legitimately interpreted as an instrument of divine will, rather than a challenge to it, within this theological framework?',
    'Further theological development or a new revelatory event that explicitly addresses the role of external pressure in doctrinal change.',
    'If coercion is fully integrated as a valid input for doctrinal shift, it fundamentally alters the nature of divine command and could lead to a legitimacy crisis for future doctrinal pronouncements. If not, the Manifesto remains a pragmatic, rather than divinely sanctioned, act.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_as_divine_will, conceptual, 'Ambiguity regarding the theological legitimacy of coercion-induced doctrinal shifts.').

omega_variable(
    legitimacy_of_institutional_survival,
    'Is institutional survival, at the cost of a core doctrine, a theologically justifiable primary goal for a divinely established church?',
    'Internal theological consensus or a formal doctrinal statement clarifying the hierarchy of values (doctrinal purity vs. institutional continuity).',
    'If institutional survival is paramount, it reinforces a pragmatic approach to doctrine. If doctrinal purity is paramount, the Manifesto''s legitimacy is weakened, potentially leading to schism or a re-evaluation of the church''s divine claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_institutional_survival, preference, 'Theological priority of institutional survival over doctrinal consistency.').

omega_variable(
    m_set_gap_closure_mechanism,
    'How does this reading reconcile the acknowledged exogenous pressure with the claim of divine authority, specifically regarding the mechanism by which the M-set gap (between divine command and practice) was closed?',
    'Detailed historical and theological analysis of internal church records and pronouncements from the period, focusing on the explicit language used to frame the Manifesto.',
    'If the reconciliation mechanism is weak or contradictory, it undermines the reading''s internal coherence and could push it towards a ''theater'' classification. If robust, it strengthens the claim of a genuine, albeit coerced, doctrinal shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m_set_gap_closure_mechanism, empirical, 'The specific theological mechanism for closing the M-set gap under coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(divi_tr_t1980, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(divi_tr_t2024, divine_marriage_command__coercion_visibility_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1890, 0.6).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(divi_be_t1980, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(divi_be_t2024, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1890, 0.8).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(divi_su_t1980, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(divi_su_t2024, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
