% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto as Prophetic Reinterpretation (Endogenous Reading)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the 'endogenous reinterpretation'
 *   reading of the 1890 Manifesto, where the suspension of plural marriage is
 *   understood as a legitimate prophetic reinterpretation of divine will,
 *   revealed to preserve the church's salvific mission in the face of
 *   external pressure. The constraint is claimed as a Rope, reflecting its
 *   function in coordinating the church's response and ensuring its survival,
 *   with moderate extraction from those who adhered to the prior
 *   understanding of doctrine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.25).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.6).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto as Prophetic Reinterpretation (Endogenous Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, 'c581ddbf-b354-4462-a9e0-4717f8ba3587').
narrative_ontology:cs_kernel_codification('c581ddbf-b354-4462-a9e0-4717f8ba3587', formalized).
narrative_ontology:cs_authority_grounding('c581ddbf-b354-4462-a9e0-4717f8ba3587', lineage).
narrative_ontology:cs_interpretation_layer_present('c581ddbf-b354-4462-a9e0-4717f8ba3587').
narrative_ontology:cs_reading_relation('c581ddbf-b354-4462-a9e0-4717f8ba3587', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('c581ddbf-b354-4462-a9e0-4717f8ba3587', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('c581ddbf-b354-4462-a9e0-4717f8ba3587', foundational, continuing_revelation_adapts_doctrine).
narrative_ontology:cs_axiom_status(continuing_revelation_adapts_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('c581ddbf-b354-4462-a9e0-4717f8ba3587', continuing_revelation_adapts_doctrine, theological).
narrative_ontology:cs_axiom('c581ddbf-b354-4462-a9e0-4717f8ba3587', secondary, institutional_survival_is_salvific_priority).
narrative_ontology:cs_axiom_status(institutional_survival_is_salvific_priority, holdable).
narrative_ontology:cs_axiom_grounding('c581ddbf-b354-4462-a9e0-4717f8ba3587', institutional_survival_is_salvific_priority, theological).
narrative_ontology:cs_reference_frame('c581ddbf-b354-4462-a9e0-4717f8ba3587', prophetic_revelation_as_adaptive_governance).
narrative_ontology:cs_drift_state('c581ddbf-b354-4462-a9e0-4717f8ba3587', post_manifesto_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c581ddbf-b354-4462-a9e0-4717f8ba3587', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, the_church_of_jesus_christ_of_latter_day_saints).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_members).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_members).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, prophetic_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that issued the Manifesto, interpreted it as divine revelation, and enforced its new directive. Benefits from the preservation of its salvific mission and institutional survival.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, the_church_of_jesus_christ_of_latter_day_saints, agenda_setter,
    institutional, civilizational, constrained, global).

% Members who accepted the reinterpretation, allowing them to remain in good standing with the church, participate in temple ordinances, and avoid legal persecution. They benefit from continued access to church blessings and social cohesion.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_members, beneficiary,
    organized, generational, mobile, global).

% Members who believed plural marriage was an eternal, unchangeable commandment and rejected the reinterpretation. They faced excommunication, social ostracization, and the loss of temple access for adhering to the original practice. Their identity is deeply tied to the original doctrine.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_members, payer,
    powerless, biographical, identity_locked, local).

% The external coercive power that pressured the church to abandon plural marriage. From this reading's perspective, the government's actions were the context for God's revelation, not the direct cause of the change.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, us_federal_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the church's membership around a new, divinely sanctioned understanding of marriage practice, allowing the institution to survive and continue its salvific work in the face of external pressure.
% TRANSFER_FUNCTION: Transfers the practice of plural marriage from a divinely mandated ordinance to a temporally suspended one, shifting the burden of compliance from the church institution (facing legal threats) to individual members (who must now cease the practice or face excommunication).
% ABSENT_VOICES: Those who left the church to continue plural marriage, or who were excommunicated for doing so, are absent from the official narrative. They would argue that the reinterpretation was a capitulation, not a revelation.
% DISAPPEARANCE_RATIONALE: If the 1890 Manifesto and its reinterpretation vanished, the church's historical narrative and current practices would be fundamentally destabilized. The legitimacy of its leadership and the continuity of its doctrine would be called into question, leading to widespread confusion and potential schism.
% FOUNDING_PROBLEM: The church faced existential threats from the U.S. federal government due to its practice of plural marriage, including confiscation of property, disenfranchisement of members, and imprisonment of leaders, jeopardizing its ability to fulfill its divine mission.
% FOUNDING_PROBLEM_CORROBORATION: The church's official history and leadership consistently attest to the founding problem. External historical accounts and legal records from the U.S. government corroborate the severe legal and political pressure faced by the church, supporting the claim that institutional survival was at stake.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.25) because while the change was significant, it was framed as a divine directive, which for many members, legitimized the shift. Suppression is higher (0.60) due to the active excommunication of those who continued plural marriage, demonstrating the church's enforcement of the new interpretation. Theater ratio is moderate (0.40) as the narrative of divine revelation served to legitimate a pragmatic institutional adaptation, blending genuine spiritual guidance with strategic necessity. The measurement series reflects a gradual increase in extractiveness and suppression as the new interpretation was solidified and enforced over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mainstream members, this was a necessary and divinely guided adjustment, preserving their access to the church's blessings. For fundamentalist members, it was a betrayal of eternal principles, leading to their excommunication and the loss of their community. The church institution itself experienced it as a successful coordination effort to ensure its survival.
 *
 * DIRECTIONALITY LOGIC:
 *   The Church of Jesus Christ of Latter-day Saints is the primary beneficiary and agenda-setter, as the reinterpretation allowed it to survive and continue its mission. Mainstream members are also beneficiaries, as they maintained their standing within the church. Fundamentalist members are the victims, bearing the cost of excommunication and social ostracization for adhering to the original doctrine. The U.S. federal government is an observer, its actions providing the context for the reinterpretation but not directly benefiting from this specific internal church constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the reinterpretation as pure extraction by acknowledging its genuine coordination function for the church's survival. However, the moderate theater ratio and the existence of fundamentalist victims suggest that the 'divine revelation' narrative also served to legitimate a difficult institutional choice, rather than being solely a transparent act of coordination. The founding problem is 'dead' in the sense that the immediate threat of federal intervention has passed, but the constraint persists as a foundational doctrinal interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_vs_institutional_pragmatism,
    'To what extent was the 1890 Manifesto a genuine divine revelation, and to what extent was it a pragmatic institutional response to overwhelming external coercion?',
    'Further historical and theological analysis, including examination of internal church deliberations and external political pressures leading up to the Manifesto, and comparison with other instances of ''revelation'' in response to external threats.',
    'If primarily pragmatic, the constraint''s theater ratio would be higher, and its extractiveness from fundamentalists would be reclassified as more purely extractive, potentially shifting its type towards a Tangled Rope or Snare for those seats. If primarily divine, the Rope classification holds more strongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_will_vs_institutional_pragmatism, conceptual, 'Ambiguity between divine command and institutional survival strategy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by fundamentalist members primarily structural (excommunication, loss of community) or internalized (belief in prophetic authority making dissent unthinkable)?',
    'Post-exit trajectory of excommunicated members: if suppression persists (e.g., self-imposed isolation, continued belief in the church''s authority despite excommunication), it suggests internalized suppression. If they form new communities and thrive, it suggests structural suppression was primary.',
    'If internalized, the effective suppression for fundamentalist members is higher than the structural measure suggests, as they carry the suppression with them. This would amplify their effective extraction (chi).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1894, 0.35).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1898, 0.38).
narrative_ontology:measurement(plur_tr_t1901, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1901, 0.39).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.4).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.2).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1894, 0.22).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1898, 0.23).
narrative_ontology:measurement(plur_be_t1901, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1901, 0.24).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1894, 0.55).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1898, 0.58).
narrative_ontology:measurement(plur_su_t1901, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1901, 0.59).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
