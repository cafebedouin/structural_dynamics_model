% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Reformation: Theological Fragmentation
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint models the Reformation as a fundamentally theological
 *   event, where competing soteriological (salvation) and ecclesiological
 *   (church structure) commitments led to the structural incompatibility of
 *   denominations. This reading emphasizes doctrinal pluralism as the primary
 *   observable, with confessional documents serving as constraint artifacts
 *   and denominational leadership benefiting from the resulting
 *   fragmentation. The constraint is claimed as a Tangled Rope because it
 *   genuinely coordinates identity and community for its adherents, but
 *   simultaneously extracts conformity and suppresses alternatives through
 *   active enforcement of doctrine, leading to widespread conflict and
 *   persecution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.78).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.85).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Reformation: Theological Fragmentation").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, 'd5960c13-c3b6-4924-a78e-13be3aa904dd').
narrative_ontology:cs_kernel_codification('d5960c13-c3b6-4924-a78e-13be3aa904dd', formalized).
narrative_ontology:cs_authority_grounding('d5960c13-c3b6-4924-a78e-13be3aa904dd', lineage).
narrative_ontology:cs_interpretation_layer_present('d5960c13-c3b6-4924-a78e-13be3aa904dd').
narrative_ontology:cs_reading_relation('d5960c13-c3b6-4924-a78e-13be3aa904dd', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_reading_relation('d5960c13-c3b6-4924-a78e-13be3aa904dd', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('d5960c13-c3b6-4924-a78e-13be3aa904dd', foundational, sola_scriptura_principle).
narrative_ontology:cs_axiom_status(sola_scriptura_principle, holdable).
narrative_ontology:cs_axiom_grounding('d5960c13-c3b6-4924-a78e-13be3aa904dd', sola_scriptura_principle, deontological).
narrative_ontology:cs_axiom('d5960c13-c3b6-4924-a78e-13be3aa904dd', foundational, justification_by_faith_alone).
narrative_ontology:cs_axiom_status(justification_by_faith_alone, holdable).
narrative_ontology:cs_axiom_grounding('d5960c13-c3b6-4924-a78e-13be3aa904dd', justification_by_faith_alone, deontological).
narrative_ontology:cs_reference_frame('d5960c13-c3b6-4924-a78e-13be3aa904dd', sola_scriptura_sola_fide_framework).
narrative_ontology:cs_drift_state('d5960c13-c3b6-4924-a78e-13be3aa904dd', post_westphalian_settlement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d5960c13-c3b6-4924-a78e-13be3aa904dd', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, adherents_of_specific_confessions).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, theological_dissenters).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, rival_denominations).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, populations_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, adherents_of_specific_confessions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces doctrinal standards, administers church structures, and benefits from the loyalty and resources of their confessional group. Their authority is directly tied to the distinct theological commitments.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_leadership, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, denominational_leadership, beneficiary).

% Receive spiritual guidance, community, and a coherent worldview. They pay through conformity to doctrine, financial contributions, and sometimes through persecution or conflict with other groups. Their identity is deeply intertwined with their confession.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, adherents_of_specific_confessions, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, adherents_of_specific_confessions, payer).

% Bear the direct costs of non-conformity, including excommunication, social ostracization, persecution, or death. They are actively suppressed and excluded from the dominant confessional structures.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, theological_dissenters, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, theological_dissenters, excluded).

% Compete for adherents and political influence, often leading to conflict and mutual exclusion. They bear the costs of maintaining distinct identities and defending against rival claims, sometimes through warfare.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, rival_denominations, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, rival_denominations, excluded).

% Caught in the crossfire of religious wars and persecutions, suffering displacement, violence, and economic devastation due to the enforcement of confessional boundaries and theological disputes.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, populations_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Initially sought to control or leverage religious fragmentation for political ends, later often sought to establish religious peace through treaties (e.g., Westphalia) that acknowledged confessional divisions. They observe the theological dynamics but are not directly bound by them.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, secular_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:fixing_cost_class(reformation_composite__theological_fragmentation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent theological framework, moral guidance, and community identity for adherents, enabling collective worship and social cohesion within specific confessional groups (e.g., Lutheran, Calvinist, Anglican).
% TRANSFER_FUNCTION: Transfers spiritual authority, social capital, and sometimes material resources to denominational leadership; extracts conformity, loyalty, and material support from adherents; extracts lives, property, and peace from populations caught in religious conflicts.
% ABSENT_VOICES: Those who advocated for religious pluralism, tolerance, or a unified, non-coercive Christian identity. They were often persecuted, exiled, or silenced by the dominant confessional powers, whose theological commitments demanded exclusive adherence.
% DISAPPEARANCE_RATIONALE: If the theological fragmentation and its enforcement vanished overnight, the religious and political landscape of early modern Europe would have been fundamentally different. The rise of nation-states, the nature of sovereignty, and the very identities of European peoples were shaped by these confessional divisions. Its absence would imply a radically different historical trajectory.
% FOUNDING_PROBLEM: The perceived corruption and theological errors of the late medieval Catholic Church, particularly regarding the means of salvation (soteriology) and the nature and authority of the Church (ecclesiology).
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion, theologians from various traditions (including Catholic and Protestant), and political historians widely corroborate the initial problems and their theological drivers. While specific abuses of the 16th century are largely resolved, the underlying theological disagreements persist in various forms, and the question of Christian unity remains a live issue for many.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) due to the immense human and material costs of religious wars, persecution, and the imposition of confessional states. Suppression is very high (0.85) as doctrinal purity was enforced with extreme measures (inquisitions, heresy trials, state-backed religious uniformity), actively combating dissent and rival interpretations. Theater ratio is moderate (0.45): while genuine theological debate existed, much of the enforcement and doctrinal rigidity became performative, serving to maintain institutional power and justify conflict. Accessibility collapse is high (0.70) because adopting a confessional identity created strong social, political, and theological barriers to switching or leaving. Resistance is extremely high (0.90) as the entire period is characterized by intense conflict and resistance between and within emerging denominations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of denominational leadership, the constraint is a necessary coordination mechanism for spiritual truth and community. From the perspective of dissenters or populations caught in conflict, it is a highly extractive and suppressive force. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational leadership acts as the agenda-setter and primary beneficiary, gaining authority and resources from the distinct confessional identities. Adherents are beneficiaries (community, spiritual guidance) but also payers (conformity, resources, risk of conflict). Theological dissenters and rival denominations are clear targets/victims, facing suppression and extraction. Populations in conflict zones are direct victims of the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling the Reformation as pure extraction (Snare) by acknowledging its genuine coordination function for adherents, while also preventing it from being seen as pure coordination (Rope) by highlighting the asymmetric extraction and suppression inherent in its operation. The 'contested' status of the founding problem further supports the Tangled Rope classification, indicating that while the initial problem may have shifted, the structure persists with ongoing costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_primacy,
    'Is the theological fragmentation truly primary, or is it a rationalization for underlying political power struggles and economic interests?',
    'Comparative historical analysis of regions where political and economic factors diverged from theological alignment, or counterfactual history exploring alternative political outcomes without theological schism.',
    'If political/economic factors are primary, the constraint''s extractiveness and suppression might be re-attributed to political actors using theology as a cover, potentially reclassifying parts of the constraint as a Snare driven by political economy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_primacy, conceptual, 'Ambiguity regarding the causal primacy of theological vs. political factors in driving fragmentation.').

omega_variable(
    internalized_vs_structural_suppression,
    'How much of the suppression of dissent was due to external coercion (state/church power) versus internalized belief and social pressure within confessional communities?',
    'Sociological studies of religious conformity in historical contexts, examining the persistence of belief and social norms after the removal of direct coercive mechanisms.',
    'If internalized suppression is significant, the effective suppression of the constraint is higher than purely structural measures suggest, as individuals carry the suppression within their identity, making exit even harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for doctrinal conformity.').

omega_variable(
    coordination_extraction_boundary,
    'To what extent did the new denominations genuinely coordinate belief and community, versus using that coordination as a cover for extracting conformity and resources from adherents and suppressing rivals?',
    'Analysis of the ratio of resources dedicated to genuine community building and charitable works versus those dedicated to doctrinal enforcement, persecution, and inter-confessional conflict.',
    'A higher ratio of resources to enforcement/conflict would support the coordination function; a lower ratio would shift the classification closer to a Snare, indicating the coordination story is largely cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Distinguishing genuine coordination from extractive cover stories within confessional structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1517, 1617).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__theological_fragmentation_reading, theater_ratio, 1517, 0.2).
narrative_ontology:measurement(refo_tr_t1537, reformation_composite__theological_fragmentation_reading, theater_ratio, 1537, 0.3).
narrative_ontology:measurement(refo_tr_t1557, reformation_composite__theological_fragmentation_reading, theater_ratio, 1557, 0.38).
narrative_ontology:measurement(refo_tr_t1577, reformation_composite__theological_fragmentation_reading, theater_ratio, 1577, 0.42).
narrative_ontology:measurement(refo_tr_t1597, reformation_composite__theological_fragmentation_reading, theater_ratio, 1597, 0.44).
narrative_ontology:measurement(refo_tr_t1617, reformation_composite__theological_fragmentation_reading, theater_ratio, 1617, 0.45).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1517, 0.6).
narrative_ontology:measurement(refo_be_t1537, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1537, 0.68).
narrative_ontology:measurement(refo_be_t1557, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1557, 0.73).
narrative_ontology:measurement(refo_be_t1577, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1577, 0.76).
narrative_ontology:measurement(refo_be_t1597, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1597, 0.77).
narrative_ontology:measurement(refo_be_t1617, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1617, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1517, 0.65).
narrative_ontology:measurement(refo_su_t1537, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1537, 0.75).
narrative_ontology:measurement(refo_su_t1557, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1557, 0.8).
narrative_ontology:measurement(refo_su_t1577, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1577, 0.83).
narrative_ontology:measurement(refo_su_t1597, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1597, 0.84).
narrative_ontology:measurement(refo_su_t1617, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1617, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Reformation Composite' kernel, focusing on theological fragmentation. It structurally influences the political and technological readings by providing the doctrinal content and justification for their respective dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
