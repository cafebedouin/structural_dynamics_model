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
 *   (church structure) commitments led to the formation of distinct, often
 *   mutually exclusive, Christian denominations. This reading emphasizes
 *   doctrinal pluralism as the primary observable, with confessional
 *   documents as key constraint artifacts and denominational leadership
 *   benefiting from the resulting fragmentation. The high extractiveness and
 *   suppression reflect the intense religious conflicts, persecution, and
 *   forced conformity that characterized the era, driven by the perceived
 *   incompatibility of theological truths.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.85).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.9).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Reformation: Theological Fragmentation").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '6b351eee-e8e7-46e2-a74b-91133d01da1e').
narrative_ontology:cs_kernel_codification('6b351eee-e8e7-46e2-a74b-91133d01da1e', formalized).
narrative_ontology:cs_authority_grounding('6b351eee-e8e7-46e2-a74b-91133d01da1e', lineage).
narrative_ontology:cs_interpretation_layer_present('6b351eee-e8e7-46e2-a74b-91133d01da1e').
narrative_ontology:cs_reading_relation('6b351eee-e8e7-46e2-a74b-91133d01da1e', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b351eee-e8e7-46e2-a74b-91133d01da1e', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('6b351eee-e8e7-46e2-a74b-91133d01da1e', foundational, soteriological_truth_is_singular).
narrative_ontology:cs_axiom_status(soteriological_truth_is_singular, holdable).
narrative_ontology:cs_axiom_grounding('6b351eee-e8e7-46e2-a74b-91133d01da1e', soteriological_truth_is_singular, theological).
narrative_ontology:cs_axiom('6b351eee-e8e7-46e2-a74b-91133d01da1e', foundational, ecclesiological_structure_is_divinely_ordained).
narrative_ontology:cs_axiom_status(ecclesiological_structure_is_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('6b351eee-e8e7-46e2-a74b-91133d01da1e', ecclesiological_structure_is_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('6b351eee-e8e7-46e2-a74b-91133d01da1e', confessional_orthodoxy_framework).
narrative_ontology:cs_drift_state('6b351eee-e8e7-46e2-a74b-91133d01da1e', post_enlightenment_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('6b351eee-e8e7-46e2-a74b-91133d01da1e', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, theologians).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_states).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, religious_minorities).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, dissenters).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, common_people_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, imperial_papal_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Leaders of emerging Protestant denominations or the reformed Catholic hierarchy. They define and enforce doctrinal purity, organize religious life, and benefit from the loyalty and resources of their adherents. Their power is directly tied to the distinctiveness and enforcement of their theological commitments.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_leadership, agenda_setter,
    institutional, generational, constrained, regional).

% Scholars and interpreters who develop, defend, and propagate specific theological doctrines. Their intellectual authority, careers, and influence are directly enhanced by the fragmentation and the need for doctrinal articulation and defense. Exit means abandoning their intellectual framework and professional identity.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, theologians, beneficiary,
    powerful, biographical, identity_locked, continental).

% Political entities (princes, city-states) that adopted a specific religious confession as state policy. They used theological alignment to consolidate political power, assert sovereignty against imperial or papal authority, and enforce social order. They benefit from a unified religious identity within their borders.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_states, beneficiary,
    institutional, generational, constrained, national).

% Individuals or groups whose theological commitments differed from the dominant confession of their state or region. They faced persecution, forced conversion, exile, or death. Their options were to conform, flee, or resist at great personal cost.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, religious_minorities, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, religious_minorities, excluded).

% Those who questioned or subtly challenged established doctrines within their own denomination. While not always outright minorities, they faced social ostracism, academic censure, or loss of position. Their dissent was often suppressed to maintain internal cohesion.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, dissenters, payer,
    moderate, biographical, constrained, local).

% The general populace caught in the religious wars and conflicts that resulted from theological fragmentation. They suffered displacement, famine, violence, and forced changes in religious practice based on the shifting allegiances of their rulers. They bore the direct human cost of the theological incompatibility.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, common_people_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% The Holy Roman Emperor and the Papacy, who sought to maintain a unified Christian (Catholic) order. They bore the cost of widespread rebellion, loss of political and spiritual authority, and military campaigns to suppress dissent. They were also agenda-setters for the Catholic counter-reformation.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, imperial_papal_authority, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, imperial_papal_authority, agenda_setter).

% Scholars who study the Reformation as a historical event, analyzing its causes, dynamics, and consequences from a detached, evidence-based perspective. They seek to understand the interplay of theological, political, and technological factors.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a framework for religious belief, community organization, and spiritual guidance for those who adhered to specific doctrines, offering social cohesion and a sense of divine purpose within each emerging denomination.
% TRANSFER_FUNCTION: Transferred spiritual authority, political power, and material resources (tithes, confiscated lands) from the unified medieval Church to new denominational and state authorities. It extracted conformity, loyalty, and often immense human cost (through wars and persecution) from adherents and dissenters.
% ABSENT_VOICES: Advocates for religious pluralism, tolerance, or a unified, non-coercive Christianity were largely absent from positions of power; they were often suppressed, exiled, or killed as their views directly challenged the emerging confessional state model.
% DISAPPEARANCE_RATIONALE: If the theological fragmentation of the Reformation had not occurred, the political map of Europe, the development of nation-states, the concept of religious freedom, and the very structure of modern religious institutions would be fundamentally different. The event irrevocably shaped Western civilization.
% FOUNDING_PROBLEM: A perceived crisis of spiritual authority, corruption, and doctrinal deviation within the late medieval Catholic Church, particularly concerning salvation (soteriology) and the nature of the Church (ecclesiology).
% FOUNDING_PROBLEM_CORROBORATION: Contemporary reformers (e.g., Martin Luther, John Calvin) and their followers widely attested to the crisis, as did later Protestant historians. The Catholic Church's own internal reforms (e.g., Council of Trent) corroborate the existence of significant problems, though they dispute the nature of the 'founding problem' and its proposed solutions. Modern historians acknowledge the theological disputes as central, alongside political and technological factors.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) and suppression (0.90) reflect the immense human and material costs of religious wars, heresy trials, and forced conversions, as well as the strict doctrinal adherence demanded within each confession. Accessibility collapse is high (0.88) because individuals had very limited options outside the dominant confession of their region. Resistance is also high (0.75) due to the widespread theological debates, military conflicts, and popular uprisings. The theater ratio (0.45) indicates that while genuine theological debate was present, much of the public enforcement and display of orthodoxy served to consolidate power and suppress alternatives, rather than purely to clarify truth. The claimed type is 'tangled_rope' because it provided a coordination function for adherents within each denomination (shared belief, community) while simultaneously extracting conformity and suppressing dissent from those outside or within who did not align.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of denominational leaders and theologians, the constraint was a necessary 'rope' for establishing true Christian order and coordinating salvation. From the perspective of religious minorities or common people caught in the conflicts, it was a 'snare' of forced conformity and violence. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational leadership, theologians, and confessional states are beneficiaries, gaining authority, influence, and political stability from the establishment and enforcement of specific theological boundaries. Religious minorities, dissenters, and common people in conflict zones are victims, bearing the direct costs of persecution, forced conformity, and violence. Imperial/papal authority, while initially an agenda-setter, became a payer as it lost power and resources to the emerging Protestant states and denominations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_causation,
    'To what extent was the theological fragmentation a genuine outcome of incompatible commitments, versus a convenient justification for political realignment and state-building?',
    'Comparative historical analysis of regions where political and theological motivations diverged, or counterfactual analysis of how the Reformation might have unfolded without strong state actors.',
    'If political factors were primary, the measured extraction might be more directly attributable to state power consolidation (a Snare of political economy) rather than inherent theological conflict, shifting the classification of the underlying driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_causation, conceptual, 'Ambiguity in the primary causal driver of Reformation fragmentation.').

omega_variable(
    internalized_vs_structural_suppression,
    'Was the high suppression primarily structural (confessional laws, military force) or internalized (deeply held belief in the necessity of doctrinal purity, fear of damnation)?',
    'Analysis of post-conflict societal behavior: if doctrinal conformity persisted strongly even after external enforcement weakened, it suggests a higher degree of internalized suppression.',
    'If internalized suppression was a significant factor, the effective suppression for individuals was higher and more persistent than purely structural measures suggest, making exit even more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism in theological conformity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__theological_fragmentation_reading, theater_ratio, 1517, 0.2).
narrative_ontology:measurement(refo_tr_t1540, reformation_composite__theological_fragmentation_reading, theater_ratio, 1540, 0.3).
narrative_ontology:measurement(refo_tr_t1565, reformation_composite__theological_fragmentation_reading, theater_ratio, 1565, 0.4).
narrative_ontology:measurement(refo_tr_t1590, reformation_composite__theological_fragmentation_reading, theater_ratio, 1590, 0.5).
narrative_ontology:measurement(refo_tr_t1615, reformation_composite__theological_fragmentation_reading, theater_ratio, 1615, 0.55).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__theological_fragmentation_reading, theater_ratio, 1648, 0.45).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1517, 0.6).
narrative_ontology:measurement(refo_be_t1540, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1540, 0.75).
narrative_ontology:measurement(refo_be_t1565, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1565, 0.82).
narrative_ontology:measurement(refo_be_t1590, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1590, 0.88).
narrative_ontology:measurement(refo_be_t1615, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1615, 0.9).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1648, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1517, 0.7).
narrative_ontology:measurement(refo_su_t1540, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1540, 0.8).
narrative_ontology:measurement(refo_su_t1565, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1565, 0.88).
narrative_ontology:measurement(refo_su_t1590, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1590, 0.92).
narrative_ontology:measurement(refo_su_t1615, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1615, 0.95).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1648, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'reformation_composite' kernel, focusing on theological drivers. It coexists with political and technological readings, as they offer different lenses on the same historical event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
