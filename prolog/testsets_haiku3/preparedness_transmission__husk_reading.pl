% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__husk_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Hollowed Preparedness Transmission (Husk Reading)
 *   domain: institutional/disaster_management
 *
 * SUMMARY:
 *   A national civil defense system maintains annual flood-preparedness
 *   drills and facility inspections as mandated institutional ritual. Under
 *   the husk reading, the organizational form persists — compliance is high,
 *   schedules are followed, reports are filed — but the operative knowledge
 *   has atrophied. Drills train field responders in scenarios that no longer
 *   match evolved flood behavior; inspections check for failure modes that
 *   the current threat environment has superseded. The constraint extracts
 *   from flood-exposed communities (who depend on a hollow system) to benefit
 *   the civil defense administration (which derives legitimacy and budgetary
 *   continuity from the appearance of readiness) and political overseers (who
 *   avoid funding genuine modernization). The theater ratio is high and
 *   rising: performative compliance increasingly dominates, while adaptive
 *   capacity stagnates.
 *
 * KEY AGENTS:
 *   - Civil defense administration: orchestrates the ritual, administers the framework, benefits from the appearance of readiness without funding the modernization that would make readiness real.
 *   - Flood-exposed communities: trapped in the region, depend on the system, bear the cost when preparedness fails to adapt.
 *   - Field responders: execute the drills from manuals, know the mismatch, constrained by organizational hierarchy.
 *   - Political overseers: accept compliance reports as proof of preparedness, avoid the cost of demanding modernization.
 *   - Hydrologists/engineers (absent): would attest to changed flood patterns but are not in the decision loop.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.71).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Hollowed Preparedness Transmission (Husk Reading)").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "institutional/disaster_management").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, '23a46390-ff81-4160-bbaf-19a1e2e29f31').
narrative_ontology:cs_kernel_codification('23a46390-ff81-4160-bbaf-19a1e2e29f31', formalized).
narrative_ontology:cs_authority_grounding('23a46390-ff81-4160-bbaf-19a1e2e29f31', extraction).
narrative_ontology:cs_interpretation_layer_present('23a46390-ff81-4160-bbaf-19a1e2e29f31').
narrative_ontology:cs_reading_relation('23a46390-ff81-4160-bbaf-19a1e2e29f31', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('23a46390-ff81-4160-bbaf-19a1e2e29f31', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('23a46390-ff81-4160-bbaf-19a1e2e29f31', foundational, preparedness_knowledge_decays_under_institutional_inertia).
narrative_ontology:cs_axiom_status(preparedness_knowledge_decays_under_institutional_inertia, holdable).
narrative_ontology:cs_axiom_grounding('23a46390-ff81-4160-bbaf-19a1e2e29f31', preparedness_knowledge_decays_under_institutional_inertia, empirically_contingent).
narrative_ontology:cs_axiom('23a46390-ff81-4160-bbaf-19a1e2e29f31', foundational, organizational_form_persists_after_functional_atrophy).
narrative_ontology:cs_axiom_status(organizational_form_persists_after_functional_atrophy, holdable).
narrative_ontology:cs_axiom_grounding('23a46390-ff81-4160-bbaf-19a1e2e29f31', organizational_form_persists_after_functional_atrophy, conventional).
narrative_ontology:cs_reference_frame('23a46390-ff81-4160-bbaf-19a1e2e29f31', post_disaster_adaptive_preparedness_system).
narrative_ontology:cs_drift_state('23a46390-ff81-4160-bbaf-19a1e2e29f31', contemporary_changed_flood_regimes, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('23a46390-ff81-4160-bbaf-19a1e2e29f31', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, civil_defense_administration).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, flood_exposed_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, political_oversight_bodies).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, field_responders).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, organizational_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the national civil defense framework, including annual flood-preparedness drills and facility inspections. Perpetuates the ritual schedule and publishes compliance reports. The drills provide institutional continuity narratives and demonstrate 'readiness' to political overseers, even as the technical content of what is tested has narrowed and the ability to adapt to novel flood scenarios has deteriorated. Benefits from the appearance of maintained competence.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_administration, agenda_setter,
    institutional, generational, mobile, national).

% Live in flood-prone regions and depend on the civil defense system to respond to actual flood events. They experience drills as theater — the procedures tested are divorced from the novel flood patterns and infrastructure failures that actually threaten them. When actual floods occur, the pre-learned response templates often fail to adapt; communities bear the cost of this hollow preparedness.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, flood_exposed_communities, payer,
    powerless, immediate, trapped, regional).

% Personnel who execute the drills and would respond to actual floods. They know the formal procedures by rote; many are recent hires who learned them from manuals rather than from operational practice. They recognize the mismatch between drill scenarios and actual flood behavior but lack authority to modify the framework. They pay in credibility and operational failure when their training does not translate.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, field_responders, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, field_responders, observer).

% Accept the civil defense administration's compliance reports as evidence of preparedness. The drills and inspection results provide political cover ('we have a system in place') without requiring them to fund modernization or confront the gap between protocol compliance and actual capability.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, political_oversight_bodies, beneficiary,
    institutional, biographical, mobile, national).

% Analyze the institutional succession of preparedness knowledge. They see the drills as ritualized memory-keeping: the form persists because it is embedded in organizational practice, not because the underlying knowledge is alive and transmitted.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, organizational_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__husk_reading, civil_defense_administration).
narrative_ontology:fixing_cost_class(preparedness_transmission__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The drill schedule coordinates organizational expectation and reporting discipline across regional offices: all sites run the same scenarios on the same dates, producing comparable reports. This coordination is real but hollow — the information flowing through the system is formatted compliance, not adaptive knowledge.
% TRANSFER_FUNCTION: Moves budgetary legitimacy and political responsibility from overseers to the administration (they report 'we are ready'), and moves actual risk from the administration to flood-exposed communities (who are ostensibly protected by a system that no longer learns).
% ABSENT_VOICES: Hydrologists studying evolving flood patterns (their warnings about changed precipitation regimes would destabilize the fixed-scenario drill scripts); engineers who could redesign the inspection checklist for contemporary infrastructure vulnerabilities; survivors of failed preparedness responses (their post-flood testimony is rarely incorporated into drill design).
% DISAPPEARANCE_RATIONALE: If the drills and annual inspections ceased, the political expectation of civil defense readiness would evaporate immediately — overseers would face questions about preparedness; communities would lack even the thin fiction of protection. The constraint persists because its administrative disappearance costs institutional actors more than its operational hollowness costs them.
% FOUNDING_PROBLEM: Following a major flood disaster, a government established a national civil defense system with mandatory annual drills and regular facility inspections to ensure readiness for future floods and to institutionalize the lessons learned.
% FOUNDING_PROBLEM_CORROBORATION: Post-flood inquiries from the previous generation confirm drills were launched to institutionalize disaster lessons. Contemporary hydrologists and flood-risk engineers (outside the administration) attest that flood patterns have shifted significantly since the drill framework was designed, making the standard scenarios and inspection checklist obsolete. Field responders report the drills train them in outdated protocols.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The husk reading claims Piton type because: (1) the primary function (live, adaptive knowledge transmission) has atrophied; (2) the constraint persists through organizational inertia and the political cost to dismantling it (overseers would face 'who guards the guardians' questions); (3) no single party benefits enough to maintain it at true cost, and no single party is hurt enough to fix it — communities remain trapped by geography, the administration remains embedded in hierarchy, overseers face reputational cost of admitting the system is hollow. The theater ratio (0.81 at interval end, rising from 0.55) marks the progressive hollowing: form compliance increases while actual adaptive knowledge content decreases. Extractiveness rises as the gap widens between what is promised (readiness based on drills) and what is delivered (responses frozen in scenarios from 20–30 years ago). Suppression is high (0.71) because the constraint persists through lack of visibility into the mismatch: field responders know but are not heard; communities experience only post-disaster failure; hydrologists' warnings are not integrated into the drill design. The measurement series tracks the gradual dominance of theater over function — a classic Piton degradation profile. All three metrics share the same time grid (0, 5, 10, 15, 20, 25, 30, 35, 40).
 *
 * PERSPECTIVAL GAP:
 *   From the civil defense administration's seat, the constraint is successful institutional continuity — the drills run on schedule, reports pass inspection, budgets are stable. From the flood-exposed community's seat, the same constraint is theatrical protection that fails under novel conditions. The engine computes per-seat classifications from the structural data: the administration's position as beneficiary with high exit (mobile — they could shift to genuinely modern systems if required) yields lower directionality; the community's position as victim with trapped exit yields high directionality and thus high effective extraction. The claimed Piton type reflects the structural asymmetry: no party benefits enough to modernize it at true cost, no party is hurt enough to dismantle it entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense administration: beneficiary (derives legitimacy and budgetary continuity from the constraint's persistence); institutional power; mobile exit (they could design a new system, but organizational inertia and sunk-cost narratives keep them locked in). Directionality is low-moderate (d~0.25–0.35) — they benefit substantially but are not trapped. Flood-exposed communities: victim (depend on a hollow system, bear actual risk); powerless; trapped exit (geography and legal obligation to the system). Directionality is high (d~0.85–0.95) — they are the target of the constraint's hollow operation. Field responders: mixed payer/observer role; moderate power; constrained exit (they know the system is hollow but lack authority to change it). The asymmetry is structural: those who could fix the system benefit from not doing so; those who suffer cannot exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This is the canonical Piton scenario: founding problem (prepare for floods, learn from disaster) is dead (flood patterns have changed, the scenario library is obsolete), yet the constraint persists (drills run, inspections happen, reports file). The mandatrophy is resolved at the satisficing boundary: the cost to political overseers of admitting the system is hollow exceeds the cost of the hollow system's continued operation; the cost to the administration of genuine modernization exceeds the institutional benefit of doing so (it would require admitting past negligence); the community's trapped status means they cannot impose exit pressure. The theater ratio crossing 0.8 is the diagnostic signature — performance dominates function. The husk reading PREVENTS the false triumphalism of the competence reading (which would claim the drills maintain live adaptive knowledge) while stopping short of the snare classification (which would require identifiable intentional extraction — here, extraction arises from organizational inertia, not malice).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_atrophy_mechanism,
    'Is the hollowing of preparedness knowledge a gradual generational loss (new hires learn from manuals, not from mentorship or lived experience), or an abrupt institutional choice to freeze the scenario library?',
    'Historical analysis of drill scenario changes and staff turnover records; interviews with field responders about their training sources; comparison of scenario library updates over time to actual flood-pattern evolution in hydrological databases.',
    'If gradual generational loss, the husk reading persists as institutional inertia; if abrupt choice, it reframes as a snare. The classification boundary hinges on whether the administration chose the hollowing or merely failed to prevent it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_atrophy_mechanism, empirical, 'Whether knowledge decay is incidental inertia or deliberate institutional choice.').

omega_variable(
    suppression_internalization_boundary,
    'Are field responders and communities suppressed by structural barriers (they are not given access to current hydrological data, their feedback is not solicited), or have they internalized the suppression (they have come to believe the fixed scenario library is correct)?',
    'Post-constraint-change trajectory: if communities and field responders rapidly adapt to new scenario libraries and responsive protocols after modernization, suppression was structural; if they initially resist updated scenarios or cling to old protocols, suppression was internalized.',
    'If structural, the constraint''s effective suppression is what the metric reports; if internalized, the effective suppression is higher — communities carry it with them even after the structural barrier is removed, and remediation requires cultural retraining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_boundary, empirical, 'Structural vs. internalized suppression in the hollow preparedness system.').

omega_variable(
    kernel_reading_instability,
    'The husk reading asserts that organizational form persists while function hollows. But if a major flood occurs that the fixed scenario library does NOT handle well, does the husk reading persist or does it flip to a snare reading (the administration deliberately kept scenarios outdated to avoid costly modernization)?',
    'Post-flood post-mortem analysis: does the administration blame the flood''s novelty (supporting husk), or does evidence emerge that they withheld scenario updates they knew were needed (supporting snare)?',
    'A post-disaster revelation of withheld modernization data would reclassify the constraint to snare and escalate the founding_problem_status from ''dead'' to ''deliberately abandoned.'' The reading itself depends on the suppression being incidental, not intentional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_instability, conceptual, 'Whether the husk reading remains stable or collapses under stress to a snare reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_husk_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(prep_husk_tr_t0, observed).
narrative_ontology:measurement(prep_husk_tr_t5, preparedness_transmission__husk_reading, theater_ratio, 5, 0.61).
narrative_ontology:measurement_basis(prep_husk_tr_t5, observed).
narrative_ontology:measurement(prep_husk_tr_t10, preparedness_transmission__husk_reading, theater_ratio, 10, 0.68).
narrative_ontology:measurement_basis(prep_husk_tr_t10, observed).
narrative_ontology:measurement(prep_husk_tr_t15, preparedness_transmission__husk_reading, theater_ratio, 15, 0.73).
narrative_ontology:measurement_basis(prep_husk_tr_t15, observed).
narrative_ontology:measurement(prep_husk_tr_t20, preparedness_transmission__husk_reading, theater_ratio, 20, 0.76).
narrative_ontology:measurement_basis(prep_husk_tr_t20, observed).
narrative_ontology:measurement(prep_husk_tr_t25, preparedness_transmission__husk_reading, theater_ratio, 25, 0.79).
narrative_ontology:measurement_basis(prep_husk_tr_t25, observed).
narrative_ontology:measurement(prep_husk_tr_t30, preparedness_transmission__husk_reading, theater_ratio, 30, 0.8).
narrative_ontology:measurement_basis(prep_husk_tr_t30, observed).
narrative_ontology:measurement(prep_husk_tr_t35, preparedness_transmission__husk_reading, theater_ratio, 35, 0.81).
narrative_ontology:measurement_basis(prep_husk_tr_t35, observed).
narrative_ontology:measurement(prep_husk_tr_t40, preparedness_transmission__husk_reading, theater_ratio, 40, 0.81).
narrative_ontology:measurement_basis(prep_husk_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_husk_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(prep_husk_be_t0, observed).
narrative_ontology:measurement(prep_husk_be_t5, preparedness_transmission__husk_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(prep_husk_be_t5, observed).
narrative_ontology:measurement(prep_husk_be_t10, preparedness_transmission__husk_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(prep_husk_be_t10, observed).
narrative_ontology:measurement(prep_husk_be_t15, preparedness_transmission__husk_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(prep_husk_be_t15, observed).
narrative_ontology:measurement(prep_husk_be_t20, preparedness_transmission__husk_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(prep_husk_be_t20, observed).
narrative_ontology:measurement(prep_husk_be_t25, preparedness_transmission__husk_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(prep_husk_be_t25, observed).
narrative_ontology:measurement(prep_husk_be_t30, preparedness_transmission__husk_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(prep_husk_be_t30, observed).
narrative_ontology:measurement(prep_husk_be_t35, preparedness_transmission__husk_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(prep_husk_be_t35, observed).
narrative_ontology:measurement(prep_husk_be_t40, preparedness_transmission__husk_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(prep_husk_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_husk_su_t0, preparedness_transmission__husk_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(prep_husk_su_t0, observed).
narrative_ontology:measurement(prep_husk_su_t5, preparedness_transmission__husk_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(prep_husk_su_t5, observed).
narrative_ontology:measurement(prep_husk_su_t10, preparedness_transmission__husk_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(prep_husk_su_t10, observed).
narrative_ontology:measurement(prep_husk_su_t15, preparedness_transmission__husk_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(prep_husk_su_t15, observed).
narrative_ontology:measurement(prep_husk_su_t20, preparedness_transmission__husk_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(prep_husk_su_t20, observed).
narrative_ontology:measurement(prep_husk_su_t25, preparedness_transmission__husk_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(prep_husk_su_t25, observed).
narrative_ontology:measurement(prep_husk_su_t30, preparedness_transmission__husk_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(prep_husk_su_t30, observed).
narrative_ontology:measurement(prep_husk_su_t35, preparedness_transmission__husk_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(prep_husk_su_t35, observed).
narrative_ontology:measurement(prep_husk_su_t40, preparedness_transmission__husk_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(prep_husk_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel decomposes into three constraint readings, each instantiating a different assessment of institutional knowledge transmission in disaster preparedness systems. The husk reading (this file) claims the form persists but the function is hollow; it coexists with the competence_reading (which claims each generation re-validates capability through practice) and the hybrid_reading (which claims decay is stratified — engineering infrastructure knowledge remains high, civilian coordination knowledge has degraded). The three readings are not alternative measurements of one constraint; they are structurally distinct constraints that different organizational actors and epistemologies produce from the same kernel. The husk reading's founding_problem_status=dead + world_rearranges diverges from the competence_reading's implied status=live + world_rearranges, creating the contestation. Link all three via network.affects_constraints to signal the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
