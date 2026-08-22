% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Hybrid Preparedness Commitment System
 *   domain: institutional/social
 *
 * SUMMARY:
 *   Disaster preparedness operates as a hybrid system where memorial elements
 *   (historical narratives, ritual drills, institutional identity,
 *   commemorative structures) stabilize long-term social commitment to
 *   preparedness, while competence elements (training, equipment, procedure
 *   refinement, operational testing) maintain actual disaster-response
 *   capacity. This reading holds BOTH layers as structurally necessary and
 *   structurally extractive. The memorial layer prevents political
 *   abandonment of preparedness during stability periods—a coordination
 *   function—but it also creates institutional inertia that can preserve
 *   outdated knowledge and consume resources better spent on operational
 *   capability. The competence layer prevents catastrophic failure—another
 *   coordination function—but it is often subordinated to memorial
 *   compliance, creating a zone where frontline responders and
 *   resource-constrained jurisdictions pay the cost of maintaining both
 *   layers simultaneously. The tension between layers generates continuous
 *   extraction: resources diverted to memorial maintenance, labor time
 *   consumed by ritual compliance, authority asymmetries that protect
 *   institutional stewards while exposing frontline responders and vulnerable
 *   populations.
 *
 * KEY AGENTS:
 *   - Institutional memory stewards (agenda-setters, identity-locked): maintain preparedness institutions, archives, training programs, ritual rehearsals, certification bodies; legitimacy tied to historical stewardship role
 *   - Continuity planners (beneficiaries, powerful): leverage institutional legitimacy of preparedness to sustain funding and planning capacity even in low-threat periods; benefit from memorial layer's political cover
 *   - Disaster response frontline (payers, moderate/local): must maintain memorial compliance while also building operational capacity; bear costs when resources divide between memorial and competence
 *   - Resource-constrained jurisdictions (payers, powerless/regional): caught between institutional requirement to maintain full architecture and fiscal inability to do both memorial and genuine readiness; pay through performance vs. capability deficit
 *   - Political leadership (agenda-setters, institutional): cite the preparedness architecture (memorials, plans) as evidence of stewardship; can exit support while still claiming preparedness responsibility
 *   - Disaster survivors (excluded, powerless): experience system failures; testimony fed into post-disaster inquiries but not into system design; not represented in institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.62).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.41).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Hybrid Preparedness Commitment System").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "institutional/social").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, 'b0c805c0-d912-4cae-ac04-7c6e1066f5c0').
narrative_ontology:cs_kernel_codification('b0c805c0-d912-4cae-ac04-7c6e1066f5c0', distributed).
narrative_ontology:cs_authority_grounding('b0c805c0-d912-4cae-ac04-7c6e1066f5c0', extraction).
narrative_ontology:cs_interpretation_layer_present('b0c805c0-d912-4cae-ac04-7c6e1066f5c0').
narrative_ontology:cs_reading_relation('b0c805c0-d912-4cae-ac04-7c6e1066f5c0', preparedness_commitment__competence_reading, influences).
narrative_ontology:cs_reading_relation('b0c805c0-d912-4cae-ac04-7c6e1066f5c0', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_axiom('b0c805c0-d912-4cae-ac04-7c6e1066f5c0', foundational, dual_layer_structural_necessity).
narrative_ontology:cs_axiom_status(dual_layer_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('b0c805c0-d912-4cae-ac04-7c6e1066f5c0', dual_layer_structural_necessity, instrumental).
narrative_ontology:cs_axiom('b0c805c0-d912-4cae-ac04-7c6e1066f5c0', foundational, memorial_political_irreplaceability).
narrative_ontology:cs_axiom_status(memorial_political_irreplaceability, holdable).
narrative_ontology:cs_axiom_grounding('b0c805c0-d912-4cae-ac04-7c6e1066f5c0', memorial_political_irreplaceability, empirically_contingent).
narrative_ontology:cs_reference_frame('b0c805c0-d912-4cae-ac04-7c6e1066f5c0', dual_layer_integrated_preparedness).
narrative_ontology:cs_drift_state('b0c805c0-d912-4cae-ac04-7c6e1066f5c0', contemporary_post_pandemic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0c805c0-d912-4cae-ac04-7c6e1066f5c0', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, institutional_memory_stewards).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, continuity_planners).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, disaster_response_frontline).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, resource_constrained_jurisdictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, political_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain preparedness institutions—archives, training programs, ritual rehearsals, memorial days, certification bodies. Their legitimacy rests on the claim that these preserve knowledge and commitment across administrations. They collect resources (budget, staffing, real estate) justified by historical mandate. Their professional identity is fused with the continuity they steward.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, institutional_memory_stewards, agenda_setter,
    institutional, generational, identity_locked, national).

% Rely on the memorial layer to justify sustained funding for preparedness infrastructure even in periods of political inattention. They can leverage the institutional legitimacy of historical commitment (it has always been done) to resist budget cuts and maintain planning capacity. Their power derives from institutional continuity claims rather than demonstrated operational necessity in any given moment.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, continuity_planners, beneficiary,
    powerful, generational, arbitrage, national).

% Emergency responders and community preparedness coordinators operate within the dual constraint. They must maintain memorial compliance (drills, certifications, reports to satisfy the institutional layer) even when resources would better serve actual readiness competence. When disaster strikes, they bear the cost of gaps between memorial adequacy and operational capacity—disorganization, insufficient equipment or training, coordination failures that memorial performances did not reveal.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_response_frontline, payer,
    moderate, immediate, trapped, local).

% Smaller municipalities and rural areas are caught between the institutional requirement to maintain the full preparedness architecture (memorials, drills, certifications, institutional compliance) and the fiscal reality that they lack staff and budget for both memorial maintenance and genuine operational readiness. They pay through the overhead of performative compliance—resources diverted from actual capability building, burnout from ceremonial vs. functional tasks, and vulnerability to the specific disasters their context faces.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, resource_constrained_jurisdictions, payer,
    powerless, biographical, constrained, regional).

% Historians, archivists, and institutional critics who study whether the memorial layer accurately transmits knowledge or has become disconnected from operational reality. They are excluded from operational preparedness decisions; their research pointing to knowledge gaps or historical distortion is treated as external commentary, not as governance feedback that would reshape the dual-layer structure.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, historical_mandate_interpreters, excluded,
    organized, generational, constrained, national).

% People who experience the preparedness system's failures when disaster strikes. Their testimony about what went wrong feeds post-disaster inquiries but not pre-disaster preparedness planning; they have no seat at the institutional arrangement that determines whether and how preparedness is maintained. After each disaster, the system often responds by adding more memorial rituals rather than diagnosing competence gaps.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_survivors, excluded,
    powerless, biographical, trapped, local).

% Can cite the institutional preparedness architecture (memorials, drills, plans) as evidence of responsible stewardship even when competence gaps exist. The memorial layer provides political cover—a response to any criticism of unreadiness is 'we have a full preparedness system.' They benefit from the legitimacy the memorial layer provides without bearing the cost of competence maintenance; they can exit (not fund or support preparedness) and still point to the inherited architecture.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, political_leadership, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, political_leadership, beneficiary).

% Emergency management professionals, systems engineers, and capability assessors who advocate for competence-first approaches. They observe both layers of the system and document the structural tension—where memorial requirements consume resources that should build actual disaster-response capacity. They provide analytical expertise but lack authority to reshape the institutional arrangement.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, operational_competence_advocates, observer,
    moderate, immediate, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, institutional_memory_stewards).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains institutional commitment to preparedness across generations despite political turnover and competing budget priorities. The memorial layer (rituals, anniversaries, historical narratives, institutional identity) stabilizes the commitment itself—keeps society from abandoning preparedness during periods of stability or policy shifts. The competence layer (training, equipment, procedure refinement, operational testing) maintains the functional capacity to actually respond to specific disasters.
% TRANSFER_FUNCTION: Moves resources and labor time from operational capability building toward institutional maintenance (memorial compliance, ritual participation, ceremonial coordination, archival management). In resource-constrained settings, this creates a direct extraction: funds budgeted for emergency equipment or tactical training flow instead to memorial event production or institutional legitimacy defense. The transfer persists because political leadership and institutional stewards benefit from the memorial layer's legitimacy coverage, while frontline responders and vulnerable populations bear the cost of the diverted resources.
% ABSENT_VOICES: Disaster survivors and communities that have experienced preparedness failures are not represented in the institutional arrangement that determines how preparedness is structured. Historians and institutional critics who might document knowledge loss in the memorial layer are excluded from governance. Communities in low-resource jurisdictions whose competence deficits are masked by memorial performance have limited voice in the system's design.
% DISAPPEARANCE_RATIONALE: If the hybrid preparedness system disappeared, institutional commitment to preparedness would erode during periods of stability—without the memorial layer to stabilize the commitment, budget cuts and political inattention would progressively dismantle preparedness infrastructure. Simultaneously, without the competence layer, surviving institutional memory would become increasingly disconnected from actual disaster response capacity. The world would reorganize around either pure competence focus (immediate results-driven preparedness) or periodic crisis response (reactive rather than sustained preparedness), losing the intergenerational stability the memorial layer provides and the operational effectiveness the competence layer provides.
% FOUNDING_PROBLEM: After major historical disasters, societies struggled with two simultaneous challenges: (1) How to maintain institutional commitment to preparedness across generations, through periods of stability when preparedness seemed unnecessary and through political transitions that might eliminate preparedness advocates; (2) How to keep that institutional commitment connected to actual operational capacity rather than becoming hollow ritual. Early preparedness systems tried single approaches—some pure memorial (institutional commitment but no competence), others pure competence-driven (operational capacity but eroding political support across generations). The hybrid system emerged as an attempted solution: let the memorial layer stabilize the social commitment, while the competence layer maintains operational function.
% FOUNDING_PROBLEM_CORROBORATION: Post-disaster reviews and emergency management studies from outside the institutional steward class (academic research on organizational learning, comparative disaster response analysis, survivor testimony) document that the founding problem is partly live—political commitment to preparedness does erode during stability periods without memorial structures. However, the same studies show the competence problem is substantially unsolved: memorial compliance often crowds out genuine capability building, creating hollow preparedness that collapses under actual disaster stress. Historical archives and institutional critics provide testimony that the memorial layer increasingly preserves outdated procedures and narratives that no longer match the hazard landscape or operational capacity.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.48 and plateaus around 0.62 because the constraint maintains relatively stable extraction across the interval—the dual-layer structure continuously diverts resources from frontline capability to institutional maintenance, but this rate is stable rather than accumulating. Theater rises early (0.38 to 0.45 by t=15) then stabilizes (0.48–0.49), indicating that as the system matures, memorial elements become increasingly decoupled from operational testing—the memorial performance becomes more refined and polished while the competence layer suffers incremental drift. Suppression requirement increases gently (0.35 to 0.41) because the institutional arrangement faces growing pressure to justify itself: survivor testimony, failed response incidents, and resource constraints create resistance that must be actively suppressed through legitimacy defense and memorial reinforcement. The measurements are authored on a single shared time grid; every metric carries a value at every examined time point to prevent OQ-105-style misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional memory stewards' position, the hybrid system is genuine coordination—it preserves knowledge, stabilizes commitment, and enables long-term planning across generations. From the disaster response frontline's position, the same structure is extractive enforcement that consumes resources and constrains their ability to respond effectively. From continuity planners' perspective, the memorial layer is a political necessity that enables sustained competence investment. From resource-constrained jurisdictions' perspective, the memorial requirements are pure overhead that prevents them from building the capabilities their specific hazard environment demands. The engine should compute substantially different types for these seats: stewards and planners may compute as beneficiaries within a coordination function; frontline responders and constrained jurisdictions should compute as targets within an extraction regime. The authored metrics describe the structure from the frontline/constrained perspective—the view that experiences the extraction most directly.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional memory stewards sit at d ≈ 0.2 (strong beneficiary end): they direct the agenda, control resource flows, and their professional identity is protected by the arrangement. Continuity planners sit at d ≈ 0.25: they benefit from sustained funding but lack direct agenda control—they depend on stewards' institutional authority. Disaster response frontline sits at d ≈ 0.75 (strong target end): they must comply with the full dual-layer structure while bearing immediate consequences when competence gaps emerge; their exit options are trapped—they cannot opt out of the institutional compliance without losing professional legitimacy. Resource-constrained jurisdictions sit at d ≈ 0.8 (strongest target end): they have least power, least exit options (constrained by federal mandates), and highest per-capita burden of maintaining both layers on thin budgets. Political leadership sits at d ≈ 0.4 (moderate beneficiary end): they enjoy the legitimacy the memorial layer provides but can exit deeper support; they are partially insulated from competence failures because they control which disasters become political crises. The engine derives these directionalities from beneficiary/victim declarations and power-level/exit-options combinations; the divergence between stewards' experience (low extraction, high legitimacy) and frontline experience (high extraction, trapped compliance) is the seat-level classification divergence the engine computes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional commitment persistence + operational competence maintenance) was live and acute in the period after major historical disasters; society faced genuine pressure to abandon preparedness during stability periods while simultaneously maintain capacity for the rare-event response. The hybrid system provided a solution: use memorial elements to stabilize political commitment, use competence elements to maintain operational function. However, the problem is now contested: disaster survivors and emergency management research indicate that the competence problem is substantially solved (technical knowledge exists, procedures can be maintained)—the live problem is now political will and resource allocation, not knowledge loss. The memorial layer, once functionally necessary to prevent abandonment, now carries the primary weight of stabilizing an arrangement that continues partly for institutional inertia (because stewards' identities are fused with it) rather than because the founding problem remains acute. A mandatrophy reading would note that the constraint persists partly because its beneficiaries (institutional stewards, continuity planners, political leadership) have structural incentives to maintain it even as its original justification weakens. The competence reading would argue that the competence layer alone could now stabilize commitment—if preparedness capacity was demonstrably high, political support would follow—and the memorial layer has become purely extractive overhead. The husk reading would argue that the memorial layer has already become disconnected from operational reality, preserving obsolete narratives while real competence remains scattered across ad-hoc responses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memorial_necessity_empirical,
    'Is the memorial layer structurally necessary to prevent political abandonment of preparedness during stability periods, or would demonstrably high operational competence alone sustain political commitment?',
    'Natural experiment from jurisdictions that de-emphasize memorial elements while maintaining high operational competence: do their preparedness budgets erode faster than hybrid-system jurisdictions during stability periods? Do political leaders cite competence metrics to justify support, or do memorial/historical narratives remain primary?',
    'If memorial is necessary, the hybrid system is a genuine tangled_rope with asymmetric coordination/extraction. If competence alone sustains commitment, the memorial layer is pure extraction overhead and the reading shifts toward snare. If memorial and competence reinforce each other, the system approaches rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_necessity_empirical, empirical, 'Whether the memorial layer provides irreplaceable political stabilization or is redundant overlay.').

omega_variable(
    knowledge_preservation_disconnection,
    'To what extent has the memorial layer become disconnected from operational knowledge that disaster response actually requires? How much of what the memorial layer preserves is obsolete relative to evolving hazard landscapes and technical capacity?',
    'Post-disaster autopsies comparing what memorial narratives prescribe vs. what operational response actually needed; historical analysis of knowledge drift in archived procedures; comparative assessment of memorial-educated responders vs. competence-trained responders under disaster stress.',
    'High disconnection supports the husk_reading and mandatrophy diagnosis; low disconnection supports the hybrid_reading''s claim that memorial preserves functional knowledge. Partial disconnection indicates the hybrid system is becoming increasingly extractive as memorial overhead grows relative to knowledge value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(knowledge_preservation_disconnection, empirical, 'Whether the memorial layer preserves knowledge or maintains performance disconnected from operational reality.').

omega_variable(
    resource_zero_sum_evidence,
    'To what extent are memorial requirements and competence requirements actually competing for the same finite resources, vs. serving different budget silos or constituencies?',
    'Budget analysis showing where preparedness dollars flow (memorial vs. operational); interviews with jurisdictions under fiscal stress tracking what gets cut when budgets tighten; modeling of resource-constrained jurisdictions'' choices when forced to choose.',
    'If truly zero-sum, the extraction is direct and measurable—resources for memorial are resources not available for competence. If served by separate silos (memorial by cultural appropriations, competence by public safety budgets), the extraction is diffuse and less visible but may still be real in opportunity cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_zero_sum_evidence, empirical, 'Whether memorial and competence requirements directly compete for limited resources.').

omega_variable(
    identity_lock_steward_resilience,
    'How tightly is institutional memory stewards'' professional identity fused with the preparedness system? To what extent would they experience role dissolution if the system''s emphasis shifted from dual-layer to competence-focused?',
    'Qualitative research on steward career trajectories, professional networks, and identity narratives; observation of steward responses to competence-priority proposals; post-reform analysis where some jurisdictions have shifted emphasis.',
    'High identity fusion means stewards are identity-locked and will resist competence-priority reforms; the constraint''s persistence is party-defended rather than structurally necessary. This supports the snare reading. Low fusion means stewards could adapt to reformed systems; the constraint''s persistence is more structurally grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_steward_resilience, conceptual, 'Degree of identity lock binding stewards to the dual-layer arrangement.').

omega_variable(
    sibling_reading_empirical_test,
    'What observable differences would distinguish the three readings (competence, husk, hybrid) under conditions of jurisdictional choice or natural variation?',
    'Comparative case study: jurisdictions that emphasize memorial (husk-region prediction), competence (competence-region prediction), or balanced dual-layer (hybrid prediction). Track disaster outcomes, budget stability, knowledge retention, and institutional continuity. Under what conditions does each reading''s prediction match observed behavior?',
    'If competence-only jurisdictions show stable budgets and good disaster outcomes, competence_reading is empirically validated; if memorial-only jurisdictions show eroding capability, husk_reading is validated; if dual-layer shows best political durability combined with moderate competence gaps, hybrid_reading is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_test, empirical, 'Comparative test of which reading''s structural claims match observed behavior across institutional variation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__hybrid_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__hybrid_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__hybrid_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__hybrid_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(prep_tr_t25, preparedness_commitment__hybrid_reading, theater_ratio, 25, 0.49).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__hybrid_reading, theater_ratio, 30, 0.49).
narrative_ontology:measurement(prep_tr_t35, preparedness_commitment__hybrid_reading, theater_ratio, 35, 0.48).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__hybrid_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__hybrid_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__hybrid_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__hybrid_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(prep_be_t25, preparedness_commitment__hybrid_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__hybrid_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(prep_be_t35, preparedness_commitment__hybrid_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(prep_su_t5, preparedness_commitment__hybrid_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__hybrid_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(prep_su_t15, preparedness_commitment__hybrid_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__hybrid_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(prep_su_t25, preparedness_commitment__hybrid_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__hybrid_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement(prep_su_t35, preparedness_commitment__hybrid_reading, suppression_requirement, 35, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__hybrid_reading, 0.18).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the preparedness_commitment kernel. The competence_reading emphasizes that live, exercised operational knowledge provides both institutional stability and disaster-response function—knowledge persists through practice, not memorial. The husk_reading emphasizes that preparedness has atrophied into memorial performance disconnected from operational reality. The hybrid_reading (this one) holds that both layers coexist, create structural tension, and generate continuous extraction. All three readings share the referent (the standing institutional preparedness system) but attribute its persistence to different mechanisms: competence reading via knowledge vitality, husk reading via institutional inertia, hybrid reading via tension-maintenance between memorial stabilization and competence function. The three readings are linked because each prediction about preparedness outcomes depends on which reading's structure is empirically true—they affect each other through causal dependency on a shared institutional system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
