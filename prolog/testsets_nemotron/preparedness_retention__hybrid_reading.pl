% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Preparedness Retention (Hybrid Reading)
 *   domain: governance/disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   The Dutch water governance system exhibits a dual-track preparedness
 *   structure: specialized institutions (Rijkswaterstaat, regional water
 *   boards) maintain live technical competence through continuous operational
 *   engagement, while broader societal actors (municipalities, safety
 *   regions, citizen communities) increasingly perform preparedness through
 *   scripted drills, tabletop exercises, and compliance rituals that do not
 *   translate into operational capacity. This stratification is not
 *   accidental — it is sustained by resource allocation, legal authority
 *   structures, and professionalization pathways that concentrate expertise
 *   and decision-making in the core institutions. The constraint coordinates
 *   genuine flood-risk management (a real coordination problem) while
 *   extracting resilience from the periphery: local actors lose the capacity
 *   to act independently, becoming dependent on centralized instruction
 *   during events. The beneficiary is institutional continuity — the core
 *   institutions secure their mandate, funding, and authority. The victim is
 *   distributed resilience — the system's overall adaptive capacity degrades
 *   as peripheral competence atrophies.
 *
 * KEY AGENTS:
 *   - core_technical_institutions: Primary beneficiary (institutional/arbitrage) — retains live competence, controls authority and resources
 *   - institutional_continuity_bureaucracy: Primary beneficiary (institutional/generational) — administers the stratified system, secures mandate
 *   - distributed_local_resilience: Primary victim (organized/constrained) — loses operational capacity, becomes dependent on central command
 *   - peripheral_operational_staff: Primary victim (moderate/identity_locked) — performs ceremony, career path depends on compliance not competence
 *   - citizen_community_response_capacity: Primary victim (powerless/trapped) — excluded from meaningful participation, expects rescue
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.38).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Preparedness Retention (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '8d9bc539-8c59-4ff6-9a41-4886b43927ec').
narrative_ontology:cs_kernel_codification('8d9bc539-8c59-4ff6-9a41-4886b43927ec', distributed).
narrative_ontology:cs_authority_grounding('8d9bc539-8c59-4ff6-9a41-4886b43927ec', extraction).
narrative_ontology:cs_interpretation_layer_present('8d9bc539-8c59-4ff6-9a41-4886b43927ec').
narrative_ontology:cs_reading_relation('8d9bc539-8c59-4ff6-9a41-4886b43927ec', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d9bc539-8c59-4ff6-9a41-4886b43927ec', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_axiom('8d9bc539-8c59-4ff6-9a41-4886b43927ec', foundational, preparedness_is_stratified_by_institutional_tier).
narrative_ontology:cs_axiom_status(preparedness_is_stratified_by_institutional_tier, holdable).
narrative_ontology:cs_axiom_grounding('8d9bc539-8c59-4ff6-9a41-4886b43927ec', preparedness_is_stratified_by_institutional_tier, empirically_contingent).
narrative_ontology:cs_axiom('8d9bc539-8c59-4ff6-9a41-4886b43927ec', foundational, centralized_expertise_creates_peripheral_fragility).
narrative_ontology:cs_axiom_status(centralized_expertise_creates_peripheral_fragility, holdable).
narrative_ontology:cs_axiom_grounding('8d9bc539-8c59-4ff6-9a41-4886b43927ec', centralized_expertise_creates_peripheral_fragility, empirically_contingent).
narrative_ontology:cs_reference_frame('8d9bc539-8c59-4ff6-9a41-4886b43927ec', delta_works_centralized_expertise_model).
narrative_ontology:cs_drift_state('8d9bc539-8c59-4ff6-9a41-4886b43927ec', contemporary_compound_flood_risk_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8d9bc539-8c59-4ff6-9a41-4886b43927ec', '2026-08-20T14:30:00Z').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, core_technical_institutions).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, institutional_continuity_bureaucracy).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, distributed_local_resilience).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, peripheral_operational_staff).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, citizen_community_response_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, peripheral_operational_staff).
narrative_ontology:constraint_vindicates(preparedness_retention__hybrid_reading, specialized_expertise_preservation_doctrine).
narrative_ontology:constraint_vindicates(preparedness_retention__hybrid_reading, centralized_command_continuity_axiom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rijkswaterstaat and the 21 regional water boards hold the legal mandate, technical expertise, and operational authority for primary flood defense. They conduct live operations (pumping, barrier management, hydraulic modeling) daily. They set preparedness standards, control funding flows, and define what counts as competence. Their exit options are arbitrage-grade: they could restructure but have no incentive to — the system rewards their monopoly.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, core_technical_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% The policy layers above the operational core (Ministry of Infrastructure and Water Management, provincial executives, national safety region coordination) that translate operational competence into legislative mandate, budget allocations, and international reputation. They benefit from the perception of a world-class system without bearing operational risk. Their careers advance by defending the institutional structure.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, institutional_continuity_bureaucracy, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, institutional_continuity_bureaucracy, agenda_setter).

% Municipalities, safety regions (veiligheidsregio's), and organized community response teams (brandweer, GHOR, Red Cross) that would be the first operational layer during a flood event. They conduct drills but lack decision authority, real-time data access, and resource control. Their exit is constrained: they can advocate for devolution but cannot act independently during events without legal exposure. They pay in atrophied capacity and dependency.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, distributed_local_resilience, payer,
    organized, biographical, constrained, regional).

% Mid-level staff at water boards, municipalities, and safety regions who execute the drills, write the plans, and attend the exercises. They gain professional status and job security (beneficiary) but their actual operational competence decays because the system rewards compliance with procedure over adaptive capacity (payer). Their identity is fused with the ceremonial role — 'I am a preparedness professional' means 'I run the drills correctly' — making exit from the frame psychologically and professionally costly.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, peripheral_operational_staff, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, peripheral_operational_staff, beneficiary).

% Residents in flood-prone areas who have no meaningful role in preparedness beyond 'follow instructions.' They fund the system through taxes and water board levies (waterschapsbelasting) but are structurally excluded from operational participation. During events, they wait for evacuation orders. Their exit is trapped: they cannot leave the floodplain, cannot build independent response capacity, and have no voice in the governance structure.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, citizen_community_response_capacity, payer,
    powerless, immediate, trapped, local).

% The analytical seat that sees the full stratified structure: the coordination function at the core, the extraction at the periphery, the rising theater ratio, the hardening suppression. This seat does not collect or pay — it classifies.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, core_technical_institutions).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates flood-risk management across a low-lying delta: hydraulic modeling, barrier operations, pumping station management, dike maintenance, and emergency decision-making are centralized in institutions with the specialized expertise to execute them reliably.
% TRANSFER_FUNCTION: Moves operational authority, resource control, and decision-making capacity from distributed local actors (municipalities, safety regions, communities) to core technical institutions (Rijkswaterstaat, water boards), justified as necessary for technical competence but resulting in peripheral atrophy.
% ABSENT_VOICES: Citizen communities in floodplains who would demand meaningful participation in preparedness if they understood the dependency trap; municipal leaders who would push for devolved authority if not constrained by liability frameworks; water board staff who would advocate for competence-preserving practices if not identity-locked into ceremonial compliance.
% DISAPPEARANCE_RATIONALE: If the stratified structure vanished overnight, core institutions would lose their monopoly on authority and resources; peripheral actors would have to rebuild operational capacity from atrophied foundations (chaotic transition); citizens would face immediate exposure without evacuation systems. The water governance system would reorganize — likely toward a more devolved but initially less competent model. The world rearranges.
% FOUNDING_PROBLEM: After the 1953 North Sea flood (1,836 deaths), the Delta Works program centralized hydraulic expertise in Rijkswaterstaat and water boards because fragmented local response had failed catastrophically. The founding problem was: how to guarantee flood defense through centralized, expert-operated infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: The 1953 flood is historical fact. The Delta Works are completed. Modern flood risk is qualitatively different: compound events (storm surge + river discharge + pluvial flooding), climate non-stationarity, and cascading infrastructure failure require distributed adaptive capacity, not just centralized barrier operations. The Dutch National Water Authority (2023) and multiple independent safety region evaluations (OVV reports 2018, 2021) attest that the current institutional structure is misaligned with current risk — corroboration from outside the core beneficiary institutions.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).
:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the asymmetry: core institutions capture authority and resources while peripheral resilience atrophies. The coordination function is real — flood defense requires specialized hydraulic engineering — but the same structure suppresses distributed response capacity. Suppression (0.38) is moderate: the constraint operates through resource allocation, legal monopoly on emergency decision-making, and professional gatekeeping rather than overt coercion. Theater ratio (0.55) is elevated and rising: peripheral drills increasingly simulate competence without preserving it. Accessibility collapse (0.48) is partial — alternatives (community-based response, municipal operational capacity) exist but are starved of resources and authority. Resistance (0.32) is low but measurable: some water boards experiment with citizen participation, and safety regions push for devolved authority. The measurements show a 15-year trajectory of rising extraction and theater, with suppression hardening gradually — consistent with a coordination function being progressively captured by institutional self-preservation.
 *
 * PERSPECTIVAL GAP:
 *   From the core institution seat (agenda_setter, institutional power, arbitrage exit), the constraint appears as necessary specialization: expertise is preserved where it matters, ceremony elsewhere is harmless compliance. From the peripheral victim seats (payer, organized/moderate power, constrained/identity_locked exit), the same structure operates as enforced dependency — they bear the cost of atrophied capacity without the authority to rebuild it. The citizen seat (payer, powerless, trapped) experiences pure extraction: they fund the system through taxes and water board levies but have no operational role. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analytical seat's reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: core_technical_institutions and institutional_continuity_bureaucracy collect authority, funding, and mandate protection — they are structural beneficiaries (d near 0.1-0.2). Victims: distributed_local_resilience, peripheral_operational_staff, and citizen_community_response_capacity bear the cost of atrophied capacity and dependency — they are structural targets (d near 0.7-0.9). The peripheral staff are identity_locked: their professional identity is fused with the ceremonial role, making exit from the frame nearly impossible even though they could technically change jobs. Citizens are trapped: no meaningful exit from the water governance system exists. The analytical observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1953 flood response: centralized hydraulic expertise was necessary and sufficient) is DEAD — modern flood risk is distributed, compound, and requires layered response. The arrangement persists because the core institutions successfully redefined 'preparedness' to mean 'what we do' rather than 'what the system can do.' The mandate has outlived its function: centralization was the solution to a coordination problem that no longer exists in the same form, but the institutional structure that solved it now suppresses the distributed capacity needed for current risks. This is mandatrophy resolved as tangled_rope: coordination function real but captured, extraction asymmetric, enforcement active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is this constraint one reading (hybrid) of the contested kernel ''preparedness_retention'', distinct from competence_reading and husk_reading?',
    'Cross-reading structural comparison: if the three readings produce different ε, different beneficiary/victim structures, and different computed seat types from the same referent, they are structurally distinct constraints linked by network.affects_constraints.',
    'Confirms the ε-invariance decomposition: each reading gets its own constraint story with its own classification; the kernel is the committer frame, not a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Validates that hybrid_reading is a separate constraint story from its siblings.').

omega_variable(
    stratification_boundary,
    'Where exactly does the boundary lie between ''core technical institutions'' that retain live competence and ''peripheral actors'' that perform ceremony?',
    'Institutional audit mapping drill outcomes, decision authority during incidents, and resource allocation across the Dutch water governance system (Rijkswaterstaat, water boards, municipalities, safety regions).',
    'If the boundary is porous (competence diffuses outward), the constraint trends toward competence_reading (rope). If the boundary is impermeable (ceremony fully substitutes for competence at periphery), it trends toward husk_reading (piton/snare). The hybrid classification depends on a real, sustained stratification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_boundary, empirical, 'Boundary porosity between core competence retention and peripheral ceremonial substitution.').

omega_variable(
    single_point_of_failure_risk,
    'Does centralized expertise in Rijkswaterstaat/water boards create an actual single point of failure for distributed resilience, or is there sufficient redundancy?',
    'Scenario testing and historical incident analysis: examine near-miss events where core institution capacity was degraded (staff loss, system failure, communication breakdown) and measure peripheral response degradation.',
    'If single point of failure is real, the victim class ''distributed_local_resilience'' bears structural extraction — the constraint is tangled_rope with genuine asymmetric cost. If redundancy exists, extraction is lower and the constraint may be a degraded rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_point_of_failure_risk, empirical, 'Whether centralized technical competence creates structural fragility for the broader system.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of distributed resilience structural (institutional monopoly on authority/resources) or internalized (communities believe only experts can manage water threats)?',
    'Post-event community response analysis: if communities self-mobilize effectively when formal systems are overwhelmed, suppression is partly internalized. If they wait for expert instruction even when-delayed, internalization is high.',
    'If internalized, effective suppression exceeds the structural measure — communities carry the suppression with them. This would increase the constraint''s extractiveness from the victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for peripheral resilience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_ret_hybrid_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(prep_ret_hybrid_tr_t0, observed).
narrative_ontology:measurement(prep_ret_hybrid_tr_t10, preparedness_retention__hybrid_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(prep_ret_hybrid_tr_t10, observed).
narrative_ontology:measurement(prep_ret_hybrid_tr_t20, preparedness_retention__hybrid_reading, theater_ratio, 20, 0.49).
narrative_ontology:measurement_basis(prep_ret_hybrid_tr_t20, observed).
narrative_ontology:measurement(prep_ret_hybrid_tr_t30, preparedness_retention__hybrid_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement_basis(prep_ret_hybrid_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(prep_ret_hybrid_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(prep_ret_hybrid_be_t0, observed).
narrative_ontology:measurement(prep_ret_hybrid_be_t10, preparedness_retention__hybrid_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement_basis(prep_ret_hybrid_be_t10, observed).
narrative_ontology:measurement(prep_ret_hybrid_be_t20, preparedness_retention__hybrid_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(prep_ret_hybrid_be_t20, observed).
narrative_ontology:measurement(prep_ret_hybrid_be_t30, preparedness_retention__hybrid_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(prep_ret_hybrid_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_ret_hybrid_su_t0, preparedness_retention__hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(prep_ret_hybrid_su_t0, observed).
narrative_ontology:measurement(prep_ret_hybrid_su_t10, preparedness_retention__hybrid_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement_basis(prep_ret_hybrid_su_t10, observed).
narrative_ontology:measurement(prep_ret_hybrid_su_t20, preparedness_retention__hybrid_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement_basis(prep_ret_hybrid_su_t20, observed).
narrative_ontology:measurement(prep_ret_hybrid_su_t30, preparedness_retention__hybrid_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(prep_ret_hybrid_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).

% DUAL FORMULATION NOTE:
% This hybrid_reading decomposes the natural-language concept 'preparedness retention' into a structurally distinct claim from its siblings. competence_reading claims the system preserves live competence throughout (ε ≈ 0.15, rope). husk_reading claims the system is entirely ceremonial (ε ≈ 0.65, piton/snare). This reading claims a stratified dual-track structure (ε = 0.42, tangled_rope). The ε values differ because the structural referents differ: competence_reading measures the core only, husk_reading measures the periphery only, hybrid_reading measures the system-level asymmetry. All three stories share kernel_id 'preparedness_retention' and are linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, organized, 0.75).
constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, moderate, 0.8).
constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
