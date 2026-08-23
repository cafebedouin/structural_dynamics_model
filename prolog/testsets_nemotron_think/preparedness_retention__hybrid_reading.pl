% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Preparedness Retention (Hybrid Reading)
 *   domain: disaster_preparedness/governance/institutional_memory
 *
 * SUMMARY:
 *   The Dutch flood preparedness system operates on two tracks: a technical
 *   core (Rijkswaterstaat, water boards) where engineers maintain live
 *   competence through daily operational responsibility for primary defenses,
 *   and a ceremonial periphery where municipalities, volunteers, and citizens
 *   participate in drills, inspections, and awareness campaigns that resemble
 *   preparedness but lack technical decision authority. The hybrid reading
 *   asserts this stratification is real and structural — not a temporary gap,
 *   not a universal failure. The coordination function (guaranteed technical
 *   competence) is genuine; the extraction (monopoly on technical authority
 *   that atrophies distributed resilience) is asymmetric. The system persists
 *   because the core institutions benefit from the monopoly and the
 *   periphery's identity is fused to the ceremonial role.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.55).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.45).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Preparedness Retention (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "disaster_preparedness/governance/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, 'c12e6e31-be84-4563-8e3f-946f38260513').
narrative_ontology:cs_kernel_codification('c12e6e31-be84-4563-8e3f-946f38260513', formalized).
narrative_ontology:cs_authority_grounding('c12e6e31-be84-4563-8e3f-946f38260513', extraction).
narrative_ontology:cs_interpretation_layer_present('c12e6e31-be84-4563-8e3f-946f38260513').
narrative_ontology:cs_reading_relation('c12e6e31-be84-4563-8e3f-946f38260513', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c12e6e31-be84-4563-8e3f-946f38260513', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_axiom('c12e6e31-be84-4563-8e3f-946f38260513', foundational, stratified_competence_retention).
narrative_ontology:cs_axiom_status(stratified_competence_retention, holdable).
narrative_ontology:cs_axiom_grounding('c12e6e31-be84-4563-8e3f-946f38260513', stratified_competence_retention, conventional).
narrative_ontology:cs_axiom('c12e6e31-be84-4563-8e3f-946f38260513', foundational, institutional_continuity_justifies_centralization).
narrative_ontology:cs_axiom_status(institutional_continuity_justifies_centralization, holdable).
narrative_ontology:cs_axiom_grounding('c12e6e31-be84-4563-8e3f-946f38260513', institutional_continuity_justifies_centralization, conventional).
narrative_ontology:cs_reference_frame('c12e6e31-be84-4563-8e3f-946f38260513', post_1953_institutional_settlement).
narrative_ontology:cs_drift_state('c12e6e31-be84-4563-8e3f-946f38260513', contemporary_climate_adaptation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c12e6e31-be84-4563-8e3f-946f38260513', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_water_institutions).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, centralized_technical_staff).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, distributed_community_resilience).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, peripheral_actors).
narrative_ontology:constraint_vindicates(preparedness_retention__hybrid_reading, institutional_continuity_doctrine).
narrative_ontology:constraint_vindicates(preparedness_retention__hybrid_reading, specialized_expertise_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rijkswaterstaat and the regional water boards hold the legal mandate and operational infrastructure for flood defense. They set preparedness standards, control technical certification, and administer the drills and inspections that define the system. Their continuity is the system's organizing principle; they can redirect resources across scales and have no structural exit from their mandate.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, specialized_water_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Core hydraulic engineers, dike managers, and modeling specialists retain live operational competence through daily practice on the primary defense infrastructure. Their expertise is the system's coordination backbone. They benefit from professional recognition, career stability, and exclusive authority over technical judgments. Exit means leaving the specialized domain entirely — skills are not transferable to peripheral roles.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, centralized_technical_staff, beneficiary,
    powerful, biographical, constrained, national).

% Local communities, volunteer networks, and historical water-user collectives once held distributed flood response knowledge (dike watching, local evacuation routes, mutual aid). The stratified system treats this as obsolete ceremony; the knowledge decays without practice. They bear the cost of lost self-reliance and single-point-of-failure vulnerability. Exit would require rebuilding local capacity from scratch against institutional discouragement.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, distributed_community_resilience, payer,
    organized, generational, constrained, regional).

% Municipal emergency coordinators, local volunteers, and citizen participants in drills perform preparedness rituals that feel meaningful but lack technical decision authority. Their identity fuses with the ceremonial role — 'we are the ones who participate' — making exit psychologically costly even as the rituals' operational value erodes. They pay in time, trust, and false confidence.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, peripheral_actors, payer,
    moderate, biographical, identity_locked, local).

% Ministries (Infrastructure & Water Management, Interior) oversee the legal framework and funding but delegate technical execution to the specialized institutions. They commission reviews after near-misses, receive assurances of competence, and face political pressure to demonstrate preparedness. Their view is mediated by institutional reports; they neither run drills nor bear local consequences.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, national_government, observer,
    institutional, generational, analytical, national).

% Delta nations (Bangladesh, Vietnam, US Gulf Coast) and EU flood-directive partners who would challenge the Dutch model's exportability. They observe the ceremonial layer being presented as best practice while the technical core remains untransferable. Their exclusion is structural: the hybrid system's legitimacy depends on not testing its distributional claims against peer scrutiny.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, international_peers, excluded,
    powerful, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operational flood defense competence in specialized institutions through continuous technical practice on primary infrastructure — ensuring that the hardest engineering judgments (dike reinforcement priorities, storm surge modeling, real-time gate operations) rest with staff who exercise them daily.
% TRANSFER_FUNCTION: Moves distributed preparedness capability from communities and peripheral actors to centralized institutions; communities lose hands-on resilience and local knowledge, institutions gain monopoly on technical authority and control over the preparedness narrative. The transfer is not monetary but epistemic and organizational: who holds the live competence to act when the water rises.
% ABSENT_VOICES: Communities that would demand distributed preparedness capacity are structurally excluded; their knowledge is treated as obsolete by the very institutions that benefit from its atrophy. Climate adaptation NGOs and neighborhood resilience initiatives are consulted performatively but hold no technical veto. The excluded are those who would experience the single point of failure first.
% DISAPPEARANCE_RATIONALE: If the dual-track system vanished overnight, flood defense would either centralize completely (losing the residual local knowledge that still supplements formal systems) or force a chaotic distributed re-learning under climate stress. Both outcomes rearrange the governance landscape: the former creates brittle centralization; the latter demands crash investment in community capacity that the current system actively discourages.
% FOUNDING_PROBLEM: Post-1953 flood disaster, the Netherlands needed guaranteed technical flood defense capacity that could not depend on variable community engagement, volunteer retention, or local political cycles. The water boards and Rijkswaterstaat were empowered to hold that guarantee as a permanent institutional mandate.
% FOUNDING_PROBLEM_CORROBORATION: Water board archives and Rijkswaterstaat institutional histories attest the founding problem was technical guarantee against catastrophic failure. Independent disaster scholars (e.g., TU Delft Safety Science group) and community resilience advocates (e.g., Nederland Weerbaar network) attest the problem has shifted: climate adaptation now requires distributed adaptive capacity that the centralized monopoly inhibits. No external corroboration supports the claim that the original problem persists unchanged.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the monopoly rent: centralized institutions capture the legitimacy and resource flows that would otherwise support distributed capacity. Suppression (0.45) is moderate — alternatives are not banned but are structurally starved (funding, recognition, technical integration). Theater ratio (0.65) is high: the peripheral drills and public campaigns are real activities but their operational contribution is marginal compared to the core's live practice. Accessibility collapse (0.6) measures how completely the 'distributed resilience' alternative has been closed off by institutional design. Resistance (0.4) exists from resilience advocates but lacks institutional leverage. The measurement series share one time grid (0–30 years post-1990s decentralization reforms) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the specialized institution's seat, the arrangement is a necessary coordination mechanism — the only way to guarantee the 1/10,000 safety standard. From the peripheral actor's seat, the same structure is a ritual that displaces real preparedness. From the distributed resilience seat, it is an active suppression of community capacity. The engine computes this divergence; the authored claim (tangled_rope) names the structural hybridity without adjudicating which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Specialized institutions and their technical staff are structural beneficiaries (d near 0.1–0.2): they collect authority, resources, and professional monopoly. Distributed community resilience and peripheral actors are targets (d near 0.7–0.9): they bear the cost of atrophied capability and single-point-of-failure risk. The national government sits near symmetric (d ~0.5): it funds the system and bears political risk but does not operate it. International peers are excluded — their exclusion is the enforcement object that protects the Dutch model's export narrative. The engine computes per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate (post-1953 technical guarantee) is contested: the original problem (catastrophic failure from variable community capacity) has been solved, but the solution has created a new vulnerability (centralized single point of failure under climate non-stationarity). The system persists because the institutions that would need to authorize distributed capacity are the same ones that benefit from the monopoly. This is mandatrophy: the mandate has outlived its function but the constraint remains because the cost of fixing it (redesigning the institutional architecture) exceeds what any single actor bears.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the hybrid_reading a stable structural description of the Dutch preparedness system, or does it collapse into either competence_reading or husk_reading under empirical scrutiny?',
    'Longitudinal study of drill outcomes vs. actual event response across core and peripheral institutions; comparison of technical decision logs during near-misses to see whether peripheral actors ever exercise live judgment.',
    'If the dual-track structure dissolves into uniform competence or uniform ceremony, the constraint''s claimed_type (tangled_rope) and its beneficiary/victim structure would need revision. The kernel would reduce to a single constraint rather than a family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Whether the stratified structure is empirically stable or an analytical artifact.').

omega_variable(
    coordination_extraction_boundary,
    'Does the coordination function (guaranteed technical competence for primary defenses) structurally require the extraction (monopoly on technical authority that atrophies distributed resilience), or could the core competence be maintained while actively investing in peripheral capacity?',
    'Counterfactual analysis of water board budgets and mandates: what fraction of resources currently allocated to core operations would need to shift to maintain distributed resilience? Examination of the 1990s ''Water Management 21st Century'' reforms that explicitly centralized technical authority.',
    'If the boundary is permeable (core competence maintainable with distributed investment), the extraction is a policy choice, not a structural necessity — pushing classification toward snare. If impermeable (technical guarantee requires monopoly), the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the coordination and extraction components are structurally separable or necessarily coupled.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of distributed resilience structural (funding rules, legal mandates, technical standards that exclude local knowledge) or internalized (peripheral actors believe they lack competence, trust the center, fuse identity to ceremonial participation)?',
    'Post-exit suppression trajectory: track communities that have attempted to rebuild local flood response capacity (e.g., after 2021 Limburg floods) — if suppression persists after institutional barriers are lowered, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would amplify effective extraction for peripheral actors beyond the base 0.45.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for peripheral actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(prep_tr_t6, preparedness_retention__hybrid_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement(prep_tr_t12, preparedness_retention__hybrid_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement(prep_tr_t18, preparedness_retention__hybrid_reading, theater_ratio, 18, 0.58).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__hybrid_reading, theater_ratio, 24, 0.62).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__hybrid_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prep_be_t6, preparedness_retention__hybrid_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(prep_be_t12, preparedness_retention__hybrid_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(prep_be_t18, preparedness_retention__hybrid_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__hybrid_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__hybrid_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t6, preparedness_retention__hybrid_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(prep_su_t12, preparedness_retention__hybrid_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(prep_su_t18, preparedness_retention__hybrid_reading, suppression_requirement, 18, 0.41).
narrative_ontology:measurement(prep_su_t24, preparedness_retention__hybrid_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__hybrid_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, dutch_delta_program_governance).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, eu_flood_directive_implementation).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, community_resilience_funding).

% DUAL FORMULATION NOTE:
% Part of the preparedness_retention constraint family. The competence_reading claims uniform live competence (low extraction, rope/mountain). The husk_reading claims uniform ceremony (high extraction, snare). This hybrid_reading claims stratified dual-track (tangled_rope). The three constraints share the same institutional referent but disaggregate by epistemic scope: core vs. periphery vs. whole-system. Their ε values differ because they measure different structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, institutional, 0.15).
constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, powerful, 0.2).
constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, organized, 0.75).
constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
