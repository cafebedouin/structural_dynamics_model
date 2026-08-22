% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission — Hybrid Reading
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   Preparedness transmission in modern civil defense is stratified: the
 *   physical engineering layer (structural hardening, redundant systems,
 *   hardened communications) retains high, exercised competence because it is
 *   continuously validated by professional practice, codes, and market
 *   discipline. The civilian coordination layer (evacuation planning, shelter
 *   operations, warning dissemination, community liaison, mutual-aid
 *   integration) has decayed into ritual — drills are performed, plans are
 *   updated, inspections pass, but the operational knowledge of how to
 *   coordinate masses of people under stress has hollowed out. Under stress,
 *   infrastructure performs; coordination fails. The D5 break (the point
 *   where the arrangement ceases to solve its founding problem) sits in the
 *   coordination layer, not the physical layer. This is the hybrid reading:
 *   not a complete husk, not a living competence, but a stratified
 *   transmission where one stratum lives and the other is theater.
 *
 * KEY AGENTS:
 *   - civil_engineering_establishment: Primary beneficiary (institutional/arbitrage) — benefits from the perception that preparedness = engineered resilience
 *   - emergency_management_bureaucracy: Agenda setter + secondary beneficiary (institutional/constrained) — administers the ritual layer, collects legitimacy and budget
 *   - frontline_evacuation_crews: Primary payer (organized/constrained, identity_locked tendencies) — bear operational cost of coordination decay
 *   - vulnerable_civilian_populations: Primary payer (powerless/trapped) — pay with lives when coordination fails
 *   - mutual_aid_networks: Payer + secondary beneficiary (moderate/mobile) — fill the vacuum at own cost, gain social capital
 *   - disaster_sociologists: Observer (analytical/analytical) — maps the stratification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.28).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission — Hybrid Reading").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, '6a9d1662-5eab-4c49-aefa-eedab8d0e03b').
narrative_ontology:cs_kernel_codification('6a9d1662-5eab-4c49-aefa-eedab8d0e03b', formalized).
narrative_ontology:cs_authority_grounding('6a9d1662-5eab-4c49-aefa-eedab8d0e03b', practice).
narrative_ontology:cs_interpretation_layer_present('6a9d1662-5eab-4c49-aefa-eedab8d0e03b').
narrative_ontology:cs_reading_relation('6a9d1662-5eab-4c49-aefa-eedab8d0e03b', preparedness_transmission__competence_reading, influences).
narrative_ontology:cs_reading_relation('6a9d1662-5eab-4c49-aefa-eedab8d0e03b', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_axiom('6a9d1662-5eab-4c49-aefa-eedab8d0e03b', foundational, stratified_transmission_thesis).
narrative_ontology:cs_axiom_status(stratified_transmission_thesis, holdable).
narrative_ontology:cs_axiom_grounding('6a9d1662-5eab-4c49-aefa-eedab8d0e03b', stratified_transmission_thesis, empirically_contingent).
narrative_ontology:cs_axiom('6a9d1662-5eab-4c49-aefa-eedab8d0e03b', secondary, engineering_coordination_independence).
narrative_ontology:cs_axiom_status(engineering_coordination_independence, holdable).
narrative_ontology:cs_axiom_grounding('6a9d1662-5eab-4c49-aefa-eedab8d0e03b', engineering_coordination_independence, empirically_contingent).
narrative_ontology:cs_reference_frame('6a9d1662-5eab-4c49-aefa-eedab8d0e03b', cold_war_civil_defense_paradigm).
narrative_ontology:cs_drift_state('6a9d1662-5eab-4c49-aefa-eedab8d0e03b', post_911_all_hazards_reorientation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6a9d1662-5eab-4c49-aefa-eedab8d0e03b', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, civil_engineering_establishment).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, emergency_management_bureaucracy).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, frontline_evacuation_crews).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, vulnerable_civilian_populations).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, mutual_aid_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, mutual_aid_networks).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, engineered_resilience_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, institutional_continuity_as_preparedness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains high technical competence in structural hardening, flood defenses, and critical infrastructure design. Their professional standards, certification regimes, and project pipelines are well-funded and continuously exercised. They benefit from the perception that 'preparedness' equals engineered resilience, which secures their budget authority and professional status. Exit to private consulting or international markets is easy.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civil_engineering_establishment, beneficiary,
    institutional, generational, arbitrage, national).

% Administers the preparedness apparatus — drills, inspections, plans, and interagency agreements. Their legitimacy rests on the visible performance of these rituals. They collect budget, staffing, and intergovernmental authority from the system's ceremonial operation. The coordination decay is not in their interest to surface, but they cannot simply exit — their careers are constituted by the institution.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, emergency_management_bureaucracy, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, emergency_management_bureaucracy, beneficiary).

% Execute evacuations, shelter operations, and civilian coordination under stress. They bear the operational cost of the coordination decay: incomplete plans, broken communication chains, untrained volunteers, and improvisation under fire. Their professional identity is fused to the mission (identity_locked tendencies), but institutional support has hollowed. Exit means abandoning the communities they serve.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, frontline_evacuation_crews, payer,
    organized, biographical, constrained, regional).

% Elderly, disabled, non-English-speaking, car-less, and low-income residents who depend on the coordination layer for warning, transport, shelter, and re-entry. They pay the highest price when coordination fails — disproportionate mortality, displacement, and long-term impoverishment. No meaningful exit exists; they are structurally trapped by the geography and economics of vulnerability.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, vulnerable_civilian_populations, payer,
    powerless, immediate, trapped, local).

% Community-based groups that improvise coordination when the official layer fails. They bear the cost of filling the gap (volunteer labor, private resources, risk) and gain legitimacy and social capital from doing so. Their exit is mobile — they can dissolve or relocalize — but their effectiveness is capped by the structural vacuum they cannot fix.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, mutual_aid_networks, payer,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, mutual_aid_networks, beneficiary).

% Study the stratification empirically — documenting the divergence between engineered performance metrics and coordination outcomes, tracing the institutional mechanisms that sustain the ritual layer while the knowledge layer decays. They neither collect nor pay; they map the structure.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, disaster_sociologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The physical infrastructure layer (levees, hardened shelters, redundant power, hardened comms) genuinely coordinates survival at scale — it is a real, exercised, high-competence system that protects populations when hazards strike.
% TRANSFER_FUNCTION: Moves legitimacy, budget, and authority toward the engineering and bureaucratic layers (who perform the visible rituals of preparedness) while moving the operational burden of coordination failure onto frontline crews and vulnerable populations. The transfer is not monetary but capacitative: the coordination deficit is externalized to those with least power to refuse it.
% ABSENT_VOICES: The dead and displaced of past coordination failures — those who perished in evacuation collapses, shelter mismanagement, or communication blackouts — are structurally absent. Their families and communities are often excluded from after-action reviews by bureaucratic closure rules. Future generations who will inherit the uncorrected decay are also absent.
% DISAPPEARANCE_RATIONALE: If the stratified transmission constraint vanished — meaning the engineering layer were forced to internalize the coordination deficit or the coordination layer were genuinely re-exercised — budgets would shift from ceremonial drills to operational training, command structures would be stress-tested to failure, mutual-aid integration would become mandatory, and the bureaucratic ritual economy would collapse. The world rearranges because the current arrangement sustains a false legitimacy.
% FOUNDING_PROBLEM: Post-WWII civil defense was built for nuclear mass-casualty scenarios requiring centralized command, mass sheltering, and top-down resource allocation. The founding problem was: how does a state protect millions from instantaneous, civilization-scale destruction with 1950s technology?
% FOUNDING_PROBLEM_CORROBORATION: Cold War historians and institutional sociologists (e.g., Schneider 2022 'The Hollow Shield', FEMA's own 2019 capability assessment) attest the nuclear mass-casualty scenario is no longer the primary threat model. The engineering establishment contests this, arguing 'all-hazards' continuity justifies the same structure. No disinterested body has formally declared the founding problem dead — the status is contested in practice but dead in threat reality.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).
:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate and rising: the coordination deficit externalizes real operational costs onto frontline crews and vulnerable populations while the engineering/bureaucratic layers capture legitimacy and budget. The rise from 0.25 (1990) reflects post-Cold War drift — the founding threat vanished but the apparatus persisted, layering new rituals atop decaying knowledge. Suppression (0.28) is low-moderate: the constraint persists more through institutional inertia and ceremonial legitimacy than active coercion; alternatives (community-based coordination, decentralized warning) are not violently suppressed but are starved of recognition and integration. Theater ratio (0.55) is high and rising: over half the visible preparedness activity is now performational — drills that exercise communication trees that don't exist, shelter surveys for buildings that won't be used, after-action reports that change nothing. Accessibility collapse (0.45) is moderate: alternatives exist (mutual aid, neighborhood networks, decentralized tech) but are structurally excluded from the official coordination grammar. Resistance (0.35) is moderate: frontline crews and mutual aid networks push back operationally, but institutional capture of the 'preparedness' definition muffles structural challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the engineering establishment's seat, the constraint is a rope — genuine coordination of physical survival at scale, low extraction. From the bureaucracy's seat, it is a tangled rope — they coordinate the ritual layer (real coordination of bureaucratic legitimacy) while extracting operational legitimacy from the decayed coordination layer. From frontline crews and vulnerable populations, it is a snare — the coordination story is cover for a system that fails them under stress while claiming credit for the engineering layer's success. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Engineering establishment: full beneficiary (d ~ 0.1) — collects budget, status, and professional validation; arbitrage exit. Bureaucracy: mixed (d ~ 0.35) — agenda setter who benefits from the ritual economy but is constrained by identity-locked career investment. Frontline crews: target (d ~ 0.75) — bear the coordination deficit operationally; exit is constrained by mission identity and geographic embeddedness. Vulnerable populations: full target (d ~ 0.95) — trapped, no exit, pay the ultimate price. Mutual aid: near-symmetric (d ~ 0.5) — pay by filling gaps, benefit from legitimacy gained; mobile exit. Observers: analytical (d = 0.5) — neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (nuclear mass-casualty civil defense) is dead — the threat model that justified the centralized, top-down, shelter-centric apparatus no longer exists. The arrangement persists because the engineering layer's genuine competence masks the coordination layer's decay, and the bureaucracy's ritual performance sustains the legitimacy claim. This is mandatrophy: the mandate (protect civilians from catastrophe) has outlived its function (the specific threat and the coordination method), but the constraint remains because the engineering stratum validates the whole. The classification as tangled_rope (not snare) captures the real coordination function in the engineering layer; the classification as not rope captures the asymmetric extraction in the coordination layer. The hybrid reading prevents mislabeling the entire system as pure coordination (competence_reading) or pure theater (husk_reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stratification_boundary_ambiguity,
    'Where exactly does the engineering layer end and the coordination layer begin? Is the boundary structurally stable or does it shift with hazard type?',
    'Cross-hazard comparison of failure modes: in floods, the engineering layer (levees) and coordination layer (evacuation) fail distinctly; in earthquakes, structural collapse and search-and-rescue coordination are more entangled. Map the boundary per hazard class.',
    'If the boundary is hazard-contingent, the hybrid reading may overstate stratification for some hazards and understate it for others. A shifting boundary would make the constraint a family of constraints rather than a single stratified one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_boundary_ambiguity, conceptual, 'Whether the engineering/coordination stratification is a stable structural feature or hazard-contingent').

omega_variable(
    engineering_legitimacy_spillover,
    'How much of the coordination layer''s perceived legitimacy is spillover from the engineering layer''s genuine competence vs. independent bureaucratic ritual?',
    'Counterfactual survey of public trust and elite perception: if the engineering layer were visibly degraded but the ritual layer intact, would legitimacy hold? If the ritual layer were abolished but engineering intact, would legitimacy hold?',
    'If spillover is the primary legitimacy mechanism, the hybrid reading''s tangled_rope classification is strengthened — the engineering layer actively subsidizes the coordination layer''s theater. If independent, the coordination layer''s theater is self-sustaining, pointing toward piton dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(engineering_legitimacy_spillover, empirical, 'Whether engineering competence launders coordination theater''s legitimacy').

omega_variable(
    kernel_reading_relations,
    'What is the structural relationship between this hybrid reading and its sibling readings of the preparedness_transmission kernel?',
    'Analyze whether the competence_reading''s core premise (drills = live knowledge) is logically foreclosed by the hybrid reading''s stratification claim, or whether both can be held by different parties in different domains. Similarly for husk_reading.',
    'If hybrid forecloses competence in the coordination layer but not engineering layer, the relation is domain-contingent. If hybrid and husk coexist as descriptions of different strata, the kernel hosts a stratified family. This determines whether the readings are competing explanations or complementary partial truths.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relations between hybrid_reading and sibling readings competence_reading and husk_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1990, preparedness_transmission__hybrid_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(prep_tr_t2000, preparedness_transmission__hybrid_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(prep_tr_t2005, preparedness_transmission__hybrid_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(prep_tr_t2010, preparedness_transmission__hybrid_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(prep_tr_t2015, preparedness_transmission__hybrid_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement(prep_tr_t2020, preparedness_transmission__hybrid_reading, theater_ratio, 2020, 0.52).
narrative_ontology:measurement(prep_tr_t2025, preparedness_transmission__hybrid_reading, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(prep_be_t1990, preparedness_transmission__hybrid_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(prep_be_t2000, preparedness_transmission__hybrid_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(prep_be_t2005, preparedness_transmission__hybrid_reading, base_extractiveness, 2005, 0.33).
narrative_ontology:measurement(prep_be_t2010, preparedness_transmission__hybrid_reading, base_extractiveness, 2010, 0.36).
narrative_ontology:measurement(prep_be_t2015, preparedness_transmission__hybrid_reading, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement(prep_be_t2020, preparedness_transmission__hybrid_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(prep_be_t2025, preparedness_transmission__hybrid_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1990, preparedness_transmission__hybrid_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(prep_su_t2000, preparedness_transmission__hybrid_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(prep_su_t2005, preparedness_transmission__hybrid_reading, suppression_requirement, 2005, 0.2).
narrative_ontology:measurement(prep_su_t2010, preparedness_transmission__hybrid_reading, suppression_requirement, 2010, 0.22).
narrative_ontology:measurement(prep_su_t2015, preparedness_transmission__hybrid_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(prep_su_t2020, preparedness_transmission__hybrid_reading, suppression_requirement, 2020, 0.27).
narrative_ontology:measurement(prep_su_t2025, preparedness_transmission__hybrid_reading, suppression_requirement, 2025, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, community_early_warning_systems).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, mutual_aid_integration_mandates).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, evacuation_route_hardening).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel decomposes into three constraint stories: competence_reading (engineering + coordination both live), husk_reading (both theater), and hybrid_reading (this story — engineering live, coordination theater). The hybrid reading's ε (0.42) sits between competence_reading's ε (~0.15) and husk_reading's ε (~0.7). The engineering layer's genuine competence (competence_reading's domain) is the upstream constraint that lends legitimacy to the coordination layer's theater (husk_reading's domain). This story links to both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
