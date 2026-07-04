% ============================================================================
% CONSTRAINT STORY: weaponization_accessibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_weaponization_accessibility, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: weaponization_accessibility
 *   human_readable: Consumer Drone Weaponization Accessibility Threshold
 *   domain: security/technology/asymmetric_warfare
 *
 * SUMMARY:
 *   The constraint describes the technical and resource threshold required to
 *   convert a consumer quadcopter into an improvised strike platform. As of
 *   2025, this threshold sits at approximately 8-12 hours of modification
 *   time, $200-400 in additional components, and intermediate electronics
 *   knowledge available through YouTube tutorials. The accessibility level
 *   enables non-state actors to field swarms of weaponized drones faster than
 *   conventional forces can deploy countermeasures, inverting traditional
 *   force projection economics. The constraint is claimed as rope—a genuine
 *   coordination mechanism solving the collective action problem of
 *   asymmetric capability acquisition—while the metrics track modest but
 *   rising extraction as conventional forces bear escalating counter-drone
 *   costs and civilian populations near conflict zones face indiscriminate
 *   strike risk.
 *
 * KEY AGENTS:
 *   - non_state_actors: Primary beneficiary (organized/mobile) — gain asymmetric strike capability at consumer price points
 *   - insurgent_groups: Primary beneficiary (organized/constrained) — exploit accessibility to neutralize conventional advantages
 *   - hobbyist_communities: Beneficiary (moderate/mobile) — develop dual-use modification techniques for legitimate purposes
 *   - conventional_military_forces: Primary payer (institutional/constrained) — face proliferating low-cost threats that invert cost-exchange ratios
 *   - civilian_populations_near_conflict: Payer (powerless/trapped) — bear indiscriminate strike risk from actors who gain capability faster than discipline
 *   - export_control_regimes: Payer (institutional/constrained) — attempt regulation below enforcement floor
 *   - consumer_drone_manufacturers: Agenda setter (institutional/mobile) — design platforms weaponization exploits
 *   - component_suppliers: Beneficiary + agenda setter (organized/mobile) — sell dual-use components through open markets
 *   - defense_analysts: Analytical observer — document threshold erosion and tactical adaptations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(weaponization_accessibility, 0.28).
domain_priors:suppression_score(weaponization_accessibility, 0.42).
domain_priors:theater_ratio(weaponization_accessibility, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(weaponization_accessibility, extractiveness, 0.28).
narrative_ontology:constraint_metric(weaponization_accessibility, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(weaponization_accessibility, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(weaponization_accessibility, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(weaponization_accessibility, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(weaponization_accessibility, rope).
narrative_ontology:human_readable(weaponization_accessibility, "Consumer Drone Weaponization Accessibility Threshold").
narrative_ontology:topic_domain(weaponization_accessibility, "security/technology/asymmetric_warfare").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(weaponization_accessibility, non_state_actors).
narrative_ontology:constraint_beneficiary(weaponization_accessibility, insurgent_groups).
narrative_ontology:constraint_beneficiary(weaponization_accessibility, hobbyist_communities).
narrative_ontology:constraint_victim(weaponization_accessibility, conventional_military_forces).
narrative_ontology:constraint_victim(weaponization_accessibility, civilian_populations_near_conflict).
narrative_ontology:constraint_victim(weaponization_accessibility, export_control_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(weaponization_accessibility, component_suppliers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain asymmetric strike capability at consumer price points. Convert commercially available quadcopters into improvised munitions delivery platforms using widely available components and online tutorials. The technical threshold is low enough that groups with minimal engineering capacity can field swarms within weeks of procurement.
narrative_ontology:constraint_stakeholder(weaponization_accessibility, non_state_actors, beneficiary,
    organized, immediate, mobile, regional).

% Exploit the accessibility threshold to neutralize conventional force advantages in surveillance and precision strike. The same commercial supply chains that enable hobbyist racing drones provide payload release mechanisms, range extenders, and autonomous navigation modules. Modification requires hand tools and intermediate electronics knowledge.
narrative_ontology:constraint_stakeholder(weaponization_accessibility, insurgent_groups, beneficiary,
    organized, biographical, constrained, regional).

% Develop and share modification techniques for legitimate purposes—extended range photography, agricultural payload delivery, search and rescue—that are functionally identical to weaponization pathways. Online forums document every step; component suppliers ship internationally without meaningful scrutiny.
narrative_ontology:constraint_stakeholder(weaponization_accessibility, hobbyist_communities, beneficiary,
    moderate, biographical, mobile, global).

% Face proliferating low-cost threats that conventional air defense cannot economically counter. A $500 modified drone carrying a grenade requires a $50,000 interceptor or forces dispersion that degrades operational tempo. The accessibility threshold inverts the cost-exchange ratio that underpins conventional force projection.
narrative_ontology:constraint_stakeholder(weaponization_accessibility, conventional_military_forces, payer,
    institutional, generational, constrained, national).

% Bear the risk of indiscriminate strikes from actors who gain capability faster than targeting discipline. Modified consumer drones lack the precision and fail-safes of military systems; civilian casualties from navigation errors or misidentification are structurally higher. Exit means displacement.
narrative_ontology:constraint_stakeholder(weaponization_accessibility, civilian_populations_near_conflict, payer,
    powerless, immediate, trapped, local).

% Attempt to regulate dual-use components through licensing and end-user verification, but the accessibility threshold sits below the enforcement floor. Components are commodity electronics; modification knowledge is open-source; the supply chain is global e-commerce. Every control measure adds friction without closing access.
narrative_ontology:constraint_stakeholder(weaponization_accessibility, export_control_regimes, payer,
    institutional, generational, constrained, global).

% Design platforms optimized for payload capacity, flight time, and autonomous navigation—the same parameters weaponization exploits. Geofencing and firmware restrictions are implemented but routinely bypassed. Manufacturers face liability pressure to harden against misuse while maintaining the performance characteristics that drive consumer adoption.
narrative_ontology:constraint_stakeholder(weaponization_accessibility, consumer_drone_manufacturers, agenda_setter,
    institutional, biographical, mobile, global).

% Sell payload release servos, long-range radio modules, and autonomous flight controllers through open marketplaces. The same components serve agricultural spraying, wildlife monitoring, and strike modification. Suppliers have no visibility into end use and minimal incentive to restrict sales of items with legitimate applications.
narrative_ontology:constraint_stakeholder(weaponization_accessibility, component_suppliers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(weaponization_accessibility, component_suppliers, agenda_setter).

% Document the accessibility threshold's erosion of conventional deterrence models. Track modification proliferation, cost-exchange ratios, and tactical adaptations. Advise on counter-drone systems and export controls while recognizing that the technical threshold is set by consumer electronics economics, not security policy.
narrative_ontology:constraint_stakeholder(weaponization_accessibility, defense_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The low accessibility threshold coordinates rapid capability diffusion across non-state actors who lack indigenous defense industrial capacity. Shared modification knowledge and commodity supply chains solve the collective action problem of acquiring asymmetric strike capability without centralized procurement or R&D infrastructure.
% TRANSFER_FUNCTION: Transfers tactical initiative and cost-exchange advantage from conventional military forces to non-state actors. Moves the locus of innovation from defense contractors to hobbyist forums. Imposes counter-drone investment costs on militaries and export control compliance costs on manufacturers and suppliers.
% ABSENT_VOICES: Future civilian populations in regions where drone proliferation has not yet reached saturation. Technology governance bodies that would restrict dual-use component sales are structurally excluded by the speed of e-commerce and the legitimate-use defense. Arms control regimes designed for state-to-state transfers have no purchase on consumer electronics supply chains.
% DISAPPEARANCE_RATIONALE: If the accessibility threshold suddenly rose—through component scarcity, supply chain disruption, or effective firmware lockdown—non-state actors would lose their primary asymmetric strike capability within months. Conventional forces would regain freedom of maneuver in contested airspace. Hobbyist communities would fragment as modification knowledge became obsolete. The tactical landscape would revert to pre-proliferation asymmetries.
% FOUNDING_PROBLEM: Non-state actors historically lacked affordable, precise, standoff strike capability. Mortars and rockets are inaccurate and detectable; suicide attacks are personnel-intensive and unreliable. Consumer drone technology solved this by providing a platform with sufficient payload, range, and guidance at commodity prices, but only if the modification threshold remained accessible.
% FOUNDING_PROBLEM_CORROBORATION: Defense analysts and conflict documentation organizations outside the benefiting parties confirm the founding problem remains live: non-state actors continue to lack indigenous precision-strike manufacturing. The accessibility threshold is the only pathway to this capability class. Open-source intelligence tracking and academic conflict studies corroborate that removal of consumer drone access would eliminate the capability for groups without state sponsorship.
narrative_ontology:disappearance_verdict(weaponization_accessibility, world_rearranges).
narrative_ontology:founding_problem_status(weaponization_accessibility, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(weaponization_accessibility, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(weaponization_accessibility, 'none', 1).
narrative_ontology:epsilon_provenance(weaponization_accessibility, 0.28, 'claude-sonnet-4-5-20250929', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(weaponization_accessibility_tests).
:- end_tests(weaponization_accessibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-low (0.28) because the accessibility threshold genuinely solves a coordination problem for non-state actors while imposing real but bounded costs on conventional forces and export regimes. The constraint is not zero-sum: hobbyist innovation and non-state capability acquisition both benefit from the same low threshold. Suppression is moderate (0.42) because export controls and firmware restrictions add friction without closing access—the threshold persists through participant preference for dual-use technology, not through active suppression of alternatives. Theater ratio is low (0.18): geofencing and end-user verification are partly performative, but the core modification pathway remains functional. Accessibility collapse is moderate-low (0.38): alternative pathways exist (state-supplied military drones, indigenous manufacturing) but are economically or technically prohibitive for most non-state actors. Resistance is moderate-high (0.52): conventional forces and export regimes actively resist the accessibility level through counter-drone development and component controls, but cannot raise the threshold without disrupting legitimate commercial use.
 *
 * PERSPECTIVAL GAP:
 *   From the non-state actor and hobbyist seats, the constraint operates as genuine coordination: it solves the problem of acquiring capability or performance that would otherwise require state-level resources or indigenous R&D. The low threshold is the arrangement's value. From the conventional military and civilian seats, the same structure operates as imposed risk: the accessibility level enables threats faster than defenses can scale, and the cost-exchange inversion degrades operational security. From the export control seat, the constraint is a coordination failure: dual-use components flow freely because the legitimate-use defense prevents meaningful restriction. The engine computes these divergent classifications from the structural data; the authored claim (rope) reflects the beneficiary-seat framing without adjudicating the payer-seat experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-state actors and insurgent groups are structural beneficiaries (d near 0.2-0.3): the low threshold enables capability they could not otherwise acquire, with mobile or constrained exit depending on alternative supply routes. Hobbyist communities are also beneficiaries (d ~0.25): they gain extended-range and autonomous-flight capabilities for legitimate applications. Conventional military forces are targets (d ~0.7): they bear the cost-exchange inversion and must invest in counter-drone systems that are economically unfavorable. Civilian populations near conflict are full targets (d ~0.9): they bear indiscriminate strike risk with no exit and no benefit. Export control regimes are targets (d ~0.65): they bear compliance costs and enforcement futility. Manufacturers and component suppliers sit near symmetric (d ~0.45-0.5): they benefit from sales volume but face liability and regulatory pressure. The accessibility threshold's coordination function is real, but its benefits and costs are asymmetrically distributed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate—enabling consumer access to high-performance drone technology—has not outlived its function. Hobbyist communities, agricultural users, and search-and-rescue operators continue to derive genuine value from the accessibility threshold. The weaponization pathway is a side effect of design choices optimized for legitimate performance, not a degraded remnant of an obsolete function. However, the rising extraction and suppression trajectories indicate that the coordination function is accumulating extractive overhead as conventional forces invest in countermeasures and export regimes layer controls. The constraint is not yet a piton (theater ratio remains low, no concentrated beneficiary captures the extraction), but the measurement series shows drift toward tangled rope: a real coordination function increasingly entangled with asymmetric costs that require active management to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    firmware_lockdown_feasibility,
    'Can consumer drone manufacturers implement firmware restrictions that meaningfully raise the weaponization threshold without degrading legitimate performance?',
    'Natural experiment from jurisdictions mandating geofencing and payload restrictions: if modification tutorials proliferate workarounds within months, firmware lockdown is ineffective; if the threshold rises and stays elevated, it is feasible.',
    'If firmware lockdown is feasible and adopted, the accessibility threshold rises and the constraint''s coordination function shifts to state-supplied or indigenous platforms—extraction on non-state actors increases, extraction on conventional forces decreases. If infeasible, the current accessibility level persists regardless of regulatory pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firmware_lockdown_feasibility, empirical, 'Whether technical restrictions can raise the weaponization threshold without eliminating legitimate use cases.').

omega_variable(
    cost_exchange_sustainability,
    'At what proliferation density does the cost-exchange ratio inversion force conventional forces to abandon contested airspace or develop economically viable counter-drone systems?',
    'Observation of force posture changes in high-drone-density conflicts: if conventional forces maintain presence through counter-drone investment, the inversion is sustainable; if they withdraw or accept degraded operational tempo, it is not.',
    'If the cost-exchange inversion is unsustainable, conventional forces will either develop cheap counter-drone solutions (reducing extraction on the payer seat) or withdraw from drone-saturated environments (increasing extraction as non-state actors gain freedom of maneuver). If sustainable, the current extraction level persists as a permanent feature of asymmetric conflict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_exchange_sustainability, empirical, 'Whether conventional forces can economically counter proliferating low-cost drone threats.').

omega_variable(
    dual_use_separability,
    'Are the design features that enable weaponization (payload capacity, autonomous navigation, extended range) structurally separable from legitimate high-performance applications?',
    'Engineering analysis of consumer drone design space: if legitimate applications require the same performance envelope that weaponization exploits, the features are inseparable; if a restricted-performance tier can serve hobbyist and commercial users, they are separable.',
    'If separable, export controls and design restrictions can raise the weaponization threshold without eliminating legitimate use—extraction on non-state actors increases, extraction on conventional forces decreases. If inseparable, any restriction that raises the weaponization threshold also degrades legitimate performance, and the accessibility level is set by consumer demand rather than security policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_separability, conceptual, 'Whether weaponization-enabling features can be restricted without degrading legitimate drone applications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(weaponization_accessibility, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(weap_tr_t0, weaponization_accessibility, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(weap_tr_t0, observed).
narrative_ontology:measurement(weap_tr_t3, weaponization_accessibility, theater_ratio, 3, 0.1).
narrative_ontology:measurement_basis(weap_tr_t3, observed).
narrative_ontology:measurement(weap_tr_t6, weaponization_accessibility, theater_ratio, 6, 0.12).
narrative_ontology:measurement_basis(weap_tr_t6, observed).
narrative_ontology:measurement(weap_tr_t9, weaponization_accessibility, theater_ratio, 9, 0.14).
narrative_ontology:measurement_basis(weap_tr_t9, observed).
narrative_ontology:measurement(weap_tr_t12, weaponization_accessibility, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(weap_tr_t12, observed).
narrative_ontology:measurement(weap_tr_t15, weaponization_accessibility, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(weap_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(weap_be_t0, weaponization_accessibility, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(weap_be_t0, observed).
narrative_ontology:measurement(weap_be_t3, weaponization_accessibility, base_extractiveness, 3, 0.19).
narrative_ontology:measurement_basis(weap_be_t3, observed).
narrative_ontology:measurement(weap_be_t6, weaponization_accessibility, base_extractiveness, 6, 0.22).
narrative_ontology:measurement_basis(weap_be_t6, observed).
narrative_ontology:measurement(weap_be_t9, weaponization_accessibility, base_extractiveness, 9, 0.25).
narrative_ontology:measurement_basis(weap_be_t9, observed).
narrative_ontology:measurement(weap_be_t12, weaponization_accessibility, base_extractiveness, 12, 0.27).
narrative_ontology:measurement_basis(weap_be_t12, observed).
narrative_ontology:measurement(weap_be_t15, weaponization_accessibility, base_extractiveness, 15, 0.28).
narrative_ontology:measurement_basis(weap_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(weap_su_t0, weaponization_accessibility, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(weap_su_t0, observed).
narrative_ontology:measurement(weap_su_t3, weaponization_accessibility, suppression_requirement, 3, 0.3).
narrative_ontology:measurement_basis(weap_su_t3, observed).
narrative_ontology:measurement(weap_su_t6, weaponization_accessibility, suppression_requirement, 6, 0.34).
narrative_ontology:measurement_basis(weap_su_t6, observed).
narrative_ontology:measurement(weap_su_t9, weaponization_accessibility, suppression_requirement, 9, 0.37).
narrative_ontology:measurement_basis(weap_su_t9, observed).
narrative_ontology:measurement(weap_su_t12, weaponization_accessibility, suppression_requirement, 12, 0.4).
narrative_ontology:measurement_basis(weap_su_t12, observed).
narrative_ontology:measurement(weap_su_t15, weaponization_accessibility, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(weap_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(weaponization_accessibility, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of technology_diffusion_asymmetry (the broader pattern of consumer technology enabling asymmetric capability acquisition). The weaponization accessibility threshold is one instantiation of the general diffusion asymmetry: commercial supply chains optimized for consumer performance create capability pathways that bypass traditional arms control and force projection models.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
