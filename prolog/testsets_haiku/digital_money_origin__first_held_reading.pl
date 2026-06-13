% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Digital Money First Held: Implementation-Barrier Coordination and Access Extraction
 *   domain: monetary_history/technology/institutional_economics
 *
 * SUMMARY:
 *   This reading anchors the emergence of digital money to a specific
 *   historical moment: when individuals first held non-physical monetary
 *   instruments (e.g., DigiCash in 1994, online bank accounts as practical
 *   stores of value in the mid-1990s) as practical, recurring alternatives to
 *   physical currency. The constraint emerges from the infrastructure
 *   barriers and network effects that structure who can hold digital money
 *   first and who lags. Early adopters with device and network access gain
 *   speed and optionality; those without access face exclusion and pressure
 *   to adopt systems they cannot yet access. Payment network operators set
 *   the rules and capture fees. The measured extraction rises monotonically
 *   from 1983 to 2024 as digital money transitions from novelty to
 *   near-ubiquity: early (low extraction, low suppression) because
 *   alternatives are available; late (high extraction, high suppression)
 *   because digital infrastructure becomes mandatory for economic
 *   participation in many contexts. This reading is structurally distinct
 *   from the became_thinkable_reading (which anchors emergence earlier, to
 *   theoretical feasibility) and the regulatory_recognition_reading (which
 *   anchors it later, to central bank incorporation).
 *
 * KEY AGENTS:
 *   - Early adopters with infrastructure access: benefit from speed, convenience, and early-mover advantage; mobile exit as alternatives remain available during transition
 *   - Payment network operators: agenda-setter role; set technical standards and access conditions; capture transaction fees and user data; institutional power and arbitrage exit
 *   - Unconnected and unbanked populations: powerless; structurally excluded from participation; trapped exit as digital infrastructure may become mandatory
 *   - Lagging-adoption populations: facing identity-lock through cultural narratives of progress and modernity; partial access, constrained exit, powerless structural position
 *   - Central banks and monetary authorities: observer role; initially measure digital holdings outside official money supply; later incorporate into regulatory frameworks (domain of regulatory_recognition_reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.62).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.71).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money First Held: Implementation-Barrier Coordination and Access Extraction").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, '3ff167d2-752d-4667-935b-9e8543e47615').
narrative_ontology:cs_kernel_codification('3ff167d2-752d-4667-935b-9e8543e47615', distributed).
narrative_ontology:cs_authority_grounding('3ff167d2-752d-4667-935b-9e8543e47615', distributed).
narrative_ontology:cs_reading_relation('3ff167d2-752d-4667-935b-9e8543e47615', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ff167d2-752d-4667-935b-9e8543e47615', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('3ff167d2-752d-4667-935b-9e8543e47615', foundational, emergence_anchored_to_practical_holding).
narrative_ontology:cs_axiom_status(emergence_anchored_to_practical_holding, holdable).
narrative_ontology:cs_axiom_grounding('3ff167d2-752d-4667-935b-9e8543e47615', emergence_anchored_to_practical_holding, empirically_contingent).
narrative_ontology:cs_axiom('3ff167d2-752d-4667-935b-9e8543e47615', foundational, infrastructure_access_determines_participation).
narrative_ontology:cs_axiom_status(infrastructure_access_determines_participation, holdable).
narrative_ontology:cs_axiom_grounding('3ff167d2-752d-4667-935b-9e8543e47615', infrastructure_access_determines_participation, empirically_contingent).
narrative_ontology:cs_reference_frame('3ff167d2-752d-4667-935b-9e8543e47615', universal_physical_monetary_infrastructure).
narrative_ontology:cs_drift_state('3ff167d2-752d-4667-935b-9e8543e47615', post_digicash_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ff167d2-752d-4667-935b-9e8543e47615', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters_with_infrastructure_access).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, payment_network_operators).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, individuals_without_digital_access).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, lagging_adoption_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises monotonically (0.15 → 0.62) because the coordination benefit (instant transfer, portfolio optionality) is real early on and valuable, but as adoption spreads, the network-effect lock-in replaces the coordination function: once most economic actors hold digital money, individuals become trapped not by barrier (all have access now) but by inevitability. Suppression requirement rises even faster (0.35 → 0.71) because maintaining digital money's centrality requires actively suppressing alternatives (enforcing digital-only accounts in some contexts, making cash less convenient, regulatory pressure on non-digital assets) and preventing exit for lagging populations (cultural pressure, infrastructure dependency). Theater ratio is lowest of the three (rising to only 0.28) because the actual coordination work (instant settlement, global transfer) is genuine, not purely performative, even as extraction accumulates. The measurement grid anchors to 1983 (DigiCash conception/first implementation barriers) and 2024 (near-ubiquity in developed economies). The time series captures the transition from coordination-dominated (early) to extraction-dominated (late) phase.
 *
 * PERSPECTIVAL GAP:
 *   The operator and early-adopter seats see genuine coordination and beneficial innovation; payment network operators justify fees as the cost of building and maintaining trust infrastructure. The unconnected and lagging seats see infrastructure-dependent exclusion and identity-fusion pressure. Central bank observers see a transition from alternative asset to monetary aggregate. The gap is structural: from the operator seat, the constraint is rope (genuine coordination, minimal enforcement needed for willing participants). From the unconnected seat, it is snare (mandatory system, trapped alternatives). The engine computes per-seat classification from the directionality and power atoms; the authored claim (tangled_rope) sits between these perspectives because the constraint genuinely coordinates (early phase) and genuinely extracts (late phase).
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters with access sit near d=0.2 (beneficiaries: they gain speed without major burden); payment operators sit at d=0.05 (full beneficiaries: they set rules and collect fees). Unconnected populations sit near d=1.0 (full targets: structurally excluded, trapped). Lagging populations sit near d=0.85 (targets: partial access, identity-lock suppression). The beneficiary and victim declarations split the constraint across power atoms: moderate and institutional seats collect coordination benefits and extraction revenue; powerless seats bear the cost of exclusion and inclusion pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (speed and geographic reach of monetary transfer) remains live, attested by central banks adopting digital payment policy goals. The disappearance verdict is world_rearranges: if the first_held_reading never occurred, monetary infrastructure and global commerce would follow a different technology trajectory. The constraint avoids mandatrophy misclassification because its founding coordination function (instant digital settlement) persists alongside extraction. A snare classification would require the coordination to be pure cover; here, the coordination is real but has been layered with extraction as adoption scaled. Tangled_rope correctly captures this structure: genuine coordination + asymmetric extraction + active enforcement (preventing alternative systems, enforcing digital-only in some regulatory domains).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which definition of ''emergence'' is the structurally correct origin point for digital money: first conceived, first technically feasible, first held by individuals, or first formally recognized by monetary authorities?',
    'This omega documents the kernel contest itself. The three readings (became_thinkable_reading, first_held_reading, regulatory_recognition_reading) each anchor emergence to a different event. No empirical evidence can resolve this: each reading is coherent, and the choice depends on what the questioner treats as the boundary of ''digital money'' as a social fact.',
    'Different readings produce different origin dates (conceptual feasibility: ~1970s cryptography; first held: ~1983 DigiCash; regulatory recognition: ~2008 Bitcoin or ~2020 central bank guidance). Each reading implies different victim/beneficiary structures, different suppression mechanisms, and different classification pathways. The kernel is the contested claim that ''digital money'' has a single, intrinsic origin moment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The kernel contest: which definition of emergence grounds the origin of digital money?').

omega_variable(
    infrastructure_access_as_structural_barrier,
    'Is the measured suppression (0.71) a property of the digital money constraint itself, or a property of the unequal infrastructure distribution in the global economy that digital money happens to leverage?',
    'A counterfactual comparison: if digital money technology were deployed in a society with universal infrastructure access, would suppression drop substantially? If yes, the suppression is infrastructure-dependent. If no, the suppression is intrinsic to digital money systems. Evidence: pilot studies of digital currency deployment in high-access and low-access communities.',
    'If infrastructure-dependent, the constraint may be remediable by infrastructure investment without changing the digital money system itself. If intrinsic, digital money systems are structurally extractive toward the unconnected. The classification (tangled_rope vs. snare) depends on this distinction: tangled_rope requires genuine coordination function despite extraction; a purely infrastructure-extractive system might shift toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_access_as_structural_barrier, empirical, 'Whether measured suppression is intrinsic to digital money or artifact of unequal infrastructure access.').

omega_variable(
    identity_lock_mechanism_internalization,
    'Is the identity-lock suppression for lagging-adoption populations (the narrative framing of digital adoption as inevitable progress) structurally embedded in the digital money systems, or is it an internalized adoption of a cultural narrative that could persist even after barrier removal?',
    'Post-barrier removal suppression trajectory: in communities where digital access is provided but adoption is voluntary, does the cultural narrative of digital inevitability persist? If yes, the identity-lock is partially internalized. If no, it was purely structural. Evidence: studies of post-infrastructure-investment adoption in previously unconnected communities.',
    'If internalized, lagging-adoption populations carry the identity-lock forward even after infrastructure access, making the constraint''s effective suppression higher than the structural measure suggests. If structural, providing infrastructure removes the suppression. The constraint''s longevity depends on this distinction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Whether identity-lock suppression in lagging populations is structurally intrinsic or culturally internalized.').

omega_variable(
    regulatory_recognition_temporal_precedence,
    'Did this reading (first_held_reading, anchored to individual practical holdings) precede or follow the regulatory_recognition_reading in actual historical development?',
    'Historical chronology of central bank monetary aggregates and regulatory treatment: when did central banks first measure digital asset holdings, relative to when individuals began holding digital instruments as practical stores of value? A tight temporal coupling would suggest the readings coevolved; a gap would establish precedence.',
    'If first_held_reading preceded regulatory recognition by years/decades, the two readings represent genuinely distinct emergence events with different beneficiary structures. If regulatory recognition was nearly contemporaneous, the readings may be describing the same event from different analytical frames rather than truly separate constraint instantiations. This affects network.affects_constraints linkage and constraint family coherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_recognition_temporal_precedence, empirical, 'Historical precedence ordering: which emergence event happened first?').

omega_variable(
    network_effects_boundary_between_coordination_and_extraction,
    'At what scale of adoption does the network-effect coordination function (digital money becomes more useful as more people hold it) transition into extractive lock-in (individuals are trapped because alternatives have lost viability)?',
    'Adoption threshold analysis: measure switching costs and alternative-viability as a function of adoption percentage. At low adoption (10%), are alternatives still available and used? At medium (50%)? At high (90%)? The threshold where alternatives become economically unviable marks the transition.',
    'If the transition occurs at low adoption, the constraint is extractive from early on; if at high adoption, the coordination function dominates for most of the interval and extraction emerges only at saturation. The measurement series (extractiveness rising from 0.15 to 0.62 over 41 years) tracks this transition. Understanding when it occurred clarifies how much of the 0.62 measured at interval end is coordination cost vs. extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_boundary_between_coordination_and_extraction, empirical, 'When does network-effect coordination transition to extractive lock-in?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1983, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1983, digital_money_origin__first_held_reading, theater_ratio, 1983, 0.08).
narrative_ontology:measurement(digi_tr_t1995, digital_money_origin__first_held_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(digi_tr_t2005, digital_money_origin__first_held_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement(digi_tr_t2012, digital_money_origin__first_held_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(digi_tr_t2019, digital_money_origin__first_held_reading, theater_ratio, 2019, 0.26).
narrative_ontology:measurement(digi_tr_t2024, digital_money_origin__first_held_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(digi_be_t1983, digital_money_origin__first_held_reading, base_extractiveness, 1983, 0.15).
narrative_ontology:measurement(digi_be_t1995, digital_money_origin__first_held_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(digi_be_t2005, digital_money_origin__first_held_reading, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(digi_be_t2012, digital_money_origin__first_held_reading, base_extractiveness, 2012, 0.54).
narrative_ontology:measurement(digi_be_t2019, digital_money_origin__first_held_reading, base_extractiveness, 2019, 0.59).
narrative_ontology:measurement(digi_be_t2024, digital_money_origin__first_held_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1983, digital_money_origin__first_held_reading, suppression_requirement, 1983, 0.35).
narrative_ontology:measurement(digi_su_t1995, digital_money_origin__first_held_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(digi_su_t2005, digital_money_origin__first_held_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(digi_su_t2012, digital_money_origin__first_held_reading, suppression_requirement, 2012, 0.66).
narrative_ontology:measurement(digi_su_t2019, digital_money_origin__first_held_reading, suppression_requirement, 2019, 0.69).
narrative_ontology:measurement(digi_su_t2024, digital_money_origin__first_held_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_origin__first_held_reading, 0.18).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% The digital_money_origin kernel contest decomposes into three structurally distinct constraints, each anchoring emergence to a different social event. The first_held_reading (this constraint) focuses on implementation barriers and network effects, with later origin date and infrastructure-dependent victims. The became_thinkable_reading anchors to theoretical feasibility and early cryptographic development. The regulatory_recognition_reading anchors to formal incorporation into central bank monetary aggregates. All three readings are live positions in monetary history; none logically forecloses the others. They coexist as analytical frames emphasizing different aspects of the same historical transition. Linkage via affects_constraints documents the conceptual kinship and shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__first_held_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
