% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS Article 57 EEZ Exclusive Control (Strict Reading)
 *   domain: international_law/maritime_governance
 *
 * SUMMARY:
 *   UNCLOS Article 57 establishes an Exclusive Economic Zone (EEZ) extending
 *   200 nautical miles from a coastal state's baselines, within which the
 *   state has exclusive rights to resource extraction and environmental
 *   management. Under the STRICT READING, this boundary is absolute and
 *   exclusive—no overlay claims from historical occupation, non-ratifiers, or
 *   neighboring states are valid. The constraint solves a real coordination
 *   problem (commons overexploitation, boundary disputes) while
 *   simultaneously extracting exclusive control from overlapping claimants
 *   and suppressing alternative sovereignty frameworks (historical rights
 *   claims, freedom-of-navigation doctrines, non-ratifier interpretations).
 *   The constraint is CLAIMED as tangled_rope (coordination + enforcement)
 *   and the authored metrics describe a substantially extractive, heavily
 *   suppressed arrangement that has intensified over time.
 *
 * KEY AGENTS:
 *   - Coastal state EEZ controllers: Primary beneficiaries; set and enforce 200-nm boundaries; control resource extraction licensing and environmental oversight.
 *   - Overlapping claimant states (South China Sea claimants, Arctic states): Primary payers; lose access to contested waters; constrained options (negotiation, legal challenge, military escalation).
 *   - Non-ratifying maritime powers (US, others): Payers via identity-lock; reject UNCLOS binding force but subject to strict-reading enforcement; maintain costly alternative sovereignty claims.
 *   - Distant-water fishing states: Mixed role; pay licensing fees but benefit from others' EEZ enforcement; constrained but negotiable exit.
 *   - ITLOS and UNCLOS interpretation authority: Agenda-setter layer; reinforces strict reading through legal precedent.
 *   - Indigenous coastal communities: Excluded from allocation; bear costs of EEZ boundaries without voice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.79).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Article 57 EEZ Exclusive Control (Strict Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, 'df14d81b-f6af-403d-96d2-7a596f3ee810').
narrative_ontology:cs_kernel_codification('df14d81b-f6af-403d-96d2-7a596f3ee810', fixed_text).
narrative_ontology:cs_authority_grounding('df14d81b-f6af-403d-96d2-7a596f3ee810', extraction).
narrative_ontology:cs_interpretation_layer_present('df14d81b-f6af-403d-96d2-7a596f3ee810').
narrative_ontology:cs_reading_relation('df14d81b-f6af-403d-96d2-7a596f3ee810', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('df14d81b-f6af-403d-96d2-7a596f3ee810', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('df14d81b-f6af-403d-96d2-7a596f3ee810', foundational, eez_exclusivity_absolute).
narrative_ontology:cs_axiom_status(eez_exclusivity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('df14d81b-f6af-403d-96d2-7a596f3ee810', eez_exclusivity_absolute, conventional).
narrative_ontology:cs_axiom('df14d81b-f6af-403d-96d2-7a596f3ee810', foundational, unclos_text_supreme_over_prior_claims).
narrative_ontology:cs_axiom_status(unclos_text_supreme_over_prior_claims, holdable).
narrative_ontology:cs_axiom_grounding('df14d81b-f6af-403d-96d2-7a596f3ee810', unclos_text_supreme_over_prior_claims, conventional).
narrative_ontology:cs_reference_frame('df14d81b-f6af-403d-96d2-7a596f3ee810', unclos_formalized_eez_regime).
narrative_ontology:cs_drift_state('df14d81b-f6af-403d-96d2-7a596f3ee810', contemporary_overlapping_claims_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('df14d81b-f6af-403d-96d2-7a596f3ee810', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_state_eez_controllers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, non_ratifying_maritime_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, flag_state_enforcement_coalitions).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coastal states with exclusive EEZ control under UNCLOS Article 57. Set and enforce the 200-nautical-mile boundary; control resource extraction (fishing, minerals, oil/gas), environmental management, and research permissions within the zone. Justify the arrangement as enabling rational resource stewardship and preventing tragedy-of-commons scenarios. The constraint's enforcement machinery (coast guard patrols, license denial, asset seizure) depends on their institutional capacity and coalition with other ratifying states.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_state_eez_controllers, agenda_setter,
    institutional, generational, arbitrage, global).

% States with territorial or historical claims that overlap with or predate UNCLOS EEZ demarcation (e.g., Southeast Asian states in the South China Sea, arctic claimants). Under the strict reading they lose access to contested waters and must negotiate bilateral agreements or accept exclusion from resource extraction. Their options are military escalation (costlier than negotiation), legal challenge via ITLOS (uncertain outcomes), or accommodation. The constraint's enforcement—naval patrols, fishing license revocation, seizure of vessels—falls directly on their fishers, resource companies, and coastal populations.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimant_states, payer,
    powerful, generational, constrained, global).

% States that have not ratified UNCLOS (chiefly the United States) or reject the EEZ as customary law binding upon them. Under the strict reading they are structurally excluded from the convention's legitimacy framework yet subject to its enforcement by ratifying states acting in coalition. Their naval freedom-of-navigation operations and resource claims face increasing friction; they bear the cost of maintaining alternative sovereignty claims (strategic ambiguity, freedom of navigation declarations, bilateral pressure) while the strict reading's institutional machinery hardens around them. Identity-locked because rejecting UNCLOS creates domestic political and strategic costs their foreign policy establishments resist.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifying_maritime_powers, payer,
    institutional, generational, identity_locked, global).

% States whose fishing industry depends on distant-water fleets (Japan, South Korea, Spain, China). They pay directly through fishing license fees to coastal states and through catch restrictions in EEZ waters. They also benefit from UNCLOS's enforcement of others' EEZs (predictable access on negotiated terms rather than constant conflict). Their exit is constrained: they cannot fish in coastal waters without permission; their leverage lies in licensing negotiations and trade relationships.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_states, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_states, beneficiary).

% Organized coalitions of ratifying coastal states that conduct EEZ patrols, coordinate enforcement, and adjudicate boundary disputes through ITLOS. They benefit from the constraint's enforcement machinery—it amplifies their individual control and distributes the costs of enforcement across the coalition. They do not collect extraction directly but their institutional power is amplified by the rule system.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, flag_state_enforcement_coalitions, beneficiary,
    institutional, generational, analytical, global).

% Indigenous fishing and maritime communities in coastal regions of both coastal states and overlapping claimant states. Structurally excluded from negotiations and EEZ allocation decisions. Under the strict reading they may lose traditional fishing grounds if those grounds fall within a neighbor's EEZ or if a claimant state's overlapping claim is superseded. Their voices are absent from the UNCLOS framework and from bilateral EEZ negotiations; they bear the costs of exclusion from traditional waters without voice in the allocation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, indigenous_coastal_communities, excluded,
    powerless, biographical, trapped, local).

% The system of international law that gains legitimacy and operational precedent from a formalized, exclusivity-based maritime boundary regime. The constraint vindicates the principle that states can construct stable territorial claims via treaty and that international adjudication (ITLOS) can resolve disputes. Not an agent (no strategic interests), but a structure the constraint preserves.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_legal_system, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(unclos_sovereignty_boundary__strict_eez_reading, international_legal_system).

% International Tribunal for the Law of the Sea (ITLOS) and the ongoing process of legal interpretation and precedent-setting around UNCLOS articles. Interprets Article 57 boundaries, adjudicates disputes, and reinforces the strict reading through rulings that treat EEZ boundaries as exclusive and sovereign. The tribunal's authority depends on ratifying states accepting its jurisdiction.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, unclos_interpretation_authority, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__strict_eez_reading, coastal_state_eez_controllers).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__strict_eez_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes predictable, exclusive maritime sovereignty boundaries (200-nautical-mile EEZ per Article 57) to solve the commons problem of unregulated ocean resource extraction and to prevent constant boundary disputes through a single, formalized rule set applicable to all ratifying states. The rule creates a stable grid for resource management, research oversight, and environmental protection.
% TRANSFER_FUNCTION: Transfers exclusive resource control (fishing, mining, petroleum, research) from open-access or contested status to coastal states within 200 nautical miles of their baselines. Also transfers the cost of enforcing the boundary (naval patrols, license denial, vessel seizure, diplomatic pressure) to overlapping claimant states and non-ratifiers who resist the strict reading. Distant-water fishing states transfer licensing fees to coastal state authorities.
% ABSENT_VOICES: Indigenous coastal communities whose traditional fishing grounds span EEZ boundaries are structurally excluded from the allocation process; they have no seat at UNCLOS negotiations or bilateral EEZ-boundary discussions. Non-ratifying maritime powers (notably the US) reject the framework's binding force on them and are excluded from its legitimacy logic, though subject to its enforcement. States with historical or geographic claims that predate UNCLOS (Argentina, China, Vietnam in disputed zones) are nominally included in UNCLOS negotiations but lose voice under the strict reading's supercession of pre-existing claims.
% DISAPPEARANCE_RATIONALE: If the strict EEZ reading and its enforcement vanished overnight, overlapping maritime claims would reactivate immediately (South China Sea, Arctic, Mediterranean), distant-water fishing fleets would operate without licensing restrictions, non-ratifiers' freedom-of-navigation challenges would cease to be contested, and coastal states would lose the exclusive resource control the constraint guarantees. The global maritime economy would reorganize around bilateral negotiations and naval-presence deterrence rather than a formalized boundary grid.
% FOUNDING_PROBLEM: Unregulated ocean resource extraction (overfishing, nutrient-depletion, uncontrolled mining) and perpetual boundary disputes between neighboring coastal states. UNCLOS Article 57 was designed to give each coastal state exclusive control over a 200-nautical-mile zone, enabling rational resource stewardship without requiring perpetual negotiation or naval conflict.
% FOUNDING_PROBLEM_CORROBORATION: Coastal state authorities and maritime legal scholars attest the founding problem (commons overexploitation, boundary instability) remains live and that the EEZ regime mitigates it. Overlapping claimant states and non-ratifiers attest the problem has been partially solved (overfishing is moderated) but that the strict reading's exclusivity creates a different problem—the suppression of historical claims and alternative sovereignty doctrines—which they argue outweighs the coordination benefit. Independent analysis (FAO fisheries reports, ITLOS case law, academic legal scholarship) supports both: the regime does reduce unregulated extraction AND it does entrench certain claimants while excluding others.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 endpoint) is high because the exclusive resource control granted to coastal states is decoupled from their marginal service cost (surveillance, enforcement) and the benefit accrues directly to state treasuries, not to participants in the system. The measurement series shows rising extraction from 0.52 at t=0 to 0.68 at t=40, indicating rent-seeking layering: initial coordination function (preventing commons overexploitation) was sufficient at t=0, but over time coastal states expanded the extractive scope (stricter enforcement of licensing, higher fees, expanded research restrictions). Suppression is high (0.79) and rising (0.61 at t=0 to 0.79 at t=40) because the constraint's persistence depends on actively suppressing alternative frameworks—historical-rights claims, freedom-of-navigation doctrines, non-ratifier assertions. Without high suppression, overlapping claimants would reassert historical claims and non-ratifiers would ignore the boundaries. Theater_ratio is moderate (0.42) and rising: coastal states maintain a functional enforcement apparatus (coast guard patrols, license administration), but an increasing share of enforcement effort is directed toward suppressing rival claims rather than toward the stated coordination benefit (overfishing prevention). Accessibility_collapse is high (0.82): once a state accepts the strict reading, alternatives are nearly unavailable—a coastal state cannot unilaterally expand its EEZ, overlapping claimants cannot access resources without negotiation, non-ratifiers face mounting costs of rejection. Resistance is substantial (0.71): overlapping claimants mount consistent legal challenges (ITLOS cases), non-ratifiers assert freedom-of-navigation operations, indigenous communities resist exclusion from traditional grounds.
 *
 * PERSPECTIVAL GAP:
 *   The coastal-state and overlapping-claimant seats compute sharply different types from the same constraint. From the coastal state's position (d near 0.0, powerful, arbitrage exit), the constraint is coordination that they maintain—a rope or genuine tangled_rope with justified enforcement. From the overlapping-claimant position (d near 1.0, powerful but constrained exit, faced with suppression), the same arrangement is a snare: suppressed historical rights, enforced loss of access, costs borne without benefit. The engine computes the per-seat type; the authoring surface declares the structural facts that drive the divergence: coastal states benefit and set the rules; overlapping claimants lose access and face suppression. This gap is not a failure of the framework—it is exactly what the framework measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations directly feed directionality: coastal_state_eez_controllers in beneficiaries → low d → low/negative effective extraction from that seat. overlapping_claimant_states and non_ratifying_maritime_powers in victims → high d → high effective extraction from those seats. The spatial_scope (global) moderately amplifies effective extraction (larger scope makes verification harder, so enforcement must be heavier to maintain compliance). The power atoms (institutional for coastal states, powerful for claimants, institutional for non-ratifiers) feed into the directionality derivation but do not override the beneficiary/victim structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (commons overexploitation, boundary disputes) is contested as to its current status. Coastal state authorities attest it is live—overfishing continues, boundary disputes recur, the EEZ regime is necessary deterrence. Overlapping claimants and non-ratifiers attest it is substantially dead—overfishing is moderated where strict EEZ is enforced, most boundaries are stable, the regime persists not because the founding problem drives it but because coastal states have locked in exclusive control. This mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) is the mandatrophy flag: the constraint would rearrange the world if it vanished (overlapping claims would reactivate), yet the founding problem's persistence is disputed. The classification resolves this by centering the active suppression (high enforcement cost for alternative claims) as evidence that the constraint's persistence depends on suppression rather than on coordination benefit alone. Tangled_rope classification holds: there is genuine coordination (EEZ prevents commons overexploitation), but there is also asymmetric extraction (exclusive control from claimants), and the extraction is maintained by active enforcement against alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the strict EEZ reading a genuine feature of customary international law, or a contested interpretation that benefits coastal states and disadvantages historical claimants and non-ratifiers?',
    'Comparative analysis of ITLOS rulings, state practice, and academic legal consensus across readings. Compare outcomes in cases where strict reading was applied vs. cases where historical-rights or non-ratifier readings were entertained.',
    'If the strict reading is demonstrated to be a constructed preference of coastal state coalitions (not a neutral application of UNCLOS text), the constraint''s classification shifts from tangled_rope (coordination + asymmetric extraction) toward snare (pure extraction with coordination cover story). If it is demonstrated to reflect genuine consensus among ratifying states, tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether strict EEZ exclusivity is inherent to UNCLOS or a constructed reading that benefits certain parties.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the high suppression of alternative sovereignty frameworks (historical rights, freedom of navigation, non-ratifier claims) structural (enforced by naval presence, license denial, asset seizure) or internalized (non-ratifiers and claimants have accepted the legitimacy of the strict reading)?',
    'Post-exit trajectory analysis: if non-ratifiers or overlapping claimants attempt exit (explicit rejection, military assertion of historical claims), does suppression persist or dissipate? Observe behavior in high-stakes cases (Arctic sovereignty, South China Sea disputes) where stakes are high enough to test genuine vs. internalized acceptance.',
    'If suppression is primarily internalized (acceptance of UNCLOS''s interpretive authority), the constraint''s effective suppression is lower than measured and the constraint is more stable. If suppression is primarily structural (maintained only by active enforcement), the constraint is more fragile and the extraction claim is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative frameworks is enforced externally or has been internalized by non-ratifiers and claimants.').

omega_variable(
    coordination_extraction_separability_unclos,
    'Are the genuine coordination benefits (predictable boundaries, reduced overfishing, stable resource management) structurally inseparable from the extraction of exclusive control from overlapping claimants and non-ratifiers, or could coordination be achieved with a more permissive boundary system (shared jurisdiction, overlapping zones)?',
    'Comparative-institutional analysis: examine EEZ regimes that permit overlapping jurisdiction (e.g., some Arctic arrangements, joint development zones). Compare outcomes on coordination metrics (overfishing reduction, dispute frequency) vs. extraction metrics (resource access, state autonomy).',
    'If coordination and extraction are separable, the measured extraction is rent-seeking layered onto a real coordination function (supports tangled_rope classification). If inseparable, part of measured extraction is the inherent cost of the coordination mechanism and should be treated as legitimate coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability_unclos, empirical, 'Whether UNCLOS EEZ exclusivity is necessary for coordination benefits or whether benefits could be achieved with permissive/overlapping zones.').

omega_variable(
    ratification_consensus_authenticity,
    'Is the near-universal ratification of UNCLOS and acceptance of the EEZ regime a genuine consensus, or a coerced/incentive-driven acceptance by weaker states seeking legitimacy and trade benefits?',
    'Historical analysis of ratification pressures, economic incentives offered, and statements of reservations or reluctance. Survey of states that ratified while simultaneously filing sovereignty claims (indicating ambivalent acceptance).',
    'If ratification was coerced, the constraint''s legitimacy is lower than its formal near-universality suggests, and the classification should emphasize the suppression dimension. If ratification was authentic, the constraint reflects a true baseline preference for the strict reading among coastal states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratification_consensus_authenticity, empirical, 'Whether UNCLOS ratification represents genuine state consensus or coerced/incentivized acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(uncl_tr_t0, observed).
narrative_ontology:measurement(uncl_tr_t5, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(uncl_tr_t5, observed).
narrative_ontology:measurement(uncl_tr_t10, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(uncl_tr_t10, observed).
narrative_ontology:measurement(uncl_tr_t15, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(uncl_tr_t15, observed).
narrative_ontology:measurement(uncl_tr_t20, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(uncl_tr_t20, observed).
narrative_ontology:measurement(uncl_tr_t30, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(uncl_tr_t30, observed).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(uncl_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(uncl_be_t0, observed).
narrative_ontology:measurement(uncl_be_t5, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(uncl_be_t5, observed).
narrative_ontology:measurement(uncl_be_t10, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(uncl_be_t10, observed).
narrative_ontology:measurement(uncl_be_t15, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(uncl_be_t15, observed).
narrative_ontology:measurement(uncl_be_t20, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(uncl_be_t20, observed).
narrative_ontology:measurement(uncl_be_t30, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(uncl_be_t30, observed).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(uncl_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0, 0.61).
narrative_ontology:measurement_basis(uncl_su_t0, observed).
narrative_ontology:measurement(uncl_su_t5, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(uncl_su_t5, observed).
narrative_ontology:measurement(uncl_su_t10, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(uncl_su_t10, observed).
narrative_ontology:measurement(uncl_su_t15, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(uncl_su_t15, observed).
narrative_ontology:measurement(uncl_su_t20, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement_basis(uncl_su_t20, observed).
narrative_ontology:measurement(uncl_su_t30, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement_basis(uncl_su_t30, observed).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(uncl_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__strict_eez_reading, 0.18).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% The constraint family unclos_sovereignty_boundary comprises three structurally distinct readings of Article 57: strict_eez_reading (this constraint, tangled_rope, high extraction, coastal state beneficiary), historical_rights_reading (snare-toward-tangled-rope, overlapping claimants' extracted rents from suppressed claims), and non_ratifier_enforcement_reading (snare, maritime powers coerced into implicit acceptance or costly rejection). All three share the same referent (Article 57 EEZ provision) but instantiate different constraints because the readings assign different beneficiary/victim sets, different suppression mechanisms, and different ε values. The strict reading's ε (0.68) reflects extraction from claimants and non-ratifiers specifically; the historical-rights reading's ε will be lower (it treats claimants as beneficiaries with a voice, reducing suppression from their perspective); the non-ratifier reading's ε will be higher (freedom of navigation constraints are not granted, only assumed, creating identity-lock suppression). Each reading is a separate story with its own six-questions answers and directionality logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__strict_eez_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
