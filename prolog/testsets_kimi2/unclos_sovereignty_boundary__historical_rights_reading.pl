% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Rights Override of UNCLOS EEZ Provisions
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates the historical_rights_reading of the
 *   unclos_sovereignty_boundary kernel. The reading holds that historical
 *   usage and occupation generate sovereign maritime rights that predate and
 *   override UNCLOS EEZ provisions, creating expanded maritime zones for
 *   claimant states at the expense of UNCLOS-defined EEZ holders and
 *   navigational freedom. It is contested by strict_eez_reading (which holds
 *   EEZ boundaries exclusive per UNCLOS Article 57) and
 *   non_ratifier_enforcement_reading (which asserts freedom of navigation as
 *   customary law enforceable by naval presence regardless of UNCLOS
 *   ratification). This story authors only the historical_rights_reading;
 *   sibling readings are separate constraints in the same family.
 *
 * KEY AGENTS:
 *   - Expansive claimant states (powerful/mobile): Primary beneficiaries who gain extended maritime control through historical-legal narrative and naval enforcement.
 *   - EEZ-holding coastal states (moderate/constrained): Primary payers who lose exclusive resource and regulatory rights to overlapping historical claims.
 *   - Navigational actors (organized/constrained): Secondary payers bearing increased route risk and legal uncertainty in contested waters.
 *   - UNCLOS arbitration bodies (institutional/analytical): Observers issuing rulings that claimant states frequently reject.
 *   - Regional fishing communities (powerless/trapped): Excluded voices displaced from traditional grounds without legal standing to challenge claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.72).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.78).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Rights Override of UNCLOS EEZ Provisions").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, 'a9ec9f0f-12de-4d46-be5d-e3db20024ef5').
narrative_ontology:cs_kernel_codification('a9ec9f0f-12de-4d46-be5d-e3db20024ef5', fixed_text).
narrative_ontology:cs_authority_grounding('a9ec9f0f-12de-4d46-be5d-e3db20024ef5', lineage).
narrative_ontology:cs_interpretation_layer_present('a9ec9f0f-12de-4d46-be5d-e3db20024ef5').
narrative_ontology:cs_reading_relation('a9ec9f0f-12de-4d46-be5d-e3db20024ef5', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('a9ec9f0f-12de-4d46-be5d-e3db20024ef5', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('a9ec9f0f-12de-4d46-be5d-e3db20024ef5', foundational, historical_occupation_establishes_maritime_title).
narrative_ontology:cs_axiom_status(historical_occupation_establishes_maritime_title, holdable).
narrative_ontology:cs_axiom_grounding('a9ec9f0f-12de-4d46-be5d-e3db20024ef5', historical_occupation_establishes_maritime_title, conventional).
narrative_ontology:cs_axiom('a9ec9f0f-12de-4d46-be5d-e3db20024ef5', secondary, effective_control_validates_pre_conventional_rights).
narrative_ontology:cs_axiom_status(effective_control_validates_pre_conventional_rights, holdable).
narrative_ontology:cs_axiom_grounding('a9ec9f0f-12de-4d46-be5d-e3db20024ef5', effective_control_validates_pre_conventional_rights, conventional).
narrative_ontology:cs_reference_frame('a9ec9f0f-12de-4d46-be5d-e3db20024ef5', historical_practice_sovereignty_framework).
narrative_ontology:cs_drift_state('a9ec9f0f-12de-4d46-be5d-e3db20024ef5', post_2016_arbitration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a9ec9f0f-12de-4d46-be5d-e3db20024ef5', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, navigational_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that assert pre-modern historical usage, discovery records, and cartographic evidence to claim maritime zones beyond or overlapping with UNCLOS EEZ limits. They deploy naval patrols, construct installations on disputed features, and cite continuous administration to justify expanded resource and strategic control. They can modulate enforcement intensity but cannot unilaterally compel international recognition.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, beneficiary,
    powerful, generational, mobile, regional).

% Coastal states whose UNCLOS-granted EEZ entitlements are overridden by expansive historical claims from more powerful neighbors. They lose exclusive fisheries, hydrocarbon, and regulatory rights in waters they would control under strict UNCLOS interpretation. Exit options are limited to slow international litigation and diplomatic coalition-building, with no guaranteed enforcement of favorable rulings.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    moderate, generational, constrained, regional).

% Naval forces and commercial shipping operators whose freedom of navigation is restricted by expanded historical claims. They face increased risk of interception, legal uncertainty, and route constraints in contested maritime zones where claimant states assert domestic jurisdiction over foreign military and commercial transit.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, navigational_actors, payer,
    organized, biographical, constrained, global).

% International tribunals and Annex VII arbitration panels that adjudicate maritime boundary disputes under UNCLOS. They issue rulings interpreting the Convention's relationship to historical claims, but lack direct enforcement mechanisms against non-compliant powerful states and cannot compel appearance or compliance.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, unclos_arbitration_bodies, observer,
    institutional, generational, analytical, global).

% Coastal fishing populations whose traditional grounds fall within newly asserted historical zones. They are displaced by claimant state enforcement actions and excluded from the international legal conversations that determine access rights, lacking standing to challenge claims before tribunals.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, regional_fishing_communities, excluded,
    powerless, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal-narrative framework for maritime territorial ordering in regions where UNCLOS EEZ allocations overlap with long-standing national claims based on historical usage, discovery, and cartographic precedent.
% TRANSFER_FUNCTION: Moves exclusive maritime resource and regulatory control from UNCLOS-defined EEZ holders to states asserting pre-modern historical occupation, while increasing constraint burdens on third-party navigational actors operating in claimed waters.
% ABSENT_VOICES: Regional fishing communities whose traditional grounds fall within newly asserted historical zones; smaller coastal states lacking the legal capacity to challenge claims before international tribunals.
% DISAPPEARANCE_RATIONALE: If the historical rights override vanished overnight, EEZ-holding coastal states would regain exclusive control per UNCLOS Article 57, expansive claimant navies would withdraw from contested perimeters, and freedom of navigation operations would no longer be necessary in affected zones â the maritime order would shift from historical-claim-based to strict-UNCLOS-based allocation.
% FOUNDING_PROBLEM: How to reconcile pre-existing national maritime claims rooted in centuries of usage, discovery, and cartographic assertion with the post-WWII move toward codified, distance-based ocean governance under UNCLOS.
% FOUNDING_PROBLEM_CORROBORATION: Expansive claimant states attest the problem is live, citing pre-modern records. EEZ-holding coastal states and non-claimant maritime powers attest the problem was resolved by UNCLOS ratification and that historical claims are now revanchist cover for expansion; the 2016 South China Sea arbitration and subsequent scholarship from outside the beneficiary set support the UNCLOS-supremacy reading.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers substantial EEZ entitlements from coastal states to historical claimants without reciprocal compensation. Suppression (0.78) is higher still because the arrangement requires active naval patrols, island occupation, and diplomatic coercion to maintain against tribunal rulings and neighboring state resistance. Theater_ratio (0.55) is moderate-high: the historical research and cartographic production serve genuine coordination functions in claimant state domestic politics but also perform legitimacy for international audiences. Accessibility_collapse (0.68) reflects that once historical claims are militarized, strict UNCLOS remedies become practically inaccessible to weaker coastal states. Resistance (0.55) captures ongoing but non-decisive FONOPs, tribunal filings, and diplomatic protests. Measurements trace the constraint's intensification from dormant claims (1982) through naval modernization and island-building to the present rejection of the 2016 arbitration.
 *
 * PERSPECTIVAL GAP:
 *   The expansive claimant state seat experiences this constraint as restoration of rightful territorial order and legitimate resource management; the engine will compute a low effective extraction for this seat. The EEZ-holder and navigational seats experience the same structure as expropriation and restricted access; the engine computes high effective extraction for these seats. The per-seat divergence is the central measurement the framework exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive claimant states are structural beneficiaries: they collect expanded maritime jurisdiction and resource access through the historical rights doctrine, with mobile exit options (they can modulate enforcement intensity). Their directionality sits near the beneficiary end. EEZ-holding coastal states are structural targets: they bear the loss of exclusive EEZ control and have constrained exit options (international litigation is slow and unenforced). Navigational actors are secondary targets: they face increased operational constraints in claimed waters with limited ability to reroute around strategic chokepoints. UNCLOS tribunals are observers with analytical exit; their rulings against the constraint do not alter claimant state behavior.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as pure snare because it does solve a genuine coordination problem: without some framework for overlapping maritime claims, anarchy and conflict would be more likely in contested seas. However, the coordination is inseparable from asymmetric extraction: the same historical narrative that 'coordinates' the region also redistributes control unilaterally. The presence of active enforcement (0.78) and the rising theater_ratio (0.15 to 0.55) over the interval indicate that the coordination function has increasingly become a cover for extraction, but the genuine coordination problem at the kernel's origin prevents reclassification to snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_authenticity,
    'Are the historical maps and records cited by claimant states genuine evidence of continuous state practice, or retroactive constructions?',
    'Independent archival and cartographic forensic analysis by third-party historians; multi-source verification of claimed historical usage against contemporaneous records from other states.',
    'If evidence is largely constructed, the constraint''s legitimacy collapses toward pure extraction; if authentic, the coordination function gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_authenticity, empirical, 'Authenticity of historical evidence supporting sovereignty claims').

omega_variable(
    customary_law_status,
    'Does the historical rights doctrine possess sufficient consistent state practice and opinio juris to constitute customary international law independent of UNCLOS?',
    'Systematic review of state conduct and legal opinio juris across maritime nations; ICJ or ITLOS advisory opinion on the interaction between UNCLOS and pre-existing customary claims.',
    'If recognized as customary law, the constraint gains legal durability; if rejected, it remains a unilateral extraction mechanism enforceable only by power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_status, conceptual, 'Customary international law status of historical rights doctrine').

omega_variable(
    enforcement_sustainability,
    'Can expansive claimant states sustain coercive maritime enforcement indefinitely without triggering collective security responses or naval counter-balancing?',
    'Longitudinal tracking of naval expenditure, alliance formation patterns, and incident frequency in contested zones; observation of whether affected states shift from litigation to collective security arrangements.',
    'If enforcement proves unsustainable, the constraint drifts toward piton or mandatrophy; if sustainable, it stabilizes as a durable tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Long-term sustainability of unilateral maritime enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_hist_tr_t0, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(unclos_hist_tr_t10, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(unclos_hist_tr_t20, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(unclos_hist_tr_t30, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(unclos_hist_tr_t40, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(unclos_hist_be_t0, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(unclos_hist_be_t10, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(unclos_hist_be_t20, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(unclos_hist_be_t30, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(unclos_hist_be_t40, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(unclos_hist_su_t0, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(unclos_hist_su_t10, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(unclos_hist_su_t20, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(unclos_hist_su_t30, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(unclos_hist_su_t40, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the unclos_sovereignty_boundary kernel, which decomposes into three structurally distinct constraints: strict_eez_reading (UNCLOS exclusivity), historical_rights_reading (pre-UNCLOS occupation overrides EEZ), and non_ratifier_enforcement_reading (customary FONOPs independent of ratification). Each reading has a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
