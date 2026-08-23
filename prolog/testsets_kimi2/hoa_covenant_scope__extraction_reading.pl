% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__extraction_reading, []).

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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant as Revenue Extraction and Board Power Consolidation Mechanism
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story instantiates the extraction reading of the
 *   contested kernel hoa_covenant_scope: the claim that homeowners
 *   association (HOA) covenants function primarily as revenue generation
 *   mechanisms and board power consolidation tools rather than as genuine
 *   coordination devices. Under this reading, covenant enforcement is
 *   characterized by fine proliferation, selective targeting of financially
 *   vulnerable homeowners, expedited lien processes, and attorney fee
 *   extraction that enriches board-controlled budgets, property management
 *   contractors, and legal counsel. The nominal coordination
 *   functionâshared infrastructure maintenance and aesthetic
 *   standard-settingâhas been subordinated to a fiscal extraction cycle in
 *   which enforcement discretion is deployed to maximize revenue and
 *   centralize governance authority. This reading is one of three live
 *   positions on the kernel, alongside the coordination reading (genuine
 *   shared-infrastructure coordination) and the behavioral control reading
 *   (aesthetic uniformity as property value protection).
 *
 * KEY AGENTS:
 *   - board_members: Agenda-setter (organized/mobile/local) â administer covenant enforcement, control violation identification and fine levying, consolidate governance authority
 *   - property_management_firms: Beneficiary (organized/mobile/regional) â contract for enforcement administration, revenue scales with violation volume
 *   - legal_counsel: Beneficiary (organized/mobile/regional) â extract attorney fees from adversarial enforcement and lien proceedings
 *   - financially_vulnerable_homeowners: Payer (powerless/trapped/local) â bear disproportionate fines and liens, lack resources to contest or exit
 *   - renters_via_pass_through: Payer (powerless/constrained/local) â absorb covenant costs via rent increases without governance standing
 *   - fair_housing_advocates: Observer (organized/analytical/national) â document disparities in enforcement and advocate for reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.66).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.78).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant as Revenue Extraction and Board Power Consolidation Mechanism").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, '6ffaabdc-8489-4887-a934-8afdd85ea6a7').
narrative_ontology:cs_kernel_codification('6ffaabdc-8489-4887-a934-8afdd85ea6a7', fixed_text).
narrative_ontology:cs_authority_grounding('6ffaabdc-8489-4887-a934-8afdd85ea6a7', extraction).
narrative_ontology:cs_interpretation_layer_present('6ffaabdc-8489-4887-a934-8afdd85ea6a7').
narrative_ontology:cs_reading_relation('6ffaabdc-8489-4887-a934-8afdd85ea6a7', hoa_covenant_scope__coordination_reading, influences).
narrative_ontology:cs_reading_relation('6ffaabdc-8489-4887-a934-8afdd85ea6a7', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('6ffaabdc-8489-4887-a934-8afdd85ea6a7', foundational, enforcement_revenue_as_legitimate_budgetary_tool).
narrative_ontology:cs_axiom_status(enforcement_revenue_as_legitimate_budgetary_tool, holdable).
narrative_ontology:cs_axiom_grounding('6ffaabdc-8489-4887-a934-8afdd85ea6a7', enforcement_revenue_as_legitimate_budgetary_tool, conventional).
narrative_ontology:cs_axiom('6ffaabdc-8489-4887-a934-8afdd85ea6a7', foundational, board_discretion_over_violation_targets).
narrative_ontology:cs_axiom_status(board_discretion_over_violation_targets, holdable).
narrative_ontology:cs_axiom_grounding('6ffaabdc-8489-4887-a934-8afdd85ea6a7', board_discretion_over_violation_targets, conventional).
narrative_ontology:cs_reference_frame('6ffaabdc-8489-4887-a934-8afdd85ea6a7', board_fiscal_sovereignty).
narrative_ontology:cs_drift_state('6ffaabdc-8489-4887-a934-8afdd85ea6a7', contemporary_regulatory_scrutiny, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6ffaabdc-8489-4887-a934-8afdd85ea6a7', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, legal_counsel).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer covenant enforcement with discretionary power over violation identification, fine levying, and lien initiation. Collect personal and institutional benefits from expanded board authority and control over community budget. Can choose to enforce selectively against non-board allies or financially vulnerable targets.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, board_members, agenda_setter,
    organized, biographical, mobile, local).

% Contracted by HOAs to manage operations, enforcement logistics, and violation processing. Revenue grows with the volume of enforcement actions, fines, and administrative fees, creating alignment with expansive covenant interpretation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    organized, biographical, mobile, regional).

% Bill the HOA and individual homeowners for enforcement actions, lien filings, collection proceedings, and covenant interpretation. Attorney fee provisions in covenants often allow fee recovery from violating homeowners, making legal counsel a direct beneficiary of adversarial enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_counsel, beneficiary,
    organized, biographical, mobile, regional).

% Bear disproportionate fine burdens and expedited liens for violations that may be minor or selectively enforced. Lack resources to contest enforcement legally or to relocate. Fines compound and can lead to foreclosure, extracting wealth directly from those least able to pay.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, immediate, trapped, local).

% Do not participate in HOA governance but absorb fines and assessment increases through rent hikes or direct pass-through charges from landlords. Have no standing to contest covenant enforcement actions that raise their housing costs.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% Document disparities in covenant enforcement incidence, fine amounts, and lien foreclosure rates across demographic groups. Advocate for legislative caps on fines, lien reform, and mandatory due process in HOA proceedings. Do not directly pay or collect from the constraint.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, fair_housing_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally coordinates shared property maintenance and aesthetic standards, but the operative function has shifted to revenue generation and board power consolidation through fine proliferation, expedited liens, and selective enforcement of ambiguous covenant terms.
% TRANSFER_FUNCTION: Moves money from homeowners (especially financially vulnerable ones) and renters to HOA boards, property management firms, and legal counsel through fines, enforcement fees, lien processing charges, and attorney fee awards; also moves governance discretion from distributed homeowners to centralized board control.
% ABSENT_VOICES: Renters, who bear pass-through costs but lack voting rights or standing in covenant proceedings; financially vulnerable homeowners facing foreclosure from lien stacking, who are procedurally excluded from negotiation by attorney-fee-shifting provisions; and alternative governance organizers proposing municipal annexation or cooperative maintenance models, who are excluded by the covenant's legal monopoly on collective action within the subdivision.
% DISAPPEARANCE_RATIONALE: If covenant enforcement vanished overnight, board members would lose a primary tool of fiscal and political control; property management firms and legal counsel would lose a revenue stream; homeowner budgets would shift as fines and fees disappeared; and community governance would reorganize around either municipal services or voluntary coordination.
% FOUNDING_PROBLEM: Governing documents were established to maintain common property, protect property values, and coordinate shared infrastructure in the absence of municipal services during early suburban development.
% FOUNDING_PROBLEM_CORROBORATION: Municipal planners and urban governance historians attest that HOAs originally formed to fill service gaps in suburban development. Fair housing advocates and consumer protection agencies attest the founding problem is largely solved by municipal incorporation and that the arrangement now persists as a fiscal extraction layer. The board and management industry assert the problem remains live, but no corroboration from outside the benefiting parties supports that claim.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__extraction_reading, 0.66, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenant_scope__extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66) is high because the constraint systematically transfers wealth from homeowners and renters to boards, management firms, and attorneys through fines and fees that are decoupled from actual maintenance costs. Suppression (0.78) is higher still because the arrangement depends on active enforcementâexpedited liens, fee-shifting provisions, and selective violation huntingâto persist; without active suppression of alternative governance models (municipal annexation, voluntary cooperatives), the extraction collapses. Theater ratio (0.55) reflects significant performative maintenance of 'community standards' and 'property value protection' that masks the fiscal function. Accessibility collapse (0.68) is high because exiting an HOA-governed property is prohibitively expensive for financially vulnerable owners, and lien encumbrances further trap them. Resistance (0.45) is moderate: isolated homeowners resist individually but lack collective organizing capacity, while legislative reform efforts mount slowly at state levels.
 *
 * PERSPECTIVAL GAP:
 *   The board and management seats experience the constraint as legitimate governance and necessary fiscal management; they derive authority and revenue from its operation. The homeowner and renter seats experience the same structure as unpredictable, extractive, and inescapable. The engine computes this divergence from the structural dataâbeneficiaries with mobile exit versus victims with trapped or constrained exitâwithout relying on the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Board members are structural beneficiaries and agenda-setters with low directionality (d near 0.0): they control the rules and collect governance power. Property management firms and legal counsel are pure beneficiaries (d near 0.0): they collect fees without bearing covenant costs. Financially vulnerable homeowners are full targets (d near 1.0): they pay fines and liens with no offsetting benefit and have trapped exit. Renters are also targets (d near 0.9): they pay pass-through costs with constrained exit and no governance voice. Fair housing advocates sit at analytical distance (d near 0.5 by default, though their institutional power is moderate).
 *
 * MANDATROPHY ANALYSIS:
 *   The extraction reading prevents mislabeling this constraint as pure coordination (Rope) by identifying the active enforcement and asymmetric extraction that parasitize any genuine maintenance function. It prevents mislabeling as pure extraction (Snare) by acknowledging the nominal coordination shell: covenants do maintain some shared infrastructure, and the extraction rides on that institutional form rather than existing as bare coercion. The dead founding problem (R5) confirms the mandatrophy: the original service-gap rationale has been superseded by municipal development, yet the constraint persists as a fiscal zombie.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_extraction_vs_coordination,
    'Is the HOA covenant primarily a revenue extraction and power consolidation mechanism, or a genuine coordination tool for shared infrastructure maintenance?',
    'Comparative analysis of budget allocation: what share of covenant-derived revenue funds maintenance versus administrative and legal costs, board-controlled discretionary funds, and management company contracts; also fine incidence distribution across homeowner demographics.',
    'If extraction dominates budget flows, the coordination reading is falsified for this association and the extraction reading is validated; if maintenance dominates, the extraction reading overstates the case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_extraction_vs_coordination, conceptual, 'Whether the covenant is extraction or coordination').

omega_variable(
    selective_enforcement_intent,
    'Does selective enforcement reflect intentional targeting of financially vulnerable homeowners for revenue extraction, or does it reflect resource-constrained prioritization of visible violations?',
    'Statistical audit of violation notices matched to homeowner equity, payment history, and board relationship; controlling for violation visibility and type.',
    'Intentional targeting would validate the extraction reading''s classification as tangled_rope or snare; resource-constrained prioritization would support a piton or degraded-coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_intent, empirical, 'Whether selective enforcement is intentional extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is homeowner compliance driven primarily by structural barriers to exit (transaction costs of selling, lien encumbrances) or by internalized norms of community belonging and contractual obligation?',
    'Post-exit surveys and market analysis: do homeowners who sell and exit HOA jurisdictions show persistent deference to covenant-like norms, or does compliance behavior drop immediately upon removal?',
    'If internalized, effective suppression exceeds structural measures and the constraint operates partly as identity coordination; if purely structural, extraction is maintained by exit barriers alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').

omega_variable(
    renter_pass_through_visibility,
    'To what extent do renters actually bear covenant extraction via rent pass-through versus landlord absorption of fines?',
    'Rent differential analysis in matched HOA and non-HOA units, combined with lease clause review for fine pass-through provisions.',
    'Confirmed pass-through validates renters_via_pass_through as victims; landlord absorption would shift victim designation to property investors and alter directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renter_pass_through_visibility, empirical, 'Whether renters bear extraction costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_extract_tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hoa_extract_tr_t4, hoa_covenant_scope__extraction_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(hoa_extract_tr_t8, hoa_covenant_scope__extraction_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(hoa_extract_tr_t12, hoa_covenant_scope__extraction_reading, theater_ratio, 12, 0.44).
narrative_ontology:measurement(hoa_extract_tr_t16, hoa_covenant_scope__extraction_reading, theater_ratio, 16, 0.5).
narrative_ontology:measurement(hoa_extract_tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(hoa_extract_be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hoa_extract_be_t4, hoa_covenant_scope__extraction_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(hoa_extract_be_t8, hoa_covenant_scope__extraction_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(hoa_extract_be_t12, hoa_covenant_scope__extraction_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(hoa_extract_be_t16, hoa_covenant_scope__extraction_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(hoa_extract_be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(hoa_extract_su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(hoa_extract_su_t4, hoa_covenant_scope__extraction_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(hoa_extract_su_t8, hoa_covenant_scope__extraction_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(hoa_extract_su_t12, hoa_covenant_scope__extraction_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(hoa_extract_su_t16, hoa_covenant_scope__extraction_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(hoa_extract_su_t20, hoa_covenant_scope__extraction_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is the extraction reading of the hoa_covenant_scope kernel, decomposed from the coordination and behavioral control readings due to structurally distinct epsilon values, beneficiary and victim asymmetries, and enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
