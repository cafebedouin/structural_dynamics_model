% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement Integration Reading
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This constraint is the integration_reading of the contested
 *   federation_membership_kernel. The kernel concerns the legal and political
 *   status of free movement within the European Union. This reading treats
 *   free movement of workers as a fundamental right constitutive of EU
 *   citizenship and single-market completion, assigning supranational
 *   authority (the ECJ) an expansive interpretive mandate to maximize labor
 *   mobility and equal treatment. The constraint coordinates a continental
 *   labor market while asymmetrically extracting regulatory authority and
 *   fiscal resources from stationary national populations, displaced local
 *   labor, and sending-state public budgets.
 *
 * KEY AGENTS:
 *   - ECJ and supranational authority (agenda_setter, institutional/analytical) â sets and enforces expansive interpretation overriding national law
 *   - Mobile EU workers (beneficiary, moderate/mobile) â receive cross-border rights and equal-treatment protections
 *   - Receiving-state employers (beneficiary, powerful/mobile) â capture expanded labor-pool access
 *   - Displaced local workers (payer, powerless/trapped) â bear wage and security compression
 *   - Sending-state taxpayers (payer, moderate/constrained) â externalized brain-drain costs
 *   - Receiving-state taxpayers (payer, moderate/constrained) â uncompensated welfare fiscal costs
 *   - Member-state governments (payer, institutional/constrained) â lose sovereignty over labor and social policy
 *   - National labor unions (excluded, organized/constrained) â solidarity objections structurally marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.72).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.76).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement Integration Reading").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '820c9cc3-1950-4e03-b00a-98be85a92def').
narrative_ontology:cs_kernel_codification('820c9cc3-1950-4e03-b00a-98be85a92def', formalized).
narrative_ontology:cs_authority_grounding('820c9cc3-1950-4e03-b00a-98be85a92def', lineage).
narrative_ontology:cs_interpretation_layer_present('820c9cc3-1950-4e03-b00a-98be85a92def').
narrative_ontology:cs_reading_relation('820c9cc3-1950-4e03-b00a-98be85a92def', federation_membership_kernel__member_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('820c9cc3-1950-4e03-b00a-98be85a92def', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('820c9cc3-1950-4e03-b00a-98be85a92def', foundational, free_movement_constitutive_right).
narrative_ontology:cs_axiom_status(free_movement_constitutive_right, holdable).
narrative_ontology:cs_axiom_grounding('820c9cc3-1950-4e03-b00a-98be85a92def', free_movement_constitutive_right, conventional).
narrative_ontology:cs_axiom('820c9cc3-1950-4e03-b00a-98be85a92def', foundational, supremacy_of_supranational_interpretation).
narrative_ontology:cs_axiom_status(supremacy_of_supranational_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('820c9cc3-1950-4e03-b00a-98be85a92def', supremacy_of_supranational_interpretation, conventional).
narrative_ontology:cs_reference_frame('820c9cc3-1950-4e03-b00a-98be85a92def', supranational_integration_frame).
narrative_ontology:cs_drift_state('820c9cc3-1950-4e03-b00a-98be85a92def', contemporary_political_economy, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('820c9cc3-1950-4e03-b00a-98be85a92def', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, receiving_state_employers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_taxpayers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Treaties and Charter expansively to maximize labor mobility and equal treatment, issuing preliminary rulings that override national labor-market protections and welfare-eligibility boundaries. Its authority grows through each ruling that member states cannot block individually.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, ecj_supranational_authority, agenda_setter,
    institutional, generational, analytical, continental).

% Exercise Treaty-based rights to move, reside, and access social advantages on equal terms in host states. Their mobility is protected by supranational law against national restrictions, transferring opportunity from closed national labor markets to individual cross-border job seekers.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Gain access to an expanded, geographically mobile labor pool and benefit from reduced bargaining power of stationary local labor. Hiring friction across borders is lowered by centralized legal certainty, while wage-setting remains national.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_employers, beneficiary,
    powerful, biographical, mobile, national).

% Face wage compression, reduced job security, and diminished political voice when mobile workers enter local labor markets under equal-treatment rules. Stationary status leaves them without the legal privileges mobility confers.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_workers, payer,
    powerless, immediate, trapped, local).

% Bear the fiscal and human-capital loss when publicly educated workers emigrate without compensating fiscal transfers from receiving states. The constraint externalizes brain-drain costs onto the sending jurisdiction.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_taxpayers, payer,
    moderate, generational, constrained, national).

% Fund welfare systems and social-insurance pools that must extend benefits to mobile workers on equal terms, without fiscal compensation from sending states. The net fiscal burden shifts to the stationary resident population.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_taxpayers, payer,
    moderate, biographical, constrained, national).

% Lose regulatory authority to set labor-market protections, welfare-eligibility rules, and minimum-residence requirements. ECJ preliminary rulings override national legislation and constitutional social-solidity provisions, concentrating regulatory loss at the member-state level.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, member_state_governments, payer,
    institutional, generational, constrained, national).

% Advocate for local solidarity wages and protective labor standards, but are structurally sidelined in supranational jurisprudence where mobility rights and equal-treatment claims systematically outweigh solidarity-based objections.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, national_labor_unions, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates fragmented national labor markets into a single European labor space, solving cross-border matching frictions and preventing parochial restrictions that would fragment the internal market.
% TRANSFER_FUNCTION: Moves labor-market access and welfare entitlements from stationary national populations and sending-state public budgets to mobile EU citizens and receiving-state employers; simultaneously transfers regulatory authority over labor and social law from member states to supranational judicial institutions.
% ABSENT_VOICES: National labor unions defending local solidarity bargains, sending-state governments seeking fiscal compensation for brain drain, and constitutional courts invoking national identity limits are present in political discourse but systematically overruled in ECJ jurisprudence.
% DISAPPEARANCE_RATIONALE: If free movement as a supranationally enforceable fundamental right disappeared overnight, member states would reassert welfare-eligibility boundaries and labor-market protections, cross-border labor matching would renationalize, and the political economy of the single market would reorganize around territorial solidarity and segmented welfare states.
% FOUNDING_PROBLEM: Post-war Western Europe needed to prevent renewed conflict through economic interdependence and to remedy labor shortages and surpluses across segmented national economies by enabling cross-border worker mobility.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and founding-member historiography attest the problem is still live (competitiveness, demographic aging). Member-state governments, national labor unions, and independent comparative-political-economy scholarship from outside the ECJ beneficiary circle attest the original problem has transformed and the current arrangement now produces social dumping and fiscal externalization; no corroboration from outside the benefiting parties supports the unmodified founding narrative.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically transfers welfare costs and labor-market rents from stationary populations to mobile actors, decoupled from fiscal compensation. Suppression (0.76) is higher still because the arrangement depends on ECJ jurisprudence actively overriding national alternatives and member-state resistance. Theater_ratio (0.40) reflects that genuine market-integration function remains, but an increasing share of enforcement activity is performative legalism defending supranational authority rather than solving coordination failures. Accessibility_collapse (0.68) is elevated because national protective alternatives are legally barred by supremacy doctrine, yet political resistance prevents full collapse. Resistance (0.75) is high due to persistent member-state litigation, national constitutional court pushback, and electoral salience of migration.
 *
 * PERSPECTIVAL GAP:
 *   The ECJ and mobile-worker seats experience the constraint as genuine coordination and rights enforcement (low effective extraction, potentially computing as rope from those seats). Displaced local workers, sending-state taxpayers, and receiving-state taxpayers experience it as extraction backed by coercive override (high effective extraction, computing as snare or tangled rope). Member-state governments experience a sovereignty loss that is structural rather than monetary, producing a distinct high-d seat. The engine computes this divergence from the shared structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (mobile workers, receiving-state employers) sit near the full-beneficiary end: the constraint subsidizes their mobility and hiring capacity. Victims (displaced workers, both taxpayer groups) sit near the full-target end: they bear uncompensated costs with constrained or trapped exit. The ECJ agenda-setter seat is low-d but not zero, because the institution accumulates authority from the constraint's operation. Member-state governments are mid-to-high d because they pay in sovereignty what they do not collect in revenue.
 *
 * MANDATROPHY ANALYSIS:
 *   Without declared victims and active enforcement, this constraint could be misread as a rope (market coordination) or scaffold (transitional integration). The mandatrophy guard is satisfied by naming three distinct victim groups and requiring active enforcement, ensuring the engine detects the asymmetric extraction component. The founding problem is contested rather than dead, preventing premature piton classification; the temporal measurements show extraction accumulation over decades rather than sunset decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_externalization_ambiguity,
    'Are uncompensated welfare costs and brain-drain externalities transitional coordination costs of an incomplete single market, or permanent structural extraction inherent to the integration reading?',
    'Comparative analysis of fiscal-transfer mechanisms (e.g., proposed EU social-security coordination reforms) and brain-drain compensation schemes; if costs persist after decades without offsetting institutions, they are structural extraction.',
    'If transitional, the constraint remains a tangled rope trending toward rope; if permanent, the extraction component dominates and the computed type shifts toward snare for victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_externalization_ambiguity, empirical, 'Whether fiscal externalization is a transition cost or permanent extraction').

omega_variable(
    supranational_mandate_scope,
    'Does the ECJ''s expansive interpretation of free movement reflect the treaty kernel''s inherent meaning, or has the interpretation drifted beyond the kernel''s original scope?',
    'Historical-legal analysis of travaux prÃ©paratoires and subsequent treaty amendments; empirical tracking of ECJ case-law expansion relative to member-state ratifier intent.',
    'If the interpretation has drifted substantially beyond the kernel, the constraint''s legitimacy is conventional extraction rather than faithful lineage; this would reclassify the authority_grounding and increase theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_mandate_scope, conceptual, 'ECJ expansive interpretation as faithful or drifted from treaty kernel').

omega_variable(
    kernel_reading_contest,
    'How would the beneficiary-victim structure change if the member_sovereignty_reading or welfare_coordination_reading were adopted instead?',
    'Comparative analysis of the full federation_membership_kernel constraint family; examining which agents change role and directionality under each reading.',
    'If member_sovereignty_reading prevailed, mobile workers would shift to victim set and member states would become agenda-setters; if welfare_coordination_reading prevailed, victims would emphasize social-dumping firms and the coordination function would dominate over extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Sibling reading structural deltas in the kernel family').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__integration_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__integration_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__integration_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(fede_tr_t30, federation_membership_kernel__integration_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(fede_tr_t40, federation_membership_kernel__integration_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(fede_tr_t50, federation_membership_kernel__integration_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__integration_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__integration_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__integration_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(fede_be_t30, federation_membership_kernel__integration_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(fede_be_t40, federation_membership_kernel__integration_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(fede_be_t50, federation_membership_kernel__integration_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__integration_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__integration_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__integration_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(fede_su_t30, federation_membership_kernel__integration_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(fede_su_t40, federation_membership_kernel__integration_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(fede_su_t50, federation_membership_kernel__integration_reading, suppression_requirement, 50, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% The federation_membership_kernel decomposes into three structurally distinct readings because the natural-language label 'EU free movement' conflates: (1) a supranational-integration claim with ECJ supremacy (this file), (2) a member-state-sovereignty claim with national boundary authority, and (3) a welfare-coordination claim with inter-state social-policy autonomy. Their epsilon values, beneficiary sets, and directionality profiles differ widely and must be modeled as separate constraints linked in a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
