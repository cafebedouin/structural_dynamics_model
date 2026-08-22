% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__developer_liability, []).

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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer Primary Liability for Underlying Capability
 *   domain: legal/regulatory
 *
 * SUMMARY:
 *   The developer-liability reading holds that software developers, as
 *   creators of underlying capabilities, bear primary legal responsibility
 *   for harms those capabilities cause. This is a kernel reading
 *   instantiating one position within a contested liability-attribution
 *   domain. The referent is the doctrine-in-operation: the standing legal
 *   arrangement that places developers at the top of the causal and
 *   responsibility chain. The reading's endorsed alternative
 *   (deployer-liability or shared-liability regimes) is NOT the referent; ε
 *   measures the extraction the developer-liability arrangement itself
 *   exhibits. The claim is tangled_rope: it solves a genuine coordination
 *   problem (who is responsible?) while asymmetrically extracting from
 *   developers. The author's assessment is that this reading has come to
 *   operate as enforced extraction with growing performative justification,
 *   not pure coordination.
 *
 * KEY AGENTS:
 *   - software_developers: primary payers, bear liability burden and compliance costs, trapped by constrained exit
 *   - deployers: institutional beneficiaries, externalize risk, retain arbitrage-grade exit
 *   - end_users: diffuse beneficiaries (clarity of defendant) and payers (distance from accountability)
 *   - regulatory_authorities: institutional beneficiaries and agenda-setters, enforce the doctrine, define developer obligations
 *   - liability_insurers: organized payers, bear residual risk and premium volatility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.72).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer Primary Liability for Underlying Capability").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "legal/regulatory").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, '69d21a5b-9b60-43ee-a915-0799ad73ee07').
narrative_ontology:cs_kernel_codification('69d21a5b-9b60-43ee-a915-0799ad73ee07', formalized).
narrative_ontology:cs_authority_grounding('69d21a5b-9b60-43ee-a915-0799ad73ee07', lineage).
narrative_ontology:cs_interpretation_layer_present('69d21a5b-9b60-43ee-a915-0799ad73ee07').
narrative_ontology:cs_reading_relation('69d21a5b-9b60-43ee-a915-0799ad73ee07', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('69d21a5b-9b60-43ee-a915-0799ad73ee07', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('69d21a5b-9b60-43ee-a915-0799ad73ee07', foundational, creator_bears_primary_responsibility).
narrative_ontology:cs_axiom_status(creator_bears_primary_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('69d21a5b-9b60-43ee-a915-0799ad73ee07', creator_bears_primary_responsibility, deontological).
narrative_ontology:cs_axiom('69d21a5b-9b60-43ee-a915-0799ad73ee07', secondary, creator_controls_capability_definition).
narrative_ontology:cs_axiom_status(creator_controls_capability_definition, holdable).
narrative_ontology:cs_axiom_grounding('69d21a5b-9b60-43ee-a915-0799ad73ee07', creator_controls_capability_definition, empirically_contingent).
narrative_ontology:cs_reference_frame('69d21a5b-9b60-43ee-a915-0799ad73ee07', original_creator_accountability).
narrative_ontology:cs_drift_state('69d21a5b-9b60-43ee-a915-0799ad73ee07', contemporary_regulatory_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('69d21a5b-9b60-43ee-a915-0799ad73ee07', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deployers).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, end_users).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, regulatory_authorities).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, software_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, end_users).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, liability_insurers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, open_source_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create software capabilities and release them. Under the developer-liability reading, they bear primary legal responsibility for all harms those capabilities cause, including harms from deployment configurations they did not design or control. They must manage or disclose risks, maintain liability insurance, implement defensive practices, and defend themselves in litigation. Their exit options are constrained: they can leave the market entirely, but cannot simply reduce liability by disclaiming responsibility. They have no control over deployment decisions yet bear the liability consequences of those decisions.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, software_developers, payer,
    moderate, biographical, constrained, global).

% Integrate software into specific operational contexts, make deployment decisions (configuration, access controls, monitoring), and decide how to use the capability. Under this reading, they externalize risk: liability points upstream to developers, not to their own integration choices. They benefit from clear developer accountability without bearing comparable legal exposure for their deployment decisions. They retain high flexibility in deployment choices and can shift responsibility if harms occur. They have more context and control over deployment than developers do, but the liability regime does not reflect this.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deployers, beneficiary,
    powerful, biographical, arbitrage, global).

% Use or are affected by deployed software systems. They benefit from the clarity that a developer is a named party liable for capability defects, giving them a potential defendant. They also bear some cost: they may experience harms, be subject to remediation burdens, or face restricted access if developers scale back risky capabilities. They lack visibility into developer design choices and deployer integration decisions, making it difficult for them to evaluate actual risk. They are trapped by their dependence on digital systems.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, end_users, beneficiary,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, end_users, payer).

% Enforce liability through tort law, product liability statutes, regulatory mandates, and licensing requirements. They benefit by having a clear chain of responsibility and a named party (developers) they can compel to maintain standards, disclose risks, and respond to incidents. This reading gives regulatory authorities a lever to standardize developer obligations across markets. They set the rules and monitor developer compliance; they have institutional power and analytical perspective on the entire system.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulatory_authorities, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, regulatory_authorities, agenda_setter).

% Provide professional liability and product liability coverage for developers. Under this reading, they bear residual risk: they must underwrite the developer's exposure and pay claims when harms are attributed to the underlying capability. Their premium structures reflect the breadth of the liability regime; when liability exposure broadens (new harm categories, lower causation thresholds), insurers must raise premiums or withdraw. They have constrained exit: they cannot easily exit the market because developers need coverage to operate. They face premium volatility driven by regulatory changes and liability jurisprudence.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, liability_insurers, payer,
    organized, biographical, constrained, global).

% Contribute code to open-source projects that may be deployed in sensitive contexts (critical infrastructure, security-sensitive applications). Under the developer-liability reading, they are creators of underlying capability and thus theoretically liable, despite having no deployment context, no enforcement capacity, and no insurance. They are trapped by the reading's attribution rule: any harm caused by their code, no matter how deployed, points back to them. They have minimal practical exit options and often no compensation for the liability exposure created by the reading.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_contributors, payer,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, deployers).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear chain of responsibility for software harms: developers must certify capabilities as safe, disclose known risks, maintain standards, and respond to vulnerability reports. This solves the coordination problem of 'who is accountable when distributed software causes harm' by naming the creator as the responsible party. It enables regulatory bodies to set uniform developer obligations across markets and gives end-users a clear defendant.
% TRANSFER_FUNCTION: Transfers legal liability, insurance cost, disclosure burden, and regulatory compliance expense from deployers and end-users to software developers. Deployers benefit by externalizing risk; end-users benefit by having a named accountability point; regulatory authorities benefit by having a clear enforcement lever. Developers pay through insurance premiums, compliance budgets, litigation costs, and defensive documentation. Open-source contributors and individual developers face exposure without compensation.
% ABSENT_VOICES: Deployers' causal contributions to harms and their risk-allocation choices are not centered in this reading. The deployer-liability and shared-liability readings would argue that deployment decisions, integration choices, and operational context are primary causal factors in many harms, and that holding only developers accountable is an incomplete and unfair attribution. Those arguments are structurally excluded from the developer-liability frame.
% DISAPPEARANCE_RATIONALE: If the developer-primary liability regime vanished and no alternative regime replaced it, deployers would bear uninsured risk for integration failures, end-users would have no clear defendant, regulatory authorities would lose their primary enforcement lever, and liability would either diffuse across the value chain or collapse onto deployers. Insurance markets would reorganize, development practices would shift (possibly toward greater conservatism or retreat), and the coordination problem of accountability would be reopened.
% FOUNDING_PROBLEM: Early software-caused harms were unattributable: when a deployed system failed, it was unclear whether responsibility lay with the original developer's design, the deployer's integration choices, or the operational context. Multiple parties contributed to the harm; no party bore clear accountability. End-users were left without effective recourse.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies and consumer advocates attest that harms remain frequently unattributable and that developer-primary liability is necessary to force upstream accountability and care. Developers and deployers dispute this, citing numerous cases where deployer decisions (insufficient testing, misconfigurations, context mismatches) were the primary causal factors. Comparative legal analysis from jurisdictions with deployer-liability and shared-liability regimes (the European Union's shared-liability approach, some emerging AI governance frameworks) attests that the founding problem can be addressed under those regimes with different burden distributions.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__developer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__developer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__developer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval, indicating that the liability regime increasingly extracts from developers beyond the cost of genuine coordination. Suppression is high and rising (0.58→0.72), reflecting the growing enforcement machinery: regulatory mandates, tort doctrines, licensing requirements, and disclosure obligations that keep developers bound to the regime. Theater is moderate and rising (0.22→0.41): defensive documentation, risk management theater, and compliance performance grow as developers are pressured to manage or disclose all risks. The regime's original coordination function (clear attribution of accountability) is real but increasingly subordinated to extraction (cost-shifting to developers). Accessibility collapse is moderate (0.58): developers theoretically could exit by ceasing to develop, but the market structure constrains that exit (constrained exit_options assigned to software_developers). Resistance is moderate (0.64): developers mount real resistance through lobbying, liability caps, safe harbor arguments, and indemnification clauses, but institutional regulatory pressure overrides much of it.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (developers) and the agenda-setter seat (regulatory authorities) should compute to different classifications. From the developer position, this is enforced extraction riding on a real but now-attenuated coordination function; from the regulatory authority position, it is legitimate doctrine alignment with creator responsibility. The engine computes this gap from structural data: developers have constrained exit and bear the transfer; authorities have institutional power and set the rules. The measurement series documents how theater grows as compliance burdens mount, signaling the shift from coordination to extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers sit at d≈0.85 (near full target): they are liability bearers, insurance payers, and compliance cost-bearers with no substantial benefit from the regime. Deployers sit at d≈0.15 (beneficiary end): they externalize risk and retain decision control. End-users and regulatory authorities sit between d≈0.4-0.6 (partial beneficiaries with some costs: users are distant from accountability, authorities must maintain the enforcement apparatus). Liability insurers sit at d≈0.75 (near targets, though organized): they underwrite developer liability and face premium volatility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unattributable harm) remains contested—alternative liability regimes (deployer-primary, shared) claim to solve it equally well without centralizing burden on developers. The developer-liability doctrine persists not because it is uniquely effective at preventing harm, but because it is established in law and regulatory practice and because deployers benefit from externalizing risk. Mandatrophy is present in moderate form: the doctrine's original justification has been partly superseded by alternative causal theories (deployer decisions drive harm), yet the doctrine persists as institutional inertia and power distribution. The measurement of rising theater (compliance burdens without proportional safety gains) suggests the regime's function has decoupled from its founding justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_contribution_vs_role_assignment,
    'What is the actual causal contribution of developers versus deployers to software-caused harms? Does liability follow causal contribution, or does it follow institutional role (creator vs. deployer)?',
    'Post-incident forensic analysis: detailed case studies of software failures examining what fraction of causal weight rests on original design decisions versus deployment configuration and operation choices.',
    'High developer causal weight supports developer-liability framing; high deployer causal weight supports shared or deployer-primary liability. A finding of heterogeneous contribution (varies by incident type) would support shared-liability reading. Current regime assumes liability follows role (creator) rather than measured causation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_contribution_vs_role_assignment, empirical, 'Whether the developer-liability doctrine aligns with actual causal dynamics of software harms.').

omega_variable(
    extractiveness_vs_coordination_boundary,
    'How much of the measured extractiveness (0.68) represents necessary coordination cost (clear accountability, incentive for developer care) versus opportunistic cost-shifting (liability externalization without proportional safety gain)?',
    'Comparative analysis: jurisdictions with deployer-liability or shared-liability regimes—measure safety outcomes, incident attribution clarity, and developer compliance burden. If safety outcomes are equivalent or superior with lower developer burden, the excess burden in developer-liability regimes is extractive.',
    'Evidence of equivalent safety under alternative regimes would support reclassification toward snare; evidence of better safety outcomes would support the rope classification. Current author assessment (tangled_rope) assumes substantial extraction beyond coordination necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractiveness_vs_coordination_boundary, empirical, 'Boundary between coordination cost and extractive overhead in the developer-liability regime.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Do the developer-liability, deployer-liability, and shared-liability readings logically foreclose one another within a single legal framework, or can they coexist as competing doctrines held by different jurisdictions and parties?',
    'Legal-framework analysis: examine whether a single jurisdiction can adopt principles from multiple readings (e.g., developer-primary for some capability classes, deployer-primary for integration-failures), or whether the readings are fundamentally incompatible.',
    'If coexistent, the engine relation is coexists_with; if foreclosing, is forecloses. This determines the reading_relations field in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Logical compatibility of the liability readings in a single legal system.').

omega_variable(
    opacity_as_developer_burden,
    'Is developers'' inability to observe deployment context and actual use a feature developers must manage (disclose uncertainty, refuse high-risk contexts), or is it a structural given that liability regimes must accommodate?',
    'Examine developer disclosure and liability-cap practices under this reading: if developers are required to disclose unknowables and face liability for risks they cannot observe, suppression is higher and opacity becomes the burden. If the regime accommodates opacity (e.g., liability caps for emergent uses), opacity is treated as a shared constraint rather than a developer burden.',
    'High developer burden for opacity management supports high suppression (0.72 is current measure); regime accommodation of opacity would lower suppression. Measurement trajectory (0.58→0.72) suggests rising burden, indicating suppression is driven by opacity expectations, not just legal accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_as_developer_burden, empirical, 'Whether opacity obligation rests on developers or is distributed across the value chain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.22).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__developer_liability, theater_ratio, 5, 0.26).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__developer_liability, theater_ratio, 10, 0.31).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__developer_liability, theater_ratio, 15, 0.37).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__developer_liability, theater_ratio, 20, 0.39).
narrative_ontology:measurement(liab_tr_t25, liability_attribution__developer_liability, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(liab_be_t5, liability_attribution__developer_liability, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(liab_be_t10, liability_attribution__developer_liability, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(liab_be_t15, liability_attribution__developer_liability, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(liab_be_t20, liability_attribution__developer_liability, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(liab_be_t25, liability_attribution__developer_liability, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(liab_su_t5, liability_attribution__developer_liability, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(liab_su_t10, liability_attribution__developer_liability, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(liab_su_t15, liability_attribution__developer_liability, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(liab_su_t20, liability_attribution__developer_liability, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(liab_su_t25, liability_attribution__developer_liability, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__developer_liability, 0.12).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, software_capability_disclosure_standards).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_insurance_market_structure).

% DUAL FORMULATION NOTE:
% The liability_attribution kernel decomposes into three structurally distinct constraints: developer_liability (this story, high extraction, developers in victim set), deployer_liability (deployers in victim set, different ε), and shared_liability (distributed along value chain). Each reading instantiates a different constraint with different victim/beneficiary structures and ε values. The three constraints are linked via network.affects_constraints to indicate their kernel kinship; the reading_relations in cs_structure indicate the logical relationships (foreclosure, coexistence, influence) between the readings. A jurisdiction's shift from one reading to another reclassifies the constraint and redistributes liability burden, affecting dependent constraints (disclosure standards, insurance market pricing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__developer_liability, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
