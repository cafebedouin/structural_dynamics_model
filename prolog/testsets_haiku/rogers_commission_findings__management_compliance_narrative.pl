% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_management_compliance_narrative, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rogers_commission_findings__management_compliance_narrative
 *   human_readable: Rogers Commission Management Compliance Narrative Constraint
 *   domain: organizational_safety/regulatory_compliance/technology_governance
 *
 * SUMMARY:
 *   The Rogers Commission (1986) investigated the Challenger disaster and
 *   issued findings about the gap between known O-ring risk and the launch
 *   decision. This constraint story models ONE reading of those findings: the
 *   management-compliance-narrative reading holds that Rogers established a
 *   COMPLIANCE PROCESS requirement—management must demonstrate documented
 *   risk awareness and mitigation efforts sufficient to proceed, retaining
 *   launch authority. This reading competes with two sibling readings: the
 *   engineering-absolute-threshold reading (Rogers established a technical
 *   safety boundary; flight ceases until redesign certified) and the
 *   actuarial-risk-acceptance reading (Rogers established a risk
 *   quantification requirement; acceptable to fly if failure probability is
 *   documented and accepted). The three readings instantiate structurally
 *   distinct constraints with different ε values, beneficiary/victim
 *   structures, and time signatures. This JSON models the
 *   management-compliance-narrative reading only (Rule 1).
 *
 * KEY AGENTS:
 *   - space_program_management: institutional agenda-setter; retains launch authority through compliance-narrative framing; benefits from program continuity.
 *   - engineering_safety_authority: powerful payer; loses veto power; trapped by identity-lock and organizational dependency.
 *   - risk_assessment_engineers: moderate-power payers with identity-locked exit; conduct analysis consumed as compliance evidence rather than binding judgment.
 *   - astronaut_crews: organized beneficiary/payers; gain flight opportunities; carry undisclosed residual risk.
 *   - congress_appropriations_authority: institutional beneficiary; benefits from program continuity and schedule adherence.
 *   - external_safety_experts: excluded; contradictory analyses kept structurally absent from authority chain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.68).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.71).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.68).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Management Compliance Narrative Constraint").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/regulatory_compliance/technology_governance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '3d31c161-a53a-4688-9211-6c40f582c95e').
narrative_ontology:cs_kernel_codification('3d31c161-a53a-4688-9211-6c40f582c95e', formalized).
narrative_ontology:cs_authority_grounding('3d31c161-a53a-4688-9211-6c40f582c95e', extraction).
narrative_ontology:cs_interpretation_layer_present('3d31c161-a53a-4688-9211-6c40f582c95e').
narrative_ontology:cs_reading_relation('3d31c161-a53a-4688-9211-6c40f582c95e', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('3d31c161-a53a-4688-9211-6c40f582c95e', rogers_commission_findings__actuarial_risk_acceptance, influences).
narrative_ontology:cs_axiom('3d31c161-a53a-4688-9211-6c40f582c95e', foundational, documented_awareness_satisfies_safety_obligation).
narrative_ontology:cs_axiom_status(documented_awareness_satisfies_safety_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3d31c161-a53a-4688-9211-6c40f582c95e', documented_awareness_satisfies_safety_obligation, conventional).
narrative_ontology:cs_axiom('3d31c161-a53a-4688-9211-6c40f582c95e', foundational, management_retains_launch_authority_post_documentation).
narrative_ontology:cs_axiom_status(management_retains_launch_authority_post_documentation, holdable).
narrative_ontology:cs_axiom_grounding('3d31c161-a53a-4688-9211-6c40f582c95e', management_retains_launch_authority_post_documentation, instrumental).
narrative_ontology:cs_reference_frame('3d31c161-a53a-4688-9211-6c40f582c95e', rogers_commission_mandate_for_risk_documentation).
narrative_ontology:cs_drift_state('3d31c161-a53a-4688-9211-6c40f582c95e', post_columbia_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3d31c161-a53a-4688-9211-6c40f582c95e', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, space_program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, mission_continuity_stakeholders).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_safety_authority).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, technical_risk_accountability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, astronaut_crews).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, congress_appropriations_authority).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, risk_assessment_engineers).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, astronaut_crews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets launch authorization standards after Rogers findings; retains decision authority by framing compliance as documented risk awareness and mitigation effort rather than absolute technical safety. Justifies the standard as balancing safety against program viability. Benefits from constraint by preserving launch authority and maintaining schedule momentum.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, space_program_management, agenda_setter,
    institutional, generational, arbitrage, national).

% Loses formal veto power over launch decisions; technical safety concerns can be documented and mitigated narratively without blocking flight. Must operate within the compliance-process framework even when engineering judgment assesses residual risk as unacceptable. Trapped between professional obligation to safety and institutional pressure to demonstrate compliance.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_safety_authority, payer,
    powerful, biographical, constrained, national).

% Conduct risk analysis and document mitigation efforts; their work is consumed as evidence of compliance rather than as binding safety judgment. Professional identity fused with the organization's mission creates identity-lock that persists even when they assess risk as inadequately mitigated. Departure from the program means loss of technical voice.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, risk_assessment_engineers, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__management_compliance_narrative, risk_assessment_engineers, observer).

% Gain opportunity to fly missions under the documented-compliance framework; also carry undisclosed residual risk if engineering mitigation efforts prove insufficient. Choice to fly or not to fly is theoretically available but practically constrained by career incentives and mission assignment structures. Receive public trust framing that their flights represent acceptable risk.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, astronaut_crews, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__management_compliance_narrative, astronaut_crews, payer).

% Review launch authorizations and compliance documentation. Can audit the process but operate within the Rogers Commission's reading that documented risk awareness plus mitigation narrative satisfies the safety mandate. Positioned to assess process compliance rather than technical adequacy.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, regulatory_oversight_bodies, observer,
    institutional, generational, analytical, national).

% Benefits from program continuity and mission success; funding approval rides on demonstrated launch capability and schedule adherence. Has authority to mandate different safety standards but benefits from compliance-narrative framing that permits flight without technical redesign delays.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, congress_appropriations_authority, beneficiary,
    institutional, generational, arbitrage, national).

% Independent safety assessments and contradictory risk analyses are structurally absent from the authority chain. Their expertise is not incorporated into launch authorization; inclusion would require challenging the compliance-narrative framework itself.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, external_safety_experts, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__management_compliance_narrative, space_program_management).
narrative_ontology:fixing_cost_class(rogers_commission_findings__management_compliance_narrative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a transparent, documented process for risk awareness and mitigation that allows stakeholders to understand the safety rationale for launch decisions and creates accountability audit trails. Solves the coordination problem of synchronizing technical risk analysis with institutional decision-making authority after a catastrophic failure revealed process gaps.
% TRANSFER_FUNCTION: Transfers launch authorization authority from engineering safety veto to management-controlled compliance documentation. Moves the locus of safety judgment from technical assessment to institutional narrative sufficiency. Transfers credibility from 'design is safe' to 'we understand and have mitigated documented risks'.
% ABSENT_VOICES: External independent safety experts, engineering dissent from within the organization (identity-locked position makes external voicing costly), astronaut safety advocacy (crews benefit from the compliance narrative and have constrained exit), and international space program safety standards (national program sovereignty dominates). These voices would argue for absolute technical thresholds rather than compliance narratives.
% DISAPPEARANCE_RATIONALE: If the management-compliance-narrative constraint disappeared, launch authority would revert to engineering veto over documented safety concerns; the program would face extended grounding until technical redesign completed and engineering could certify safety margins acceptable. The institutional structure of mission continuity and schedule momentum would reorganize around technical safety certification rather than documented-risk-narrative sufficiency.
% FOUNDING_PROBLEM: The Rogers Commission found that the 1986 Challenger disaster resulted not from unknown risk but from known O-ring failure risk that was not adequately communicated through institutional channels to senior decision-makers. Fragmented risk awareness and unclear accountability for safety judgment enabled a launch decision despite technical objections.
% FOUNDING_PROBLEM_CORROBORATION: Rogers Commission official findings and technical testimony establish the communication and accountability gap. Space program management argues the compliance-narrative process solves this gap by mandating documented risk awareness. Engineering safety advocates and subsequent independent reviews (including later accident investigations) attest the problem persists: documented awareness does not ensure risk is adequately mitigated, and the compliance narrative can become performative cover for launches that engineering assesses as unsafe.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__management_compliance_narrative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__management_compliance_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because the constraint transfers launch authority from engineering veto to management narrative sufficiency—a structural win for management's ability to fly despite engineering concerns. Suppression is similarly high (0.71) because engineering dissent must be documented and integrated into the compliance narrative rather than blocking flight; the suppression machinery keeps alternative safety framings (absolute thresholds, independent review) out of the authority chain. Theater rises sharply (0.38→0.59 over 15 time points, then stabilizes) because initially the constraint performs genuine coordination (documenting risk awareness, creating accountability), but over time the documented-mitigation narrative becomes performative—the ritual of compliance substitutes for the substance of risk reduction. The measurement series spans 25 time units (representing roughly the period 1986-2011, from Rogers through Columbia) with 7 time points on one shared grid. At t=10-15 (mid-period), theater ratio reaches its maximum, indicating the constraint has shifted from functional process to procedural theater. By t=20-25 (Columbia-era and beyond), theater stabilizes as the compliance narrative becomes institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (space_program_management) experiences this constraint as enabling rational risk management—documented awareness plus mitigation efforts satisfy safety obligation while preserving schedule. The payer seats (engineering_safety_authority, risk_assessment_engineers) experience it as suppression of their professional judgment—their risk assessments are documentation requirements rather than authorization requirements. The identity-locked engineers face a particularly asymmetric relationship: their expertise is extracted as compliance evidence while their ability to block unsafe flights is suppressed. Astronaut crews sit in a performative beneficiary role: they gain flight opportunities, but the compliance narrative masks the degree to which residual risk has been mitigated versus merely documented. The engine should compute these seats as radically divergent types: management sees coordination with authority retained; engineers see extraction with veto suppressed; crews see ambiguous benefit with hidden risk.
 *
 * DIRECTIONALITY LOGIC:
 *   Space program management (institutional, arbitrage exit, agenda-setter role) sits near d=0.2 (beneficiary end): the constraint is authored and administered by them, they retain authority, they benefit from program continuity. Engineering safety authority (powerful, constrained exit, payer role) sits near d=0.82 (target end): they lose formal veto, they must operate within the compliance-narrative framework even when their professional judgment objects, their authority is suppressed. Risk assessment engineers occupy d≈0.75 (target-leaning) despite moderate power because their identity-lock prevents real exit; they can leave the program but cannot exit the space program ecosystem meaningfully. Astronaut crews sit near d≈0.55 (symmetric): genuine coordination benefit (opportunity to fly) balanced against indirect cost (carry undisclosed residual risk). Congress sits near d≈0.15 (beneficiary): benefits from program continuity, has authority to mandate different standards but benefits from compliance framing. External experts are excluded (not a stakeholder seat in this reading).
 *
 * MANDATROPHY ANALYSIS:
 *   The Rogers Commission's founding problem was a communication and accountability gap—risk awareness and institutional decision-making were decoupled. The management-compliance-narrative reading claims to solve this by mandating documented risk awareness and mitigation efforts in the authority chain. However, documented awareness does not ensure risk is adequately mitigated; it can become a procedural substitute for substantive risk reduction. The measured theater_ratio increase (0.38→0.59→0.58) indicates the constraint has drifted from functional communication mechanism (early period) toward performative documentation (later period). By the Columbia era (t≈20), the constraint persists largely as procedural theater: compliance documentation is authored and maintained, but engineering concerns remain structurally suppressed. The founding problem (communication gap) may be technically solved (documentation now occurs), but the root problem (management retains authority to launch despite engineering objection) remains. This is mandatrophy in the strict sense: the founding problem's solution is in place, but the constraint persists because it serves management's interest in launch authority retention, independent of whether documented risk mitigation is substantive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentation_substantiveness,
    'Does documented risk awareness and mitigation effort actually reduce residual risk, or does the compliance narrative substitute for substantive risk reduction?',
    'Post-flight analysis and failure investigation: if failures occur in systems with documented mitigation efforts that were assessed as implemented, documentation was performative; if failures occur in systems where documented mitigation was not implemented, documentation was accurate but overseen.',
    'If performative, the constraint is a pure snare (extraction of veto with nominal risk reduction). If substantive, the constraint is genuinely tangled rope (coordination value in documented risk awareness balanced against extraction of engineering authority). Columbia debris analysis (2003) provides evidence: O-ring mitigation was documented but not fundamentally redesigned; insulation foam impact occurred despite documented awareness and risk management.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(documentation_substantiveness, empirical, 'Whether compliance documentation correlates with substantive risk reduction or substitutes for it.').

omega_variable(
    engineering_veto_suppression_mechanism,
    'Is the measured suppression of engineering veto a structural requirement of the compliance process, or an institutional choice by management to interpret the process as non-blocking?',
    'Comparative analysis of other organizational safety contexts (aviation, nuclear power, medical device) where similar documented-risk processes exist: do they suppress technical veto or preserve it? Examine Rogers Commission language and subsequent policy documents for explicit vs. implicit veto suppression.',
    'If structural to compliance processes, suppression is an inherent cost of the coordination mechanism (true tangled rope). If institutional choice, the constraint is more purely extractive (snare-leaning) because an alternative (documented-risk-plus-veto retention) was feasible but rejected. Evidence suggests institutional choice: Rogers did not mandate veto suppression; NASA management chose the non-blocking interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_veto_suppression_mechanism, conceptual, 'Whether engineering veto suppression is inherent to the compliance mechanism or a management choice.').

omega_variable(
    identity_lock_persistence,
    'Do risk-assessment engineers remain trapped in the space program even after the constraint suppresses their veto power, or do they exit the program at rates consistent with other technical careers?',
    'Career trajectory analysis of risk-assessment engineers post-Rogers: retention rates, lateral moves within aerospace vs. exit to other industries, testimony about decision factors. Compare to engineering departures in periods when their veto power was more effective.',
    'High retention despite veto suppression indicates strong identity-lock (professional identity fused with space program participation). Exit rates equal to other technical careers suggest the constraint''s suppression is institutional rather than identity-fused. Identity-lock deepens the effective suppression and makes the constraint more extractive from that seat''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether identity-lock reinforces suppression of engineering veto beyond structural institutional constraints.').

omega_variable(
    reading_kernel_relationship,
    'Is the management-compliance-narrative reading a faithful interpretation of the Rogers Commission''s actual findings, or an institutional choice by NASA management to frame the findings in compliance-process terms?',
    'Textual analysis of Rogers Commission findings and recommendations vs. NASA post-Rogers policy documents; testimony from Commission members and NASA leadership about interpretation intent; comparison to how other space agencies and countries implemented comparable safety frameworks post-Challenger.',
    'If faithful interpretation, the reading represents Rogers'' intent (the kernel genuinely instantiates this reading). If institutional choice, the reading is an appropriation of the kernel''s authority for management''s preferred policy (false-summit risk: management uses Rogers language to justify non-blocking compliance narrative when Rogers may have intended technical safety thresholds). This determines whether the constraint''s legitimacy rests on genuine Rogers findings or on institutional narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Whether the management-compliance-narrative reading authentically instantiates the Rogers Commission kernel or appropriates it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rcn_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(rcn_tr_t0, observed).
narrative_ontology:measurement(rcn_tr_t3, rogers_commission_findings__management_compliance_narrative, theater_ratio, 3, 0.44).
narrative_ontology:measurement_basis(rcn_tr_t3, observed).
narrative_ontology:measurement(rcn_tr_t6, rogers_commission_findings__management_compliance_narrative, theater_ratio, 6, 0.5).
narrative_ontology:measurement_basis(rcn_tr_t6, observed).
narrative_ontology:measurement(rcn_tr_t10, rogers_commission_findings__management_compliance_narrative, theater_ratio, 10, 0.56).
narrative_ontology:measurement_basis(rcn_tr_t10, observed).
narrative_ontology:measurement(rcn_tr_t15, rogers_commission_findings__management_compliance_narrative, theater_ratio, 15, 0.59).
narrative_ontology:measurement_basis(rcn_tr_t15, observed).
narrative_ontology:measurement(rcn_tr_t20, rogers_commission_findings__management_compliance_narrative, theater_ratio, 20, 0.58).
narrative_ontology:measurement_basis(rcn_tr_t20, observed).
narrative_ontology:measurement(rcn_tr_t25, rogers_commission_findings__management_compliance_narrative, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(rcn_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(rcn_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(rcn_be_t0, observed).
narrative_ontology:measurement(rcn_be_t3, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 3, 0.54).
narrative_ontology:measurement_basis(rcn_be_t3, observed).
narrative_ontology:measurement(rcn_be_t6, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 6, 0.61).
narrative_ontology:measurement_basis(rcn_be_t6, observed).
narrative_ontology:measurement(rcn_be_t10, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(rcn_be_t10, observed).
narrative_ontology:measurement(rcn_be_t15, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(rcn_be_t15, observed).
narrative_ontology:measurement(rcn_be_t20, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(rcn_be_t20, observed).
narrative_ontology:measurement(rcn_be_t25, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(rcn_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(rcn_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(rcn_su_t0, observed).
narrative_ontology:measurement(rcn_su_t3, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 3, 0.59).
narrative_ontology:measurement_basis(rcn_su_t3, observed).
narrative_ontology:measurement(rcn_su_t6, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 6, 0.66).
narrative_ontology:measurement_basis(rcn_su_t6, observed).
narrative_ontology:measurement(rcn_su_t10, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(rcn_su_t10, observed).
narrative_ontology:measurement(rcn_su_t15, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(rcn_su_t15, observed).
narrative_ontology:measurement(rcn_su_t20, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(rcn_su_t20, observed).
narrative_ontology:measurement(rcn_su_t25, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(rcn_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__management_compliance_narrative, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% The rogers_commission_findings kernel instantiates three structurally distinct constraints via three readings. management_compliance_narrative (this story) establishes launch authority via documented risk narrative; engineering_absolute_threshold establishes launch prohibition via technical redesign requirement; actuarial_risk_acceptance establishes informed-acceptance requirement for quantified risk. The three readings coexist as competing institutional interpretations of the same kernel. All three stories must link via network.affects_constraints to document the kernel family. The management_compliance_narrative reading influences both siblings by establishing the documented-narrative framework within which they operate; it does not foreclose either (coexists_with relation). Decomposition driven by ε-invariance: the three readings have structurally distinct ε values (management-compliance is extraction of veto; engineering-absolute is extraction of schedule; actuarial is balanced coordination with risk transparency), different beneficiary/victim structures, and different time signatures in institutional practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__management_compliance_narrative, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
