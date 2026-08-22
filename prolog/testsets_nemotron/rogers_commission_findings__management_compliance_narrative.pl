% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__management_compliance_narrative, []).

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
 *   constraint_id: rogers_commission_findings__management_compliance_narrative
 *   human_readable: Rogers Commission Management Compliance Narrative
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   The Rogers Commission findings on the Challenger disaster are interpreted
 *   by NASA management and contractor leadership as establishing a compliance
 *   process: demonstrate documented risk awareness and mitigation efforts
 *   sufficient to proceed with launch. This reading preserves management's
 *   launch authority while requiring procedural documentation. The constraint
 *   coordinates continued program operation (beneficiary: program continuity)
 *   while extracting the engineering veto power that previously could stop a
 *   launch on technical grounds alone (victims: engineering safety offices,
 *   frontline engineers, astronaut crew). The compliance narrative creates a
 *   tangible coordination function — a shared process for risk documentation
 *   — but simultaneously displaces the harder technical boundary that the
 *   engineering_absolute_threshold reading would impose.
 *
 * KEY AGENTS:
 *   - nasa_program_management: Primary beneficiary (institutional/arbitrage) — retains launch authority, gains procedural legitimacy
 *   - contractor_executive_leadership: Secondary beneficiary (powerful/arbitrage) — protects contract continuity, gains predictable process
 *   - engineering_safety_offices: Primary victim (organized/constrained) — loses veto power, gains documentation burden
 *   - frontline_engineers: Primary victim (moderate/identity_locked) — professional identity fused to safety advocacy, exit means career abandonment
 *   - astronaut_crew: Victim (organized/trapped) — bears residual risk, no launch decision authority
 *   - external_oversight_bodies: Observer (institutional/analytical) — monitors compliance adequacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.58).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.62).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.58).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Management Compliance Narrative").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, 'e0135176-b0e3-4a91-95a9-093ae979a0e8').
narrative_ontology:cs_kernel_codification('e0135176-b0e3-4a91-95a9-093ae979a0e8', formalized).
narrative_ontology:cs_authority_grounding('e0135176-b0e3-4a91-95a9-093ae979a0e8', extraction).
narrative_ontology:cs_interpretation_layer_present('e0135176-b0e3-4a91-95a9-093ae979a0e8').
narrative_ontology:cs_reading_relation('e0135176-b0e3-4a91-95a9-093ae979a0e8', rogers_commission_findings__engineering_absolute_threshold, influences).
narrative_ontology:cs_reading_relation('e0135176-b0e3-4a91-95a9-093ae979a0e8', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('e0135176-b0e3-4a91-95a9-093ae979a0e8', foundational, documented_process_legitimizes_proceeding).
narrative_ontology:cs_axiom_status(documented_process_legitimizes_proceeding, holdable).
narrative_ontology:cs_axiom_grounding('e0135176-b0e3-4a91-95a9-093ae979a0e8', documented_process_legitimizes_proceeding, conventional).
narrative_ontology:cs_axiom('e0135176-b0e3-4a91-95a9-093ae979a0e8', foundational, management_authority_over_technical_risk).
narrative_ontology:cs_axiom_status(management_authority_over_technical_risk, holdable).
narrative_ontology:cs_axiom_grounding('e0135176-b0e3-4a91-95a9-093ae979a0e8', management_authority_over_technical_risk, conventional).
narrative_ontology:cs_reference_frame('e0135176-b0e3-4a91-95a9-093ae979a0e8', post_challenger_reform_mandate).
narrative_ontology:cs_drift_state('e0135176-b0e3-4a91-95a9-093ae979a0e8', post_columbia_caib, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e0135176-b0e3-4a91-95a9-093ae979a0e8', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, nasa_program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, contractor_executive_leadership).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_safety_offices).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, frontline_engineers).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, astronaut_crew).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, management_authority_over_technical_risk).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, documented_process_legitimizes_proceeding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the Flight Readiness Review process and launch authorization. Uses the compliance narrative to retain decision authority while satisfying oversight requirements. Collects program continuity and schedule predictability as benefits. Can shift risk definitions and documentation standards to maintain operational tempo.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, nasa_program_management, agenda_setter,
    institutional, generational, arbitrage, national).

% Manages shuttle contracts and production schedules. Benefits from predictable launch cadence and clearly defined compliance requirements that protect against contract disruption. Has sufficient political access to influence the compliance process design. Extracts contract stability from the constraint.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, contractor_executive_leadership, beneficiary,
    powerful, biographical, arbitrage, national).

% Formally responsible for safety assessment but stripped of veto authority by the compliance narrative. Must produce risk documentation that fits the management process template. Professional credibility and institutional position depend on participating in the process they cannot control. Exit means leaving NASA or accepting marginalization.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_safety_offices, payer,
    organized, biographical, constrained, national).

% Technical experts who identify anomalies (e.g., O-ring erosion, foam strikes). Their professional identity is fused to safety advocacy — raising concerns is not just their job but their self-concept. The compliance process channels their dissent into documentation that management can accept or override. Exit means abandoning their professional identity; they are structurally trapped in the role of Cassandra.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, frontline_engineers, payer,
    moderate, biographical, identity_locked, local).

% Bear the physical risk of launch decisions made under the compliance narrative. Have no formal role in the Flight Readiness Review and no authority to veto a launch. Their only leverage is public refusal to fly, which ends their career. The constraint extracts their risk acceptance without their consent.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, astronaut_crew, payer,
    organized, immediate, trapped, local).

% Congressional committees, GAO, ASAP, and other oversight entities. Monitor whether the compliance process is substantively improving safety or merely performing compliance. Can impose reforms but lack day-to-day operational authority. Their assessments shape the legitimacy of the constraint over generational time.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, external_oversight_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, documented process for risk assessment and launch authorization that allows the shuttle program to continue operating under public and congressional scrutiny. Solves the coordination problem of 'how to proceed after Challenger without admitting management failure' by creating a procedural framework that all parties must engage with.
% TRANSFER_FUNCTION: Moves decision authority from engineering technical judgment to management procedural compliance. Transfers the burden of proof from 'prove it's safe to fly' to 'document that risks are understood and mitigated.' The extraction is engineering veto power; the gain is program continuity and management authority.
% ABSENT_VOICES: The families of the Challenger and Columbia crews — they would object to a process that legitimizes proceeding with known residual risks. The public, who funds the program but has no voice in the risk acceptance calculus. Future astronauts who will fly under the same compliance regime. These voices are structurally excluded from the Flight Readiness Review.
% DISAPPEARANCE_RATIONALE: If the compliance narrative vanished overnight, NASA would revert to either the engineering_absolute_threshold (stand down until technical certification) or the actuarial_risk_acceptance (explicit risk acceptance by named decision-makers). The shuttle program's operational tempo would be disrupted; management would lose its procedural shield; engineering safety offices would regain de facto veto power. The world rearranges.
% FOUNDING_PROBLEM: After Challenger, NASA needed to restore public and congressional trust while maintaining the shuttle program's launch schedule. The Rogers Commission identified organizational silence and management pressure as root causes. The management_compliance_narrative reading solved this by creating a documentation process that demonstrated responsiveness without ceding launch authority to engineers.
% FOUNDING_PROBLEM_CORROBORATION: NASA management and contractor leadership attest the founding problem (organizational silence) is still live and the compliance process addresses it. The Rogers Commission report itself, the Columbia Accident Investigation Board (CAIB), and independent safety experts (e.g., Diane Vaughan's normalization of deviance analysis) attest the founding problem was only partially solved and the compliance narrative became a new form of organizational silence. CAIB explicitly found the same cultural causes persisted 17 years later.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.58) reflects the displacement of engineering authority by procedural compliance — the cost of the constraint falls on those who must document rather than decide. Suppression (0.62) captures the active enforcement of the compliance frame: engineering objections that don't fit the documentation template are marginalized. Theater ratio (0.48) is high because the compliance process increasingly serves as a ritual that permits proceeding while the substantive safety improvements lag. Accessibility collapse (0.55) is moderate: alternatives (redesign, stand-down) exist but are procedurally difficult to activate. Resistance (0.42) reflects engineering pushback that is real but structurally contained within the compliance framework.
 *
 * PERSPECTIVAL GAP:
 *   From the management seat, this is genuine coordination: a shared process that allows the program to proceed responsibly. From the engineering seat, it is extraction: their technical authority is replaced by a documentation burden they cannot refuse. From the astronaut seat, it is a snare: they bear the risk of a process that legitimizes proceeding without their consent. The engine computes these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Management and contractor leadership are beneficiaries (d ~ 0.15-0.25): they collect program continuity and schedule predictability. Engineering safety offices are targets (d ~ 0.75): they bear the documentation burden and lose veto power. Frontline engineers are identity-locked targets (d ~ 0.85): professional identity makes exit nearly impossible. Astronaut crew are trapped targets (d ~ 0.9): they cannot exit the risk. External oversight are analytical observers (d ~ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Challenger's organizational silence) was real but the compliance narrative solved it by legitimizing management authority rather than empowering engineering dissent. The mandate has partially atrophied: the compliance process persists but the safety culture it claimed to create shows recurring erosion (Columbia, Artemis schedule pressure). The constraint is a tangled_rope because it retains a coordination function (shared risk documentation) while extracting engineering authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_vs_safety_boundary,
    'Does the compliance narrative structurally require genuine safety improvement, or does it only require performative documentation that permits proceeding regardless of residual risk?',
    'Longitudinal analysis of post-Rogers launch decisions: correlate compliance documentation completeness with actual risk reduction vs. schedule adherence.',
    'If only performative, the constraint is a snare masquerading as coordination; if genuinely safety-improving, the extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_vs_safety_boundary, empirical, 'Whether the compliance process is substantively safety-producing or procedurally extractive').

omega_variable(
    kernel_reading_identity,
    'Is the management_compliance_narrative reading a distinct constraint from the engineering_absolute_threshold and actuarial_risk_acceptance readings of the Rogers findings, or do they represent different enforcement intensities of the same constraint?',
    'Counterfactual launch decision tracing: for each post-Challenger launch, identify which reading''s criteria were actually dispositive in the go/no-go decision.',
    'If distinct, three separate constraints with different ε and beneficiary/victim structures; if unified, a single constraint with observer-dependent classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Constraint identity across the Rogers findings kernel readings').

omega_variable(
    engineering_veto_displacement,
    'Did the compliance narrative structurally displace the pre-existing engineering veto power, or did it merely formalize a pre-existing management prerogative?',
    'Institutional archaeology of pre-Challenger Flight Readiness Review process: compare formal authority vs. de facto veto patterns.',
    'If displacement occurred, the constraint is extractive against engineering authority; if formalization, the extraction predates the Rogers findings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engineering_veto_displacement, empirical, 'Historical baseline of engineering authority before the compliance narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.25).
narrative_ontology:measurement(roge_tr_t5, rogers_commission_findings__management_compliance_narrative, theater_ratio, 5, 0.32).
narrative_ontology:measurement(roge_tr_t10, rogers_commission_findings__management_compliance_narrative, theater_ratio, 10, 0.38).
narrative_ontology:measurement(roge_tr_t15, rogers_commission_findings__management_compliance_narrative, theater_ratio, 15, 0.42).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__management_compliance_narrative, theater_ratio, 20, 0.45).
narrative_ontology:measurement(roge_tr_t25, rogers_commission_findings__management_compliance_narrative, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(roge_be_t5, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(roge_be_t10, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(roge_be_t15, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(roge_be_t25, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(roge_su_t5, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(roge_su_t10, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(roge_su_t15, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(roge_su_t25, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__management_compliance_narrative, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__actuarial_risk_acceptance).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, nasa_flight_readiness_review_process).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, contractor_flight_rationale_process).

% DUAL FORMULATION NOTE:
% The Rogers findings kernel decomposes into three constraint stories with distinct ε and beneficiary/victim structures. This story (management_compliance_narrative) has moderate extractiveness (0.58) with management as beneficiary and engineering as victim. The engineering_absolute_threshold story has low extractiveness (~0.15) with crew safety as beneficiary and schedule as victim. The actuarial_risk_acceptance story has intermediate extractiveness (~0.35) with decision-makers as beneficiaries and excluded stakeholders as victims. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__management_compliance_narrative, organized, 0.75).
constraint_indexing:directionality_override(rogers_commission_findings__management_compliance_narrative, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
