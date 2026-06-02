% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Rogers Commission Findings: Management Compliance Narrative
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   The Rogers Commission, convened after the Space Shuttle Challenger
 *   disaster in 1986, established that a systematic process for documenting
 *   risk awareness and mitigation efforts must be implemented before
 *   launching complex systems. This constraint — the 'management compliance
 *   narrative' reading — interprets the Rogers findings as requiring
 *   demonstrated documentation of risk mitigation sufficient to justify
 *   proceeding despite known residual risks. The reading treats this as a
 *   legitimate governance mechanism: management retains launch authority
 *   provided it documents risk awareness and mitigation efforts meeting
 *   regulatory standards. This is ONE of three distinct structural readings
 *   of the Rogers kernel. An alternative reading
 *   ('engineering_absolute_threshold') interprets Rogers as strengthening the
 *   engineering veto: no launch without engineering sign-off on sufficiency.
 *   A third reading ('actuarial_risk_acceptance') treats Rogers as
 *   establishing explicit risk acceptance frameworks. These readings coexist
 *   in organizational practice and produce materially different constraint
 *   structures. The management_compliance_narrative reading is the dominant
 *   institutional implementation: it permits launch despite engineering
 *   reservations if documentation is adequate. The extractiveness value
 *   (0.48) reflects that the constraint does coordinate genuine risk
 *   assessment but converts engineering veto power into a consultative
 *   function whose output is absorbed into management decision-making.
 *   Theater ratio (0.58) shows moderate performativity: documentation happens
 *   systematically, but the functional gate (whether launches are actually
 *   halted) has weakened over time.
 *
 * KEY AGENTS:
 *   - Program Management: Primary beneficiary (institutional/arbitrage) — retains launch authority after compliance documentation; can arbitrage to alternative risk frameworks
 *   - Engineering Safety Authority: Primary victim (powerless/trapped) — dissent is absorbed into compliance documentation without stopping the launch; no exit from the authority structure
 *   - Safety Engineers: Secondary victim (moderate/constrained) — must participate in risk assessment and documentation but face career risk if they escalate beyond the compliance process
 *   - Regulatory Authority (FAA, NASA Oversight): Organized participant (organized/constrained) — enforces the constraint but defers to management's sufficiency determination; constrained by having no unilateral intervention trigger post-compliance
 *   - The Compliance Documentation System: Institutional actor (institutional/arbitrage) — maintains the performative ritual of risk assessment; benefits from treating documentation-complete as sufficient-for-launch
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the institutional choice (management veto authority) as an immutable feature of complex-system safety
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.48).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.62).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.48).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Findings: Management Compliance Narrative").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '019dd554-1223-4e94-a521-f78dc5b66cc4').
narrative_ontology:cs_kernel_codification('019dd554-1223-4e94-a521-f78dc5b66cc4', formalized).
narrative_ontology:cs_authority_grounding('019dd554-1223-4e94-a521-f78dc5b66cc4', extraction).
narrative_ontology:cs_interpretation_layer_present('019dd554-1223-4e94-a521-f78dc5b66cc4').
narrative_ontology:cs_reading_relation('019dd554-1223-4e94-a521-f78dc5b66cc4', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('019dd554-1223-4e94-a521-f78dc5b66cc4', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('019dd554-1223-4e94-a521-f78dc5b66cc4', foundational, management_risk_determination_authority).
narrative_ontology:cs_axiom_status(management_risk_determination_authority, holdable).
narrative_ontology:cs_axiom_grounding('019dd554-1223-4e94-a521-f78dc5b66cc4', management_risk_determination_authority, conventional).
narrative_ontology:cs_axiom('019dd554-1223-4e94-a521-f78dc5b66cc4', foundational, documented_awareness_sufficient_for_launch).
narrative_ontology:cs_axiom_status(documented_awareness_sufficient_for_launch, holdable).
narrative_ontology:cs_axiom_grounding('019dd554-1223-4e94-a521-f78dc5b66cc4', documented_awareness_sufficient_for_launch, instrumental).
narrative_ontology:cs_reference_frame('019dd554-1223-4e94-a521-f78dc5b66cc4', management_directed_risk_governance).
narrative_ontology:cs_drift_state('019dd554-1223-4e94-a521-f78dc5b66cc4', contemporary_post_columbia_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('019dd554-1223-4e94-a521-f78dc5b66cc4', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, mission_continuity).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_veto_authority).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, structural_failure_prevention).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENGINEERING VETO AUTHORITY (SNARE) — Engineers who raised safety concerns pre-launch face structural extraction: their objections are absorbed into 'documented risk awareness' without stopping the launch. The constraint permits launch despite known risks, converting engineer dissent into a compliance checkbox. No exit from the authority structure that overrode them. Maximum extraction experienced.
constraint_indexing:constraint_classification(rogers_commission_findings__management_compliance_narrative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAFETY ENGINEER (TANGLED ROPE) — Participates in risk mitigation planning and can document efforts, but faces career risk if escalation halts the program. Coordination function exists: the constraint does require genuine risk assessment and mitigation planning. But extraction flows upward: the engineer's work becomes justification for a decision already made. Constrained by career dependence on program success.
constraint_indexing:constraint_classification(rogers_commission_findings__management_compliance_narrative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROGRAM MANAGEMENT (ROPE) — Experiences the constraint as enabling: documented risk awareness permits launch without engineering veto. The constraint coordinates legitimate mission continuity with safety oversight — or appears to. Net beneficiary. Can arbitrage to alternative risk frameworks or launch schedules.
constraint_indexing:constraint_classification(rogers_commission_findings__management_compliance_narrative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Organized actors (FAA, NASA oversight, regulatory bodies) must enforce the constraint. They coordinate genuine safety requirements with program viability. But the constraint's structure — 'sufficient' risk awareness permits launch — gives management interpretation authority. The regulator is constrained by having no unilateral veto after the 'compliance' ritual.
constraint_indexing:constraint_classification(rogers_commission_findings__management_compliance_narrative, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPLIANCE THEATER (PITON) — The documentary requirement ('demonstrate risk awareness') has become largely performative. Post-Rogers, the rituals of risk assessment and mitigation documentation persist, but the functional gate — whether engineers can stop a launch — has eroded. The theater persists through institutional inertia and legitimacy claims, not because it prevents failures. Theater ratio (0.58) reflects this degradation: the documentation happens but risk vetoes do not.
constraint_indexing:constraint_classification(rogers_commission_findings__management_compliance_narrative, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational perspective, the constraint appears to instantiate an immutable principle: all complex systems operate under some residual risk; perfect safety is impossible; therefore, documented risk acceptance is a natural law of engineering. However, the structural data reveals this as a false summit: the constraint naturalizes a specific institutional choice (management authority to override engineering veto) as an inherent limit.
constraint_indexing:constraint_classification(rogers_commission_findings__management_compliance_narrative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__management_compliance_narrative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rogers_commission_findings__management_compliance_narrative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rogers_commission_findings__management_compliance_narrative, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, TR),
    TR >= 0.70.

:- end_tests(rogers_commission_findings__management_compliance_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts from engineering authority (they lose veto power) and from the epistemic commons (false assurance that documented risks are acceptable). The extraction is real but not total — risk assessment and mitigation planning are genuine coordination functions, not pure extraction theater. The value reflects that legitimate coordination exists alongside extraction. The upward trajectory in measurements (0.35 → 0.48) shows the extractiveness growing as the constraint becomes institutionalized and the documentary compliance ritual strengthens while the veto gate weakens. Suppression (0.62): Moderate-high. Significant barriers exist to engineering escalation: career risk (engineers who refuse to sign off face marginalization), structural (management has final authority by design), and epistemic (once risk has been 'documented,' further objection appears irrational). But suppression is not total — some engineers do escalate and trigger re-evaluation. Theater ratio (0.58): Moderate. The compliance documentation process is substantially performative: risk assessments happen, mitigation plans are written, sign-offs are obtained. But the documents often describe risks that are known to be non-mitigatable; the 'mitigation' is often 'proceed with awareness.' The theater persists because it satisfies regulatory requirements and provides legitimate cover for launch decisions already made. Rising trajectory (0.42 → 0.58) shows increasing formalization of the ritual without corresponding increase in actual veto authority.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a profound perspectival gap between its institutional beneficiaries and the agents who bear its costs. Program management sees the constraint as coordinating legitimate mission continuity with safety oversight — a governance mechanism that is working as intended. Engineering safety authority sees the constraint as stripping veto power and converting dissent into a compliance checkbox. The regulatory authority is positioned between these perspectives: it must satisfy both the management's need for launch authority and the engineering demand for serious risk gates. This gap reveals the constraint's true structure: it is not coordination (all parties benefiting), nor is it simple extraction (one party gaining at others' expense). It is tangled_rope — genuine coordination functions coexist with asymmetric extraction. The management_compliance_narrative reading instantiates the institutional/beneficiary interpretation. The engineering_absolute_threshold reading (sibling) instantiates the engineering/victim interpretation. The piton perspective (degraded ritual) reveals the theatrical character of the compliance process over time.
 *
 * DIRECTIONALITY LOGIC:
 *   Program management (institutional/arbitrage) has low directionality (d ≈ 0.15): they are net beneficiaries of the constraint and have exit options (they can arbitrage to alternative risk frameworks or launch schedules if compliance fails). The sigmoid f(d) produces low effective extraction from their perspective — they experience the constraint as enabling. Engineering safety authority (powerless/trapped) has high directionality (d ≈ 0.95): they are net victims and have no exit (their dissent is absorbed into the compliance process without stopping it; walking away means abandoning safety responsibility). The sigmoid f(d) produces high experienced extraction. Safety engineers (moderate/constrained) have moderate-high directionality (d ≈ 0.65): they participate in risk assessment (some benefit) but face career cost for escalation (significant extraction). The regulatory authority (organized/constrained) occupies d ≈ 0.50: they have some power to enforce but are constrained by management's final authority and their own institutional incentives to avoid program halts. The analytical observer (analytical/analytical) uses canonical d ≈ 0.73, producing the mountain classification as a false-summit candidate — the naturalizing move that converts institutional choice into apparent necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves its mandatrophy by locating the ambiguity in the Rogers kernel itself. The kernel's formulation — 'demonstrate documented risk awareness and mitigation efforts sufficient to proceed' — does not specify who determines sufficiency or what authority structure enforces the constraint. The three sibling readings occupy this interpretive gap. The management_compliance_narrative reading resolves mandatrophy by declaring: sufficiency is determined by management in consultation with engineering and regulatory oversight; launch authority remains with management; documented compliance satisfies the requirement. This is coherent as a governance mechanism but creates the perspectival gaps evident in the six perspectives above. The resolution is not 'this is the correct reading' but 'this reading's coherence comes at the cost of extracting from engineering veto authority, which is why the organizational dispute persists.' The mandatrophy is resolved through structural clarity, not through consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_sufficiency_threshold,
    'What constitutes ''documented risk awareness and mitigation sufficient to proceed''? Who determines sufficiency — engineers, management, or the regulatory authority?',
    'Post-incident analysis: correlation between documented risk assessments and actual failure modes; comparison of launches approved vs. halted under the same regulatory framework',
    'If threshold is engineer-determined: constraint classifies as Rope (coordination) or Mountain (absolute constraint). If threshold is management-determined: constraint classifies as Snare (extraction masquerading as compliance). Current ambiguity permits both interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_sufficiency_threshold, conceptual, 'Who determines whether documented risk awareness is sufficient').

omega_variable(
    engineering_escalation_cost,
    'What is the true career cost for an engineer who refuses to sign off on risk mitigation deemed sufficient by management?',
    'Historical analysis of post-dissent career trajectories; comparison of advancement, assignment, and retention rates for engineers who escalated vs. complied',
    'If cost is zero: engineers have genuine veto; constraint is coordination (Rope). If cost is severe: engineers face suppression despite formal participation; constraint is extraction (Snare/Tangled Rope). This cost feeds directly into the suppression metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_escalation_cost, empirical, 'Career cost of engineering escalation or refusal to sign compliance documentation').

omega_variable(
    regulatory_deference_boundary,
    'At what point does the regulatory authority''s deference to management''s risk decision cross from coordination into extraction?',
    'Analysis of regulatory intervention triggers; comparison of safety outcomes between proactive intervention vs. post-incident investigation; longitudinal tracking of regulatory capacity to halt programs',
    'If regulators retain veto authority and exercise it: constraint is Tangled Rope with genuine coordination. If regulators defer completely to management: constraint is Snare with regulatory capture as a sub-mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_deference_boundary, empirical, 'Regulatory veto authority and exercise of intervention power').

omega_variable(
    kernel_reading_ambiguity,
    'Is the Rogers Commission''s mandate to establish a ''compliance process'' requiring documented risk awareness a directive to engineer management authority to override veto, or to strengthen the veto''s rational grounding?',
    'Textual and institutional history analysis: what did Rogers intend? What did NASA implement? Do subsequent failures align with the implementation interpretation?',
    'If intended to strengthen veto grounds: this reading misses the Commission''s intent. If intended to permit launch with documented rationale: this reading is correct. The interpretive ambiguity in the Rogers kernel itself is the source of the three-way factional split.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Rogers Commission mandate interpretation: does it authorize management override or strengthen veto authority?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rogers_mgmt_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rogers_mgmt_tr_t5, rogers_commission_findings__management_compliance_narrative, theater_ratio, 5, 0.51).
narrative_ontology:measurement(rogers_mgmt_tr_t10, rogers_commission_findings__management_compliance_narrative, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(rogers_mgmt_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rogers_mgmt_be_t5, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(rogers_mgmt_be_t10, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(rogers_mgmt_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(rogers_mgmt_su_t5, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(rogers_mgmt_su_t10, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__actuarial_risk_acceptance).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, challenger_disaster_cultural_narrative).

% DUAL FORMULATION NOTE:
% The Rogers Commission kernel spawns three distinct constraint readings with materially different epsilon values and beneficiary/victim structures. This story instantiates the management_compliance_narrative reading (ε=0.48, tangled_rope as dominant). The engineering_absolute_threshold reading will show ε≈0.15-0.25 (rope/mountain) from engineering perspective; actuarial_risk_acceptance reading will show ε≈0.35-0.42 (rope). All three link via network.affects_constraints and share the same kernel_id in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__management_compliance_narrative, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
