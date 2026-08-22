% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Multi-Mechanism Competence Occupation (Hybrid Reading)
 *   domain: safety/training/organizational
 *
 * SUMMARY:
 *   In high-reliability organizations, competence occupation requires
 *   continuous verification that personnel have not suffered skill decay.
 *   This constraint is the HYBRID READING: competence requires exercising all
 *   four mechanisms simultaneously (simulation, classroom refresher,
 *   procedural reinforcement, line audits) without consensus on optimal
 *   configuration or necessity. The reading asserts that no single mechanism
 *   is sufficient and that the perpetual research question—which combination
 *   is truly necessary?—must be held OPEN, not resolved. Alternative readings
 *   contest this: the simulation_sufficiency reading claims simulation alone,
 *   properly calibrated, is sufficient (efficiency gain through
 *   single-mechanism focus); the real_incident_necessity reading claims only
 *   authentic catastrophic incidents provide the authentic signal needed to
 *   occupy the kernel (all artificial mechanisms are theater). This story
 *   instantiates the hybrid reading by authoring metrics that reflect ongoing
 *   multi-mechanism operation, persistent research disagreement, and
 *   extraction that accrues to the training establishment and auditors who
 *   benefit from the perpetual unresolvability of the optimization problem.
 *
 * KEY AGENTS:
 *   - training_establishment: agenda-setter, institutional power, arbitrage exit — designs all four mechanisms, controls credentialing, justifies complexity as necessary
 *   - operational_personnel: organized power, constrained exit — must cycle through all four, time and compliance costs concentrate here
 *   - regulatory_compliance_auditors: institutional power, analytical exit — benefit from auditable artifact trail, verify completion of all mechanisms
 *   - instructional_designers: moderate power, identity-locked exit — trapped inside perpetual optimization problem, cannot remove any mechanism
 *   - incident_investigation_community: moderate power, mobile exit — produces evidence about mechanism efficacy but remains intellectually marginal
 *   - simulation_research_community: moderate power, mobile exit — maintains simulation sufficiency is possible but subordinate to multi-mechanism mandate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.62).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.58).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Multi-Mechanism Competence Occupation (Hybrid Reading)").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "safety/training/organizational").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, 'e03acc5d-2e68-42f6-b62b-e2e2b8b10e76').
narrative_ontology:cs_kernel_codification('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76', fixed_text).
narrative_ontology:cs_authority_grounding('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76', extraction).
narrative_ontology:cs_interpretation_layer_present('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76').
narrative_ontology:cs_reading_relation('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76', foundational, no_single_mechanism_sufficient).
narrative_ontology:cs_axiom_status(no_single_mechanism_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76', no_single_mechanism_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76', secondary, redundancy_justifies_portfolio_complexity).
narrative_ontology:cs_axiom_status(redundancy_justifies_portfolio_complexity, holdable).
narrative_ontology:cs_axiom_grounding('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76', redundancy_justifies_portfolio_complexity, instrumental).
narrative_ontology:cs_reference_frame('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76', multi_mechanism_competence_occupation).
narrative_ontology:cs_drift_state('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76', contemporary_research_challenged, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e03acc5d-2e68-42f6-b62b-e2e2b8b10e76', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_establishment).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, regulatory_compliance_auditors).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, operational_personnel).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, instructional_designers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, simulation_research_community).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, operational_supervisors).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, operational_supervisors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, operates, and certifies the multi-mechanism training portfolio (simulators, classrooms, procedural exercises, line audits). Controls the definition of competence occupation through curriculum approval, examination design, and credential renewal. Justifies the complexity as necessary for authentic skill development and risk mitigation. Benefits from the mandate to operate all mechanisms — revenues flow from operational personnel cycling through each modality.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Must cycle through all four mechanisms (simulation, refresher classroom, procedural reinforcement, line audits) on a mandated schedule. Time away from operational duties, credential maintenance costs, and competency anxiety all concentrate on this seat. Their exit from the training regime requires abandoning the occupation itself — employment in the industry is conditioned on continuous occupation of the competence kernel through all prescribed mechanisms.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, operational_personnel, payer,
    organized, biographical, constrained, national).

% Verify that personnel have occupied the competence kernel by checking completion of all four mechanisms. The multi-mechanism mandate produces a readily auditable artifact trail. Auditors benefit from a system whose compliance is unambiguous — mechanism completion records are objective and defend against liability. Their regulatory authority rests partly on the assumption that the prescribed portfolio IS sufficient.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, regulatory_compliance_auditors, beneficiary,
    institutional, generational, analytical, national).

% Must continuously optimize the portfolio without consensus on what optimization means. No mechanism is recognized as sufficient, so removing any mechanism is politically costly even if data suggest efficacy. They live inside the perpetual research problem: designing interventions whose adequacy cannot be falsified because all mechanisms must always be present. Professional identity is fused with the competence occupation mandate itself.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, instructional_designers, payer,
    moderate, biographical, identity_locked, national).

% Maintains that simulation alone could suffice if properly calibrated. Benefits from continued funding of simulation research and development, but the multi-mechanism mandate constrains their leverage — they must argue for simulation IMPROVEMENT rather than simulation REPLACEMENT. Their position is academically live but institutionally subordinate.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, simulation_research_community, beneficiary,
    moderate, biographical, mobile, global).

% Investigates actual incidents and asks why pre-incident training did not prevent them. They gather evidence about which mechanisms correlate with performance under authentic stress. Their findings are intellectually formative but institutionally marginal — the multi-mechanism mandate is already in place, and their research is read as refinement rather than falsification.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, incident_investigation_community, observer,
    moderate, biographical, mobile, global).

% Manage personnel through the training pipeline and defend against liability if an incident occurs. They benefit from the auditable artifact trail (completion records prove due diligence) but bear the operational cost of staff absence during training cycles. They push back against training load but cannot exit the mandate — their institutional authority depends partly on demonstrating training compliance.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, operational_supervisors, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, operational_supervisors, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__hybrid_occupation, training_establishment).
narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Competence occupation is a collective-action problem: the organization must verify that personnel remain operationally ready despite inevitable skill decay. The multi-mechanism approach pools signal from diverse observables (simulation performance, classroom engagement, procedural execution, auditor observation) to build confidence that readiness has been maintained. A single mechanism would be cheaper but less robust to that mechanism's failure modes.
% TRANSFER_FUNCTION: Transfers time, attention, and compliance effort from operational personnel and instructional designers to the training establishment and regulatory auditors. The establishment receives enrollment, the auditors receive credible artifacts to verify, operational personnel lose days away from their primary duties, and designers lose autonomy to reconfigure the training system.
% ABSENT_VOICES: Incident survivors and families of operational failures are structurally absent — they would attest whether the four-mechanism portfolio actually prevented harm or merely created a liability shield. Alternative modality researchers and practitioners are excluded by the regulatory requirement that all four mechanisms must persist. Evidence-based critics (those whose empirical work shows some mechanisms are redundant) have no institutional seat to speak from.
% DISAPPEARANCE_RATIONALE: If the multi-mechanism mandate vanished, the training establishment would shrink, operational personnel would return days of capacity, instructional designers would have autonomy to optimize, and regulatory auditors would need to find new verification mechanisms. The system is not a natural law — it persists because institutional actors benefit from its continuance.
% FOUNDING_PROBLEM: Early competence occupation relied on single mechanisms (classroom or simulation alone) and incidents revealed skill decay or transfer failures. The organization needed to build redundancy into training to catch the gaps each mechanism missed.
% FOUNDING_PROBLEM_CORROBORATION: The training establishment attests the founding problem is still live and justifies all four mechanisms as still necessary. Incident investigators attest that specific incidents HAVE occurred despite single-mechanism training, supporting the redundancy rationale. However, no systematic study outside the training establishment has quantified whether ALL FOUR mechanisms are necessary or whether a smaller portfolio (e.g., simulation + line audits) would achieve the same outcome. The corroboration is partial — defenders of the current mandate speak loudly; independent critics have not been resourced to produce competing data.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.62 over the interval, plateauing by year 15. The rise reflects the training establishment's increasing capture of operational time without resolution of the research question — once the multi-mechanism mandate is institutionalized, the establishment's leverage grows because all four mechanisms become regulatory standard. Theater_ratio rises from 0.35 to 0.48, indicating that increasing effort goes into auditable compliance record-keeping rather than authentic skill reinforcement. By year 15, theater stabilizes because the system reaches equilibrium: all mechanisms are routinely completed, auditors verify completion, personnel accumulate training artifacts regardless of whether each mechanism is individually justified. Suppression_requirement rises from 0.52 to 0.58, reflecting the enforcement machinery required to keep all four mechanisms present despite periodic pressure to consolidate or eliminate redundant components. The measurement grid is shared across all three metrics; every time point is authored for every metric. This is a tangled_rope: the coordination function (building redundant signal about competence through diverse observables) is genuine, but it coexists with asymmetric extraction (training establishment gains enrollment, personnel lose autonomy, designers lose optimization authority). Active enforcement is required because the system is artificially complex — without regulatory mandate and organizational policy, operational personnel would negotiate for fewer mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is primary here. The training_establishment reads the constraint as rope: genuine coordination, all parties depend on the system, exit is costly but rationally chosen by all. Operational_personnel read it as snare: the constraint extracts their time and compliance effort, the four mechanisms are not independently justified, and their exit is blocked (they cannot leave the occupation without losing livelihood). Instructional_designers occupy an unstable middle: they believe in competence occupation as a genuine problem but are trapped inside the argument that all four mechanisms must always be present — they cannot advance their design without first proving an alternative is inadequate, a burden that shifts all evidence pressure toward them rather than toward the establishment defending the status quo. The engine computes per-seat classifications; this story's authored metrics and beneficiary/victim declarations will reveal which seat's classification diverges from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Training_establishment sits at d near 0.0 (full beneficiary: controls agenda, collects revenue, suffers no performance penalty from the arrangement). Operational_personnel sit near d = 0.8 (high target: constrained exit, carry compliance burden, cannot negotiate terms). Instructional_designers sit at d near 0.85 (trapped in the perpetual research problem by identity fusion with training authority — they cannot exit their professional role without abandoning expertise in competence occupation). Regulatory_compliance_auditors sit at d near 0.15 (beneficiary: the mandate makes their verification role unambiguous and defensible). The disagreement is structural: from the training establishment seat, the four-mechanism portfolio is genuine coordination built by competent authorities; from the operational_personnel seat, it is a compliance burden justified by post-hoc authority rather than prospective evidence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (skill decay causes incidents) is live and contested. The multi-mechanism mandate addresses it by building redundancy, but the mandate itself is now the object of contention: has it become an end in itself rather than a means? The theater_ratio rising to 0.48 suggests that roughly half the effort is now compliance documentation rather than authentic competence reinforcement. Yet the establishment continues to justify all four mechanisms as necessary because falsifying the requirement (removing one mechanism and observing outcomes) is politically costly, creates liability exposure, and would require authority figures to admit that previous requirements were excessive. This is mandatrophy in progress: the founding problem is partially solved (incidents have not risen; the system appears to be working), but the mandate persists and intensifies because the establishment benefits from its continuation and has no institutional incentive to simplify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_vs_sufficiency_unresolvable,
    'Is each of the four mechanisms NECESSARY to competence occupation, or would a smaller portfolio (e.g., simulation + line audits) be SUFFICIENT?',
    'Randomized control trial or natural experiment: jurisdictions that mandate fewer mechanisms and observe whether incident rates or competence metrics differ. Blocked by institutional resistance because the current mandate is already embedded in regulation and organizational practice.',
    'If a smaller portfolio is sufficient, the current system is pure extraction (training establishment captures unnecessary enrollment, personnel lose unnecessary time, designers defend unnecessary complexity). If all four are necessary, the current system is genuine coordination despite its cost. Classification hinges on this distinction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_vs_sufficiency_unresolvable, empirical, 'Whether all four mechanisms are individually justified or whether the portfolio is overconfigured.').

omega_variable(
    extraction_via_perpetual_research,
    'Does the multi-mechanism mandate persist not because it is optimal but because no single mechanism can be removed without opening a research question that the establishment then funds indefinitely?',
    'Institutional history and budget analysis: examine whether mechanism-removal proposals consistently trigger new research funding and whether research findings recommending mechanism reduction are incorporated into policy.',
    'If the perpetual research model is active, the constraint qualifies as snare at the training_establishment seat and snare-via-capture at the instructional_designer seat. The designers are trapped inside a perpetual justification burden. If research is genuinely independent and findings do get incorporated, the system remains tangled_rope despite its extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_via_perpetual_research, empirical, 'Whether the multi-mechanism mandate is self-perpetuating through research funding cycles.').

omega_variable(
    identity_lock_mechanism_instructional_designers,
    'Are instructional designers trapped by structural constraint (cannot remove mechanisms without regulatory violation) or by identity fusion (they believe no mechanism should be removed and fuse their professional identity with that belief)?',
    'Post-role-change trajectory: if designers who leave the training establishment and join industries with different mandates readily accept simpler training portfolios, the trap is structural + partly internalized; if they maintain advocacy for multi-mechanism approaches, the trap is deep identity fusion.',
    'If internalized, designers carry the suppression with them after exit. If structural, exit removes the suppression. The degree of internalization affects the effective suppression the constraint exerts and determines whether the constraint persists through genuine belief or through coercive lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_instructional_designers, empirical, 'Structural vs. internalized suppression in the instructional_designer seat.').

omega_variable(
    reading_boundary_simulation_vs_hybrid,
    'Do the sibling readings (simulation_sufficiency and real_incident_necessity) coexist with the hybrid reading in the same institutional framework, or does the hybrid mandate actively foreclose them?',
    'Policy analysis: examine whether institutions mandate ALL FOUR mechanisms, whether simulation-only and real-incident-only approaches are ever implemented as legitimate alternatives, and whether policy explicitly forbids them.',
    'If the hybrid mandate forbids the alternatives, the relation is ''forecloses'' (rare, strong); if alternatives are permitted in different jurisdictions or organizations, the relation is ''coexists_with'' (common, weaker). If the hybrid mandate creates conditions that make alternatives less viable (resource competition, regulatory burden), the relation is ''influences''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_simulation_vs_hybrid, conceptual, 'Whether the hybrid reading coexists with or forecloses its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comp_tr_t3, competence_occupation__hybrid_occupation, theater_ratio, 3, 0.38).
narrative_ontology:measurement(comp_tr_t6, competence_occupation__hybrid_occupation, theater_ratio, 6, 0.42).
narrative_ontology:measurement(comp_tr_t9, competence_occupation__hybrid_occupation, theater_ratio, 9, 0.45).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__hybrid_occupation, theater_ratio, 12, 0.47).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__hybrid_occupation, theater_ratio, 15, 0.48).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.48).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__hybrid_occupation, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comp_be_t3, competence_occupation__hybrid_occupation, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(comp_be_t6, competence_occupation__hybrid_occupation, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(comp_be_t9, competence_occupation__hybrid_occupation, base_extractiveness, 9, 0.59).
narrative_ontology:measurement(comp_be_t12, competence_occupation__hybrid_occupation, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(comp_be_t15, competence_occupation__hybrid_occupation, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(comp_be_t25, competence_occupation__hybrid_occupation, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(comp_su_t3, competence_occupation__hybrid_occupation, suppression_requirement, 3, 0.54).
narrative_ontology:measurement(comp_su_t6, competence_occupation__hybrid_occupation, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(comp_su_t9, competence_occupation__hybrid_occupation, suppression_requirement, 9, 0.57).
narrative_ontology:measurement(comp_su_t12, competence_occupation__hybrid_occupation, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(comp_su_t15, competence_occupation__hybrid_occupation, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(comp_su_t25, competence_occupation__hybrid_occupation, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__hybrid_occupation, 0.12).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__real_incident_necessity).

% DUAL FORMULATION NOTE:
% The competence_occupation kernel decomposes into three readings reflecting different operative theories of how competence is maintained: HYBRID_OCCUPATION (this story, multi-mechanism simultaneous exercise), SIMULATION_SUFFICIENCY (simulation alone is adequate), REAL_INCIDENT_NECESSITY (only actual crises suffice). Each reading has a distinct ε referent: the standing arrangement under contest (as that reading sees it). The hybrid reading authors ε as high (0.62) because the multi-mechanism mandate is seen as extraction riding on coordination; the simulation_sufficiency reading would author ε as lower (unnecessary mechanisms removed, efficiency gain) because the same arrangement is seen as redundant; the real_incident_necessity reading would author ε as very high (all pre-incident training is theater) because the standing arrangement is seen as displacement of authentic signal. The three stories share a kernel (the competence_occupation problem) but instantiate different readings with different beneficiary structures, different victim sets, and different ε values. They are linked by network.affects_constraints to indicate family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
