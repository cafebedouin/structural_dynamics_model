% ============================================================================
% CONSTRAINT STORY: simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simulation_fidelity_threshold, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: simulation_fidelity_threshold
 *   human_readable: Simulation Fidelity Threshold for Competence Retention
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The simulation fidelity threshold doctrine holds that operator competence
 *   in catastrophic scenarios can be retained through simulation training,
 *   provided the simulation crosses a fidelity threshold where induced stress
 *   and uncertainty match real events. This constraint coordinates investment
 *   in simulation technology and legitimizes training programs, but embeds
 *   extraction through the naturalization of 'fidelity' as a measurable,
 *   technology-dependent property. The threshold framing creates a binary
 *   sufficiency condition (threshold crossed = competence retained) that
 *   benefits simulation vendors and training administrators while shifting
 *   epistemic risk to frontline operators. The constraint has drifted toward
 *   higher theater over the interval (0.25 → 0.42) as simulation technology
 *   marketing has outpaced empirical validation of transfer-of-training, and
 *   extractiveness has increased modestly (0.18 → 0.28) as procurement cycles
 *   have concentrated around high-end vendors claiming threshold-crossing
 *   capability.
 *
 * KEY AGENTS:
 *   - Simulation Technology Vendors: Primary beneficiary (institutional/arbitrage) — the threshold doctrine creates a market for increasingly sophisticated systems; each generation claims to approach or cross the threshold
 *   - Training Program Administrators: Secondary beneficiary (moderate/constrained) — the binary sufficiency condition simplifies budget justification and shields programs from scrutiny
 *   - High-Reliability Organizations: Beneficiary (institutional/mobile) — genuine coordination benefit through safer training, but also bears cost of potential over-investment in fidelity that may not transfer
 *   - Frontline Operators: Mixed position (powerless/trapped) — benefit from safer training but bear the risk if simulation competence does not transfer to real catastrophes; cannot challenge the sufficiency claim without career consequences
 *   - Safety Research Community: Organized observer (organized/mobile) — sees the threshold as a temporary approximation to be refined by empirical evidence; can exit to alternative frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simulation_fidelity_threshold, 0.28).
domain_priors:suppression_score(simulation_fidelity_threshold, 0.35).
domain_priors:theater_ratio(simulation_fidelity_threshold, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simulation_fidelity_threshold, extractiveness, 0.28).
narrative_ontology:constraint_metric(simulation_fidelity_threshold, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(simulation_fidelity_threshold, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(simulation_fidelity_threshold, "Simulation Fidelity Threshold for Competence Retention").
narrative_ontology:topic_domain(simulation_fidelity_threshold, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simulation_fidelity_threshold, '6ae05c67-14ad-471a-b3f4-633727ad4dd5').
narrative_ontology:cs_kernel_codification('6ae05c67-14ad-471a-b3f4-633727ad4dd5', distributed).
narrative_ontology:cs_authority_grounding('6ae05c67-14ad-471a-b3f4-633727ad4dd5', expertise).
narrative_ontology:cs_interpretation_layer_present('6ae05c67-14ad-471a-b3f4-633727ad4dd5').
narrative_ontology:cs_reading_relation('6ae05c67-14ad-471a-b3f4-633727ad4dd5', simulation_fidelity_threshold__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ae05c67-14ad-471a-b3f4-633727ad4dd5', simulation_fidelity_threshold__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ae05c67-14ad-471a-b3f4-633727ad4dd5', simulation_fidelity_threshold__hybrid_degradation_reading, influences).
narrative_ontology:cs_axiom('6ae05c67-14ad-471a-b3f4-633727ad4dd5', foundational, technological_fidelity_sufficiency).
narrative_ontology:cs_axiom_status(technological_fidelity_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('6ae05c67-14ad-471a-b3f4-633727ad4dd5', technological_fidelity_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('6ae05c67-14ad-471a-b3f4-633727ad4dd5', secondary, stress_equivalence_achievability).
narrative_ontology:cs_axiom_status(stress_equivalence_achievability, holdable).
narrative_ontology:cs_axiom_grounding('6ae05c67-14ad-471a-b3f4-633727ad4dd5', stress_equivalence_achievability, empirically_contingent).
narrative_ontology:cs_reference_frame('6ae05c67-14ad-471a-b3f4-633727ad4dd5', technological_substitution_paradigm).
narrative_ontology:cs_drift_state('6ae05c67-14ad-471a-b3f4-633727ad4dd5', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6ae05c67-14ad-471a-b3f4-633727ad4dd5', '').
narrative_ontology:cs_kernel_id(simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(simulation_fidelity_threshold, training_program_administrators).
narrative_ontology:constraint_beneficiary(simulation_fidelity_threshold, high_reliability_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(simulation_fidelity_threshold, frontline_operators).
narrative_ontology:constraint_vindicates(simulation_fidelity_threshold, technological_substitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, market, and sell simulation systems to high-reliability organizations. Set the agenda by defining 'fidelity' metrics and claiming threshold-crossing capability for each technology generation. Collect revenue through procurement cycles justified by the threshold doctrine. Can exit to other markets (entertainment, consumer VR, defense) if the safety training market contracts.
narrative_ontology:constraint_stakeholder(simulation_fidelity_threshold, simulation_technology_vendors, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(simulation_fidelity_threshold, simulation_technology_vendors, beneficiary).

% Manage training programs within high-reliability organizations. Set training requirements and justify budgets using the threshold doctrine. Benefit from the binary sufficiency condition (threshold crossed = competence retained) which simplifies resource allocation and shields programs from scrutiny. Constrained by vendor lock-in and regulatory frameworks.
narrative_ontology:constraint_stakeholder(simulation_fidelity_threshold, training_program_administrators, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(simulation_fidelity_threshold, training_program_administrators, beneficiary).

% Nuclear power plants, aviation, chemical processing, emergency response — organizations where catastrophic failure has severe consequences. Invest in simulation training to maintain operator competence without exposing personnel to real catastrophes. Benefit from safer training and regulatory compliance, but bear the cost of potential over-investment in fidelity that may not transfer to real events. Can shift to alternative training modalities if evidence accumulates.
narrative_ontology:constraint_stakeholder(simulation_fidelity_threshold, high_reliability_organizations, beneficiary,
    institutional, biographical, mobile, national).

% Personnel who must respond to real catastrophic events. Receive simulation training and are told it will retain their competence. Benefit from safer training (no real catastrophe exposure during training) but bear the risk if simulation competence does not transfer to real events. Cannot challenge the sufficiency claim without career consequences. Trapped — must accept the training regime provided by their organization.
narrative_ontology:constraint_stakeholder(simulation_fidelity_threshold, frontline_operators, payer,
    powerless, biographical, trapped, local).

% Human factors researchers, organizational psychologists, safety scientists studying transfer-of-training and competence retention. Observe the threshold doctrine as a temporary coordination mechanism while empirical evidence accumulates. Can exit to alternative frameworks (distributed practice, real-world micro-exposures, cognitive task analysis) as evidence develops. Organized through professional societies and research networks.
narrative_ontology:constraint_stakeholder(simulation_fidelity_threshold, safety_research_community, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables operator training for catastrophic scenarios without exposing personnel to real catastrophes during training. Provides a decision-making heuristic for training program investment: invest in simulation technology until fidelity threshold is crossed, then competence is retained.
% TRANSFER_FUNCTION: Money flows from high-reliability organizations to simulation technology vendors through procurement cycles. Legitimacy and budget security flow to training program administrators. Epistemic risk flows to frontline operators (if threshold doctrine is wrong, they bear the consequences in real catastrophes).
% ABSENT_VOICES: Operators who have experienced real catastrophes and found their simulation training insufficient are structurally excluded — their negative evidence is treated as anecdotal or attributed to insufficient fidelity rather than challenging the threshold doctrine itself. Safety researchers who question the construct validity of 'fidelity' face funding barriers because the threshold doctrine is embedded in procurement and regulatory frameworks.
% DISAPPEARANCE_RATIONALE: If the threshold doctrine disappeared, high-reliability organizations would need alternative frameworks for competence retention: either accept competence decay without catastrophe exposure (unacceptable for safety-critical systems), invest in real-world micro-exposures (higher risk), or adopt hybrid approaches combining simulation with other modalities (distributed practice, cognitive task analysis, mentorship). Training budgets would shift away from high-fidelity simulation vendors toward alternative approaches. The binary sufficiency condition would be replaced by continuous evaluation of training effectiveness.
% FOUNDING_PROBLEM: How to maintain operator competence in catastrophic scenarios when real catastrophes are rare (competence decays between events) and exposing personnel to real catastrophes for training purposes is unacceptable (too dangerous, too expensive, ethically prohibited).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by: (1) High-reliability organizations across multiple domains (nuclear, aviation, chemical, emergency response) who face the competence retention challenge. (2) Safety researchers who document competence decay in low-frequency high-consequence scenarios. (3) Regulatory bodies who mandate training requirements without specifying sufficiency conditions. The problem is live because real catastrophes remain rare and training exposure to real catastrophes remains unacceptable.
narrative_ontology:disappearance_verdict(simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(simulation_fidelity_threshold, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-RELIABILITY ORGANIZATION (ROPE) — Experiences the constraint as coordination: investing in simulation technology solves the genuine problem of maintaining operator competence without exposing personnel to real catastrophic events. The threshold creates a clear sufficiency condition — once fidelity crosses it, competence is retained. Net beneficiary through risk reduction and training efficiency.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: SIMULATION TECHNOLOGY VENDOR (ROPE) — Primary beneficiary. The threshold doctrine creates a market for increasingly sophisticated simulation systems. Each technology generation claims to approach or cross the fidelity threshold, justifying procurement cycles. Experiences the constraint as pure coordination — providing tools that solve a real organizational problem.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: TRAINING PROGRAM ADMINISTRATOR (ROPE) — Benefits from the threshold doctrine through budget justification and program legitimacy. The binary sufficiency condition (threshold crossed = competence retained) simplifies resource allocation decisions and shields training programs from scrutiny. Constrained by procurement cycles and vendor lock-in, but net beneficiary.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FRONTLINE OPERATOR (TANGLED ROPE) — Experiences both coordination (simulation training is safer and more accessible than real catastrophe exposure) and extraction (competence claims based on simulation may not transfer to real events; operator bears the risk if the threshold doctrine is wrong). Trapped — cannot opt out of the training regime or challenge the sufficiency claim without career consequences.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: SAFETY RESEARCH COMMUNITY (SCAFFOLD) — Sees the threshold doctrine as a temporary coordination mechanism while empirical evidence accumulates. The binary sufficiency condition is a useful approximation that will be refined or replaced as transfer-of-training research matures. Organized researchers can exit to alternative frameworks (distributed practice, real-world micro-exposures, cognitive task analysis) as evidence accumulates.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes genuine coordination function (simulation enables training without catastrophe exposure) alongside embedded extraction (threshold doctrine naturalizes technology-dependent sufficiency, obscuring that 'fidelity' is a contested construct and transfer-of-training is empirically uncertain). The binary threshold is a simplification that benefits vendors and administrators while shifting epistemic risk to operators.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simulation_fidelity_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(simulation_fidelity_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(simulation_fidelity_threshold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The constraint coordinates genuine investment in training infrastructure, but the threshold framing naturalizes technology-dependent sufficiency and obscures empirical uncertainty about transfer-of-training. Vendors capture rents through procurement cycles justified by threshold-crossing claims. The extraction is real but not severe — much of the investment produces genuine training value, even if the binary threshold is a simplification. Suppression (0.35): Low-moderate. Operators cannot easily challenge the sufficiency doctrine without career risk, and alternative training modalities (real-world micro-exposures, cognitive task analysis, distributed practice) are suppressed by the threshold framing. But suppression is not total — safety research continues, and some organizations experiment with hybrid approaches. Theater ratio (0.42): Moderate. Simulation fidelity demonstrations (visual realism, scenario complexity, haptic feedback) are partly performative — they signal investment in safety without necessarily predicting competence transfer. The theater has increased as marketing has outpaced validation. But the theater is not dominant — simulation training does produce measurable skill acquisition, even if transfer-of-training is uncertain.
 *
 * PERSPECTIVAL GAP:
 *   The simulation technology vendor and training program administrator see pure coordination (Rope) — they are solving the real problem of competence retention without catastrophe exposure, and the threshold provides a clear sufficiency condition. The high-reliability organization also sees coordination but with awareness of investment risk. The frontline operator sees tangled rope — genuine training benefit alongside epistemic risk that the threshold doctrine may not deliver on its sufficiency claim. The safety research community sees scaffold — a temporary approximation to be refined by empirical evidence. The analytical observer sees tangled rope — genuine coordination function with embedded extraction through naturalization of technology-dependent sufficiency. The perspectival gap reveals that the constraint's classification depends on whether the observer is collecting from it (vendors, administrators), bearing its risk (operators), or evaluating its empirical validity (researchers, analysts).
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation technology vendors are primary beneficiaries with arbitrage exit options — they collect from the constraint through procurement cycles and can exit to other markets if the threshold doctrine loses legitimacy. Training program administrators are secondary beneficiaries with constrained exit — they benefit from budget justification but are locked into vendor relationships and regulatory frameworks. High-reliability organizations are beneficiaries with mobile exit — they gain genuine coordination value but can shift to alternative training modalities if evidence accumulates against the threshold doctrine. Frontline operators are in a mixed position — they benefit from safer training (coordination function) but bear the risk of insufficient competence transfer (extraction function) and cannot challenge the doctrine without career consequences (trapped exit). The analytical observer recognizes both the genuine coordination function and the embedded extraction, classifying the constraint as tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that a single structural arrangement can be both coordination and extraction depending on the observer's position. The threshold doctrine coordinates real investment in training infrastructure and provides a decision-making heuristic for resource allocation (coordination function). Simultaneously, it naturalizes technology-dependent sufficiency, obscures empirical uncertainty about transfer-of-training, and concentrates procurement around high-end vendors (extraction function). The mandatrophy is not 'which function is real?' but 'both functions coexist, and their relative salience depends on the observer's structural relationship to the constraint.' Vendors and administrators experience coordination; operators bear extraction risk; researchers see a temporary scaffold; analysts see the hybrid structure. The presheaf over observation sites captures this multiplicity without collapsing it into a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_construct_validity,
    'Does ''fidelity'' as measured by simulation technology vendors (visual realism, haptic feedback, scenario complexity) actually predict transfer-of-training to real catastrophic events?',
    'Longitudinal tracking of operator performance in real incidents vs. simulation training history; controlled studies comparing high-fidelity vs. low-fidelity training outcomes in actual emergency response',
    'If construct validity is low: the threshold doctrine is extractive (vendors sell fidelity that doesn''t transfer). If high: the threshold doctrine is genuine coordination (technology investment produces real competence retention).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_construct_validity, empirical, 'Whether simulation fidelity predicts real-world competence transfer').

omega_variable(
    threshold_binary_vs_continuous,
    'Is competence retention a binary threshold phenomenon (sufficient fidelity = full transfer) or a continuous function (higher fidelity = incrementally better transfer with diminishing returns)?',
    'Dose-response studies varying simulation fidelity across a range and measuring competence transfer; identification of plateau effects or linear relationships',
    'If binary: threshold doctrine is structurally accurate, extraction is minimal. If continuous with diminishing returns: threshold framing is extractive (justifies over-investment in high-end simulation technology beyond the point of marginal benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_binary_vs_continuous, empirical, 'Whether competence transfer is threshold or continuous phenomenon').

omega_variable(
    stress_inoculation_sufficiency,
    'Can simulation-induced stress inoculate operators against real catastrophe stress, or does the knowledge that ''this is a simulation'' create an unbridgeable psychological gap?',
    'Psychophysiological measurement (cortisol, heart rate variability, decision latency) comparing simulation vs. real incident responses; qualitative interviews with operators who have experienced both',
    'If simulation stress transfers: threshold doctrine is coordination. If psychological gap is unbridgeable: no simulation fidelity can cross the threshold, and the doctrine is extractive cover for unavoidable competence decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stress_inoculation_sufficiency, empirical, 'Whether simulation stress inoculates against real catastrophe stress').

omega_variable(
    committer_frame_kernel_ambiguity,
    'Is this constraint (simulation fidelity threshold) one reading of a contested kernel about catastrophe proxy sufficiency, or is it an independent coordination mechanism?',
    'Cross-reading analysis: do sibling readings (catastrophe necessity, hybrid degradation, simulation-as-proxy) share a common structural kernel, or are they addressing different coordination problems that happen to use similar vocabulary?',
    'If genuine kernel: the readings foreclose or influence each other, and the committer frame reveals structural relationships. If independent: the readings coexist without logical tension, and the kernel framing is a narrative convenience rather than a structural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_ambiguity, conceptual, 'Whether the committer frame identifies a real kernel or a narrative grouping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simulation_fidelity_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simfid_tr_t0, simulation_fidelity_threshold, theater_ratio, 0, 0.25).
narrative_ontology:measurement(simfid_tr_t3, simulation_fidelity_threshold, theater_ratio, 3, 0.32).
narrative_ontology:measurement(simfid_tr_t6, simulation_fidelity_threshold, theater_ratio, 6, 0.38).
narrative_ontology:measurement(simfid_tr_t10, simulation_fidelity_threshold, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(simfid_be_t0, simulation_fidelity_threshold, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(simfid_be_t3, simulation_fidelity_threshold, base_extractiveness, 3, 0.22).
narrative_ontology:measurement(simfid_be_t6, simulation_fidelity_threshold, base_extractiveness, 6, 0.25).
narrative_ontology:measurement(simfid_be_t10, simulation_fidelity_threshold, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simulation_fidelity_threshold, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_proxy_sufficiency kernel. Other readings (simulation_as_proxy_catastrophe, catastrophe_necessity, hybrid_degradation) are separate constraint stories with different beneficiary structures and extractiveness values. The readings are linked through the kernel but have distinct structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
