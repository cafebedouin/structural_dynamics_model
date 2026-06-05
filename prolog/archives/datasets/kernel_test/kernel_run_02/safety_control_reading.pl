% ============================================================================
% CONSTRAINT STORY: safety_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_safety_control_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: safety_control_reading
 *   human_readable: AI Safety Control: Preventing Catastrophic Loss of Control
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   The safety_control_reading frames AI alignment as a constraint-based
 *   problem: preventing catastrophic loss of control over advanced AI systems
 *   through safety mechanisms, capability restrictions, and control-theoretic
 *   governance. This is ONE reading of the contested kernel
 *   'ai_alignment_commitment,' distinct from the ethics_justice_reading
 *   (which prioritizes present-day harms, fairness, and participatory
 *   governance) and the integrated_reading (which attempts to combine
 *   catastrophic risk mitigation with near-term safety). The
 *   safety_control_reading instantiates a specific structural pattern: high
 *   extraction from multiple victim sets (near-term application developers,
 *   Global South AI development, competing governance frameworks) justified
 *   by a speculative catastrophe scenario that cannot be empirically
 *   falsified in advance. The constraint exhibits tangled rope structure —
 *   genuine coordination (shared safety standards reduce liability and
 *   interoperability risk) layered with asymmetric extraction (resources
 *   concentrated toward existential risk, capability development constrained,
 *   pathways lock in safety-first prioritization). Theater ratio (0.65)
 *   reflects that safety certification for unprecedented scenarios is
 *   necessarily performative: no regulatory checklist can actually verify
 *   prevention of a catastrophic failure mode that has not yet occurred and
 *   may have multiple independent pathways.
 *
 * KEY AGENTS:
 *   - Safety Research Institutions: Primary beneficiary (institutional/arbitrage) — capture research funding, institutional prestige, policy influence through catastrophe framing; arbitrage option to shift emphasis without exiting governance structure
 *   - Frontier AI Companies: Powerful secondary beneficiary (powerful/constrained) — capability restrictions provide liability cover and justification for slowing competitors; constrained by compliance overhead but shaped standards to favor their position
 *   - Future Generations: Speculative victim (powerless/trapped) — hypothetical catastrophe victims cannot organize; treated as passive beneficiaries of safety measures despite having zero voice in governance
 *   - Near-Term Application Developers: Moderate victim (moderate/constrained) — experience coordination benefits but also extraction via compliance costs, delayed deployment, uneven regulatory burden
 *   - Global South AI Development: Structural victim (powerless/trapped) — caught between adoption of Northern-designed safety standards and competitive exclusion; no meaningful participation in control framework design
 *   - Regulatory Apparatus: Institutional performer (institutional/arbitrage) — maintains theater of safety certification; cannot empirically verify catastrophe prevention so ritual becomes the legitimacy mechanism
 *   - Analytical Observer: Observes the full structure (analytical/analytical) — identifies tangled rope: real coordination function plus asymmetric extraction from multiple victim sets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(safety_control_reading, 0.58).
domain_priors:suppression_score(safety_control_reading, 0.62).
domain_priors:theater_ratio(safety_control_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(safety_control_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(safety_control_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(safety_control_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(safety_control_reading, tangled_rope).
narrative_ontology:human_readable(safety_control_reading, "AI Safety Control: Preventing Catastrophic Loss of Control").
narrative_ontology:topic_domain(safety_control_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(safety_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(safety_control_reading, '3570c50a-d231-4785-ae1d-471c5bf56053').
narrative_ontology:cs_created_at('3570c50a-d231-4785-ae1d-471c5bf56053', '').
narrative_ontology:cs_kernel_codification('3570c50a-d231-4785-ae1d-471c5bf56053', distributed).
narrative_ontology:cs_authority_grounding('3570c50a-d231-4785-ae1d-471c5bf56053', extraction).
narrative_ontology:cs_kernel_id(safety_control_reading, ai_alignment_commitment).
narrative_ontology:cs_reading_relation('3570c50a-d231-4785-ae1d-471c5bf56053', ethics_justice_reading, coexists_with).
narrative_ontology:cs_reading_relation('3570c50a-d231-4785-ae1d-471c5bf56053', integrated_reading, influences).
narrative_ontology:cs_axiom('3570c50a-d231-4785-ae1d-471c5bf56053', foundational, catastrophic_loss_of_control_is_highest_priority_harm).
narrative_ontology:cs_axiom_status(catastrophic_loss_of_control_is_highest_priority_harm, holdable).
narrative_ontology:cs_axiom_grounding('3570c50a-d231-4785-ae1d-471c5bf56053', catastrophic_loss_of_control_is_highest_priority_harm, empirically_contingent).
narrative_ontology:cs_axiom('3570c50a-d231-4785-ae1d-471c5bf56053', foundational, control_through_capability_restriction_is_legitimate_governance).
narrative_ontology:cs_axiom_status(control_through_capability_restriction_is_legitimate_governance, holdable).
narrative_ontology:cs_axiom_grounding('3570c50a-d231-4785-ae1d-471c5bf56053', control_through_capability_restriction_is_legitimate_governance, instrumental).
narrative_ontology:cs_reference_frame('3570c50a-d231-4785-ae1d-471c5bf56053', distributed_human_control_framework).
narrative_ontology:cs_drift_state('3570c50a-d231-4785-ae1d-471c5bf56053', contemporary_frontier_ai_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(safety_control_reading, safety_research_institutions).
narrative_ontology:constraint_beneficiary(safety_control_reading, existential_risk_advocates).
narrative_ontology:constraint_victim(safety_control_reading, near_term_ai_applications).
narrative_ontology:constraint_victim(safety_control_reading, global_south_ai_development).
narrative_ontology:constraint_victim(safety_control_reading, competing_governance_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Hypothetical victims of catastrophic AI misalignment cannot organize, negotiate, or exit. Bear the full cost of present-day decisions. Maximum suppression: they have no voice in current governance structures, and the constraint defines their welfare as derivative of safety priorities. The extraction flow runs entirely toward safety institutions: if catastrophe does not occur, they never knew they were 'harmed'; if it does occur, they cannot contest the framing.
constraint_indexing:constraint_classification(safety_control_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEAR-TERM APPLICATION DEVELOPERS (TANGLED ROPE) — Experience mixed coordination and extraction. The safety control framework provides genuine coordination benefit (shared safety standards, liability reduction, interoperability). But also bears extraction: resource diversion to safety compliance, delayed deployment, reduced competitive advantage if safety requirements are unevenly applied. Constrained exit — can adopt safety practices or face regulatory penalty, but cannot fully exit the constraint.
constraint_indexing:constraint_classification(safety_control_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SAFETY RESEARCH INSTITUTIONS (ROPE) — Primary beneficiaries. Capture research funding, institutional prestige, and policy influence through catastrophe framing. The constraint coordinates the field by establishing shared safety priorities. Arbitrage exit: can shift framing to emphasize near-term harms or benefits from present-day AI systems without exiting the governance structure. Net beneficiary — the extraction flow runs toward them.
constraint_indexing:constraint_classification(safety_control_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL SOUTH AI DEVELOPMENT (SNARE) — Structural victim. Safety control frameworks developed in high-income countries impose compliance costs and delay competitive participation in AI markets. Exit options blocked by institutional coupling: cannot develop AI systems that compete globally without safety certification designed elsewhere. Suppression: enforcement through export control, investment screening, and international coordination. No meaningful participation in setting the control framework that constrains their development.
constraint_indexing:constraint_classification(safety_control_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 5: FRONTIER AI COMPANIES (TANGLED ROPE) — Mixed coordination and extraction. The safety control framework reduces liability and provides cover for capability restrictions ('we're doing it for safety'). But also constrains capability development and increases engineering overhead. Constrained exit: must comply with safety standards or lose access to critical infrastructure, talent, and markets. Powerful enough to shape the standards being imposed.
constraint_indexing:constraint_classification(safety_control_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY APPARATUS (PITON) — Performative institutional role. Safety control frameworks require prediction of low-probability, high-impact events that cannot be empirically verified before deployment. Regulatory review becomes theater: checklists, red-team exercises, alignment certifications that cannot actually predict catastrophic failure. The apparatus persists through institutional inertia and risk-aversion, not through demonstrated efficacy. Theater ratio high because verification of 'catastrophe prevention' is structurally impossible.
constraint_indexing:constraint_classification(safety_control_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Synthesizes the perspectival gap. The safety_control_reading is a genuine coordination mechanism (safety standards have real benefits) layered with extraction (resource capture, capability restrictions, pathway lock-in for Global South). The classification reflects the real hybrid structure: not pure extraction, not pure coordination. The constraint's legitimacy rests on catastrophic tail risks that cannot be empirically falsified in advance.
constraint_indexing:constraint_classification(safety_control_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(safety_control_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(safety_control_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(safety_control_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(safety_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(safety_control_reading, TR),
    TR >= 0.70.

:- end_tests(safety_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The safety_control_reading channels substantial resources toward catastrophe-prevention research and capability restriction, diverted from near-term safety and development efficiency. The extraction is not maximized because safety standards do provide some coordination benefit (reduced liability, interoperability). Extraction has grown over the measured interval (0.32 → 0.58) as catastrophic risk framing has gained institutional traction and as frontier AI capabilities have outpaced safety investments. Suppression (0.62): High. Multiple mechanisms prevent exit or voice: Global South countries face institutional coupling via export controls and investment screening; near-term developers face regulatory penalties for non-compliance; frontier companies face liability and reputational cost for reduced safety commitment. The speculative nature of catastrophic risk prevents empirical falsification, making suppression of alternative framings self-justified ('we can't prove catastrophe won't happen, so we must restrict capabilities'). Theater ratio (0.65): Moderately high. Safety certification for unprecedented AI scenarios is inherently performative — regulators cannot actually verify prevention of catastrophic failure because the failure has not occurred and may involve multiple independent pathways. Red-team exercises, alignment audits, and safety certifications function as ritual demonstrations of prudence rather than empirical validation. Theater has increased as the need for safety mechanisms has grown while empirical verification has become harder.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. Safety institutions see rope (genuine coordination providing shared standards and liability reduction). Frontier companies see tangled rope (mixed coordination and extraction, but shaped standards favor them). Near-term developers see tangled rope (coordination benefits undercut by compliance costs). Global South sees snare (pure extraction with no exit). Future generations are treated as passive beneficiaries but cannot contest the framing. The regulatory apparatus sees piton (performative ritual with minimal empirical verification). The analytical observer sees tangled rope with asymmetric extraction concentrated on powerless actors. No perspective sees pure coordination, and no perspective aligns with the beneficiary perspective except safety institutions and frontier companies.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives. Future generations (powerless/trapped) experience maximum extraction: d ≈ 0.95, f(d) ≈ 1.42. Global South developers (powerless/trapped) experience high extraction: d ≈ 0.92, f(d) ≈ 1.33. Near-term developers (moderate/constrained) experience moderate extraction: d ≈ 0.65, f(d) ≈ 1.00. Safety institutions (institutional/arbitrage) experience negative effective extraction: d ≈ 0.05, f(d) ≈ -0.12 (beneficiaries). The analytical observer (analytical/analytical) derives d ≈ 0.72 from the mix of beneficiary and victim positions weighted across all other perspectives. The wide range in d values reflects the structural polarization of the constraint: few institutional beneficiaries, multiple powerless victims, high asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing genuine safety coordination (real function: shared standards reduce liability and interoperability risk) from extractive catastrophe framing (concentrated resources, capability restriction, pathway lock-in). The tangled rope classification reflects this hybrid: extractiveness 0.58 is high enough to register extraction but low enough to acknowledge the coordination benefit. If extractiveness were ≤0.35, the constraint would misclassify as pure rope, overlooking the real asymmetric burden on Global South and near-term developers. If extractiveness were ≥0.75, the constraint would misclassify as snare, overlooking the genuine coordination function. The tangled rope classification captures that both elements are structurally present: the coordination is real, and so is the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_probability_radical_uncertainty,
    'Is the probability of AI-induced catastrophic loss of control empirically estimable, or fundamentally unquantifiable due to the unprecedented nature of the scenario?',
    'Bayesian decomposition of catastrophe pathway; identification of which sub-claims are empirically grounded (capability growth rates, system complexity) vs. fundamentally speculative (goal misalignment, mesa-optimization); historical comparison to prior existential risk assessments (nuclear weapons, engineered pandemics) and track record of their accuracy',
    'If empirically estimable: safety control extractiveness is proportional to risk probability, and governance should scale extraction with confidence in estimates. If fundamentally unquantifiable: extractiveness is driven by precautionary principle (infinite expected harm × small probability), creating unbounded extraction justified only by prudence, not evidence. This drives classification direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_probability_radical_uncertainty, empirical, 'Whether catastrophe probability is estimable or fundamentally unknowable').

omega_variable(
    near_term_vs_long_term_resource_tradeoff,
    'Do resources deployed to prevent speculative future catastrophes have measurable opportunity cost in present-day AI safety (bias mitigation, fairness, privacy, near-term harms)?',
    'Comparative resource allocation analysis across safety research domains; correlation studies between funding for existential risk vs. near-term harms; longitudinal tracking of capability gains vs. safety investment ratios; measurement of capability-safety gap change over time under different resource allocation regimes',
    'If near-term harms are measurable and present-day resources are diverted to speculative futures: extraction from present-day victims is real and concentrated. Reclassify toward snare for near-term harm victims. If near-term and long-term safety are decoupled or if long-term investment produces spillover benefits: extraction is lower and coordination function is genuine. Affects victim set and suppression measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(near_term_vs_long_term_resource_tradeoff, empirical, 'Whether catastrophic-risk-focused resources displace near-term safety investment').

omega_variable(
    control_metaphor_vs_alignment_metaphor,
    'Is ''control'' (external constraint preventing misalignment) the right technical approach, or does it preclude ''alignment'' (internal value coherence) as a distinct alternative with different governance implications?',
    'Technical review of control-theoretic vs. alignment-based safety proposals; assessment of whether these framings are complementary or mutually exclusive in capability constraints; comparative analysis of governance requirements under each framing',
    'If control and alignment are complementary: this reading (safety_control_reading) coexists with alignment-focused readings. If control is technically incompatible with or actively prevents alignment: this reading forecloses alternative framings. This affects reading_relations classification and determines whether the kernel permits both readings within a single framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_metaphor_vs_alignment_metaphor, conceptual, 'Whether control and alignment are complementary or mutually exclusive approaches').

omega_variable(
    precautionary_principle_scope_creep,
    'Does the precautionary principle applied to AI catastrophe risk expand indefinitely as the set of possible failure modes is expanded, creating unbounded extraction justifiable only by prudence?',
    'Logical analysis of failure mode enumeration; identification of closure conditions (when enumeration stops); comparison of precautionary thresholds across domains (aviation, nuclear, biotech) and their empirical outcomes; assessment of whether safety_control_reading''s extraction is bounded or theoretically limitless',
    'If precautionary extraction is unbounded: the constraint''s suppression and extractiveness are not natural thresholds but governance choices. The constraint can expand indefinitely. If bounded: there are identifiable capacity limits. Affects whether tangled_rope classification is stable or drifts toward snare as extraction accumulates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precautionary_principle_scope_creep, conceptual, 'Whether precautionary principle creates unbounded extraction justification').

omega_variable(
    global_coordination_impossibility,
    'Can safety control standards be coordinated globally without either (a) powerful countries imposing unilateral standards that constrain weaker actors, or (b) reducing standards to the least common denominator, undermining safety?',
    'Game-theoretic analysis of incentives under different coordination structures; empirical review of prior international technology governance (nuclear, bio, chem); assessment of whether Global South participation in safety standard-setting is meaningfully present or performative',
    'If global coordination requires either unilateral imposition or lowest-common-denominator: safety_control_reading necessarily extracts from Global South or produces weak standards. Confirms snare classification for Global South victims. If meaningful coordination is possible: beneficiary and victim sets are less polarized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_coordination_impossibility, empirical, 'Whether global AI safety coordination is achievable without coercion or defection').

omega_variable(
    kernel_reading_disagreement_location,
    'Is the disagreement between safety_control_reading and ethics_justice_reading located in (a) empirical premises (AI risk magnitude), (b) normative priorities (catastrophe vs. present-day injustice), or (c) the kernel itself (what counts as ''alignment'' commitment)?',
    'Comparative analysis of sibling readings'' foundational axioms; identification of which axioms are shared and which are unique; assessment of whether a party could coherently hold both readings or whether they are logically exclusive',
    'If (a) empirical: readings coexist and can be reconciled by evidence. If (b) normative: readings coexist but reflect different value priorities. If (c) kernel: readings may foreclose each other. Determines reading_relations classification (coexists_with vs. forecloses).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Location of disagreement between safety_control and ethics_justice readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(safety_control_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(safety_ctrl_tr_t0, safety_control_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(safety_ctrl_tr_t5, safety_control_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(safety_ctrl_tr_t10, safety_control_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(safety_ctrl_be_t0, safety_control_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(safety_ctrl_be_t5, safety_control_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(safety_ctrl_be_t10, safety_control_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(safety_control_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(safety_control_reading, ai_capability_scaling).
narrative_ontology:affects_constraint(safety_control_reading, international_ai_governance).
narrative_ontology:affects_constraint(safety_control_reading, corporate_ai_research_incentives).

% DUAL FORMULATION NOTE:
% The safety_control_reading is one reading of a three-part kernel decomposition. Distinct constraint stories must be generated for ethics_justice_reading and integrated_reading with their own ε values, beneficiary/victim declarations, and perspectives. The safety_control_reading instantiates catastrophe-framed governance (high extractiveness from present-day agents); ethics_justice_reading instantiates harm-reduction governance (likely lower extractiveness, different victim set); integrated_reading attempts hybrid approach (mixed metrics). All three link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
