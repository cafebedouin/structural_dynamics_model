% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: AI Alignment as Ethics-Justice Commitment (Bias/Harm Prevention Reading)
 *   domain: AI_governance/technology_ethics/algorithmic_justice
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'ai_alignment_commitment': alignment as prevention of social bias and
 *   present-day harm in AI systems, with primary focus on marginalized
 *   communities experiencing documented discrimination through algorithmic
 *   decision-making. This reading prioritizes concrete, measurable harms
 *   (loan denial, criminal risk assessment bias, hiring discrimination,
 *   surveillance targeting) over hypothetical future risks. The structural
 *   data reveals a tangled rope: genuine coordination function (deploying
 *   less-biased systems does reduce certain categories of harm), but
 *   accompanied by asymmetric extraction (AI capability labs extract
 *   compliance legitimacy without fundamental design changes; safety
 *   researchers experience identity-lock preventing adoption of the justice
 *   framing). Theater ratio rising from 0.28 to 0.55 indicates growing
 *   performative layer (diversity metrics, fairness audits, ethics review
 *   boards) increasingly disconnected from actual deployment outcomes. The
 *   ethics-justice reading competes with at least two sibling readings:
 *   safety_control_reading (alignment as preventing catastrophic loss of
 *   control) and integrated_reading (alignment requires simultaneous
 *   attention to control and justice problems). Each reading has distinct
 *   beneficiary/victim sets, different temporal horizons, and different
 *   extractiveness profiles. This analysis treats ethics_justice_reading as a
 *   single constraint with stable ε (not as a collapsed average over
 *   readings), per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - Marginalized Communities: Primary victims (powerless/trapped) — experience concrete algorithmic harms with no meaningful appeal or design recourse
 *   - Civil Rights and Social Justice Organizations: Secondary beneficiaries (organized/constrained) — gain regulatory legitimacy and coalition capacity through ethics-justice framing, though constrained by resource limits
 *   - AI Ethics Industry: Primary beneficiary (institutional/arbitrage) — careers, funding, institutional authority flow from ethics-as-alignment positioning; high arbitrage capacity
 *   - AI Capability Labs: Dual role (powerful/constrained) — constrained by regulatory/reputational requirement to audit systems but extract legitimacy and avoid fundamental design changes through compliance theater
 *   - Long-Term Safety Research: Primary victim (analytical/identity_locked) — structurally mobile but identity-fused with control-focused framing; cannot adopt justice framing without identity dissolution; experiences extraction through resource reallocation and legitimacy erosion
 *   - D&I Compliance Function: Performative institution (institutional/arbitrage) — maintains fairness metrics and review boards increasingly disconnected from deployment outcomes; theater ratio rising
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.58).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.68).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "AI Alignment as Ethics-Justice Commitment (Bias/Harm Prevention Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "AI_governance/technology_ethics/algorithmic_justice").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, '38fc9dc1-ead4-4989-b6f7-8d76f1f0c458').
narrative_ontology:cs_kernel_codification('38fc9dc1-ead4-4989-b6f7-8d76f1f0c458', distributed).
narrative_ontology:cs_authority_grounding('38fc9dc1-ead4-4989-b6f7-8d76f1f0c458', distributed).
narrative_ontology:cs_reading_relation('38fc9dc1-ead4-4989-b6f7-8d76f1f0c458', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('38fc9dc1-ead4-4989-b6f7-8d76f1f0c458', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('38fc9dc1-ead4-4989-b6f7-8d76f1f0c458', foundational, present_day_harm_primacy).
narrative_ontology:cs_axiom_status(present_day_harm_primacy, holdable).
narrative_ontology:cs_axiom_grounding('38fc9dc1-ead4-4989-b6f7-8d76f1f0c458', present_day_harm_primacy, deontological).
narrative_ontology:cs_axiom('38fc9dc1-ead4-4989-b6f7-8d76f1f0c458', foundational, marginalized_community_epistemic_authority).
narrative_ontology:cs_axiom_status(marginalized_community_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('38fc9dc1-ead4-4989-b6f7-8d76f1f0c458', marginalized_community_epistemic_authority, deontological).
narrative_ontology:cs_reference_frame('38fc9dc1-ead4-4989-b6f7-8d76f1f0c458', justice_first_alignment_paradigm).
narrative_ontology:cs_drift_state('38fc9dc1-ead4-4989-b6f7-8d76f1f0c458', contemporary_ai_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('38fc9dc1-ead4-4989-b6f7-8d76f1f0c458', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, affected_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_research).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, frontier_capabilities_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED COMMUNITIES (SNARE) — Trapped within deployment of biased systems with no meaningful exit or appeal mechanism. Face concrete, documented harms: loan denial, criminal risk assessment inflation, hiring discrimination, surveillance targeting. No recourse to design decisions; zero degrees of freedom. Maximum structural extraction with minimal perception of coordination benefit.
constraint_indexing:constraint_classification(ai_alignment_commitment__ethics_justice_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVIL RIGHTS ORGS (TANGLED ROPE) — Constrained by limited resources and political power to enforce algorithmic audits. But also benefit from the alignment-as-ethics framing: legitimizes their accountability demands, enables coalition-building with AI researchers, creates regulatory foothold (disparate impact doctrine, algorithmic impact assessments). Mixed extraction and coordination within the constraint itself.
constraint_indexing:constraint_classification(ai_alignment_commitment__ethics_justice_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AI ETHICS INDUSTRY (ROPE) — Experiences constraint as coordination. The ethics-justice framing legitimizes ethics research funding, creates career pathways, establishes institutional accountability requirements. Benefits from first-mover advantage in ethics-as-alignment positioning. Low extraction — beneficiary position with high arbitrage capacity (can shift focus if external incentives change).
constraint_indexing:constraint_classification(ai_alignment_commitment__ethics_justice_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AI CAPABILITY LABS (TANGLED ROPE) — Constrained by regulatory requirement and reputational risk to audit systems for bias. But extract resources and legitimacy from this constraint: bias auditing becomes marketing narrative ('trustworthy AI'), enables continued deployment without fundamental design changes, converts justice demands into compliance theater. Significant suppression of alternative framings (e.g., capability moratoria).
constraint_indexing:constraint_classification(ai_alignment_commitment__ethics_justice_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LONG-TERM SAFETY RESEARCH (SNARE / IDENTITY_LOCKED) — Structurally mobile (can pursue alternatives; not physically trapped) but identity-fused with the alignment-as-control framing. Safety researchers' professional identity is constituted through catastrophic-risk thinking. Adoption of ethics-justice framing threatens identity because it reframes their core research agenda as secondary or insufficient. Analytically capable of seeing the structural shift but cannot exercise it without becoming 'not a safety researcher.' Extracted through resource reallocation and legitimacy erosion — not by coercion but by identity-lock preventing them from embracing the alternative framing even when analytically aware of its structural validity.
constraint_indexing:constraint_classification(ai_alignment_commitment__ethics_justice_reading, snare,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: D&I COMPLIANCE (PITON) — The ethics-justice framing has accumulated a substantial performative layer: diversity requirements in training data, fairness metrics in evaluation rubrics, ethics review boards. Much of this theater is disconnected from actual design decisions and deployment outcomes. Theater ratio is moderate (0.55) but rising: the ritual of fairness auditing persists even when findings are ignored or findings are structurally insufficient to prevent documented harms. Institutional inertia maintains this layer despite acknowledged gap between metrics and real-world outcomes.
constraint_indexing:constraint_classification(ai_alignment_commitment__ethics_justice_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational perspective, the logic appears: any system reflecting historical data will reproduce historical bias; this is an immutable property of how machine learning works. The constraint appears as a law of statistical mechanics rather than a social choice. However, the structural data contradicts mountain classification: identifiable beneficiaries (AI labs avoiding design costs via compliance theater), identifiable victims (marginalized communities), asymmetric extraction. The 'laws of ML' framing naturalizes contingent technical choices (training data selection, loss function design, deployment context).
constraint_indexing:constraint_classification(ai_alignment_commitment__ethics_justice_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_alignment_commitment__ethics_justice_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_alignment_commitment__ethics_justice_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, TR),
    TR >= 0.70.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, rising): Moderate-high. The constraint forces resource reallocation (ethics teams, fairness audits, data scrutiny) without necessarily achieving proportionate harm reduction. Capability labs can satisfy the constraint through compliance theater (fairness metrics, diverse training data) while maintaining deployment trajectories unchanged. Long-term safety research experiences extraction through legitimacy erosion and funding reallocation to ethics initiatives. However, extractiveness is not snare-level (0.72+) because the constraint does produce real harm-reduction outcomes for some deployment contexts and some marginalized groups—the coordination function is genuine, not purely theatrical. Suppression (0.68, rising): Moderate-high. Strong suppression of alternative framings: capability moratoria, architectural limitations on deployment scope, pre-deployment human evaluation, meaningful community consent. The ethics-justice framing positions these alternatives as impediments to beneficial deployment rather than as legitimate risk-mitigation strategies. Theater ratio (0.55, rising): Moderate. Rising trajectory reflects accumulation of performative layer (fairness metrics loosely coupled to actual deployment decisions; ethics review boards that do not block deployment; diversity requirements that do not address systemic bias in training data or deployment context). At t=0 (early ethics framing), theater was lower because ethics requirements had real design consequences. As the constraint was institutionalized, theater rose: compliance mechanisms decoupled from actual outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. The marginalized communities see a snare: trapped, no appeal, no design recourse. The capability labs see rope: coordinating legitimate harm-reduction with acceptable compliance cost. The ethics industry sees rope: coordination function, career pathways, institutional authority. Safety researchers see snare: their identity-constituting research is being displaced; they are trapped by identity-lock preventing alternative framing adoption. Civil rights organizations see tangled rope: genuine coordination (ethics framing legitimizes their demands) mixed with extraction (labs can satisfy ethics requirements without meaningful design change). The D&I compliance function sees piton: performs fairness ritual increasingly disconnected from actual outcomes. The civilizational analytical observer risks mountain classification (justice is inherent requirement of any learning system reflecting historical data) but this is false summit—the constraint is fundamentally about resource allocation and interpretive authority, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status and exit options. Marginalized communities: victim status + trapped exit → d≈0.95 → high f(d) → high experienced extractiveness. Safety researchers: victim status + identity_locked exit → d≈0.89 → high f(d) but with cognitive barrier component. AI ethics industry: beneficiary status + arbitrage exit → d≈0.05 → negative f(d) → low or negative experienced extractiveness (institutional subsidy). Capability labs: mixed (beneficiary of compliance framework + constrained exit) → d≈0.45 → moderate f(d). The perspectival gap between beneficiary labs (rope/low extraction) and victim communities (snare/high extraction) is the diagnostic signal: same structural constraint, opposite experienced extractiveness, driven by exit asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution is: this constraint is genuinely tangled rope with rising theater, not a choice between pure coordination and pure extraction. The coordination function (reducing algorithmic bias does prevent concrete documented harms) is real. The extraction function (compliance theater enables continued high-impact deployment without design change) is also real. Neither can be dismissed. The constraint's evolution (rising theater, rising extractiveness, rising suppression) traces an institutional maturation pattern: early ethics framing had high friction (real design consequences); later institutionalization converted ethics into compliance mechanism (low friction, disconnected from deployment outcomes). The mandatrophy does not resolve by choosing one perspective. It resolves by recognizing that the constraint serves simultaneous coordination and extraction functions, and the ratio is shifting toward extraction as the constraint ages and institutionalizes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_definition_instability,
    'Does ''bias'' in the ethics-justice reading refer to statistical disparity, historical injustice, or deviation from a normative standard of fairness—and do these three definitions point to the same constraint or three different ones?',
    'Specify which forms of disparity trigger alignment responsibility: (1) any measurable difference in error rates across demographic groups? (2) disparity traceable to historical discrimination in training data? (3) disparity that violates explicit fairness axioms? Different thresholds activate different constraints.',
    'If definition 1 (any disparity): many benign systems misclassified as misaligned; extractiveness inflates. If definition 2 (historical injustice): extractiveness applies only to systems trained on unjust data; responsibility becomes contingent on data provenance, not deployment outcomes. If definition 3 (normative fairness): responsibility is framework-dependent; competing fairness axioms generate incompatible classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bias_definition_instability, conceptual, 'Whether ''bias'' refers to statistical disparity, historical injustice, or normative fairness deviation').

omega_variable(
    harm_baseline_underspecification,
    'Against what baseline do we measure ''prevention of present-day harm''? Against the status quo pre-AI deployment? Against an idealized fair system? Against what the system would have done without alignment interventions?',
    'Explicit specification of the counterfactual baseline and measurement protocol. Compare: (a) AI system outcomes vs. human decision-maker outcomes in same context; (b) current deployment vs. pre-deployment status quo; (c) current deployment vs. explicitly debiased system; (d) actual system vs. system without any fairness constraints.',
    'Baseline choice determines whether the constraint is achievable. If baseline is (b) status quo, many AI systems reduce harm despite containing bias. If baseline is (c) explicitly debiased, extractiveness rises sharply—almost no deployed system meets it. If baseline is (d) unconstrained, almost all safety measures show as harm-reduction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_baseline_underspecification, conceptual, 'Counterfactual baseline for measuring ''prevention of present-day harm''').

omega_variable(
    marginal_vs_systemic_attribution,
    'Is the constraint responsible for bias/harm that the AI system introduces net new, or for bias/harm that the system reproduces or amplifies from systemic sources outside the system''s control?',
    'Decompose observed disparity into: (1) disparity introduced by the system (marginal harm); (2) disparity in system that reflects disparity in its inputs (systemic reproduction); (3) disparity that the system amplifies through feedback or at-scale deployment. Measure causal contribution of the system vs. upstream sources.',
    'If responsibility includes (2) and (3): extractiveness and suppression rise sharply—the system becomes responsible for addressing structural injustice, not just its own algorithmic choices. If responsibility limited to (1): extractiveness drops—many systems that reproduce bias incur no alignment cost because they add no novel harm. This distinction determines whether alignment-as-ethics is achievable or requires wholesale social restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_vs_systemic_attribution, empirical, 'Whether constraint applies to novel harm, reproduction, or amplification of systemic bias').

omega_variable(
    reading_kernel_contest,
    'This constraint is one reading of the contested kernel ''ai_alignment_commitment.'' Do the readings ethics_justice_reading, safety_control_reading, and integrated_reading describe structurally distinct constraints, or are they different framings of a single constraint?',
    'Comparison of ε values, beneficiary/victim sets, and temporal horizons across readings. If ε values differ by >0.20 points, or beneficiary/victim sets are disjoint, the readings instantiate different constraints. If ε values overlap and beneficiary sets intersect, the readings are competing framings of the same constraint.',
    'If distinct constraints: each has its own type, its own perspectives, its own lifecycle. The kernel is a genuine fork, not a perspectival ambiguity. If competing framings: the kernel is under-determined by structural data; the contest is about interpretive authority. This determines whether the engine classifies this as a constraint family (separate stories) or as a single constraint with contested readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Whether AI alignment kernel readings are structurally distinct constraints or competing framings').

omega_variable(
    safety_research_extraction_causality,
    'Does the adoption of ethics-justice framing as primary alignment goal causally reduce long-term safety research, or does research reduction follow from other factors (funding scarcity, career incentives, technical difficulty)?',
    'Comparative analysis: research funding allocation to safety vs. ethics over time; career trajectory data for safety researchers adopting vs. rejecting ethics framing; counterfactual: if safety framing remained dominant, would research volume differ?',
    'If causal relationship is strong: the constraint extraction from safety research is real and measurable. If weak or confounded by other factors: the victim designation is partially incorrect; extractiveness drops. This affects whether the snare classification for long-term safety research is accurate or whether it should be rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_research_extraction_causality, empirical, 'Whether ethics-justice framing causally reduces long-term safety research volume').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(align_ej_theater_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(align_ej_theater_t3, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 3, 0.42).
narrative_ontology:measurement(align_ej_theater_t6, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(align_ej_extractiveness_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(align_ej_extractiveness_t3, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(align_ej_extractiveness_t6, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(align_ej_suppression_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(align_ej_suppression_t3, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(align_ej_suppression_t6, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, training_data_historical_injustice).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, algorithmic_accountability_theater).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, marginalized_community_ai_governance_power).

% DUAL FORMULATION NOTE:
% The ai_alignment_commitment kernel decomposes into three structurally distinct constraints with different ε values and beneficiary/victim sets. The ethics_justice_reading (this story) has ε≈0.58, focuses on present-day harm in marginalized communities, and positions long-term safety research as a victim. The safety_control_reading would have lower ε (more pure coordination, less extraction) and position catastrophic risk prevention as the primary goal. The integrated_reading would attempt to reconcile both but would show ε between the two extremes. Each story has its own perspectives and measurements. They are linked as a constraint family: each reading is downstream of the kernel and upstream of domain-specific implementations (training data audits, algorithmic accountability mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__ethics_justice_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
