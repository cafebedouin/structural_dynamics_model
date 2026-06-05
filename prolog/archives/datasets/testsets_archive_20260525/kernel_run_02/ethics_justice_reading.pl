% ============================================================================
% CONSTRAINT STORY: ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ethics_justice_reading, []).

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
 *   constraint_id: ethics_justice_reading
 *   human_readable: Alignment-as-Ethics-Justice: Preventing Reproduction of Social Bias and Present-Day Harm in AI Systems
 *   domain: AI_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   The ethics-justice reading of AI alignment frames the alignment problem
 *   as preventing the reproduction of social bias and present-day harms in
 *   deployed AI systems. This reading prioritizes demonstrable current harms
 *   to marginalized communities — discriminatory hiring algorithms, biased
 *   predictive policing, inequitable loan and credit decisions, content
 *   moderation that silences marginalized voices — as the primary failure
 *   mode alignment must address. From this perspective, an AI system is
 *   'aligned' if it does not reproduce or amplify existing social
 *   stratification, and misalignment is measured by the scale and severity of
 *   bias-driven harms to vulnerable populations. This reading generates
 *   structural tension with other readings of the alignment commitment
 *   (safety-control reading focused on AGI existential risk; integrated
 *   reading attempting to balance both). The ethics-justice reading is one
 *   instantiation of the broader AI alignment kernel — a contested,
 *   non-decomposable commitment that different stakeholders read as mandating
 *   fundamentally different safety priorities and resource allocations.
 *
 * KEY AGENTS:
 *   - Marginalized Communities: Primary victims (powerless/trapped) — experience bias-driven harms across hiring, criminal justice, lending, content moderation; cannot exit these systems
 *   - Equity Advocacy Organizations: Primary beneficiaries (moderate/constrained) — gain policy influence, funding, institutional legitimacy from ethics-justice framing; also constrained by resource competition
 *   - AI Safety Researchers (embedded in ethics frame): Secondary beneficiaries (institutional/arbitrage) — safety research gains legitimacy, funding, policymaker access when aligned with justice frame
 *   - Long-Term Safety Research Agenda: Primary victim (organized/constrained) — access to funding, researcher attention, institutional prestige is diverted to present-day bias remediation; high suppression because public opposition to bias work is costly
 *   - Regulatory and Compliance Bodies: Implementing agents (organized/mobile) — build temporary scaffolding (fairness standards, algorithmic audits, bias detection); scaffold has sunset as technical standards mature
 *   - AI Organizations: Mixed (institutional/arbitrage) — adopt diversity initiatives and fairness frameworks as compliance and optics; often theater-heavy with minimal substantive change
 *   - Analytical Observer: Cross-position perspective (analytical/analytical) — recognizes constraint as genuine tangled rope (real coordination function AND asymmetric extraction), not false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ethics_justice_reading, 0.58).
domain_priors:suppression_score(ethics_justice_reading, 0.65).
domain_priors:theater_ratio(ethics_justice_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ethics_justice_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ethics_justice_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ethics_justice_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ethics_justice_reading, "Alignment-as-Ethics-Justice: Preventing Reproduction of Social Bias and Present-Day Harm in AI Systems").
narrative_ontology:topic_domain(ethics_justice_reading, "AI_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(ethics_justice_reading, distributed).
narrative_ontology:cs_authority_grounding(ethics_justice_reading, distributed).
narrative_ontology:cs_kernel_id(ethics_justice_reading, ai_alignment_commitment).
narrative_ontology:cs_reading_relation(ethics_justice_reading, safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation(ethics_justice_reading, integrated_reading, influences).
narrative_ontology:cs_axiom(ethics_justice_reading, foundational, present_day_bias_harm_immediate_priority).
narrative_ontology:cs_axiom_status(present_day_bias_harm_immediate_priority, holdable).
narrative_ontology:cs_axiom_grounding(ethics_justice_reading, present_day_bias_harm_immediate_priority, deontological).
narrative_ontology:cs_axiom(ethics_justice_reading, foundational, marginalized_community_voice_in_alignment_definition).
narrative_ontology:cs_axiom_status(marginalized_community_voice_in_alignment_definition, holdable).
narrative_ontology:cs_axiom_grounding(ethics_justice_reading, marginalized_community_voice_in_alignment_definition, conventional).
narrative_ontology:cs_reference_frame(ethics_justice_reading, justice_centered_technology_governance).
narrative_ontology:cs_drift_state(ethics_justice_reading, contemporary_ai_deployment_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ethics_justice_reading, marginalized_communities_at_risk_from_bias).
narrative_ontology:constraint_beneficiary(ethics_justice_reading, equity_advocates_and_civil_rights_organizations).
narrative_ontology:constraint_victim(ethics_justice_reading, long_term_ai_safety_research_agenda).
narrative_ontology:constraint_victim(ethics_justice_reading, capability_acceleration_and_deployment_timelines).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED COMMUNITIES (SNARE) — Face immediate harms from biased hiring algorithms, predictive policing, loan denial systems, and content moderation filters. Cannot exit these systems; harms accumulate during the biographical horizon. No alternative institutional pathways. Maximum extraction: present-day material and social damage.
constraint_indexing:constraint_classification(ethics_justice_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EQUITY ADVOCATES (TANGLED ROPE) — Benefit from alignment-as-ethics framing (strengthens their policy advocacy, secures funding, centers their constituencies). Also constrained by the bottleneck: resources devoted to bias auditing and fairness testing could redirect toward other equity work; timeline pressure to demonstrate bias in systems constrains deeper structural analysis. Mixed coordination (justice framework) and extraction (resource capture from broader AI governance).
constraint_indexing:constraint_classification(ethics_justice_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMBEDDED SAFETY RESEARCHERS (ROPE) — Safety researchers adopting ethics-justice framing gain legitimacy, institutional support, and funding. They experience the ethics frame as pure coordination: it makes their safety work legible to policymakers and publics. Net beneficiaries with arbitrage (can move between safety and ethics frames; profit from both).
constraint_indexing:constraint_classification(ethics_justice_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LONG-TERM SAFETY AGENDA (SNARE) — The safety research program focused on future-horizon risks (AGI alignment, scalable interpretability, long-term value learning) is materially constrained by the ethics-justice reading's demand for present-day harm remediation. Funding is diverted; researcher attention is allocated to bias auditing; institutional prestige flows toward ethics work. Organized resistance is weak because the ethical demands are morally legitimate. High suppression: cannot publicly argue against bias remediation without reputational damage.
constraint_indexing:constraint_classification(ethics_justice_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPLIANCE SCAFFOLDING (SCAFFOLD) — Regulatory emergence (EU AI Act, algorithmic impact assessments, fairness-by-design mandates) creates temporary compliance infrastructure. Structured sunset: as regulatory clarity increases and technical standards (fairness metrics, bias detection tools) mature, the scaffolding function declines — compliance becomes routine rather than requiring intensive governance. Organized actors with mobile exit (can adopt compliance or move to unregulated jurisdictions).
constraint_indexing:constraint_classification(ethics_justice_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: DIVERSITY THEATER (PITON) — Many AI organizations adopt diversity and inclusion initiatives framed as bias mitigation but functionally serving as symbolic compliance. Theater ratio high: programs are visible, performative, easy to announce, and hard to verify. Actual systemic bias remediation is minimal; the programs persist through institutional inertia and public pressure. They extract value (optics, recruitment) while delivering minimal substantive change.
constraint_indexing:constraint_classification(ethics_justice_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL (TANGLED ROPE) — From a civilizational perspective, this reading instantiates genuine coordination (justice framework for AI governance) AND asymmetric extraction (diversion of safety resources, subordination of long-term risk agendas to present-day equity demands). The constraint is not a false summit — the ethical demands are structurally real — but it is hybrid rather than pure coordination or pure safety.
constraint_indexing:constraint_classification(ethics_justice_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ethics_justice_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ethics_justice_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ethics_justice_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ethics_justice_reading, TR),
    TR >= 0.70.

:- end_tests(ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The ethics-justice reading exhibits significant extraction from the long-term safety research agenda. Resources, researcher attention, and institutional prestige flow toward present-day bias remediation and fairness engineering. The suppression mechanism is particularly potent: publicly opposing expanded bias work invites reputational damage and marginalizes safety researchers as indifferent to present-day suffering. The extraction is not total because embedded safety researchers successfully reframe their work as ethics-aligned, and some resource flows remain directed to long-term problems. The upward trend in measurements (t0: 0.35 → t10: 0.62) reflects consolidation of the ethics-justice frame in AI governance institutions. Suppression (0.65): High. Multiple mechanisms prevent alternative readings from fully articulating their positions: (1) moral legitimacy asymmetry — arguing against bias remediation appears to dismiss marginalized communities; (2) institutional capture — civil rights organizations and tech ethics initiatives set research agendas; (3) funding concentration — bias/fairness research receives elevated institutional support; (4) public discourse dominance — ethics-justice framing is more legible to policymakers than long-term safety concerns. Theater ratio (0.48): Moderate, but rising. Early adoption (t0: 0.32) centered on genuine bias detection and audit mechanisms with substantive impact. As the constraint has consolidated (t7-t10: 0.48-0.52), theater has increased — organizations adopt diversity programs, fairness initiatives, and algorithmic audits with visible but often minimal functional impact. The plateau suggests theater is stabilizing around a functional baseline, not declining toward full function.
 *
 * PERSPECTIVAL GAP:
 *   The ethics-justice reading produces sharp perspectival divergence. Marginalized communities experiencing bias see a Snare (no exit, maximum extraction). Long-term safety researchers see a Snare competing with them (constrained by the reading's demands). Equity advocates and embedded safety researchers see Rope or favorable Tangled Rope (genuine coordination with justice framework + some benefit). Regulatory bodies see Scaffold with meaningful sunset (standards will mature, compliance will routinize). The diversity theater perspective sees Piton (performative persistence without function). The analytical observer sees Tangled Rope (real coordination on justice + real extraction from alternative agendas). This perspectival gap is not a measurement ambiguity — it is a structural feature revealing that the ethics-justice reading is a genuine hybrid (not pure coordination, not pure extraction) and that it asymmetrically benefits different stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from the agent's structural relationship to THIS reading of alignment. Marginalized communities are primary victims (identity_locked or trapped by bias mechanisms) — d→0.95, high experienced extraction. Long-term safety researchers are secondary victims constrained by the reading's resource demands (organized but suppressed) — d→0.65, moderate-high experienced extraction. Equity advocates are beneficiaries with constrained exit (gain policy influence but cannot abandon the justice frame without loss of constituency) — d→0.35, low extraction. Embedded safety researchers are beneficiaries with arbitrage (can reframe safety as justice-aligned, gaining institutional legitimacy) — d→0.15, minimal experienced extraction. The scaffold perspective (regulatory agents) experiences d→0.50, symmetric (they both implement and navigate the constraint). The analytical observer experiences d→0.72 (analytical agents observing hybrid constraint experience moderate-high extraction because they must navigate the frame's constraints while maintaining analytical distance).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the kernel lens: the ethics-justice reading is neither simply 'good' (pure Rope) nor 'bad' (pure Snare), but a genuinely hybrid constraint that solves a collective action problem (justice in AI) while asymmetrically extracting from alternative research agendas. The mandatrophy paradox — 'if alignment prevents harm, why does it show extraction?' — dissolves when we recognize that 'alignment' is not a single constraint but a kernel with multiple readings, each reading as a distinct constraint with its own extractiveness. The ethics-justice reading is not extractive from the standpoint of justice (it genuinely coordinates marginalized communities' interests); it IS extractive from the standpoint of long-term safety research. Both are legitimate readings of the alignment commitment. The resolution is not to choose one but to recognize that the AI governance system must carry both constraints simultaneously, and that Mandatrophy (the impossibility of simultaneously optimizing for all readings) indicates genuine value pluralism rather than analytical confusion. Institutional design should reflect this: separate funding streams, protected research agendas, and institutional spaces where both readings maintain voice, rather than subordinating one to the other through false integration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_remediation_sufficiency,
    'Can present-day bias remediation (fairness metrics, bias detection, debiasing techniques) reduce harms to marginalized communities to acceptable thresholds, or is bias an inherent feature of scaled deployment in stratified societies?',
    'Longitudinal tracking of fairness metrics across deployment cycles; measurement of actual harm reduction vs. metric improvement; community satisfaction and self-reported impact studies',
    'If remediation sufficient: ethics-justice reading is sustainable (Rope becomes more Rope-like). If insufficient: ethics-justice reading becomes structural Snare diagnosis rather than actionable constraint (classification remains Tangled Rope, but with recognition that the underlying problem cannot be solved within the constraint''s frame).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bias_remediation_sufficiency, empirical, 'Whether fairness techniques can sufficiently reduce bias-driven harms').

omega_variable(
    resource_allocation_asymmetry,
    'What portion of AI governance resources flows to present-day bias remediation vs. long-term safety research, and is this allocation legitimate given the relative certainty and scale of harms?',
    'Funding audit across AI safety research groups, AI ethics initiatives, and regulatory bodies; comparison of harms prevented (present) vs. harms risked (future); stakeholder impact assessment',
    'If allocation is disproportionate to harm likelihood and scale: extraction is confirmed, suggesting the safety research constraint should be reclassified upward in severity. If allocation reflects proportionate harm weighting: tangled rope classification stands; both agendas operate at justified scales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_asymmetry, empirical, 'Whether resource flows to ethics-justice reflect proportionate harm weighting').

omega_variable(
    alignment_reading_committer_structure,
    'Is the ethics-justice reading of AI alignment a genuinely distinct normative commitment, or is it a rhetorical reframing of safety research designed to capture resources and legitimacy?',
    'Textual and institutional analysis: Do ethics-justice advocates maintain positions and resource allocations inconsistent with safety maximization when the two diverge? Do they accept constraints (e.g., deployment delays for safety verification) that conflict with immediate bias mitigation?',
    'If genuinely distinct commitment: reading_relations correctly declare coexists_with or influences. If rhetorical frame: relation should be forecloses or more aggressive. This resolves the kernel-level ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_reading_committer_structure, conceptual, 'Whether ethics-justice is distinct normative commitment vs. rhetorical capture').

omega_variable(
    safety_research_temporal_mismatch,
    'If long-term safety problems (AGI alignment, scalable interpretability) will not be solved in the biographical time horizon, does present-day bias remediation create a false dichotomy, or is it a legitimate re-prioritization?',
    'Verification of whether long-term safety research has plausible paths to maturation before critical deployment timelines; assessment of whether bias remediation accelerates or delays safety research maturation',
    'If safety research has realistic near-term maturation: present-day bias work may be extraction (diverts resources from critical path). If safety research is longer-horizon: bias work is independent and legitimate (two distinct agendas on different timescales).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_research_temporal_mismatch, empirical, 'Temporal mismatch between present-day bias remediation and long-term safety research maturation').

omega_variable(
    marginalized_community_voice_representation,
    'Are marginalized communities experiencing AI harms given structural power in the ethics-justice reading''s implementation, or are their interests represented through proxy advocates (equity organizations, researchers)?',
    'Governance analysis: Who sits on AI ethics boards and fairness research teams? Do funding and agenda-setting mechanisms include direct community voice? Do community preferences diverge from advocate-determined priorities?',
    'If genuine community power: the victims designation is accurate and communities are not doubly victimized (first by bias, then by exclusion from remedy design). If proxy-only: the constraint exhibits an additional layer of extraction (community voice capture by institutional advocates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_community_voice_representation, empirical, 'Whether marginalized communities have structural power in ethics-justice implementation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ethics_justice_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eth_just_theater_t0, ethics_justice_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(eth_just_theater_t3, ethics_justice_reading, theater_ratio, 3, 0.4).
narrative_ontology:measurement(eth_just_theater_t7, ethics_justice_reading, theater_ratio, 7, 0.48).
narrative_ontology:measurement(eth_just_theater_t10, ethics_justice_reading, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(eth_just_extract_t0, ethics_justice_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eth_just_extract_t3, ethics_justice_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(eth_just_extract_t7, ethics_justice_reading, base_extractiveness, 7, 0.58).
narrative_ontology:measurement(eth_just_extract_t10, ethics_justice_reading, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ethics_justice_reading, identity_coordination).
narrative_ontology:affects_constraint(ethics_justice_reading, safety_control_reading).
narrative_ontology:affects_constraint(ethics_justice_reading, integrated_reading).
narrative_ontology:affects_constraint(ethics_justice_reading, ai_capability_deployment_timelines).
narrative_ontology:affects_constraint(ethics_justice_reading, algorithmic_fairness_metrics).

% DUAL FORMULATION NOTE:
% This constraint is part of the AI_ALIGNMENT_COMMITMENT kernel family. The ethics-justice-reading is one of three competing readings (alongside safety-control-reading and integrated-reading), each with distinct ε values, victim/beneficiary structures, and institutional instantiations. All three affect downstream constraints (deployment timelines, fairness metrics, safety verification standards). The readings are linked not as a decomposition (where structurally distinct claims are separated) but as a kernel (where a single persisting commitment is read multiple ways by different communities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ethics_justice_reading, powerless, 0.95).
constraint_indexing:directionality_override(ethics_justice_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
