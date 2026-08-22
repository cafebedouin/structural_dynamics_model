% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance Priority — Near-Term Harms Reading
 *   domain: technology/policy/ethics
 *
 * SUMMARY:
 *   This story instantiates the near-term-harms reading of the contested AI
 *   risk governance priority kernel: the claim that governance attention,
 *   funding, and enforcement capacity should be directed at demonstrated
 *   present harms — algorithmic bias, misinformation, labor displacement,
 *   surveillance — rather than at speculative existential-risk scenarios.
 *   Under this reading's own account, the existential-risk framing functions
 *   as a beneficiary structure for the technology companies whose deployed
 *   systems cause present harm: by funding and amplifying long-horizon
 *   catastrophic-risk discourse, they help direct regulatory bandwidth away
 *   from binding present-harm regulation. The victims under this reading are
 *   populations already experiencing measurable algorithmic harm — Global
 *   South communities, algorithmically marginalized groups, and displaced
 *   workers — who bear costs now while attention is captured by scenarios
 *   that may never materialize. This is a single, ε-invariant reading: it
 *   does not adjudicate whether existential risk is real or important, only
 *   what the governance-priority claim looks like when the referent is
 *   present deployment harm assessed by this reading's own lights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.52).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance Priority — Near-Term Harms Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "technology/policy/ethics").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '5c632765-2fff-4dac-871c-2576b3c0c4f0').
narrative_ontology:cs_kernel_codification('5c632765-2fff-4dac-871c-2576b3c0c4f0', distributed).
narrative_ontology:cs_authority_grounding('5c632765-2fff-4dac-871c-2576b3c0c4f0', distributed).
narrative_ontology:cs_reading_relation('5c632765-2fff-4dac-871c-2576b3c0c4f0', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c632765-2fff-4dac-871c-2576b3c0c4f0', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('5c632765-2fff-4dac-871c-2576b3c0c4f0', foundational, recoverable_harm_still_demands_priority).
narrative_ontology:cs_axiom_status(recoverable_harm_still_demands_priority, holdable).
narrative_ontology:cs_axiom_grounding('5c632765-2fff-4dac-871c-2576b3c0c4f0', recoverable_harm_still_demands_priority, deontological).
narrative_ontology:cs_axiom('5c632765-2fff-4dac-871c-2576b3c0c4f0', foundational, xrisk_framing_functions_as_attention_capture).
narrative_ontology:cs_axiom_status(xrisk_framing_functions_as_attention_capture, holdable).
narrative_ontology:cs_axiom_grounding('5c632765-2fff-4dac-871c-2576b3c0c4f0', xrisk_framing_functions_as_attention_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('5c632765-2fff-4dac-871c-2576b3c0c4f0', documented_present_harm_baseline).
narrative_ontology:cs_drift_state('5c632765-2fff-4dac-871c-2576b3c0c4f0', generative_ai_mass_deployment_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5c632765-2fff-4dac-871c-2576b3c0c4f0', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, existential_risk_research_institutes).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, algorithmically_marginalized_groups).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, automation_displaced_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploys AI systems at scale into hiring, lending, content moderation, and surveillance contexts while funding and amplifying existential-risk discourse that draws regulatory and media attention toward speculative superintelligence scenarios. Faces comparatively little binding regulation of present-day deployment harms, and can absorb or litigate the fairness-audit compliance costs that do exist far more easily than it could absorb hard deployment restrictions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_companies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, technology_companies, agenda_setter).

% Receives substantial philanthropic and corporate funding to study long-horizon catastrophic AI scenarios. Benefits from public and legislative attention remaining fixed on frontier-model safety rather than on present-harm regulation, which would compete for the same funding and legislative bandwidth.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, existential_risk_research_institutes, beneficiary,
    organized, civilizational, mobile, global).

% Subject to AI-driven credit scoring, welfare eligibility screening, and content moderation systems trained predominantly on data and norms from wealthier markets, producing systematic misclassification and exclusion. Has essentially no access to the regulatory bodies debating AI governance priorities and no exit from systems increasingly embedded in state and financial infrastructure.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, immediate, trapped, global).

% Experiences documented disparate outcomes from facial recognition misidentification, biased hiring screens, and predictive policing tools. Can pursue individual legal remedies in some jurisdictions but bears the burden of proof and litigation cost, and has limited capacity to alter the systems' training or deployment before harm occurs.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, algorithmically_marginalized_groups, payer,
    powerless, immediate, constrained, national).

% Loses employment or bargaining position as generative and automation tools are deployed faster than retraining or labor-market adjustment programs can absorb. Organized labor has some voice through unions, but displacement decisions are made unilaterally by employers and platform operators.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, automation_displaced_workers, payer,
    moderate, biographical, constrained, national).

% Civil-society organizations, algorithmic-justice researchers, and labor advocates who author fairness audit standards, push for bias-mitigation regulation, and compete for legislative attention against the better-funded existential-risk framing. Administers the reading's proposed regulatory apparatus but does not control the deployment decisions it targets.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, near_term_harms_advocacy_coalition, agenda_setter,
    organized, generational, constrained, global).

% Hold that catastrophic and irreversible risks warrant priority regardless of present harm severity, since present harms are recoverable and extinction-level outcomes are not. Under this reading their concern is treated as a resource-diverting distraction rather than a legitimate governance priority, though the reading does not deny their sincerity.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, existential_risk_advocates, excluded,
    organized, civilizational, analytical, global).

% Allocate legislative attention, funding, and enforcement capacity between competing AI governance framings. Take testimony from advocacy coalitions, industry, and researchers, and their allocation decisions determine which harms receive binding regulatory frameworks.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, policy_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Directs finite regulatory attention, research funding, and enforcement capacity toward auditing and mitigating AI harms that are already measurable and occurring in deployed systems, rather than toward speculative future scenarios — solving the real problem that governance bandwidth is scarce and must be triaged somehow.
% TRANSFER_FUNCTION: Under this reading's own account, attention and remedial resources that would otherwise track present, measurable harm are instead captured by existential-risk framing funded and amplified by the companies whose present deployments cause that harm — moving regulatory bandwidth from marginalized populations bearing documented costs now to speculative-scenario research communities and the firms that fund them.
% ABSENT_VOICES: Global South populations subject to AI-driven credit, welfare, and moderation decisions have essentially no seat in the governance bodies setting these priorities; existential-risk advocates are present in the debate but, under this reading, are treated as diverting attention rather than contributing to it.
% DISAPPEARANCE_RATIONALE: If this priority-claim vanished and existential-risk framing captured governance bandwidth unopposed, fairness-audit mandates, algorithmic-discrimination litigation support, and labor-transition programs currently being fought for would lose their primary advocacy vehicle, and enforcement resources would concentrate further on frontier-model safety research with no binding requirements on current deployment.
% FOUNDING_PROBLEM: Documented, present-tense AI harms — biased hiring and lending algorithms, discriminatory facial recognition, unmoderated misinformation at scale, and abrupt labor displacement — were occurring in deployed systems while public and legislative attention concentrated on distant hypothetical superintelligence scenarios.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic algorithmic-audit studies (not funded by the advocacy coalition itself), national labor-statistics agencies documenting displacement, and international human-rights bodies reporting on Global South algorithmic harms corroborate that the present-harm problem remains unresolved and is not merely a claim of the coalition that benefits from prioritizing it.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68 by interval end) because, under this reading, attention capture by existential-risk framing constitutes a real transfer of scarce regulatory and philanthropic resources away from populations bearing documented harm now. Suppression is moderate (0.52): the mechanism is not primarily coercive but works through legislative agenda-setting, funding allocation, and media salience — softer than direct suppression but still structurally effective at keeping present-harm regulation underdeveloped. Theater ratio is moderate and rising (0.30 to 0.44) reflecting that fairness-audit and bias-mitigation compliance activity has grown but a rising share is procedural (audits published, frameworks announced) without binding deployment restriction, a Goodhart-style substitution risk internal to the near-term coalition's own remedial apparatus.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology companies are the structural beneficiary: they deploy the harm-causing systems and benefit from the attention-diversion effect of existential-risk framing they help fund, giving them low derived directionality despite formally being subject to whatever regulation eventually emerges. Existential-risk research institutes are a secondary beneficiary under this reading's own accounting because they compete for and often win funding and legislative bandwidth that would otherwise flow to present-harm remediation. The three victim groups are structurally trapped or constrained — Global South populations have essentially no access to the governance conversation, algorithmically marginalized groups bear individual litigation burdens, and displaced workers have some organized voice but no control over deployment timing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — present, measurable AI harm to populations with little governance voice — remains live and independently corroborated outside the advocacy coalition itself, which is precisely what distinguishes this from a self-serving mandate: the coalition administering fairness-audit and bias-mitigation frameworks is not the entity best positioned to declare its own necessity, so corroboration is drawn from independent academic audits, labor statistics, and human-rights reporting bodies. This blocks the mislabeling risk in both directions: it prevents the near-term-harms priority from being dismissed as mere advocacy self-interest, while the authored tangled_rope classification (rather than a clean rope) honestly registers that the reading's own remedial apparatus carries theater risk and imperfect enforcement leverage over the companies causing the harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_capture_causal_mechanism,
    'Does existential-risk research funding by technology companies causally displace present-harm regulatory attention, or do the two research and policy communities draw on largely separate funding and legislative pools such that no real transfer occurs?',
    'Comparative analysis of legislative committee time, philanthropic funding allocation, and media coverage volume across the two framings over a fixed period, controlling for total AI-governance attention growth (i.e., is this zero-sum or is the pie also growing).',
    'If the pools are genuinely separate and the pie is growing, the extractiveness attributed to attention capture is substantially overstated and the constraint is closer to a rope (parallel coordination efforts) than a tangled_rope. If capture is real, the tangled_rope reading with technology companies as beneficiary is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_capture_causal_mechanism, empirical, 'Whether existential-risk framing actually displaces present-harm regulatory bandwidth or merely coexists with it.').

omega_variable(
    sincere_belief_vs_strategic_deflection,
    'Is technology-company funding of existential-risk research primarily sincere concern, reputational hedging, or deliberate strategic deflection from present-harm accountability?',
    'Internal document discovery (litigation, leaks, regulatory subpoena), comparison of public statements to internal risk-prioritization memos, and tracking whether companies simultaneously lobby against present-harm regulation while funding x-risk research.',
    'Deliberate strategic deflection would strengthen the beneficiary classification of technology companies and support a snare-leaning reading of the attention-capture mechanism; sincere but reputationally convenient concern would soften the extraction claim without eliminating the structural effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sincere_belief_vs_strategic_deflection, conceptual, 'Whether the beneficiary relationship is intentional strategy or an emergent structural effect of sincere but convenient funding choices.').

omega_variable(
    kernel_framing_selection,
    'Is the choice to treat near-term-harms and existential-risk priorities as competing for a fixed governance-attention budget (this reading''s premise) the correct framing, or does the bridge_reading''s premise of structural entanglement better describe the actual policy landscape?',
    'Track whether jurisdictions that adopt unified AI governance frameworks (addressing both present harms and frontier-model risk under one regulatory umbrella) show measurably better outcomes on present-harm metrics than jurisdictions that adopt a strict priority ordering.',
    'If unified frameworks perform as well or better on present-harm metrics, this reading''s zero-sum premise is undermined and the bridge_reading becomes the better-supported structural account; if priority-ordering jurisdictions show stronger present-harm outcomes, this reading''s premise is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether the either/or resource-competition framing this reading depends on is itself the correct structural description of AI governance attention allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 4, 0.43).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kernel ai_risk_governance_priority. existential_risk_reading inverts this story's beneficiary/victim structure, treating near-term-harms prioritization as the attention-diverting move away from catastrophic-risk prevention. bridge_reading rejects the zero-sum premise both single-priority readings share, treating present harm and existential risk as structurally entangled rather than competing. Each reading carries its own ε, beneficiary/victim structure, and claimed_type; they are linked here for contamination and drift propagation analysis, not averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
