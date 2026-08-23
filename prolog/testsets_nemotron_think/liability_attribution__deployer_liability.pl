% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__deployer_liability, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Deployer-Primary Liability Rule for AI Systems
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint story captures the deployer-liability reading of the
 *   contested liability_attribution kernel. Under this reading, legal and
 *   regulatory frameworks assign primary liability for AI harms to the
 *   deploying entity — the organization that puts an AI system into operation
 *   in a specific context. The rationale is that deployers control the
 *   deployment context, make the go/no-go decision, and are best positioned
 *   to implement safeguards. However, the structural analysis reveals that
 *   modern foundation models are opaque, their failure modes are not fully
 *   knowable even to developers, and deployers (especially SMEs) cannot
 *   meaningfully perform due diligence on upstream model behavior. The
 *   constraint thus operates as a coordination mechanism (clear liability
 *   assignment) with a substantial extractive component: deployers bear costs
 *   for risks they cannot control, while foundation model providers capture
 *   the value of deployment without commensurate liability exposure. The
 *   engine will compute per-seat classifications from the structural data
 *   authored here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.72).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.78).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.72).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer-Primary Liability Rule for AI Systems").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, '480e3896-1bba-462e-9914-b6b167daf68a').
narrative_ontology:cs_kernel_codification('480e3896-1bba-462e-9914-b6b167daf68a', formalized).
narrative_ontology:cs_authority_grounding('480e3896-1bba-462e-9914-b6b167daf68a', extraction).
narrative_ontology:cs_interpretation_layer_present('480e3896-1bba-462e-9914-b6b167daf68a').
narrative_ontology:cs_reading_relation('480e3896-1bba-462e-9914-b6b167daf68a', liability_attribution__developer_liability, forecloses).
narrative_ontology:cs_reading_relation('480e3896-1bba-462e-9914-b6b167daf68a', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('480e3896-1bba-462e-9914-b6b167daf68a', foundational, deployer_control_grounds_liability).
narrative_ontology:cs_axiom_status(deployer_control_grounds_liability, holdable).
narrative_ontology:cs_axiom_grounding('480e3896-1bba-462e-9914-b6b167daf68a', deployer_control_grounds_liability, conventional).
narrative_ontology:cs_axiom('480e3896-1bba-462e-9914-b6b167daf68a', foundational, upstream_opacity_is_deployer_risk).
narrative_ontology:cs_axiom_status(upstream_opacity_is_deployer_risk, holdable).
narrative_ontology:cs_axiom_grounding('480e3896-1bba-462e-9914-b6b167daf68a', upstream_opacity_is_deployer_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('480e3896-1bba-462e-9914-b6b167daf68a', traditional_products_liability).
narrative_ontology:cs_drift_state('480e3896-1bba-462e-9914-b6b167daf68a', generative_ai_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('480e3896-1bba-462e-9914-b6b167daf68a', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, ai_developers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, large_enterprise_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, small_enterprise_deployers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, ai_insurance_market).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, ai_developers).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, deployer_control_justifies_liability).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, deployment_context_knowledge).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, downstream_harm_visibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provide foundation models (LLMs, multimodal systems) via API or licensed weights. Under deployer liability, they are shielded from downstream harm liability regardless of model opacity, capability surprises, or alignment failures. They collect licensing revenue while externalizing deployment risk. Their exit options are maximal: they can restrict access, change terms, or withdraw models without bearing downstream consequences.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Build applications on foundation models (fine-tuning, RAG, agentic systems). Benefit from liability shield for upstream model behavior but bear some integration-layer liability. Can switch foundation model providers or move to open-weight models, giving meaningful exit. However, they still face deployer liability for their own deployment choices.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_developers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, ai_developers, payer).

% Deploy AI systems in high-stakes domains (finance, healthcare, hiring, defense). Bear primary liability for all downstream harms including those originating from foundation model opacity (hallucinations, bias, capability gaps). Have resources for due diligence, red-teaming, insurance, and legal defense but face unbounded liability surface. Exit is constrained: AI deployment is competitively necessary, and switching providers doesn't eliminate liability for past deployments.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, large_enterprise_deployers, payer,
    powerful, biographical, constrained, global).

% SMEs adopting AI for core operations (customer service, analytics, automation). Bear same primary liability as large enterprises but lack resources for meaningful due diligence on opaque foundation models. Insurance is unavailable or prohibitive. Cannot credibly threaten exit without business failure. Liability exposure is existential relative to capitalization. The opacity burden is structurally unmanageable at this scale.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, small_enterprise_deployers, payer,
    moderate, biographical, trapped, national).

% Individuals harmed by AI decisions (denied loans, misdiagnosed, unfairly hired/fired, deepfaked). Under deployer liability, their recourse is against the deployer only — not the foundation model provider whose opacity caused the harm. They have no voice in liability regime design, no exit from AI-mediated systems, and face proof burdens amplified by model opacity. Their interests are invoked to justify the regime but not represented in its architecture.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, harmed_individuals, excluded,
    powerless, immediate, trapped, local).

% Design and enforce liability frameworks (EU AI Act, US executive orders, sectoral rules). Deployer liability is administratively tractable: deployers are identifiable, jurisdictional, and capitalized. Regulators gain a clear enforcement target without needing to police global foundation model development. They benefit from the regime's enforceability but face pressure when deployer compliance costs stifle adoption or when harms persist despite deployer due diligence.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulators, agenda_setter,
    institutional, generational, analytical, national).

% Adjudicate liability cases, interpreting 'due diligence' and 'deployment context control' standards. Their rulings define the operational boundary of the liability rule. They observe the regime's operation but do not design it. Their decisions reveal whether deployer liability can be fairly applied when harm originates in upstream opacity.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, courts, observer,
    institutional, generational, analytical, national).

% Emerging market for AI liability coverage. Deployer liability creates demand for novel insurance products. Insurers benefit from mandatory or de-facto-required coverage but face unquantifiable tail risk from foundation model opacity. They price policies to reflect deployer exposure, not upstream risk, effectively capitalizing the liability transfer. Their profitability depends on the regime's persistence.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_insurance_market, beneficiary,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns clear, enforceable liability for AI harms to the party most proximate to the deployment decision, enabling victims to recover damages without tracing causation through opaque upstream models. Creates a single accountable entity per deployment, simplifying litigation and insurance.
% TRANSFER_FUNCTION: Moves the full cost of downstream AI harms — including those caused by foundation model opacity, alignment failures, and capability gaps — from foundation model providers and developers to deployers. Deployers pay via litigation, insurance, compliance, and harm remediation; upstream providers retain revenue without commensurate liability exposure.
% ABSENT_VOICES: Harmed individuals are structurally excluded from regime design — they are the nominal beneficiaries of liability rules but have no seat in legislative or standard-setting processes. Open-weight model communities and academic researchers are excluded; they would argue for shared liability that reflects distributed causal contribution. Global South deployers are excluded; they face the same liability with fewer resources and no jurisdictional leverage over frontier model providers.
% DISAPPEARANCE_RATIONALE: If deployer-primary liability vanished overnight, foundation model providers would face direct liability exposure for downstream harms, likely triggering massive risk repricing (higher API costs, restricted access, indemnification demands). Deployers would lose their sole legal recourse target but gain leverage to demand upstream accountability. Insurance markets would restructure around upstream risk. The AI deployment economy would reorganize around shared or developer-primary liability.
% FOUNDING_PROBLEM: Early AI deployment (2018-2022) created accountability gaps: harmed parties could not identify responsible actors, developers disclaimed deployment control, and regulators lacked enforceable targets. Deployer liability was proposed to close the gap by anchoring responsibility in the visible deployment decision.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and foundation model providers attest the accountability gap persists and deployer liability remains necessary. Deployers (especially SMEs), legal scholars, and consumer advocates attest the founding problem has mutated: the gap is now upstream opacity, not deployment anonymity, and the regime has become a liability shield for frontier model providers. Independent analyses (Ada Lovelace Institute, Stanford HAI, EU Parliament studies) corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__deployer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__deployer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__deployer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because deployers absorb liability for upstream opacity — a risk they cannot price, insure, or mitigate through due diligence alone. Suppression (0.78) is high because the regime is enforced by state power (courts, regulators) and exit is economically non-viable for most deployers (AI adoption is competitively necessary). Theater ratio (0.38) reflects genuine due-diligence requirements (red-teaming, monitoring, documentation) that have real safety value but are performative relative to the opacity problem — no amount of deployer-side testing can reveal all foundation model failure modes. Accessibility collapse (0.58) is moderate: alternatives exist (don't deploy AI, use open-weight models, self-host) but are economically or technically non-viable for most organizations. Resistance (0.71) is high: deployer coalitions litigate, lobby for safe harbors, and push for upstream liability; courts are increasingly asked to pierce the deployer-liability shield when harm traces to known model defects.
 *
 * PERSPECTIVAL GAP:
 *   The deployer seats (large and small enterprise) should compute as snare or tangled_rope from their perspective — they experience enforced extraction with no upstream recourse. The foundation model provider seat should compute as rope or even mountain-adjacent from their perspective — they get a liability shield that looks like stable coordination. The regulator seat may compute as scaffold (transitional enforcement mechanism) or rope (administratively tractable). The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the authoring-seat assessment that the constraint has a genuine coordination function but operates with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Foundation model providers are structural beneficiaries (d ~ 0.15): they collect deployment revenue, face no downstream liability, and hold arbitrage-grade exit (can restrict API access, change terms, or withdraw models). AI developers are secondary beneficiaries (d ~ 0.25): they gain liability clarity for upstream layers but still face deployer liability for their own deployments; exit is mobile (can switch providers). Large enterprise deployers are targets (d ~ 0.85): they bear unbounded liability, have constrained exit (competitive necessity), and power only mitigates — not eliminates — exposure. Small enterprise deployers are near-full targets (d ~ 0.95): same liability with trapped exit and no mitigation capacity. Harmed individuals are excluded (d not computed): they bear harm without liability protection or voice. Regulators are agenda_setters (d ~ 0.3): they gain enforceability but face political blowback from deployer distress. Courts are observers (d = 0.5 analytical). Insurance market is a beneficiary (d ~ 0.2): new revenue stream from mandated coverage, mobile exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accountability gaps in early AI deployment) was real but has been overtaken by a structural shift: the liability regime designed for visible, auditable systems now governs opaque foundation models where deployer control is illusory. The regime persists because it solves the regulator's enforcement problem (identifiable, jurisdictional, capitalized defendants) and the foundation model provider's risk problem (liability externalization). Mandatrophy is unresolved: the constraint's original coordination function has atrophied relative to its extraction function, but no institutional actor has both the incentive and power to restructure it. Deployers lack coalition power; harmed individuals lack voice; regulators face political cost of reform; foundation model providers actively defend the status quo.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deployer_due_diligence_feasibility,
    'Can deployers actually perform meaningful due diligence on foundation model opacity to prevent downstream harms, or is the ''due diligence'' requirement a performative standard that extracts compliance cost without reducing harm?',
    'Empirical study of deployer-side red-teaming, monitoring, and guardrailing effectiveness against foundation model failure modes (hallucination, reward hacking, capability gaps, emergent behavior). Compare harm rates for deployers with mature AI governance vs. minimal governance, controlling for model choice.',
    'If due diligence is structurally infeasible for opacity-originated harms, the constraint''s coordination function is largely performative and extraction dominates. If feasible, the extraction component is the legitimate cost of coordination. Determines whether the regime is tangled_rope (genuine coordination + extraction) or snare (coordination as cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deployer_due_diligence_feasibility, empirical, 'Whether the deployer''s due diligence burden is a real coordination mechanism or an extraction mechanism disguised as coordination.').

omega_variable(
    upstream_liability_shield_effect,
    'Does the deployer-liability regime causally reduce foundation model providers'' investment in safety, alignment, and transparency, compared to a counterfactual where they face downstream liability?',
    'Natural experiment: compare safety investment trajectories of foundation model providers operating under deployer-liability regimes (US, UK) vs. those facing potential upstream liability (EU AI Act provider obligations, proposed US legislation). Track R&D allocation, transparency reporting, third-party audit access, and incident response.',
    'If the liability shield reduces upstream safety investment, the constraint creates a moral hazard that amplifies harm — the extraction from deployers is compounded by increased harm generation. This would strengthen the tangled_rope/snare classification and undermine the coordination justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(upstream_liability_shield_effect, empirical, 'Whether the liability assignment creates a moral hazard that increases total harm while shifting its cost.').

omega_variable(
    kernel_reading_deployer_liability,
    'This constraint is the deployer_liability reading of the liability_attribution kernel. What would change structurally if the developer_liability or shared_liability reading were instantiated instead?',
    'Author the sibling constraint stories (liability_attribution__developer_liability, liability_attribution__shared_liability) and compare: victim sets, beneficiary structures, extraction vectors, coordination functions, and network effects. The kernel contest is resolved when one reading''s structural description stabilizes as the operative regime.',
    'If developer_liability becomes operative, foundation model providers enter victim set, deployers exit it, and extraction reverses direction. If shared_liability becomes operative, victim/beneficiary sets distribute along value chain, extraction dilutes, and coordination complexity rises. The current reading''s classification is contingent on which reading governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_deployer_liability, conceptual, 'Commitment-system framing: this constraint is one reading of a contested kernel; its structural description and classification depend on which reading is instantiated in law.').

omega_variable(
    small_deployer_existential_exposure,
    'Does the deployer-liability regime create an existential threat to small enterprises that effectively excludes them from AI adoption, constituting a structural barrier to entry that concentrates AI deployment in large enterprises?',
    'Survey SME AI adoption rates, insurance availability, and legal spend in jurisdictions with deployer-primary liability vs. those without. Track bankruptcy or exit events attributed to AI liability exposure.',
    'If small deployers are structurally excluded, the constraint has a regressive distributional effect that reinforces market concentration — an extraction dynamic not captured by aggregate metrics. This would support a snare classification from the small_deployer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_deployer_existential_exposure, empirical, 'Whether the liability regime functions as a barrier to entry that extracts from small deployers by forcing exit or non-participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liability_attribution__deployer_liability_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.18).
narrative_ontology:measurement(liability_attribution__deployer_liability_tr_t4, liability_attribution__deployer_liability, theater_ratio, 4, 0.22).
narrative_ontology:measurement(liability_attribution__deployer_liability_tr_t8, liability_attribution__deployer_liability, theater_ratio, 8, 0.28).
narrative_ontology:measurement(liability_attribution__deployer_liability_tr_t12, liability_attribution__deployer_liability, theater_ratio, 12, 0.33).
narrative_ontology:measurement(liability_attribution__deployer_liability_tr_t16, liability_attribution__deployer_liability, theater_ratio, 16, 0.36).
narrative_ontology:measurement(liability_attribution__deployer_liability_tr_t20, liability_attribution__deployer_liability, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(liability_attribution__deployer_liability_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(liability_attribution__deployer_liability_be_t4, liability_attribution__deployer_liability, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(liability_attribution__deployer_liability_be_t8, liability_attribution__deployer_liability, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(liability_attribution__deployer_liability_be_t12, liability_attribution__deployer_liability, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(liability_attribution__deployer_liability_be_t16, liability_attribution__deployer_liability, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(liability_attribution__deployer_liability_be_t20, liability_attribution__deployer_liability, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(liability_attribution__deployer_liability_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(liability_attribution__deployer_liability_su_t4, liability_attribution__deployer_liability, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(liability_attribution__deployer_liability_su_t8, liability_attribution__deployer_liability, suppression_requirement, 8, 0.69).
narrative_ontology:measurement(liability_attribution__deployer_liability_su_t12, liability_attribution__deployer_liability, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(liability_attribution__deployer_liability_su_t16, liability_attribution__deployer_liability, suppression_requirement, 16, 0.77).
narrative_ontology:measurement(liability_attribution__deployer_liability_su_t20, liability_attribution__deployer_liability, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, ai_act_provider_obligations).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, ai_insurance_market_formation).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, foundation_model_transparency_requirements).

% DUAL FORMULATION NOTE:
% This constraint is one member of the liability_attribution constraint family (kernel_id: liability_attribution). The three readings — deployer_liability, developer_liability, shared_liability — are structurally distinct constraints with different ε values, victim/beneficiary sets, and coordination/extraction balances. They are linked by network.affects_constraints. The deployer_liability reading is currently the dominant operative regime in most jurisdictions (US common law, UK, sectoral regulators). The developer_liability reading is instantiated in EU AI Act provider obligations for high-risk systems. The shared_liability reading is emerging in academic proposals and some legislative drafts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__deployer_liability, organized, 0.25).
constraint_indexing:directionality_override(liability_attribution__deployer_liability, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
