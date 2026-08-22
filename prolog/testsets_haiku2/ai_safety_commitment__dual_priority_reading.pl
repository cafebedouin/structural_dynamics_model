% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: Dual-Priority AI Safety Commitment (Existential + Near-Term)
 *   domain: technology/governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested
 *   'ai_safety_commitment' kernel: the claim that AI safety requires
 *   addressing BOTH existential risk and near-term harms as non-competing
 *   priorities. The kernel is contested because two other readings disagree
 *   on which category should be primary. This reading asserts dual priority
 *   and attempts to coordinate funding and research attention to serve both.
 *   It faces an irreducible coherence challenge: under resource scarcity
 *   (which always holds), declaring both non-competing is a
 *   normative/political commitment, not an empirical claim. The constraint's
 *   operation extracts from those experiencing near-term harms (marginalized
 *   populations, displaced workers) by asking them to defer concrete relief
 *   in service of theoretical long-horizon prevention research. It
 *   simultaneously benefits from the legitimacy of both risk categories and
 *   suffers from the tension between them. This reading is CLAIMED as tangled
 *   rope—genuine coordination function (both research agendas needed,
 *   fragmentation is bad) plus asymmetric extraction (near-term victims bear
 *   the cost of dual-priority scarcity splits). The measurement series show
 *   rising theater ratio (the commitment is invoked more for rhetorical
 *   legitimacy than operational equal funding) and rising suppression
 *   requirement (maintaining the dual-priority frame requires actively
 *   preventing capability developers from joining the conversation and
 *   preventing near-term-harm researchers from claiming priority).
 *
 * KEY AGENTS:
 *   - ai_safety_research_funders: agenda-setters; allocate resources, define priority; institutional power; mobile exit options
 *   - existential_risk_researchers: beneficiaries from dual-priority framing; powerful, mobile; benefit from legitimacy without competing for near-term funding
 *   - near_term_harms_researchers: payers/constrained beneficiaries; moderate power; supposed co-priority but face resource scarcity
 *   - marginalized_populations_present_harms: victims; powerless, trapped; bear concrete costs of funding competition; highest extraction target
 *   - labor_displaced_by_ai: victims; organized, constrained; depend on safety research redirecting toward transition support
 *   - future_humanity: non-agent beneficiary; invoked as constituency but has no representation in priority-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.71).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "Dual-Priority AI Safety Commitment (Existential + Near-Term)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "technology/governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, '4f4113ba-4826-4a19-a07c-a0b2dd9ce46e').
narrative_ontology:cs_kernel_codification('4f4113ba-4826-4a19-a07c-a0b2dd9ce46e', distributed).
narrative_ontology:cs_authority_grounding('4f4113ba-4826-4a19-a07c-a0b2dd9ce46e', distributed).
narrative_ontology:cs_reading_relation('4f4113ba-4826-4a19-a07c-a0b2dd9ce46e', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f4113ba-4826-4a19-a07c-a0b2dd9ce46e', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('4f4113ba-4826-4a19-a07c-a0b2dd9ce46e', foundational, both_risks_require_simultaneous_attention).
narrative_ontology:cs_axiom_status(both_risks_require_simultaneous_attention, holdable).
narrative_ontology:cs_axiom_grounding('4f4113ba-4826-4a19-a07c-a0b2dd9ce46e', both_risks_require_simultaneous_attention, instrumental).
narrative_ontology:cs_axiom('4f4113ba-4826-4a19-a07c-a0b2dd9ce46e', foundational, resource_scarcity_does_not_imply_priority_hierarchy).
narrative_ontology:cs_axiom_status(resource_scarcity_does_not_imply_priority_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('4f4113ba-4826-4a19-a07c-a0b2dd9ce46e', resource_scarcity_does_not_imply_priority_hierarchy, deontological).
narrative_ontology:cs_reference_frame('4f4113ba-4826-4a19-a07c-a0b2dd9ce46e', coordinated_dual_research_agenda).
narrative_ontology:cs_drift_state('4f4113ba-4826-4a19-a07c-a0b2dd9ce46e', contemporary_implementation_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f4113ba-4826-4a19-a07c-a0b2dd9ce46e', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_research_funders).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, future_humanity).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, marginalized_populations_present_harms).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, labor_displaced_by_ai).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, existential_risk_researchers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large institutional funders (foundations, governments, tech firms) allocate hundreds of millions annually to AI safety research. They set priorities, define which problems count as 'safety,' and determine how resources split between existential-risk research and near-term-harm mitigation. Their choices reflect both risk assessment and institutional incentives (visibility, tractability, stakeholder pressure). They can shift funding allocation, redefine research agendas, or exit specific research areas entirely.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_research_funders, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Academic and industry researchers studying long-horizon risks (alignment, scalable oversight, emergent capabilities, value specification) have substantial funding and prestige under this constraint. The dual-priority framing legitimizes their work as essential while creating resource competition with near-term-harm researchers. They benefit from the constraint's assertion that their work is non-competing; they also compete directly for the same funding pools.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, existential_risk_researchers, beneficiary,
    powerful, civilizational, mobile, global).

% Researchers studying documented present-day harms (algorithmic bias, worker displacement, misinformation, surveillance, data exploitation) are partially legitimized by the dual-priority framing but face resource scarcity. They argue their work is urgent and evidence-based; the existential-risk focus historically captured the largest share of safety funding despite their claims. Under this constraint they are supposed to be co-equal in priority, but enforcement of that equality is weak.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers, beneficiary).

% Communities experiencing documented AI harms now: algorithmic discrimination in hiring/lending/criminal-justice, exclusion from AI training data, labor displacement without transition support, misinformation targeted at vulnerable groups. They are named as co-beneficiaries of safety research under the dual-priority reading but bear the concrete costs while resources are competed over. Their exit options are minimal—they cannot opt out of algorithmic systems, cannot redirect research funding, cannot shift industry incentives unilaterally.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, marginalized_populations_present_harms, payer,
    powerless, biographical, trapped, global).

% Workers in sectors facing near-term AI displacement (customer service, content moderation, certain white-collar professions, creative work) have organizational capacity but constrained exit. They depend on labor-market stability and retraining opportunities. The dual-priority constraint nominally includes their interests; practically, funding for transition support and displacement prevention competes against both existential-risk and near-term-harm research budgets.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, labor_displaced_by_ai, payer,
    organized, biographical, constrained, national).

% Humanity as a collective, across centuries and scenarios where superintelligent AI systems exist. Not an organized actor, but a normative constituency whose interests are invoked by existential-risk researchers. The dual-priority constraint attempts to account for their interests without concrete representation in priority-setting.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, future_humanity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_safety_commitment__dual_priority_reading, future_humanity).

% Tech companies and research labs building frontier AI systems are structurally excluded from the safety research funding and priority-setting apparatus. They have incentive to minimize safety requirements and maximize capability speed. The constraint's enforcement (fund safety research, not capability) depends on keeping them outside the priority conversation; if they shaped the agenda, near-term-harm research and existential-risk research would both be deprioritized against speed-to-deployment.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_capability_developers, excluded,
    institutional, biographical, mobile, global).

% Government agencies, international bodies, and regulatory frameworks tasked with AI governance attempt to enforce the dual-priority principle at the policy level. They commission research, set safety standards, and allocate regulatory attention. They observe the constraint's operation and can modify the institutional structure that enforces it.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, safety_governance_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, ai_safety_research_funders).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: without a coordination framework that legitimizes both existential-risk and near-term-harm research as co-equal priorities, each research community would claim urgency and dismiss the other as distraction, fragmenting safety funding and attention. The dual-priority reading attempts to coordinate resource allocation that serves both existential and present risks without subordinating either.
% TRANSFER_FUNCTION: Moves safety research funding, institutional prestige, and policy attention from capability development (the counterfactual if no safety coordination existed) toward both existential-risk research and near-term-harm mitigation. The constraint's enforcement redirects resources that would otherwise go to speed-to-deployment; the contested question is whether the split between existential and near-term is genuinely equitable or systematically biased.
% ABSENT_VOICES: AI capability developers are structurally excluded (they would argue safety research slows beneficial progress and that near-term harms are manageable through market feedback). Populations in high-AI-exposure regions who lack research infrastructure (Global South communities, developing-economy workers) are absent from priority-setting conversations despite bearing concentrated near-term harms. Their presence would likely shift resource allocation toward localized harm mitigation over theoretical long-horizon risks.
% DISAPPEARANCE_RATIONALE: If the dual-priority commitment vanished, funding would bifurcate sharply: existential-risk research would likely collapse (lacks immediate constituency, depends on abstract risk framing), and near-term-harm research would consolidate around deployed-systems safety and worker protection. Capability development would accelerate unconstrained. The safety research enterprise would reorganize entirely; the two reading communities would no longer pretend co-priority and would compete openly for attention and resources.
% FOUNDING_PROBLEM: Rapid AI capability development created urgency around both existential risks (misaligned superintelligence scenarios) and near-term harms (algorithmic discrimination, labor displacement, information operations). These two risk categories have different timescales, evidence bases, and research communities. The founding problem was the risk that treating one as the sole priority would leave the other unaddressed, fragmenting safety efforts and allowing one class of harms to go mitigated.
% FOUNDING_PROBLEM_CORROBORATION: Existential-risk researchers and near-term-harm researchers both attest the founding problem is live, but disagree on priority: existential researchers argue the timescale and consequence magnitude of extinction risk dominates; near-term researchers argue documented, preventable harms to real people now cannot be deferred. Independent analyses from governance bodies and ethics researchers corroborate that both risk classes exist and that funding allocation has historically skewed toward existential risk despite near-term harms being more measurable.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint channels resources away from immediate, verifiable harm mitigation toward speculative long-horizon prevention, and near-term victims cannot opt out. The constraint benefits funders (they maintain institutional prestige by addressing both), benefits existential-risk researchers (their work is legitimized as essential without direct competition), and imposes costs on those experiencing documented harms (funding for algorithmic bias mitigation, worker transition, discrimination prevention is capped by the need to fund existential research). Suppression is high (0.71) because maintaining the dual-priority frame requires actively excluding capability developers (whose inclusion would destroy the frame entirely) and suppressing the straightforward claim that near-term harms are more urgent and measurable. Theater ratio is very high (0.58) because the 'dual-priority, non-competing' framing is invoked frequently in discourse while actual resource allocation remains skewed toward existential risk, and 'dual priority' increasingly functions as rhetorical cover for resource competition rather than genuine coordination.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (funders) experiences this as genuine coordination that balances competing obligations and maintains research diversity. The existential-risk-researcher seat experiences this as legitimate equal standing they earned through intellectual contribution. The near-term-harm-researcher seat experiences this as enforced subordination dressed in equality language—they are told their work is co-equal in priority while funding allocations contradict that claim. The victim seats (marginalized populations, displaced workers) experience this as pure extraction: resources they need are directed toward theoretical prevention of hypothetical future risks. The engine computes these divergent classifications from the structural data (power, exit options, beneficiary/victim declarations); the claim/metric independence principle means the constraint can be CLAIMED as rope while MEASURING as tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   ai_safety_research_funders: d ≈ 0.15 (full beneficiary; they set agenda, control resource distribution, can unilaterally alter priorities). existential_risk_researchers: d ≈ 0.25 (partial beneficiary; benefit from legitimacy and funding, but also face resource competition and reputational risk if the constraint fails). near_term_harms_researchers: d ≈ 0.55 (near-symmetric; genuinely benefit from the legitimacy the dual-priority frame provides, but lose directly in funding competition; constrained exit because abandoning the safety frame entirely weakens their claims). marginalized_populations_present_harms: d ≈ 0.88 (near-full target; bear concrete costs, have no exit, cannot influence allocation decisions, are invoked as constituency but have no seat at the table). labor_displaced_by_ai: d ≈ 0.75 (high target; experience near-term harms, depend on safety research redirecting toward their protection, have organized capacity but constrained exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The dual-priority reading attempts to prevent mandatrophy by asserting both agendas are non-competing and essential. However, the measurement series show theater_ratio rising steadily (0.35 → 0.58), indicating the commitment's real function has drifted: it started as a genuine resource-coordination mechanism and has become increasingly a rhetorical device to legitimize existential-risk research while deferring near-term action. The foundational mandate—coordinate both research agendas to prevent fragmentation—persists formally, but the operating mandate—direct resources toward existential risk while paying lip service to near-term harms—has become the substantive rule. This is not full mandatrophy (the constraint still redistributes resources toward safety research broadly), but it is partial mandatrophy in the near-term dimension: the founding problem (prevent fragmentation by coordinating both) has been solved structurally (both communities exist and are funded), but the constraint persists as a mechanism to suppress near-term researchers' direct claims for priority while invoking their interests rhetorically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_scarcity_arbitration,
    'How should safety research funding be allocated between existential-risk research and near-term-harm mitigation when resources are strictly limited and both cannot be fully funded?',
    'Direct empirical measurement of (a) the probability and magnitude of existential-risk scenarios vs. (b) the incidence and preventability of near-term harms, with normative weighting of future vs. present interests. Alternatively, resolution mechanism is political/institutional: whichever research community gains institutional power in a future moment can redefine priorities.',
    'If existential risk is orders of magnitude larger/more probable than measured present harms, existential-risk priority is justified and near-term researchers are correctly secondary. If near-term harms are more measurable and existential risks remain speculative, the dual-priority frame is cover for existential-risk preference, and the constraint reclassifies toward snare. The constraint''s coherence depends on which resolution emerges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_scarcity_arbitration, empirical, 'The irreducible tradeoff between competing risk categories under scarcity.').

omega_variable(
    dual_priority_as_foreclosure,
    'Does this reading''s core axiom—that both priorities are non-competing—logically foreclose the near-term-harms reading''s core axiom—that present, measurable harms should be the primary focus?',
    'Formal analysis of the logical structure: can a framework acknowledge both present harms AND existential risks without assigning a primary priority? If yes, they coexist. If no (i.e., declaring both ''non-competing'' requires denying that priority tradeoffs exist), then the dual-priority reading forecloses the near-term-specific focus.',
    'If the readings foreclose each other, the kernel has incompatible framings and the constraint cannot coordinate; at least one reading must yield to fact. If they coexist, the constraint is genuinely about resource arbitration, not logical incompatibility. Current institutional practice suggests coexistence (both communities persist, both are funded), but this codifies the absence of principled tradeoff logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_priority_as_foreclosure, conceptual, 'Whether dual-priority truly resolves the existential-vs-near-term tension or merely postpones it.').

omega_variable(
    victim_representation_deficit,
    'Populations experiencing near-term AI harms (algorithmic discrimination, labor displacement, misinformation) are named as beneficiaries of the dual-priority commitment but have no seat at the priority-setting table. How does this absence affect the constraint''s classification?',
    'Institutional redesign: if priority-setting were expanded to include representatives from affected populations, would resource allocation shift toward near-term mitigation? Observe actual shifts if such representation is implemented.',
    'If inclusion would cause priority reallocation, the constraint is currently extractive from the represented population (their interests are invoked but their voice is suppressed). If inclusion changes nothing, the constraint''s nominal inclusion of their interests is purely theatrical. Either way, the constraint moves toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_representation_deficit, empirical, 'Whether near-term-harm populations'' absence from priority-setting is constitutive of the constraint''s extraction.').

omega_variable(
    reading_identity_fusion,
    'Existential-risk researchers have built careers, institutional identity, and professional standing on the assumption that their work is essential and non-deferrable. Has existential-risk research community identity become fused with the dual-priority reading such that accepting near-term-harm priority would constitute loss of professional identity?',
    'Post-priority-shift observational cohort: if near-term harms were elevated to primary, do existential-risk researchers continue their work with modified framing, or do they experience it as invalidation of their career choice? Identity lock is structural if the latter occurs.',
    'If identity-locked, existential-risk researchers have high exit costs and strong incentive to maintain the dual-priority frame, even if it is collectively suboptimal. This converts the constraint from tangled rope (genuine coordination + asymmetric extraction) toward snare (purely extractive, persistence depends on suppressing visibility of the identity lock).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_fusion, empirical, 'Identity fusion between researchers and existential-risk priority.').

omega_variable(
    kernel_reading_identity_fusion,
    'Is this dual_priority_reading one reading of a single, stable kernel, or does declaring ''both non-competing'' constitute a separate normative claim that forecloses sibling readings within a single institutional framework?',
    'Structural analysis: if institutions adopt the dual-priority reading as their formal mandate, are they also logically committed to rejecting or subordinating the near-term-primary reading? Or can they hold both in tension without contradiction?',
    'If dual-priority forecloses near-term-primary within a single framework, then these are not coexisting readings but rather competing claims where one reading''s adoption ends the other''s legitimacy in that institution. Current institutional practice shows coexistence across different institutions, not within a single decision-maker, which suggests the readings remain separable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_fusion, conceptual, 'Whether the dual-priority reading forecloses sibling readings or merely competes with them across institutional seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__dual_priority_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__dual_priority_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(ai_s_tr_t15, ai_safety_commitment__dual_priority_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__dual_priority_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(ai_s_tr_t25, ai_safety_commitment__dual_priority_reading, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__dual_priority_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__dual_priority_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(ai_s_be_t15, ai_safety_commitment__dual_priority_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__dual_priority_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_s_be_t25, ai_safety_commitment__dual_priority_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__dual_priority_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__dual_priority_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(ai_s_su_t15, ai_safety_commitment__dual_priority_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__dual_priority_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(ai_s_su_t25, ai_safety_commitment__dual_priority_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__dual_priority_reading, 0.18).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE READING of the contested 'ai_safety_commitment' kernel. The kernel describes the fundamental commitment to address AI risks; three constraint stories represent three different readings of which risks are primary. The dual_priority_reading (this story) asserts both categories are non-competing and should receive coordinated resources. The existential_risk_reading claims extinction-level outcomes dominate and near-term harms are secondary. The near_term_harms_reading claims documented present-day harms should be the primary focus. These are not three measurements of one constraint—they are three structurally distinct constraints (different ε values, different beneficiary/victim structures, different extraction mechanics) that share a common kernel and compete for institutional adoption. All three remain live positions; no single reading has achieved foreclosure. Network links record mutual influence: the dual-priority reading creates institutional pressure on both siblings by attempting to legitimize both simultaneously; existential-risk dominance creates resource scarcity pressure on near-term advocates; near-term harms make the dual-priority framing necessary (without near-term research community, existential focus would be unopposed and the dual-priority frame would be rhetorical window dressing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, powerless, 0.88).
constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
