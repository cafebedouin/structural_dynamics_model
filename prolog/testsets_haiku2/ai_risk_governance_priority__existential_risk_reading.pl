% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: Superintelligence Risk Prioritization in AI Governance (Existential Risk Reading)
 *   domain: technology/governance/risk_assessment
 *
 * SUMMARY:
 *   The existential-risk reading of AI governance frames the primary moral
 *   imperative as preventing superintelligence scenarios that could
 *   annihilate or permanently curtail humanity's potential. This reading
 *   prioritizes alignment-as-control (safety work inside labs),
 *   capability-overhang research, and governance frameworks designed for AGI
 *   scenarios. The beneficiary structure is dual: x-risk institutions acquire
 *   resource and narrative authority; frontier AI labs claiming safety
 *   leadership gain legitimacy for continued capability development under
 *   internal-safety review. The victim structure is broad (present-harm
 *   communities) and ultimate (future humanity), but asymmetrically weighted
 *   toward speculative long-term risks. The constraint operates as tangled
 *   rope: it does solve a real coordination problem (preventing downside
 *   superintelligence outcomes), but it does so via a structure that extracts
 *   governance attention and resources from present-harm mitigation and
 *   shields capability development from external accountability. The measured
 *   theater ratio (0.48, rising to peak 0.49 at t=20 then declining slightly)
 *   reflects the constraint's operational character: the safety framing is
 *   partially genuine (real alignment work happens, real researchers pursue
 *   alignment sincerely), but the primary effect is to shield labs from
 *   external oversight and redirect governance attention. Extractiveness
 *   rises from 0.48 to 0.67 over 25 years, plateauing around t=20, suggesting
 *   the constraint reaches its institutional stability and narrative capture
 *   by the mid-interval.
 *
 * KEY AGENTS:
 *   - x-risk research institutions: agenda-setters and primary beneficiaries; define what counts as legitimate AI governance research
 *   - AI labs claiming safety leadership: institutional beneficiaries; gain legitimacy for capability control through internal-safety narratives
 *   - Present-harm affected populations (workers, marginalized groups, surveillance victims): structural payers; excluded from governance design; concentrated costs now
 *   - Future humanity: ultimate victim in the reading's moral frame; non-agent; has no voice in current decisions
 *   - Capability researchers: secondary beneficiary; existential-risk frame justifies continued advancement within safety-conscious narrative
 *   - Labor organizations and livelihoods: payer and excluded; their present-harm concerns are rendered secondary in existential-risk prioritization
 *   - Regulatory authorities: agenda-setters; mediate between frames but lack independent verification capacity; experience asymmetric pressure from research institutions
 *   - Scientific consensus institutions: observe institutional imbalance in venue-control and research distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.67).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.72).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Superintelligence Risk Prioritization in AI Governance (Existential Risk Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "technology/governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, '15be7801-9fad-4ee1-b90b-0fb7690df098').
narrative_ontology:cs_kernel_codification('15be7801-9fad-4ee1-b90b-0fb7690df098', distributed).
narrative_ontology:cs_authority_grounding('15be7801-9fad-4ee1-b90b-0fb7690df098', extraction).
narrative_ontology:cs_reading_relation('15be7801-9fad-4ee1-b90b-0fb7690df098', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('15be7801-9fad-4ee1-b90b-0fb7690df098', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('15be7801-9fad-4ee1-b90b-0fb7690df098', foundational, superintelligence_capability_overhang).
narrative_ontology:cs_axiom_status(superintelligence_capability_overhang, holdable).
narrative_ontology:cs_axiom_grounding('15be7801-9fad-4ee1-b90b-0fb7690df098', superintelligence_capability_overhang, empirically_contingent).
narrative_ontology:cs_axiom('15be7801-9fad-4ee1-b90b-0fb7690df098', foundational, future_humanity_moral_priority).
narrative_ontology:cs_axiom_status(future_humanity_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('15be7801-9fad-4ee1-b90b-0fb7690df098', future_humanity_moral_priority, deontological).
narrative_ontology:cs_reference_frame('15be7801-9fad-4ee1-b90b-0fb7690df098', pre_superintelligence_alignment_uncertainty).
narrative_ontology:cs_drift_state('15be7801-9fad-4ee1-b90b-0fb7690df098', contemporary_scaling_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('15be7801-9fad-4ee1-b90b-0fb7690df098', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_harm_affected_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, capability_research_community).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, ai_deployment_companies).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, labor_organizations_and_livelihoods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations (Future of Life Institute, Machine Intelligence Research Institute, Center for AI Safety, parts of academic AI safety research) that frame their legitimacy and resource acquisition around preventing superintelligence scenarios. They author research agendas, set conference agendas, influence funding decisions, and define what counts as 'responsible AI governance.' The prioritization of existential risk expands their mandate and legitimacy; a governance framework centered on present-harm mitigation would diminish their structural position.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, agenda_setter).

% Frontier AI companies (Anthropic, DeepMind, OpenAI, others) that position themselves as responsible players by investing in safety research, publishing alignment work, and endorsing existential-risk frames in policy discussions. The superintelligence risk narrative legitimizes their control over capabilities development ('we are the responsible actors preventing the bad outcome') and provides a rationale for proprietary training, internal safety review, and resistance to external auditing frameworks. They can exit to markets where this frame is weaker or irrelevant.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, agenda_setter).

% Communities currently experiencing demonstrable harms from deployed AI systems: workers displaced by automation, people subject to algorithmic bias in hiring/lending/criminal justice, marginalized groups targeted by AI-optimized misinformation, surveillance victims in high-monitoring regimes. These populations bear costs now while governance attention is redirected to speculative long-term scenarios. They lack the technical expertise and institutional access to shape risk-prioritization debates; their exclusion from governance design is structural, not accidental.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, present_harm_affected_populations, payer,
    powerless, biographical, trapped, global).

% A non-agent placeholder representing humanity's long-term continuity and potential. This reading treats future generations as the ultimate victim class if superintelligence scenarios materialize unchecked. The reading's moral argument structures around protecting this constituency, but future humanity has no voice in current governance decisions; they are present only as a projected scenario.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__existential_risk_reading, future_humanity).

% AI application companies (non-frontier: recommendation systems, recruitment platforms, content moderation services) operating at scale but not at the research frontier. They pay the enforcement cost of alignment and safety requirements demanded by the existential-risk frame (red-teaming, interpretability work, safety audits) but lack the capability-control of frontier labs. They face regulatory and reputational pressure to adopt existential-risk framings even where their deployment risks are primarily present-harm risks (bias, misinformation amplification).
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_deployment_companies, payer,
    powerful, generational, constrained, global).

% Academic and corporate researchers focused on advancing AI capabilities (scaling, efficiency, multimodality, reasoning). The existential-risk frame justifies their continued work within a safety-conscious narrative: advancing toward superintelligence while internally managing the risks of that advancement. They benefit from a governance model that prioritizes alignment-as-control over external accountability and democratized decision-making about capability development itself.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, capability_research_community, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, capability_research_community, agenda_setter).

% Worker unions, skill guilds, labor economists, and communities whose employment and economic security depend on the pace and distribution of AI automation. Present harms (job displacement, wage suppression, unequal transition support) are their immediate concern. The existential-risk frame's long-term orientation can render present labor impacts secondary or invisible in governance discussions. They are structurally excluded from setting priorities despite bearing concentrated costs.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, labor_organizations_and_livelihoods, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, labor_organizations_and_livelihoods, excluded).

% Government agencies, legislative bodies, and international bodies (EU, US executive, UN bodies) tasked with AI governance. They mediate between competing risk frames and allocate regulatory attention. Adopting the existential-risk prioritization shifts their governance mandate toward capability-control and long-term scenario planning; rejecting it would center present-harm mitigation and worker protections. They experience pressure from both research institutions and affected populations but limited technical capacity to independently verify existential-risk claims.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Academic journals, peer review bodies, and interdisciplinary research networks that assess evidence claims. They face asymmetric inputs: existential-risk researchers publish extensively on speculative scenarios and control major AI safety venues; present-harm researchers (computer scientists, social scientists, economists, ethicists outside AI labs) submit work through distributed venues with less agenda-setting power. The narrative landscape they observe reflects this institutional imbalance, not necessarily underlying evidence distribution.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, scientific_consensus_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: developing AI systems with the potential to affect humanity's long-term future creates a coordination challenge — no individual researcher, company, or nation can unilaterally prevent superintelligence scenarios if others do not cooperate. The prioritization of existential risk attempts to establish a shared frame around which coordination can happen: 'we all align on preventing the worst outcome, and that requires advance safety work and governance structures.'
% TRANSFER_FUNCTION: Redirects research funding, policy attention, regulatory bandwidth, and legitimacy from present-harm mitigation toward speculative long-term capability-control and alignment-as-inside-the-lab-safety. Resources and narrative authority flow from marginalized-population advocacy toward x-risk institutions and frontier labs claiming safety leadership. Governance frameworks that would mandate external accountability, transparency, and democratized decisions about capability development are displaced by internal-safety review and 'responsible scaling' narratives.
% ABSENT_VOICES: Workers and labor organizations affected by automation displacement; present-harm researchers in non-AI disciplines (social scientists, ethicists, economists studying inequality); communities currently experiencing algorithmic bias; developing-world nations whose populations are subject to surveillance and AI-powered misinformation but excluded from governance deliberation; future generations have no seat but are invoked as the reading's ultimate beneficiary. These groups would object to the existential-risk prioritization as a mechanism for deflecting attention from present, measurable harms in their communities.
% DISAPPEARANCE_RATIONALE: If this prioritization frame disappeared overnight, governance and research attention would reallocate substantially: resources currently directed at superintelligence-prevention would flow toward present-harm mitigation; regulatory bandwidth would expand to external auditing and transparency requirements that frontier labs resist; the narrative authority of x-risk institutions would diminish; labor protections and equity considerations would rise in salience. The world's institutional configuration would rearrange — so would the distribution of legitimacy and resources among different actor groups. Whether that rearrangement would be beneficial (because the existential-risk prioritization is overblown cover story) or catastrophic (because superintelligence risks are real and undersupported) is itself the contested question the kernel frames.
% FOUNDING_PROBLEM: Advanced AI systems pose a capability overhang: their decision-making power is increasing faster than our ability to ensure alignment with human values and intentions. Without advance work on safety and governance, the story goes, reaching superintelligence without solving alignment could result in outcomes that annihilate or permanently constrain humanity's potential.
% FOUNDING_PROBLEM_CORROBORATION: X-risk researchers and some frontier labs attest the founding problem is live and urgent, citing scaling dynamics and capability emergence in large language models. Present-harm researchers and labor organizations attest the founding problem is either unproven (claims about superintelligence remain speculative) or has been used as cover for inadequate present-harm governance ('we'll handle ethics when AGI arrives'). Computer scientists working on interpretability, alignment researchers in academic institutions, and some ethicists outside the x-risk mainstream attest the founding problem framing is real but *weaponized* — used to justify labs' internal control rather than external accountability. Independent technical verification of superintelligence-risk claims remains minimal; the corroboration comes almost exclusively from benefiting institutions.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.67 at interval end) reflects the constraint's dual structure: it solves genuine coordination (preventing superintelligence), but the solution mechanism extracts governance authority from present-harm constituencies and privileges labs' internal safety over external accountability. Suppression (0.72) is high because the constraint's persistence depends on sustained exclusion of labor organizations and present-harm researchers from governance tables and on the narrative dominance of existential-risk framing (which suppresses questions about why labs should control their own safety review). Theater ratio (0.48, rising to 0.49) captures that the safety work is partially real (genuine researchers, published work) but increasingly performative: as the constraint stabilizes, more of the enforcement machinery becomes about narrative management and governance capture than about actual technical safety advancement. Accessibility collapse (0.38) is relatively low because alternatives remain intelligible and articulate: the near-term-harms reading and bridge reading both maintain coherent critiques of existential-risk prioritization, and labor/equity communities continuously propose alternative governance frameworks (even though they are suppressed). Resistance (0.71) is high because present-harm communities, labor organizations, and developing-world governments actively resist the existential-risk prioritization, even though they lack the institutional power to shift governance outcomes. The shared time grid allows the trajectories to be read together: extractiveness and suppression rise together (the constraint consolidates institutional power), while theater rises more slowly (performativity increases as extractiveness plateaus, suggesting the constraint moves from coordination maintenance to narrative defense). The slight decline in theater ratio after t=20 (0.49 → 0.48) may reflect external pressure responses (legislative attention, labor organizing, civil-society critique) that force labs to increase substantive safety work to maintain narrative legitimacy, or it may reflect measurement noise; the trend is stable at higher confidence from t=15 onward.
 *
 * PERSPECTIVAL GAP:
 *   The existential-risk institution and frontier-lab seats compute this constraint as Rope or weak-Tangled Rope: real coordination problem solved, transparent safety work advancing, governance maturation for long-term challenges. The present-harm-affected and labor seats compute it as Snare: extraction of governance authority under safety cover, speculative victim (future humanity) invoked to render present victims invisible, power consolidation by capable actors claiming responsibility for outcomes they don't yet face. The regulatory-authority seat experiences both pressures simultaneously: the existential-risk framing is technically coherent and comes from high-credibility sources, but the governance gap for present harms is measurably widening. The engine's per-seat computation will diverge here because the structural directionality diverges: beneficiary seats have high mobility and arbitrage options (can move research between institutions, claim leadership across multiple framings); payer seats are trapped or identity-locked (worker identity fused to automation threat, present-harm populations constituted as the objects of research rather than decision-makers). These divergences are the point — they show how the same constraint looks structurally different from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for x-risk institutions and frontier labs: d near 0.0 (beneficiaries). They set agendas, define legitimate research, collect resources and legitimacy, can exit to other markets where the frame is weaker. Directionality for present-harm affected populations: d near 1.0 (targets). They pay the cost of governance misdirection now, are trapped by economic dependence on AI-shaped labor markets, lack institutional exit, are suppressed by the narrative dominance of existential-risk framing. Directionality for capability researchers: d near 0.15–0.25 (weak beneficiary with extraction burden). They benefit from the safety-conscious narrative legitimizing capability advancement, but bear some compliance cost (red-teaming, interpretability work) and face reputational pressure to adopt existential-risk language even where their work is neutral-to-positive on present harms. Directionality for regulatory authorities: d near 0.5 (symmetric). They experience genuine pressure to coordinate around some risk frame, but the existential-risk frame is not their preferred equilibrium — it delegates safety to labs and leaves them underfunded for present-harm mitigation. Directionality for future humanity: non-agent, d undefined, but the reading treats it as the ultimate target (d→1.0) in terms of the moral priority structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is the prime test case for mandatrophy resolution in the AI governance domain. The founding problem (superintelligence capability overhang creating alignment risk) remains contested at the empirical level: do the dynamics of current scaling point toward superintelligence, or are the scenarios speculative extrapolations from limited evidence? The founding problem's status is coded as 'contested' because research institutions disagree, verification remains minimal, and the null hypothesis (superintelligence is not a likely outcome of current scaling dynamics) is held by serious researchers outside the x-risk mainstream. The disappearance verdict is also 'contested': frontier labs argue that if superintelligence prioritization disappeared, catastrophic misalignment outcomes would become more likely (governance attention would lapse, safety work would defund); present-harm researchers argue that if this prioritization disappeared, governance would reallocate to present harms that create foundation for more responsible long-term AI development. This mismatch (founding_problem_status=contested AND disappearance_verdict=contested) is NOT mandatrophy in the terminal sense — the constraint's founding problem has not been proven dead (research continues, scaling continues, new evidence arrives). However, the constraint exhibits proto-mandatrophy symptoms: extractiveness has plateaued (0.67, stable from t=20 onward), theater has begun rising above the functional threshold (0.49), and suppression mechanics are increasingly required to maintain the prioritization frame in the face of present-harm evidence and labor organizing. The constraint is drifting from coordination maintenance toward institutional preservation. If the founding problem's empirical basis were to be seriously challenged by future evidence (e.g., if scaling plateaus, or if no emergent alignment problems appear despite scale), the constraint would transition to clear mandatrophy: the founding problem would shift from 'contested' to 'dead,' and the extractive architecture would persist only through theatrical maintenance and institutional capture. Regulatory attention should center on whether this constraint is solving its founding problem or has become a mechanism for governance capture; that distinction is not yet resolvable from current evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superintelligence_capability_timeline_uncertainty,
    'What is the probability distribution over the timeline to superintelligence-level capabilities? Is it 5 years, 20 years, 100+ years, or fundamentally irreducible uncertainty?',
    'Continued empirical observation of scaling dynamics, breakthrough discoveries in AI architectures, emergence of unexpected capability ceilings or plateaus, and external verification studies (not authored by labs making superintelligence claims) on the correlation between scale and emergent capabilities.',
    'If superintelligence is 50+ years away, the existential-risk prioritization extracts governance attention from present harms on behalf of uncertainty rather than measured threat, shifting the constraint from tangled-rope (real coordination problem) toward snare (speculative-victim extraction). If superintelligence is plausibly imminent (5-10 years), the prioritization becomes justified by precautionary principle and the extractive framing diminishes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(superintelligence_capability_timeline_uncertainty, empirical, 'Timeline uncertainty for superintelligence-level capabilities').

omega_variable(
    alignment_technical_solvability,
    'Are the technical problems of aligning superintelligent systems actually solvable through the safety research and lab-internal review that the existential-risk frame prioritizes? Or are they fundamentally political/governance problems requiring external accountability and democratized decisions about capability development?',
    'Advances in interpretability and control theory (if breakthrough progress occurs on technical alignment, the question tilts toward technical solvability); case studies of internal safety review failures (if labs'' internal mechanisms systematically fail to catch misalignment, the question tilts toward governance); emergence of superintelligence without prior alignment solutions (empirical proof of the pudding).',
    'If alignment is fundamentally technical, the existential-risk frame is justified and extraction is coordination overhead. If alignment is political, the frame is cover for refusing external accountability, and extraction is the primary mechanism. If the question is inherently undecidable (both matter, but their interaction is not yet understood), the constraint represents genuine coordination under irreducible uncertainty, maintaining the tangled-rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alignment_technical_solvability, conceptual, 'Whether superintelligence alignment is primarily a technical or political problem').

omega_variable(
    victim_set_temporal_weighting_ambiguity,
    'Is it normatively justified to weight potential future superintelligence victims (humanity''s long-term potential) as more morally significant than present victims experiencing measurable algorithmic bias, displacement, and surveillance? How is that weighting decided, and by whom?',
    'Explicit ethical frameworks articulating the moral weight of speculative future harms vs. present documented harms; cross-cultural and cross-disciplinary deliberation on victim prioritization (not confined to AI-safety researchers); deliberative democracy experiments with affected communities on governance priorities.',
    'If future victims are weighted equally with present victims, the existential-risk prioritization extracts governance attention and should be rejected in favor of integrated frameworks. If future victims are weighted higher, the prioritization is justified. If the weighting question is viewed as irreducibly plural (different communities reasonably prioritize differently), the constraint should be classified as imposing one group''s values on others — turning it from coordination toward extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_temporal_weighting_ambiguity, preference, 'Normative weighting of future speculative victims vs. present documented harm victims').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression that keeps present-harm researchers and labor organizations out of AI governance primarily structural (institutional barriers, funding gatekeeping, venue control) or internalized (researchers outside x-risk adopt the existential-risk frame as the correct one and self-silence criticism)?',
    'Post-exit suppression trajectory analysis: if suppression decreases when external barriers are removed, it is primarily structural; if suppression persists in isolated communities that have exited the constraint (local AI ethics boards not subject to x-risk institution influence), it is internalized. Survey research on self-censorship among non-x-risk researchers.',
    'If suppression is primarily structural, removing barriers (democratizing governance, defunding x-risk monopolies, opening venues for present-harm work) would solve the problem. If internalized, the constraint has created lasting cognitive patterns that persist even after the external mechanism is removed — the victims carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanisms').

omega_variable(
    kernel_reading_contestation,
    'Is this constraint one reading of a contested kernel (''AI risk governance priorities''), or is the contest actually about incompatible underlying facts (superintelligence risk is/is not real) rather than shared recognition with different framings?',
    'Detailed analysis of what would convince parties in each reading to adopt another reading. If superintelligence denial would convince existential-risk researchers to adopt near-term-harms reading, and vice versa, the contest is about empirical facts (not readings). If parties maintain their reading despite empirical changes, the readings are genuinely alternative value framings (true readings of a kernel). If parties insist on their reading regardless of evidence, the contest involves ideological investment, not just value choice.',
    'If the contest is empirical, the constraint should be reformulated into separate stories for ''superintelligence risk is real'' and ''superintelligence risk is overblown'' (different constraints, different ε). If the contest is about readings of a shared kernel, the three reading stories are correct and linked via network effects. If the contest involves ideological investment, additional omega variables should document the identity-fusion and institutional-survival stakes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether this is genuinely a contested kernel (shared facts, different values) or a contested empirical claim (disagreement about what is true)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airgp_xrisk_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(airgp_xrisk_tr_t3, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 3, 0.39).
narrative_ontology:measurement(airgp_xrisk_tr_t6, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 6, 0.43).
narrative_ontology:measurement(airgp_xrisk_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement(airgp_xrisk_tr_t15, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(airgp_xrisk_tr_t20, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 20, 0.49).
narrative_ontology:measurement(airgp_xrisk_tr_t25, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(airgp_xrisk_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(airgp_xrisk_be_t3, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 3, 0.53).
narrative_ontology:measurement(airgp_xrisk_be_t6, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(airgp_xrisk_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(airgp_xrisk_be_t15, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(airgp_xrisk_be_t20, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(airgp_xrisk_be_t25, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 25, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(airgp_xrisk_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(airgp_xrisk_su_t3, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(airgp_xrisk_su_t6, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(airgp_xrisk_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(airgp_xrisk_su_t15, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(airgp_xrisk_su_t20, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(airgp_xrisk_su_t25, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__existential_risk_reading, 0.18).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% The AI risk governance priority kernel decomposes into three reading-instantiated constraints: existential_risk_reading (this story, superintelligence prioritization), near_term_harms_reading (present algorithmic harms prioritization), and bridge_reading (integrated frameworks). These are not different constraints evaluated from different angles — they are structurally distinct claims with different ε values, different victim sets, different beneficiary structures, and different enforcement mechanisms. The existential-risk reading treats future humanity as the ultimate victim (non-agent, invoked in narrative); the near-term-harms reading treats present marginalized populations as primary victims (concrete, organized); the bridge reading treats both as structurally entangled. The ε values differ by magnitude: near-term-harms reading has high ε on algorithmic bias (measured impact), low ε on coordination problems (governance is possible without superintelligence scenarios); existential-risk reading has speculative ε on superintelligence (no observed misalignment catastrophe yet), low ε on present algorithmic bias (treated as secondary). These are separate constraints, linked by network effects: if the near-term-harms reading gains policy traction, it reduces existential-risk frame dominance; if existential-risk prioritization monopolizes governance, it forecloses funding for near-term-harm work. Each reading should be generated as its own story with its own stakeholder analysis, six-questions answers, and metrics, then linked via this network block.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__existential_risk_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
