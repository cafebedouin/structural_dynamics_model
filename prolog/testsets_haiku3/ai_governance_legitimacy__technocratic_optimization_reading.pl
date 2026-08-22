% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__technocratic_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__technocratic_optimization_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_governance_legitimacy__technocratic_optimization_reading
 *   human_readable: Technocratic Optimization Reading of AI Governance Legitimacy
 *   domain: political_theology/technology_governance/ethical_theory
 *
 * SUMMARY:
 *   This constraint embodies one reading of how AI governance legitimacy
 *   should be constituted. The technocratic optimization reading treats
 *   technical expertise, demonstrated capability, efficiency metrics, and
 *   innovation speed as the primary sources of governance legitimacy. Ethical
 *   constraints (dignity, justice, subsidiarity, common good) are reframed as
 *   secondary optimization parameters to be balanced against feasibility and
 *   growth. This generates a governance regime that concentrates
 *   decision-making authority with technology firms, investors, and technical
 *   experts; benefits early adopters and high-skill workers; and concentrates
 *   costs on displaced workers, digitally unconnected communities, and those
 *   subject to algorithmic discrimination. This reading is one of four
 *   sibling readings of a contested kernel (ai_governance_legitimacy) — the
 *   Catholic tradition's encyclical statements on AI, technology, and
 *   economics ground a different reading (magisterial_subsidiarity) where
 *   theological and ethical principles are primary. Democratic pluralist and
 *   market libertarian readings offer two other framings. The task here is to
 *   author ONLY this reading as a structurally independent constraint with
 *   its own ε, beneficiary/victim structure, and temporal dynamics. The
 *   constraint claims rope status (genuine coordination around efficiency
 *   metrics), but the metrics reflect substantial extraction and ongoing
 *   suppression of alternatives. The authored claim and metrics diverge
 *   intentionally — the engine will measure that divergence.
 *
 * KEY AGENTS:
 *   - technology_firms: Agenda-setters; benefit from efficiency-first governance; shape standards and regulatory interpretation; extract gains from growth metrics
 *   - venture_capital_investors: Beneficiaries; profit from efficiency gains and exit valuations; fund narratives privileging innovation imperatives
 *   - technical_experts: Agenda-setters; occupy gatekeeping roles; legitimate governance through demonstrated competence; authority rests on technical capability
 *   - high_skill_workers: Beneficiaries; concentrate in AI firms; benefit from expertise-based status and wage premiums
 *   - early_adopters: Beneficiaries; gain early access to AI capabilities; can navigate emerging systems
 *   - displaced_workers: Victims; trapped by job displacement; lack resources for transition; bear concentrated costs of automation prioritized by efficiency-first governance
 *   - digitally_unconnected_communities: Victims; constrained by lack of infrastructure; governance prioritizing efficiency over universal access leaves them unserved
 *   - algorithmically_profiled_minorities: Victims; subject to opaque discrimination; governance treating opacity as acceptable efficiency cost; constrained exit from algorithmic systems
 *   - democratic_constituencies: Excluded; voice enters only after expert-set frameworks; pluralist value integration is downstream
 *   - magisterial_authority: Excluded; ethical and theological principles treated as aspirational rather than primary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__technocratic_optimization_reading, 0.35).
domain_priors:suppression_score(ai_governance_legitimacy__technocratic_optimization_reading, 0.42).
domain_priors:theater_ratio(ai_governance_legitimacy__technocratic_optimization_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__technocratic_optimization_reading, rope).
narrative_ontology:human_readable(ai_governance_legitimacy__technocratic_optimization_reading, "Technocratic Optimization Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__technocratic_optimization_reading, "political_theology/technology_governance/ethical_theory").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__technocratic_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__technocratic_optimization_reading, '667e2327-92c3-40da-aa15-e226b091e592').
narrative_ontology:cs_kernel_codification('667e2327-92c3-40da-aa15-e226b091e592', distributed).
narrative_ontology:cs_authority_grounding('667e2327-92c3-40da-aa15-e226b091e592', expertise).
narrative_ontology:cs_interpretation_layer_present('667e2327-92c3-40da-aa15-e226b091e592').
narrative_ontology:cs_reading_relation('667e2327-92c3-40da-aa15-e226b091e592', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('667e2327-92c3-40da-aa15-e226b091e592', ai_governance_legitimacy__democratic_pluralist_reading, influences).
narrative_ontology:cs_reading_relation('667e2327-92c3-40da-aa15-e226b091e592', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('667e2327-92c3-40da-aa15-e226b091e592', foundational, technical_expertise_confers_governance_legitimacy).
narrative_ontology:cs_axiom_status(technical_expertise_confers_governance_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('667e2327-92c3-40da-aa15-e226b091e592', technical_expertise_confers_governance_legitimacy, instrumental).
narrative_ontology:cs_axiom('667e2327-92c3-40da-aa15-e226b091e592', foundational, efficiency_maximization_is_primary_welfare_criterion).
narrative_ontology:cs_axiom_status(efficiency_maximization_is_primary_welfare_criterion, holdable).
narrative_ontology:cs_axiom_grounding('667e2327-92c3-40da-aa15-e226b091e592', efficiency_maximization_is_primary_welfare_criterion, empirically_contingent).
narrative_ontology:cs_axiom('667e2327-92c3-40da-aa15-e226b091e592', secondary, ethical_constraints_secondary_to_feasibility).
narrative_ontology:cs_axiom_status(ethical_constraints_secondary_to_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('667e2327-92c3-40da-aa15-e226b091e592', ethical_constraints_secondary_to_feasibility, instrumental).
narrative_ontology:cs_reference_frame('667e2327-92c3-40da-aa15-e226b091e592', expert_technical_authority_over_ai_governance).
narrative_ontology:cs_drift_state('667e2327-92c3-40da-aa15-e226b091e592', contemporary_regulatory_backlash_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('667e2327-92c3-40da-aa15-e226b091e592', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, technology_firms).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, venture_capital_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, digitally_unconnected_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_minorities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, workers_in_automated_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shape AI governance frameworks through technical standard-setting bodies, regulatory consultation, and control over deployment infrastructure. Benefit from governance regimes that prioritize speed-to-market, efficiency metrics, and innovation incentives. Their interpretation of feasibility constraints often determines what governance structures are deemed viable.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, technology_firms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__technocratic_optimization_reading, technology_firms, agenda_setter).

% Seek returns from AI companies and exit valuations. Profit from efficiency gains and growth metrics that this reading privileges. Fund narratives emphasizing innovation imperative and technical capability as primary legitimacy sources. Constrain capital flows to firms adopting 'over-regulated' governance models.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, venture_capital_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Concentrate in AI firms, research institutions, and technical roles that this reading's emphasis on expertise elevates. Experience wages, status, and opportunity concentration from governance models that treat technical capability as the primary legitimacy criterion. Exit options remain open to move between firms or jurisdictions.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers, beneficiary,
    powerful, biographical, mobile, global).

% Gain disproportionate access to AI capabilities, products, and services prioritized under this reading. Benefit from optimization for efficiency and scale before welfare impacts stabilize or alternatives emerge. Can choose to adopt or wait; possess enough technical literacy to navigate early-stage deployments.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Experience job displacement from automation prioritized under efficiency-first governance. Lack the technical skills or capital to transition to growth sectors. No meaningful exit: retraining programs are under-resourced, alternative employment is scarce in their localities. Bear concentrated costs of governance that subordinates transition support to growth imperatives.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers, payer,
    powerless, biographical, trapped, local).

% Lack infrastructure, education, or resources to participate in AI economy benefits. Governance prioritizing efficiency and scale over universal access means infrastructure investment is determined by market profitability rather than need. Exit requires geographic relocation or large-scale infrastructure subsidy, neither under their control.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, digitally_unconnected_communities, payer,
    powerless, generational, constrained, local).

% Subject to opaque algorithmic profiling and discrimination (lending, employment, housing, criminal justice). Governance treating ethical constraints as secondary to efficiency allows deployment of high-extractive algorithms with minimal transparency or remediation. Exit options are limited: cannot opt out of systems (credit scoring, job applications, police algorithms) that govern access to essential services.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_minorities, payer,
    moderate, biographical, constrained, global).

% Employment threatened by automation accelerated under efficiency-first governance. Governance that treats displacement as an acceptable efficiency cost rather than a welfare constraint leaves them with limited protection or transition support. Constrained exit: cannot move between sectors without retraining investment they cannot afford.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, workers_in_automated_sectors, payer,
    moderate, biographical, constrained, regional).

% Occupy gatekeeping roles in AI governance through membership in standards bodies, academic institutions, technical committees, and corporate research divisions. Legitimacy rests on demonstrated technical competence and successful optimization of metrics (speed, capability, efficiency). Authority to set governance boundaries derives from this expertise, which this reading privileges as primary.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, technical_experts, agenda_setter,
    institutional, generational, arbitrage, global).

% The Catholic Magisterium issues encyclicals and social doctrine (Evangelium Gaudium, Fratelli Tutti, and statements on AI) grounding governance legitimacy in subsidiarity, solidarity, and common good. This reading treats those principles as aspirational values to be balanced against feasibility rather than as primary legitimacy criteria. The Magisterium is structurally excluded from primary governance authority by the technical-expertise gate.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, magisterial_authority, excluded,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(ai_governance_legitimacy__technocratic_optimization_reading, magisterial_authority).

% Populations whose consent is typically sought only after governance frameworks are set by technical experts and firms. Democratic deliberation and pluralist value integration are treated as downstream implementation concerns rather than primary legitimacy sources. Excluded from setting initial governance parameters; their voice enters only as constraint on otherwise expert-determined optimization.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, democratic_constituencies, excluded,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_non_agent(ai_governance_legitimacy__technocratic_optimization_reading, democratic_constituencies).

% National governments and regulatory bodies face pressure to adopt frameworks that technical experts and firms present as 'necessary for competitiveness.' Often lack internal technical expertise to adjudicate against industry framing. Regulatory authority is squeezed between innovation imperative (efficiency gains) and welfare protections (ethical constraints subordinated in this reading).
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, regulatory_authorities, observer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI governance around shared efficiency metrics, technical standards, and demonstrated capability as legitimacy source. Enables rapid innovation iteration, market competition, and optimization of aggregate welfare outcomes defined by growth, capability expansion, and productivity gains.
% TRANSFER_FUNCTION: Transfers governance authority from democratic institutions and ethical frameworks to technical experts and firms. Moves social costs (worker displacement, algorithmic harm, unequal access) from corporations and investors to workers, marginalized communities, and digitally unconnected populations. Concentrates control over what counts as 'legitimate' AI governance to those who can articulate their interests in efficiency language.
% ABSENT_VOICES: Democratic constituencies, displaced workers, and those profiled by algorithms are structurally excluded from primary governance standard-setting. They enter only as implementation constraints or after-the-fact objectors. The Magisterium's subsidiarity and solidarity principles are treated as external values to be 'balanced' rather than as constitutive of governance legitimacy.
% DISAPPEARANCE_RATIONALE: If this reading's legitimacy disappeared overnight, AI governance would reorganize around democratic deliberation, ethical principles as primary rather than secondary constraints, and subsidiarity in deployment. Technical expertise would remain necessary but not sufficient for legitimacy. Efficiency would be one optimization target among many rather than the primary one. Capital flows, firm strategies, and regulatory frameworks would shift significantly.
% FOUNDING_PROBLEM: Early AI governance suffered from fragmentation, unclear standards, and competing jurisdictional claims. No clear decision-procedure for allocating governance authority. Technical capability was advancing faster than ethical or legal frameworks could keep pace.
% FOUNDING_PROBLEM_CORROBORATION: Technology firms and venture capital attest the founding problem remains live and requires ongoing efficiency-prioritized governance to maintain innovation velocity. Displaced workers, marginalized communities, and ethicists attested in multiple public forums (academic papers, regulatory testimony, civil society reports from outside the tech sector) that the problem is partly solved but the solution has created new harms by treating welfare as secondary to efficiency.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__technocratic_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__technocratic_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__technocratic_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__technocratic_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__technocratic_optimization_reading, 0.35, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__technocratic_optimization_reading_tests).
:- end_tests(ai_governance_legitimacy__technocratic_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate rather than low or high: the constraint does generate genuine coordination benefits (unified standards, faster innovation, market-driven optimization), but it also concentrates gains toward beneficiaries while distributing costs to victims. The measurement trajectory shows extractiveness rising from 0.28 to 0.38 over the first 20 time units (rising standards, growing firm concentration, increased automation), then declining slightly to 0.35 at t=25 (possibly reflecting early regulatory backlash or public awareness of harms). Suppression (0.42) is moderate: the constraint's persistence depends on ongoing active suppression of alternatives (excluding democratic voice, overriding ethical principles, managing regulatory frames), but suppression is not total (resistance maintains ~0.52). Theater ratio (0.38) reflects that roughly 38% of governance activity is performative rather than functional: standards bodies convening to discuss ethics while efficiency decisions are pre-made; CSR commitments that do not constrain core business; expert consultations where technical constraint is predetermined. The measurement trajectory shows theater rising from 0.25 to 0.42 (increasing performative compliance layers) then declining to 0.38 (possible simplification or cynicism setting in). All three measurements are authored on one shared time grid: every metric has a value at every time point, ensuring no metric substitution artifacts in temporal analysis.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (technology firms, technical experts) seat and the payer (displaced workers, algorithmically profiled minorities) seats should compute radically different types from the engine. From the agenda-setter perspective, this is genuine rope: coordination around efficiency metrics, market competition, technical standards. From the payer perspective, the same structure operates as constrained extraction (they bear costs they did not choose, cannot exit, and whose necessity is asserted by those who benefit). The engine will compute directionality per seat from power + exit_options + beneficiary/victim declarations, and should show this divergence. The high-skill-worker and early-adopter beneficiaries should compute beneficiary-end directionality (low d); displaced workers and digitally unconnected communities should compute target-end directionality (high d). The constraint's persistence appears coordinated from the beneficiary seats because efficiency gains are real; appears extractive from the payer seats because costs are concentrated and exits are constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology firms and technical experts hold institutional power and arbitrage-grade exit (can shift standards, move operations, re-frame governance). Their directionality is beneficiary-end (d near 0.0): they benefit from the efficiency frame and can revise it if it becomes disadvantageous. Venture capital and high-skill workers are powerful to powerful-plus-mobile: they benefit (d low). Early adopters are moderate power but mobile exit: they benefit (d moderate-low). Displaced workers are powerless with trapped exit: they pay and cannot leave (d high, near 1.0). Digitally unconnected communities are powerless with constrained exit: they pay without choice (d high). Algorithmically profiled minorities are moderate power but constrained exit from systems: they pay through discrimination they cannot opt out of (d high-moderate). Democratic constituencies and the Magisterium are excluded: they are not seated in the constraint mechanism itself, though their exclusion is constitutive of how the constraint operates. The directionality overrides are not needed here: the structural derivation from power + exit + beneficiary/victim declarations should produce the right d values without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented governance, competing standards, uncertainty about allocation of authority) appears live according to the beneficiary seats and contested according to the payer seats. From the tech firm perspective, the problem is unsolved because competitive pressure and innovation speed are still subject to uncertainty. From the displaced-worker perspective, the founding problem was partly about ensuring just transitions and protecting dignity—a problem now worse, not solved. The constraint shows classic mandatrophy risk: the original problem (fragmentation) has been solved by consolidating authority around technical expertise and firms, but the solution created new harms (displacement, algorithmic discrimination, digital exclusion) not present in the problem being solved. If the founding problem is reclassified as 'dead' (fragmentation is solved; AI governance is now highly coordinated) but the constraint persists with new justifications (innovation requires speed, competition requires flexibility), and the measurement trajectory shows rising theater and extraction, the constraint becomes a candidate for mandatrophy reclassification. The theater_ratio trajectory (rising from 0.25 to peak 0.42 at t=20, then declining to 0.38) suggests performative maintenance may be strengthening: governance statements about ethics, standards, and inclusion multiplying while actual decision-making remains efficiency-driven.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_welfare_measurement_ambiguity,
    'How is ''aggregate welfare'' measured and whose welfare is included in the aggregation? Is it utilitarian summation across all affected parties, GDP growth as proxy, investor returns, or aggregate capability expansion? Each choice produces radically different governance implications.',
    'Explicit declaration of the welfare function used in governance decisions. Cross-comparison with empirical welfare outcomes for different constituencies (displaced workers vs. investors, early adopters vs. digitally unconnected) to test whether declared welfare aggregation matches observed outcomes.',
    'If aggregate welfare excludes or systematically down-weights certain constituencies'' experienced harms (displacement, algorithmic discrimination), the constraint reclassifies from rope toward snare. If welfare function is transparent and includes all constituencies, the coordination story strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aggregate_welfare_measurement_ambiguity, empirical, 'Whether the efficiency metric genuinely aggregates all stakeholder welfare or covertly weights it.').

omega_variable(
    technical_expertise_authority_grounding,
    'Does technical competence in AI systems genuinely confer legitimacy for AI governance, or does it confer only necessary-but-insufficient expertise that must be combined with ethical, democratic, and theological reasoning?',
    'Comparative analysis of governance outcomes under technical-expertise-primary vs. participatory-democracy-primary models. Examination of whether major governance failures (algorithmic discrimination, unjust displacement, privacy harms) correlate with over-reliance on technical authority vs. under-incorporation of ethical/democratic input.',
    'If technical expertise is necessary-but-not-sufficient, this reading''s claim to legitimate authority collapses. The constraint reclassifies from rope (genuine coordination) toward tangled rope or snare (authority extraction under coordination cover). If technical expertise proves sufficient across diverse stakeholder outcomes, the rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_expertise_authority_grounding, conceptual, 'Whether technical competence constitutes sufficient legitimacy for governance authority.').

omega_variable(
    encyclical_principles_subordination_mechanism,
    'Are Catholic Social Doctrine principles (subsidiarity, solidarity, common good) genuinely ''balanced against feasibility'' in practice, or are they systematically overridden whenever they conflict with efficiency and innovation imperatives?',
    'Tracing specific governance decisions where these principles would require different outcomes (e.g., priority for displaced worker transition over automation speed, mandatory transparency over algorithmic opacity, subsidiarity-driven local control over centralized standards). Empirical record of whether such conflicts are resolved through genuine negotiation or default to efficiency.',
    'If principles are consistently subordinated without genuine balancing, the claim to rope-type coordination weakens. The arrangement appears more as extraction (firms extracting efficiency gains while paying lip-service to ethical constraints) than coordination. If genuine balancing occurs, the rope claim strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(encyclical_principles_subordination_mechanism, empirical, 'Whether ethical principles are genuinely balanced or functionally subordinated.').

omega_variable(
    kernel_contestation_sibling_foreclosure,
    'This reading treats technical expertise and efficiency as primary legitimacy sources. Does this premise logically foreclose the magisterial_subsidiarity_reading (which treats theological authority and common-good principles as primary), or do the readings coexist as genuinely alternative frameworks neither logically contradicting the other?',
    'Formal analysis of the core premises: does technical-expertise-primary entail the falsity of magisterial-authority-primary? Or do they rest on incommensurable rather than contradictory claims? (Technical vs. theological authority can rest on different incommensurable epistemologies; they need not logically foreclose each other unless one makes an exclusive claim.)',
    'If the readings logically foreclose each other, the reading_relation is ''forecloses'' rather than ''coexists_with''. If they rest on incommensurable but not contradictory premises, they coexist as live alternatives, and the relation remains ''coexists_with''. This affects the downstream classification of kernel legitimacy contests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contestation_sibling_foreclosure, conceptual, 'Whether this reading''s premises logically foreclose the subsidiarity reading or merely offer an alternative framing.').

omega_variable(
    regulatory_capture_vs_genuine_expertise,
    'To what extent does the framing of ''technical expertise as primary legitimacy source'' function to legitimate regulatory capture—where firms and investors fund the standards bodies, research institutions, and regulatory committees that articulate that expertise?',
    'Funding source analysis of standards bodies and expert committees. Comparison of governance recommendations from experts with conflicting interests (funded by affected firms) vs. experts without conflicts. Examination of how ''infeasibility'' objections are raised when welfare protections are proposed.',
    'If regulatory capture is systematic, the authority structure is structurally compromised. The extraction (from workers and marginalized communities) is obscured as technical necessity rather than acknowledged as power asymmetry. The constraint reclassifies toward snare (pure extraction wearing a coordination cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_vs_genuine_expertise, empirical, 'Whether technical-expertise framing masks regulatory capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__technocratic_optimization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ai_g_tr_t25, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(ai_g_be_t25, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 25, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(ai_g_su_t25, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__technocratic_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__technocratic_optimization_reading, 0.12).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested kernel ai_governance_legitimacy. The kernel is the persisting framework for legitimate AI governance and the allocation of authority. Each reading instantiates a distinct constraint with its own ε, beneficiary/victim structure, and temporal dynamics. The readings form a constraint family linked by network.affects_constraints (each reading influences the structural conditions for the others). The decomposition respects ε-invariance: each reading authors one stable ε relative to the standing arrangement it describes, never averaged across readings. Readers should not expect 'balance' or averaging—each reading describes the constraint from its own epistemic and normative standpoint. The kernel itself (what legitimate AI governance should be) remains contested; the constraints model the four live readings, each internally coherent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
