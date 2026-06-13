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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_governance_legitimacy__technocratic_optimization_reading
 *   human_readable: Technocratic Optimization Reading of AI Governance Legitimacy
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the technocratic optimization reading
 *   of AI governance legitimacy—the claim that legitimate AI governance
 *   derives from maximizing aggregate welfare, efficiency, and innovation,
 *   with ethical constraints treated as secondary optimization parameters.
 *   Authority rests with technical expertise and demonstrated performance
 *   rather than democratic deliberation, ethical tradition, or market
 *   competition. This is ONE reading of a contested kernel (the kernel being
 *   'AI governance legitimacy' itself). Three sibling readings—magisterial
 *   subsidiarity, democratic pluralism, and market libertarianism—offer
 *   structurally distinct accounts of what makes AI governance legitimate.
 *   This constraint story describes only the technocratic reading, as a
 *   clean, ε-invariant structure. The contest with siblings is routed to
 *   omega variables and cs_structure fields per the kernel-reading protocol.
 *   The authored metrics describe a moderate-extractiveness coordination
 *   constraint: beneficiaries (tech firms, investors, high-skill workers,
 *   early adopters) are consolidated and benefit from the framing; victims
 *   (displaced workers, algorithmically profiled populations, communities
 *   lacking infrastructure) are dispersed, lack voice in the legitimacy
 *   frame, and have minimal exit options. The claim/metric gap is INTENTIONAL
 *   and STRUCTURAL: the reading claims itself as 'rope' (coordination for
 *   genuine welfare gain), while the metrics show activation energy required
 *   to suppress alternative readings and excluded voices. This divergence is
 *   exactly what the measurement apparatus exists to detect.
 *
 * KEY AGENTS:
 *   - technology_firms: institutional power, set optimization targets, capture regulatory bodies, frame legitimacy in efficiency terms
 *   - institutional_investors: concentrate gains from rapid deployment, benefit from deferred ethical compliance
 *   - high_skill_workers: embedded in the expertise hierarchy, benefit from professional standing and wage premiums
 *   - early_adopters: capture first-mover advantages before regulatory constraints tighten
 *   - displaced_workers: powerless, trapped, bear concentrated job-loss and wage costs
 *   - communities_lacking_digital_infrastructure: powerless, trapped, receive no benefit and bear exclusion from governance
 *   - algorithmically_profiled_populations: identity-locked (cannot exit the systems that determine their opportunities), structurally excluded from decisions about the algorithms that govern them
 *   - magisterial_authority: excluded from legitimacy frame, would demand subordination of technology to ethical principles
 *   - democratic_publics: excluded, would demand deliberative participation and protection of vulnerable populations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__technocratic_optimization_reading, 0.35).
domain_priors:suppression_score(ai_governance_legitimacy__technocratic_optimization_reading, 0.42).
domain_priors:theater_ratio(ai_governance_legitimacy__technocratic_optimization_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__technocratic_optimization_reading, rope).
narrative_ontology:human_readable(ai_governance_legitimacy__technocratic_optimization_reading, "Technocratic Optimization Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__technocratic_optimization_reading, "theological_ethics/technology_governance/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__technocratic_optimization_reading, '88a671c4-840c-44d4-ac7e-7fc6e41b4b7a').
narrative_ontology:cs_kernel_codification('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', fixed_text).
narrative_ontology:cs_authority_grounding('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', extraction).
narrative_ontology:cs_interpretation_layer_present('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a').
narrative_ontology:cs_reading_relation('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', foundational, welfare_maximization_primacy).
narrative_ontology:cs_axiom_status(welfare_maximization_primacy, holdable).
narrative_ontology:cs_axiom_grounding('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', welfare_maximization_primacy, instrumental).
narrative_ontology:cs_axiom('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', foundational, technocratic_expertise_deference).
narrative_ontology:cs_axiom_status(technocratic_expertise_deference, holdable).
narrative_ontology:cs_axiom_grounding('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', technocratic_expertise_deference, empirically_contingent).
narrative_ontology:cs_reference_frame('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', technical_expertise_optimization_primacy).
narrative_ontology:cs_drift_state('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', contemporary_post_ethical_objection_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88a671c4-840c-44d4-ac7e-7fc6e41b4b7a', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, technology_firms).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, institutional_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, communities_lacking_digital_infrastructure).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_populations).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, technocratic_expertise_legitimacy).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, welfare_maximization_primacy).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, innovation_growth_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and deploy AI systems; frame legitimacy in terms of aggregate welfare gains, efficiency metrics, and innovation output. Control the technical definitions of success and optimization targets. Justify rapid deployment and opacity as necessary for performance. Benefit directly from reduced regulatory friction and from capturing efficiency gains as shareholder value.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, technology_firms, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__technocratic_optimization_reading, technology_firms, beneficiary).

% Profit from technology firm growth and market dominance. Support the technocratic reading because it legitimizes rapid scaling and defers costly ethical compliance to secondary status. Benefit from capturing portions of efficiency gains through equity appreciation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, institutional_investors, beneficiary,
    institutional, biographical, mobile, global).

% Employed in AI development, data annotation oversight, algorithm auditing, and technical governance roles. Benefit from high wages, professional prestige, and the intellectual challenge of optimization work. Their expertise is centered in the legitimacy frame; they are the 'technical experts' whose demonstrated performance justifies authority. Exit to other sectors is available but opportunity cost is high.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers, beneficiary,
    moderate, biographical, mobile, global).

% Capture disproportionate gains from early access to AI capabilities before regulatory constraints or ethical scrutiny limit deployment. Includes financial institutions, large platforms, data-rich enterprises. Benefit from first-mover advantages in efficiency gains and from the legitimacy frame that delays competitive regulation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters, beneficiary,
    powerful, biographical, arbitrage, global).

% Workers whose occupations are automated or deskilled by AI deployment: manufacturing, administrative processing, routine knowledge work. Bear the concentrated costs of job loss and wage depression. Their interest in slowing deployment or demanding transition support is structurally excluded from the optimization frame, which treats displacement as a normal externality.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers, payer,
    powerless, biographical, trapped, local).

% Rural, low-income, or geographically isolated populations without reliable broadband, digital literacy infrastructure, or local technical capacity. AI governance optimized for aggregate welfare in high-connectivity zones excludes them from the definition of 'everyone' the optimization serves. They receive no benefit from efficiency gains but carry costs as AI systems optimized elsewhere degrade service quality, increase fraud targeting, or determine their credit/employment/benefit eligibility.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, communities_lacking_digital_infrastructure, payer,
    powerless, generational, trapped, local).

% Individuals and groups whose behavior, creditworthiness, job suitability, or risk profile is determined by opaque AI systems optimized for aggregate welfare, not individual fairness. Minorities, low-income applicants, and those with limited data profiles are systematically disadvantaged by optimization that ignores group-level disparity. The technocratic frame treats their exclusion from decision-making about the systems that govern their lives as an acceptable trade-off for faster innovation. Exit is structurally impossible: the systems are mandatory and refusal to participate is not an option.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_populations, payer,
    powerless, biographical, identity_locked, global).

% The Catholic Magisterium (the teaching authority of the Church, grounded in Catholic Social Doctrine) is excluded from the technocratic legitimacy frame. Its principles (common good, subsidiarity, solidarity, universal destination of goods, human dignity as intrinsic and non-instrumental) directly contradict the elevation of efficiency and innovation above ethical constraints. The Magisterium would object to treating dignity as a secondary parameter rather than the optimization target itself.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, magisterial_authority, excluded,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(ai_governance_legitimacy__technocratic_optimization_reading, magisterial_authority).

% Citizens and democratic representatives are structurally excluded from the legitimacy frame. The technocratic reading vests authority in technical expertise and demonstrated performance, not in democratic deliberation. Public objection to rapid deployment, demands for transparency, or insistence on placing ethical constraints before innovation are reframed as obstacles to welfare maximization rather than legitimate expressions of democratic will.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, democratic_publics, excluded,
    organized, generational, constrained, national).

% The structural tendency for technology firms to shape regulatory bodies through expert advisory positions, revolving-door employment, funding of academic research, and control of technical standards. This is not a conscious conspiracy but a systematic feature of the legitimacy frame: firms that can demonstrate technical expertise and performance gains acquire the standing to define what counts as acceptable governance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, regulatory_capture_mechanisms, observer,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_non_agent(ai_governance_legitimacy__technocratic_optimization_reading, regulatory_capture_mechanisms).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the deployment, evaluation, and governance of AI systems around shared metrics of aggregate welfare, efficiency, and innovation speed. Solves the problem of how to make rapid AI advancement decisions in the absence of (or prior to) comprehensive ethical and democratic consensus. Provides a unified framework for resource allocation, priority-setting, and trade-off justification across diverse technical domains.
% TRANSFER_FUNCTION: Transfers the right to set governance priorities from inclusive democratic or ethical deliberation to technical experts and firm leaders. Transfers the benefits of efficiency gains from displaced workers and vulnerable populations to technology firms, investors, and high-skill workers. Transfers decision-making authority about algorithmic systems from those affected by them to those who build them. Transfers the framing of legitimacy itself from multiple traditions to a single metric (aggregate welfare as measured by technical experts).
% ABSENT_VOICES: The Catholic Magisterium and religious ethical traditions are structurally excluded—they would argue that human dignity is non-negotiable and the optimization target itself, not a secondary constraint. Democratic publics and affected communities are excluded—they would demand deliberative participation and protection of vulnerable groups before rapid deployment. Displaced workers and those profiled by opaque systems are excluded—they would demand that their interests be centered, not treated as externalities. Labor unions, civil society organizations, and philosophical ethicists from non-technocratic traditions would object if present. The exclusion itself is the enforcement mechanism: framing these voices as 'obstacles to innovation' removes them from the legitimacy frame entirely.
% DISAPPEARANCE_RATIONALE: If the technocratic optimization reading of AI governance legitimacy disappeared—if authority reverted to democratic deliberation, Magisterial ethical principles, market libertarianism, or multi-tradition pluralism—the entire governance structure would reorganize. Deployment timelines would slow to permit deliberation. Ethical constraints would be elevated from secondary parameters to non-negotiable principles. Affected communities would gain standing in governance decisions. Regulatory capture would lose its normative justification. The firms and investors who benefit would face new constraints and slower growth. The efficiency gains that were the reading's primary success metric would be subordinated to other values.
% FOUNDING_PROBLEM: Early AI development lacked adequate governance frameworks. Decisions were made ad hoc, without systematic evaluation of consequences. The founding problem was one of coordination: how to make AI advancement decisions responsibly when the technology was moving faster than policy consensus, ethical clarity, and institutional capacity to regulate.
% FOUNDING_PROBLEM_CORROBORATION: Technology firms and their advocates attest the founding problem remains live—governance frameworks are still inadequate and rapid deployment is necessary to seize the innovation window. Democratic publics and the Magisterium attest the founding problem has been MISDEFINED: the real problem is not 'how to govern AI faster' but 'how to govern AI justly and democratically.' Independent ethicists, labor organizations, and civil society groups outside the technology sector attest that the founding problem has been SOLVED FOR ONE PARTY (technology firms achieved coordination and deployment) but never solved for the broader public interest. Legislative testimony and academic research from outside the technology industry document that governance frameworks now exist but are systematically weakened by regulatory capture and by the legitimacy frame that treats ethical constraints as secondary.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__technocratic_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__technocratic_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__technocratic_optimization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__technocratic_optimization_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.35) rather than high because the constraint does coordinate genuine gains: aggregate welfare, efficiency, innovation speed, and the coordination of technical decision-making ARE real benefits, not pure cover. However, extractiveness is substantial rather than minimal because those gains are asymmetrically distributed—captured by beneficiary seats—while costs (displacement, exclusion, algorithmic profiling) are concentrated on victim seats with no voice in the optimization frame. Suppression is moderate (0.42) because the constraint's persistence does NOT primarily depend on coercive force; it depends on the perceived legitimacy of the technocratic frame itself. Democratic publics and religious ethical traditions are excluded not through violence but through the delegitimization of their standing ('obstacles to innovation,' 'not relevant to technical questions'). Theater ratio climbs to 0.48–0.50 at mid-interval because the constraint increasingly requires performative compliance with ethical-sounding language (ethics boards, fairness metrics, responsible AI frameworks) that operate WITHIN the optimization framework rather than challenging it. These are not dishonest: they are genuine efforts to make technocratic optimization more humane. But they are insufficient to the scale of ethical objection, so they increasingly function as legitimacy theater that preserves the framework while appearing to address criticism. The slight decline in theater at t=25 reflects the constraint's maturation: initial defensive rhetoric has given way to consolidation of institutional power, reducing the need for performative ethical engagement. Accessibility collapse (0.58) reflects that alternatives to the technocratic frame ARE available (democratic deliberation, Magisterial ethics, market libertarianism) but they require departing from the institutional field (leaving technology governance, exiting the labor market, opting out of algorithmic systems). For those embedded in the frame—tech firms, high-skill workers, investors—alternatives are highly collapsed. For excluded populations—displaced workers, algorithmically profiled individuals—they are already trapped. Resistance (0.62) is substantial: labor unions, democratic movements, religious institutions, civil society organizations, and affected communities all mount real resistance to technocratic optimization. This resistance is visible and ongoing, not suppressed into invisibility. However, it is systematically excluded from the legitimacy frame's self-understanding, allowing the frame to persist while resistance is reframed as 'obstruction' or 'opposition to progress.' The shared time grid reflects the constraint's real temporal dynamics: extractiveness rises initially as deployment accelerates and efficiency gains accrue to beneficiaries, plateaus as labor displacement reaches saturation, and declines slightly as political pressure and regulatory accumulation slow deployment. Theater rises as ethical objections accumulate, peaks as performative compliance becomes standard practice, then plateaus as performative compliance is institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence should be marked. From the technology firm and investor seat, this constraint is genuine rope: it solves real coordination problems, enables rapid innovation, and produces aggregate welfare gains that are broadly shared (many people use AI services, efficiency gains lower prices, etc.). From the displaced worker seat, it is closer to snare: the coordination benefits accrue to others while costs are concentrated and unavoidable. From the magisterial or democratic seat, it is a false coordination claim covering unjust extraction: the 'aggregate welfare' metric excludes the dignity and participation of those it purports to serve. The engine should compute DIFFERENT types from each seat given the structural data: technology firm (moderate power, arbitrage exit, beneficiary status) likely computes as rope or tangled_rope; displaced workers (powerless, trapped exit, victim status) likely compute as snare; excluded democratic publics (powerful but excluded from framing, constrained exit) likely compute as tangled_rope with high suppression. These divergences ARE the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology firms and investors sit at d ≈ 0.1–0.2: they benefit substantially (low d for beneficiaries) and have high exit optionality (arbitrage, mobile exits available). High-skill workers sit at d ≈ 0.3–0.4: they benefit significantly but are somewhat identity-locked into the technical expertise hierarchy. Displaced workers sit at d ≈ 0.85–0.95: they are trapped (no exit), victims (bear costs), and have minimal power to reshape the frame. Algorithmically profiled populations sit at d ≈ 0.90–0.98: identity-locked into mandatory algorithmic systems with no exit option, victimized by optimization that ignores their dignity, powerless. Excluded democratic publics sit at d ≈ 0.70–0.80: they have structural power (democratic standing) but are actively excluded from this particular governance frame and their exit options are constrained (they cannot simply leave democratic society). The directionality override is NOT needed here because the structural data derive d accurately without it: beneficiary status and exit options place tech firms low, trapped status and victim status place displaced workers and profiled populations high, exclusion and constrained exit place democratic publics in the mid-to-high range.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: early AI development needed governance frameworks. The constraint has partially SOLVED that problem—there is now a coordination frame, decision procedures, and resource allocation mechanisms for AI governance. However, the founding problem has also been MISDEFINED by the benefiting parties (technology firms): they have treated 'how to govern AI faster' as the problem rather than 'how to govern AI justly.' The mandatrophy question is whether the constraint's original function (coordination of technical decisions) persists or has atrophied into theatrical maintenance of a legitimacy frame. Evidence suggests PARTIAL atrophy: the constraint DOES coordinate technical decisions (that function is live). But it ALSO performs legitimacy theater (performative ethical engagement without substantive constraint on optimization). The mismatch between founding problem (genuine governance need) and current function (efficiency maximization with ethical window-dressing) suggests mandatrophy is EMERGING but not yet complete. The constraint persists because it benefits consolidated parties (tech firms, investors) who maintain it, not because it serves the founding problem anymore. This is not yet full Piton (the coordination function IS still working), but it is trending toward it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_welfare_measurement_contest,
    'Whose conception of ''aggregate welfare'' is authoritative? Does the technocratic frame''s definition (efficiency gains, innovation speed, GDP-correlated metrics) capture actual human flourishing, or does it exclude goods (dignity, meaningful work, community participation, environmental integrity, political voice) that other readings treat as essential to welfare?',
    'Empirical research on subjective wellbeing, capability deprivation, and life satisfaction across beneficiary and victim populations. Philosophical argument about the relationship between measured welfare and substantive human goods. Deliberative engagement with excluded voices to assess whether they experience the constraint as welfare-enhancing or welfare-degrading.',
    'If aggregate welfare is substantially different from the technocratic frame''s metrics, the constraint fails its own legitimacy claim and should be reclassified as extraction (snare) rather than coordination (rope). If the measurements align, the technocratic reading has stronger standing. If the readings diverge by population (high measured welfare for beneficiaries, low for victims), the constraint is a clear case of asymmetric extraction masked by an aggregate metric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aggregate_welfare_measurement_contest, empirical, 'Whether technocratic welfare metrics capture actual human flourishing or exclude essential goods.').

omega_variable(
    expertise_legitimacy_grounding,
    'On what basis do technical experts have standing to define the public good in AI governance? Is their expertise purely epistemic (they know more about how systems work), normative (they understand what humans should value), or political (they hold power to shape outcomes)? Can technical expertise alone adjudicate ethical and democratic questions about how AI should be deployed?',
    'Genealogical and historical analysis of how technical expertise acquired normative and political standing in governance. Comparison with other domains (medicine, engineering, environmental science) where expertise is consulted but not granted sole authority. Democratic experiments in inclusive deliberation about AI governance to test whether non-expert publics can engage substantively.',
    'If technical expertise is purely epistemic, its role should be advisory to democratic and ethical deliberation, not the primary legitimacy ground. If technical experts conflate their epistemic standing with normative authority, the constraint''s legitimacy foundation is unsound. If expertise has been mistaken for democracy (experts deciding FOR the public rather than advising the public), the constraint is a case of regulatory capture masked as coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expertise_legitimacy_grounding, conceptual, 'Whether technical expertise can bear the weight of normative and political authority the technocratic frame places on it.').

omega_variable(
    identity_lock_in_algorithmic_profiling,
    'To what extent is the suppression of victim voices (algorithmically profiled populations) structural (external barriers: inability to opt out of systems, lack of legal standing, powerlessness) versus internalized (cognitive patterns: acceptance of algorithmic verdicts, belief in system legitimacy, learned helplessness)? If the identity lock is substantially internalized, does the constraint carry that suppression beyond the algorithmic system itself?',
    'Post-constraint thought experiments: if algorithmic profiling were prohibited but the cognitive patterns persisted, would individuals whose life outcomes were determined by algorithms continue to defer to algorithmic verdicts? Qualitative research on subjective experience of algorithmic systems by profiled populations. Study of belief change in communities that gain exit options from algorithmic systems.',
    'If suppression is primarily internalized, the constraint''s effective suppression is higher than the authored measure suggests—victims carry the internalization with them. If structural, the suppression ends when exit is available. Mixed mechanisms (both structural and internalized) would require separate treatment for escape versus lasting harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_algorithmic_profiling, empirical, 'Whether algorithmic system suppression is structural or internalized.').

omega_variable(
    sibling_reading_logical_status,
    'Do the sibling readings logically foreclose each other, or can they coexist as competing readings held by different institutional actors? Specifically: does the magisterial reading''s elevation of dignity as the optimization TARGET foreclose the technocratic reading''s treatment of dignity as a secondary CONSTRAINT? Or are these genuinely different framings that different parties can hold simultaneously?',
    'Formal logical analysis of the axioms: if one reading''s foundational claim directly contradicts the other''s core premise, foreclosure obtains. If readings can coexist by assigning different interpretive authority (one to the Magisterium, one to technical experts, one to democratic deliberation), coexistence obtains. Test case: can a single institutional actor (e.g., a Catholic technology company) hold both readings, or must they choose?',
    'If foreclosure: the kernel contest is resolvable through logical argument; one reading will eventually win. If coexistence: the kernel contest is structural and will persist; the resolution is political and institutional, not logical. If coexistence but with one party having power to marginalize others: the constraint becomes a case of regulatory capture and legitimacy theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_logical_status, conceptual, 'Whether sibling readings foreclose or coexist logically.').

omega_variable(
    performance_capture_by_theater,
    'As the constraint''s theater ratio rises (from 0.35 to 0.48–0.50), is performative ethical compliance (ethics boards, fairness metrics, responsible AI frameworks) a genuine constraining force on optimization, or a legitimacy-maintenance mechanism that allows optimization to continue? Are the performative elements being absorbed into the constraint''s real function, or do they represent emergent counterpressure that will eventually force reorganization?',
    'Audit of ethics board decisions: do they ever block a deployment the optimization frame would permit? Measurement of how often ''fairness'' metrics change actual system behavior versus how often they are honored in the breach. Longitudinal study of whether performative compliance increases or decreases the actual harm experienced by victim populations.',
    'If performative elements genuinely constrain optimization, the constraint is stabilizing into sustainable form and may be transitioning away from snare-like extraction toward genuine rope. If performative elements are pure theater, the constraint is moving toward full Piton: atrophied real function maintained by institutional inertia and legitimacy performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_capture_by_theater, empirical, 'Whether rising theater ratio reflects genuine constraint or pure legitimacy theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__technocratic_optimization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(ai_g_tr_t0, observed).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(ai_g_tr_t5, observed).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(ai_g_tr_t10, observed).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(ai_g_tr_t15, observed).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(ai_g_tr_t20, observed).
narrative_ontology:measurement(ai_g_tr_t25, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(ai_g_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(ai_g_be_t0, observed).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement_basis(ai_g_be_t5, observed).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(ai_g_be_t10, observed).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement_basis(ai_g_be_t15, observed).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(ai_g_be_t20, observed).
narrative_ontology:measurement(ai_g_be_t25, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement_basis(ai_g_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(ai_g_su_t0, observed).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(ai_g_su_t5, observed).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(ai_g_su_t10, observed).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(ai_g_su_t15, observed).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(ai_g_su_t20, observed).
narrative_ontology:measurement(ai_g_su_t25, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(ai_g_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__technocratic_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__technocratic_optimization_reading, 0.18).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'ai_governance_legitimacy.' It is linked to three sibling readings that offer structurally distinct accounts of what makes AI governance legitimate: magisterial_subsidiarity_reading (grounds authority in Catholic Social Doctrine), democratic_pluralist_reading (grounds authority in democratic deliberation), and market_libertarian_reading (grounds authority in voluntary exchange). Each sibling has its own constraint story file with its own ε, beneficiary/victim structure, and cs_structure declaration. The kernel contest is expressed through network linkage and cs_structure reading_relations rather than collapsed into a single multi-option story. Sibling stories should be generated separately; this constraint describes the technocratic reading alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_legitimacy__technocratic_optimization_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
