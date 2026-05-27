% ============================================================================
% CONSTRAINT STORY: dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_priority_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dual_priority_reading
 *   human_readable: Dual Priority Reading: AI Safety as Concurrent Existential and Near-Term Harm Mitigation
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   The dual priority reading instantiates a specific commitment to treating
 *   existential AI risk and near-term harms as non-competing priorities
 *   requiring concurrent governance and research investment. This reading
 *   emerges from institutional pressure to avoid explicit prioritization
 *   while addressing valid claims from both populations: immediate harms from
 *   current AI systems are measurable and affecting vulnerable groups now,
 *   while long-term existential risks reflect genuine system capability
 *   dynamics requiring long-term institutional attention. The constraint
 *   exhibits tangled rope structure because it attempts genuine coordination
 *   (both priorities can be addressed through overlapping governance
 *   infrastructure) while embedding asymmetric extraction (the framing
 *   obscures resource tradeoffs, enables institutional actors to benefit from
 *   ambiguity about which priority actually guides allocation, and can
 *   paralyze action on both fronts compared to explicit prioritization). The
 *   theater ratio has risen over the interval as the dual priority commitment
 *   has become institutionalized in mission statements and governance
 *   documents without proportional change in actual resource distribution,
 *   suggesting increasing performative content relative to functional
 *   constraint on AI development.
 *
 * KEY AGENTS:
 *   - Near-Term Harm Populations: Primary victim (powerless/trapped) — communities facing algorithmic bias, surveillance, content moderation failures, labor displacement. Trapped by dependency on AI systems with no alternative infrastructure. Experience dual priority framing as extraction when resources flow to speculative existential research.
 *   - Future Generations / Existential Concern Advocates: Secondary victim (powerless/trapped) — trapped by temporal distance and inability to self-organize. Benefit from dual priority framing's institutional legitimacy but subordinated when near-term problems demand resources.
 *   - AI Governance and Policy Bodies: Institutional actor (moderate/constrained) — face stakeholder pressure and technical uncertainty. Experience genuine coordination function (oversight addressing both harms is technically coherent) but also asymmetric extraction from institutional ambiguity.
 *   - Safety Research Institutions: Primary beneficiary (institutional/arbitrage) — arbitrage between longtermist and civil rights funding sources. Experience constraint as pure coordination enabling broad research programs.
 *   - AI Industry Deployment Actors: Institutional actor (institutional/arbitrage) — dual commitment has become performative (piton), justifying incremental governance without constraining deployment timelines.
 *   - Governance Reform Coalitions: Organized agents (organized/constrained) — perceive dual priority as temporary institutional arrangement with sunset (scaffold) as evidence and norms clarify true priorities.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function embedded in extractive institutional dynamic (tangled rope).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_priority_reading, 0.58).
domain_priors:suppression_score(dual_priority_reading, 0.62).
domain_priors:theater_ratio(dual_priority_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_priority_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(dual_priority_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dual_priority_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(dual_priority_reading, "Dual Priority Reading: AI Safety as Concurrent Existential and Near-Term Harm Mitigation").
narrative_ontology:topic_domain(dual_priority_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_priority_reading, '7d624c4a-6635-4652-a6bd-cad26716872e').
narrative_ontology:cs_created_at('7d624c4a-6635-4652-a6bd-cad26716872e', '').
narrative_ontology:cs_kernel_codification('7d624c4a-6635-4652-a6bd-cad26716872e', distributed).
narrative_ontology:cs_authority_grounding('7d624c4a-6635-4652-a6bd-cad26716872e', distributed).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_priority_reading, safety_research_institutions).
narrative_ontology:constraint_beneficiary(dual_priority_reading, governance_frameworks).
narrative_ontology:constraint_victim(dual_priority_reading, near_term_harm_populations).
narrative_ontology:constraint_victim(dual_priority_reading, long_term_existential_concern_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEAR-TERM HARM POPULATIONS (SNARE) — Communities facing immediate AI harms (algorithmic bias in hiring/lending, surveillance systems, labor displacement, content moderation failures) experience the dual priority framing as extraction disguised as resource distribution. The commitment to also address existential risk is presented as non-competing, but resource and attention scarcity are real. These populations bear concrete, measurable harms now while existential harm remains hypothetical. Trapped by dependency on AI systems with no alternative infrastructure. Maximum experienced extraction — abstract future concern paralyzes present harm mitigation.
constraint_indexing:constraint_classification(dual_priority_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS / EXISTENTIAL CONCERN (SNARE) — Advocates for existential risk mitigation face symmetric entrapment: the dual priority framing promises attention to their concern but subordinates it to more tractable near-term problems. When resources conflict, near-term harm reduction wins because it is measurable and legible. Future generations have no present advocate with exit options; their interests are subordinated through the justification of 'solving both.' Trapped by temporal distance and inability to self-organize. Maximum experienced extraction — concrete problems crowd out structural prevention.
constraint_indexing:constraint_classification(dual_priority_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: AI GOVERNANCE AND POLICY BODIES (TANGLED ROPE) — Constrained by stakeholder pressure, technical uncertainty, and political economy. Experience genuine coordination function: establishing oversight structures that address both immediate deployment harms and long-term system alignment is technically coherent and institutionally necessary. But also experience asymmetric extraction: the dual priority framing justifies slow governance (addressing both prevents moving decisively on either), and institutional actors benefit from maintaining ambiguity about which priority actually guides resource allocation. Mixed experience — coordination need is real, but extraction mechanism embedded in the ambiguity.
constraint_indexing:constraint_classification(dual_priority_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SAFETY RESEARCH INSTITUTIONS (ROPE) — Primary institutional beneficiary. The dual priority framing legitimizes funding for speculative existential risk research (which has high prestige and lower immediate accountability) while positioning institutions as addressing near-term harms through the same research agenda. Can arbitrage between funding sources: existential risk funding from longtermist donors, near-term harm funding from civil rights organizations and tech accountability advocates. Experiences the constraint as pure coordination — justifying broad research programs that address both. Net beneficiary.
constraint_indexing:constraint_classification(dual_priority_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: AI INDUSTRY DEPLOYMENT ACTORS (PITON) — The dual priority framing has become substantially performative: companies declare commitment to both existential safety and near-term harm reduction, but deployment timelines are driven by competitive incentives, not safety precaution. The dual commitment justifies incremental governance: 'we are addressing both concerns through responsible scaling.' Theater ratio is high because the commitment structure persists through institutional inertia and stakeholder appeasement despite minimal functional constraint on deployment decisions. Sees its own dual-priority commitment as degraded — it persists because alternatives (aggressive regulation, deployment moratoria) are politically infeasible, not because the mechanism works.
constraint_indexing:constraint_classification(dual_priority_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GOVERNANCE REFORM COALITIONS (SCAFFOLD) — Organized advocates (civil society, affected communities, technical researchers) see the dual priority framework as a temporary institutional arrangement with a sunset clause. The framework is designed to distribute attention while establishing oversight infrastructure that can later be weaponized toward either priority as evidence accumulates. Low effective extraction because these agents have agency and perceive an exit path: as evidence on existential vs. near-term risk trajectories clarifies, governance can shift to reflect true priority without wholesale institutional redesign. Constraint duration is bounded by evidence and norm convergence.
constraint_indexing:constraint_classification(dual_priority_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL VIEW (TANGLED ROPE) — From a civilizational perspective, the dual priority reading exhibits both genuine coordination and asymmetric extraction. Genuine coordination: AI systems have both immediate deployment harms and long-term capability dynamics that require concurrent analysis and governance. The analytical structure that addresses both is more robust than ignoring one. Asymmetric extraction: the framing obscures resource tradeoffs, enables institutional actors to benefit from ambiguity, and paralyzes action on both fronts compared to clear prioritization. The constraint is neither pure coordination nor pure extraction — it is a hybrid where the coordination function is real but embedded in an extractive institutional dynamic.
constraint_indexing:constraint_classification(dual_priority_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_priority_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dual_priority_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dual_priority_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dual_priority_reading, TR),
    TR >= 0.70.

:- end_tests(dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The dual priority reading produces extraction through institutional ambiguity about resource allocation. Safety research institutions and governance bodies benefit from the ambiguity because it legitimizes funding for both exploratory existential research and near-term harm mitigation without forcing difficult tradeoff decisions. The measurement trajectory (0.42 → 0.58) reflects increasing extractiveness as the commitment has become institutionalized without proportional clarification of actual priorities — institutions use the dual priority framing to expand budgets while maintaining flexibility about where resources actually flow. Suppression (0.62): Moderate-high. Real material and epistemic barriers to simultaneous action on both priorities: resource constraints, technical uncertainty about existential risks, difficulty in measuring and attributing near-term harms. Institutional suppression of clear priority articulation: the dual priority framing itself suppresses specification of which harms are tractable now vs. which require long-term prevention. Theater ratio (0.55, rising to 0.61): Moderate, increasing. Industry dual safety commitments are substantially performative — declarations of commitment to both priorities persist while deployment timelines remain driven by competitive incentives. Governance theater: dual priority committees and frameworks are established, but actual decision-making clarity on priority is limited. Safety research theater is lower but present: speculative existential research is framed as addressing both immediate and long-term concerns when the overlap is often marginal.
 *
 * PERSPECTIVAL GAP:
 *   The dual priority reading exhibits maximum perspectival divergence across agent positions. Near-term harm populations and existential risk advocates both experience entrapment (snare) — trapped because the dual priority framing paralyzes action on their specific concern by constantly referencing the other. They compete for attention within a framework that claims to serve both. Safety research institutions experience the constraint as pure coordination (rope) — the dual framing legitimizes broad research programs and enables institutional growth. Governance bodies experience tangled rope — genuine coordination need (oversight addressing both is technically coherent) embedded in extraction (ambiguity enables institutional flexibility and slower governance). Industry actors experience piton — the dual commitment is performative, maintained through institutional inertia and stakeholder appeasement rather than functional constraint on deployment. Reform coalitions see scaffold — a temporary framework that can mature into clearer governance as evidence accumulates. The analytical observer sees tangled rope — the coordination function is real but embedded in extractive institutional dynamics. This perspectival divergence is the hallmark of the dual priority reading: it attempts to serve incompatible constituencies simultaneously, producing different experienced constraints for different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value derives from their structural relationship to the dual priority constraint. Near-term harm populations are victims without exit (trapped) — d → 0.95, maximum experienced extraction. Existential risk advocates are also victims without exit but with longer temporal distance — d → 0.90, high experienced extraction. Safety research institutions are beneficiaries with arbitrage options — d → 0.10, negative or low extraction (they experience subsidy). Governance bodies are mixed: constrained agents with some coordination benefit but also extraction from ambiguity — d → 0.60, moderate extraction. Industry deployment actors are beneficiaries with high arbitrage capability — d → 0.05, minimal experienced extraction. Reform coalitions are organized agents with exit paths and perceived sunset — d → 0.50, symmetric. The analytical observer sees the full structure — d → 0.72, analytical extraction. The f(d) sigmoid transforms these d values into experienced chi according to the formula χ = ε × f(d) × σ(S). Beneficiaries and those with exit options experience lower chi even at high ε; victims without exit experience high chi even at moderate ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The dual priority reading resolves mandatrophy by articulating the specific hybrid structure: genuine coordination function (overlapping governance addressing both priorities is technically coherent and institutionally necessary) embedded in asymmetric extraction (the ambiguity about which priority guides allocation produces institutional benefit for specific actors while paralyzing action for both victim populations). This is textbook tangled rope. The coordination dimension is real — you cannot coherently develop AI governance that ignores either immediate deployment harms or long-term capability dynamics. The extraction dimension is real — the institutional response has been to maintain ambiguity about which concern drives resource allocation, enabling different stakeholders to claim victory while actual prioritization remains opaque. The mandatrophy dissolves when the constraint is understood as a hybrid: both functions are present, both are necessary, and the extraction mechanism operates through the institutional ambiguity about weighting, not through denying the coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the dual priority framing a coherent reading of AI safety commitments, or does it naturalize resource scarcity as a structural necessity when prioritization is actually a policy choice?',
    'Comparative analysis of resource allocation outcomes across institutions claiming dual priority vs. institutions with explicit single-priority mandates. Track which populations receive resource investment over 5-10 year horizons.',
    'If dual priority produces balanced resource distribution: tangled rope reading is confirmed. If one priority systematically dominates: dual priority is false framing (false summit) and sibling readings (existential_risk_reading, near_term_harms_reading) describe the actual constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether dual priority framing describes coherent institutional commitment or naturalizes resource scarcity').

omega_variable(
    resource_scarcity_ontology,
    'Is resource scarcity between existential risk mitigation and near-term harm reduction structural or contingent on current funding/governance models?',
    'Economic analysis of counterfactual resource expansion: if existential risk funding doubled without constraint on near-term harm funding, would actual allocation change? What would unified research agendas (addressing both simultaneously) require in budget and personnel?',
    'If scarcity is structural: the two priorities are genuinely in tension, and the dual priority reading is the least-bad compromise (tangled rope with high extraction). If scarcity is contingent: the dual priority framing is an institutional choice, not a natural constraint, and reclassifies toward rope or snare depending on who benefits from the scarcity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_scarcity_ontology, empirical, 'Whether resource scarcity between priorities is structural or contingent on funding models').

omega_variable(
    evidence_asymmetry_interpretation,
    'How should asymmetric evidence streams (concrete near-term harms vs. speculative existential scenarios) shape priority distribution without one dominating or the other being dismissed?',
    'Epistemological analysis: comparison of evidence standards applied to near-term vs. existential claims; tracking of how evidence updates change institutional priority rankings over time.',
    'If evidence standards are asymmetric (high bar for existential, low bar for near-term): dual priority framing masks a de facto near-term priority and near-term harm populations are not actually trapped. If standards are aligned: dual priority is coherent. If existential evidence accumulates: sibling existential_risk_reading becomes more adequate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evidence_asymmetry_interpretation, conceptual, 'Whether evidence standards for existential vs. near-term risks are symmetric or create implicit priority').

omega_variable(
    institutional_beneficiary_identification,
    'Which institutional actors benefit from the ambiguity of dual priority, and would explicit single-priority mandates reduce their extractive capacity?',
    'Network analysis of funding flows, publication patterns, and career trajectories for researchers in dual-priority vs. single-priority frameworks. Track institutional growth and prestige outcomes.',
    'If specific institutions gain arbitrage benefit from ambiguity: tangled rope with identified beneficiaries is confirmed. If institutional outcomes are neutral across framings: extraction is lower than assessed and constraint reclassifies toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_beneficiary_identification, empirical, 'Which institutional actors benefit from dual priority ambiguity').

omega_variable(
    sibling_reading_adequacy,
    'Is the dual priority reading an adequate descriptor of how AI safety commitments actually function, or do the sibling readings (existential_risk_reading, near_term_harms_reading) better capture the institutional dynamics?',
    'Longitudinal analysis of safety organizations and governance bodies: track explicit priority declarations, resource allocation decisions, and crisis response patterns. Compare predictive accuracy of dual priority model vs. single-priority models.',
    'If dual priority model predicts institutional behavior accurately: this reading is confirmed. If single-priority models better predict actual behavior: this reading is a false consensus and should be replaced by one of the sibling readings in the committed kernel framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_adequacy, empirical, 'Predictive adequacy of dual priority reading vs. sibling readings').

omega_variable(
    suppression_mechanism_clarity,
    'Does the dual priority framing suppress clear articulation of which harms are tractable now vs. which require long-term capability change?',
    'Discourse analysis: frequency of explicit harm-timeline mapping in dual priority vs. single-priority safety literature. Measurement of governance clarity on intervention targets.',
    'If dual priority obscures harm-timeline mapping: suppression is primarily institutional (framing suppresses clarity). If clarity is maintained: suppression is primarily external (material barriers to intervention), reducing tangled rope assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_clarity, empirical, 'Whether dual priority framing suppresses clarity on tractable harms').

omega_variable(
    kernel_context_sibling_readings,
    'How do the existential_risk_reading and near_term_harms_reading instantiate different constraints from the same kernel, and what structural differences distinguish them from this dual_priority_reading?',
    'Comparative constraint story analysis: map beneficiary/victim sets, ε values, and perspectival gaps across all three readings. Identify which structural elements diverge and which remain constant.',
    'This omega documents the constraint family structure. Each reading has different ε (extractiveness) because each reading''s victim set and institutional beneficiaries differ. Dual priority reading attempts to include both victim populations, creating higher suppression and extraction than either single-priority reading would face. Understanding the sibling readings illuminates why dual priority is tangled rope rather than rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_context_sibling_readings, conceptual, 'Structural differentiation of dual priority reading from sibling existential and near-term readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_priority_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_priority_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dual_tr_t3, dual_priority_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(dual_tr_t6, dual_priority_reading, theater_ratio, 6, 0.55).
narrative_ontology:measurement(dual_tr_t9, dual_priority_reading, theater_ratio, 9, 0.61).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_priority_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dual_be_t3, dual_priority_reading, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(dual_be_t6, dual_priority_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(dual_be_t9, dual_priority_reading, base_extractiveness, 9, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_priority_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dual_priority_reading, existential_risk_reading).
narrative_ontology:affects_constraint(dual_priority_reading, near_term_harms_reading).
narrative_ontology:affects_constraint(dual_priority_reading, governance_clarity_bottleneck).

% DUAL FORMULATION NOTE:
% The dual priority reading is one of three readings of the AI safety commitment kernel (ai_safety_commitment). The existential_risk_reading and near_term_harms_reading represent alternative institutional framings with different ε values and beneficiary/victim structures. The dual priority reading attempts to encompass both by establishing shared governance infrastructure, producing tangled rope (mixed coordination and extraction) rather than pure rope or snare. All three readings are linked to governance_clarity_bottleneck, which describes the meta-institutional constraint that prevents clear priority articulation across all three readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
