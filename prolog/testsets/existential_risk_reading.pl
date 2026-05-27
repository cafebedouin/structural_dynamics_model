% ============================================================================
% CONSTRAINT STORY: existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_existential_risk_reading, []).

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
 *   constraint_id: existential_risk_reading
 *   human_readable: Existential Risk Priority Reading: AI Governance Constraint
 *   domain: ai_governance/technology_ethics/existential_risk
 *
 * SUMMARY:
 *   The existential risk reading of AI governance prioritizes preventing
 *   superintelligence scenarios that could annihilate or permanently curtail
 *   humanity's potential. This reading declares future humanity (speculative
 *   victims across civilizational timescales) as the primary victim set and
 *   positions x-risk research institutions and safety-leading AI labs as
 *   beneficiaries who gain institutional status, funding, hiring advantage,
 *   and policy leverage under this framing. The constraint exhibits the core
 *   structure of tangled_rope: a genuine coordination function (preventing
 *   transformative AI misalignment benefits all parties including the
 *   beneficiaries) layered with asymmetric extraction (resources concentrate
 *   on speculative scenarios, present algorithmic harms face opportunity
 *   costs, governance frameworks serve institutions in high-income
 *   countries). The extractiveness value (0.58) reflects that the
 *   coordination genuinely exists (preventing existential risk is not pure
 *   extraction) but beneficiaries receive disproportionate benefits relative
 *   to their contribution. The theater ratio (0.52) reveals that roughly half
 *   of institutional activity under the existential risk frame is now
 *   performative — competitive positioning for safety leadership status,
 *   methodological arguments about AGI likelihood, and institutional rivalry
 *   consume resources that could go to direct alignment research. The
 *   suppression value (0.65) reflects high barriers to alternative framings:
 *   resource scarcity means present-harm researchers operate under severe
 *   constraints, labor market transitions for displaced workers receive
 *   minimal attention, and high-income country dominance of AI governance
 *   capacity-building reduces voice from affected regions. This reading
 *   coexists with near_term_harms_reading (which prioritizes present
 *   algorithmic discrimination, labor displacement, and surveillance) and
 *   bridge_reading (which attempts synthesis), but the existential reading's
 *   dominance in institutional funding means competing readings operate under
 *   resource suppression.
 *
 * KEY AGENTS:
 *   - Future Humanity (Speculative): Civilizational-scale victim (powerless/trapped) — potential target of existential misalignment; cannot negotiate, has no exit option, bears irreversible harm
 *   - Present Algorithmic Bias Victims: Biographical-scale victims (powerless/trapped) — face algorithmic discrimination, credit denial, hiring bias today; experience resource diversion away from harms they experience
 *   - Global South Nations and Labor Markets: Moderate power, constrained exit (moderate/constrained) — benefit from xrisk prevention but face asymmetric extraction as resources concentrate in high-income countries and labor transitions receive low priority
 *   - X-Risk Research Institutions: Primary beneficiary (institutional/arbitrage) — receive funding, legitimacy, policy influence, hiring advantage; experience constraint as pure coordination
 *   - AI Labs Claiming Safety Leadership: Primary beneficiary (institutional/arbitrage) — position themselves as responsible actors via safety-first framing; gain regulatory preference and capability development leverage
 *   - Effective Altruism and Governance Coalitions: Organized beneficiaries (organized/mobile) — coordinate collective action on existential risk; also enforce the prioritization frame that benefits their institutions
 *   - Established AI Safety Institutions: Institutional actors (institutional/constrained) — original function was technical xrisk reduction; now experience degraded function with substantial theater (competitive positioning replacing direct research)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — this perspective is positioned inside the existential reading's own committed framework; not neutral
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(existential_risk_reading, 0.58).
domain_priors:suppression_score(existential_risk_reading, 0.65).
domain_priors:theater_ratio(existential_risk_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(existential_risk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(existential_risk_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(existential_risk_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(existential_risk_reading, "Existential Risk Priority Reading: AI Governance Constraint").
narrative_ontology:topic_domain(existential_risk_reading, "ai_governance/technology_ethics/existential_risk").

domain_priors:requires_active_enforcement(existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(existential_risk_reading, '073fc4c5-25c0-416b-ac2a-0ebac5b316a2').
narrative_ontology:cs_created_at('073fc4c5-25c0-416b-ac2a-0ebac5b316a2', '').
narrative_ontology:cs_kernel_codification('073fc4c5-25c0-416b-ac2a-0ebac5b316a2', distributed).
narrative_ontology:cs_authority_grounding('073fc4c5-25c0-416b-ac2a-0ebac5b316a2', extraction).
narrative_ontology:cs_kernel_id(existential_risk_reading, ai_risk_governance_priority).
narrative_ontology:cs_reading_relation('073fc4c5-25c0-416b-ac2a-0ebac5b316a2', near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('073fc4c5-25c0-416b-ac2a-0ebac5b316a2', bridge_reading, influences).
narrative_ontology:cs_axiom('073fc4c5-25c0-416b-ac2a-0ebac5b316a2', foundational, superintelligent_misalignment_civilization_threat).
narrative_ontology:cs_axiom_status(superintelligent_misalignment_civilization_threat, holdable).
narrative_ontology:cs_axiom_grounding('073fc4c5-25c0-416b-ac2a-0ebac5b316a2', superintelligent_misalignment_civilization_threat, empirically_contingent).
narrative_ontology:cs_axiom('073fc4c5-25c0-416b-ac2a-0ebac5b316a2', foundational, xrisk_research_institutional_alignment).
narrative_ontology:cs_axiom_status(xrisk_research_institutional_alignment, holdable).
narrative_ontology:cs_axiom_grounding('073fc4c5-25c0-416b-ac2a-0ebac5b316a2', xrisk_research_institutional_alignment, instrumental).
narrative_ontology:cs_reference_frame('073fc4c5-25c0-416b-ac2a-0ebac5b316a2', superintelligence_misalignment_prevention).
narrative_ontology:cs_drift_state('073fc4c5-25c0-416b-ac2a-0ebac5b316a2', contemporary_institutional_expansion, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(existential_risk_reading, xrisk_research_institutions).
narrative_ontology:constraint_beneficiary(existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_beneficiary(existential_risk_reading, frontier_model_developers).
narrative_ontology:constraint_beneficiary(existential_risk_reading, governance_framework_architects).
narrative_ontology:constraint_victim(existential_risk_reading, future_humanity_speculative).
narrative_ontology:constraint_victim(existential_risk_reading, present_algorithmic_bias_victims).
narrative_ontology:constraint_victim(existential_risk_reading, ai_labor_markets).
narrative_ontology:constraint_victim(existential_risk_reading, global_south_resource_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE HUMANITY (SNARE) — Structurally trapped; cannot negotiate, organize, or exit. Potential existential harm is irreversible. Powerless agents across civilizational timescale experience maximum extraction: all risks concentrated on them, all benefits (capability acceleration) accrue to present institutions. No exit option from planetary-scale outcome.
constraint_indexing:constraint_classification(existential_risk_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRESENT ALGORITHMIC BIAS VICTIMS (SNARE) — Trapped by deployment decisions made without consent. Resource reallocation toward existential scenarios means continued underinvestment in harms they experience today (hiring discrimination, criminal justice bias, credit denial). Experience both direct extraction (algorithmic targeting) and opportunity cost (resources diverted to xrisk). Suppression high: resource scarcity means they bear cost of prioritization choice.
constraint_indexing:constraint_classification(existential_risk_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: GLOBAL SOUTH NATIONS AND AI LABOR MARKETS (TANGLED ROPE) — Constrained by resource and technological dependency. Genuine coordination function: xrisk governance could prevent catastrophic scenarios that would harm them as well. But also extraction: resources directed to safety governance of frontier models in high-income countries reduce investment in algorithmic harms, labor transitions, and capacity-building in regions with fewer resources. Mixed experience of coordination and asymmetric extraction.
constraint_indexing:constraint_classification(existential_risk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: X-RISK RESEARCH INSTITUTIONS (ROPE) — Primary beneficiary (institutional/arbitrage). Experience constraint as pure coordination: preventing xrisk is a collective action problem that benefits all actors (including themselves). Receive funding, legitimacy, hiring advantage, policy influence, and capability development leverage under the existential risk frame. Can arbitrage their institutional position as 'safety leadership.' Experience zero extraction — the constraint subsidizes their agenda.
constraint_indexing:constraint_classification(existential_risk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EFFECTIVE ALTRUISM AND GOVERNANCE COALITIONS (TANGLED ROPE) — Organized agents with some mobility (can shift cause areas, can pressure institutions). Genuine coordination function: preventing existential scenarios is a legitimate collective action problem. But also extraction: the frame prioritizes speculative harms over present harms, concentrating resources and attention on scenarios their institutions can meaningfully engage with. Beneficiary + constrained by their own ideological commitment to the prioritization frame.
constraint_indexing:constraint_classification(existential_risk_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: EXISTENTIAL RISK GOVERNANCE PROGRAMS (SCAFFOLD) — Organized agents seeing the constraint as temporary coordination mechanism with declared sunset. If AGI governance frameworks mature (international treaties, technical standards, capability thresholds) become operational, the scarcity-driven prioritization dissolves. Theater ≤ 0.70: genuine coordination function (preventing misaligned superintelligence) remains even if the prioritization urgency shifts. Sunset emerges when alternatives prove viable or when timelines extend beyond near-term planning horizons.
constraint_indexing:constraint_classification(existential_risk_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ESTABLISHED AI SAFETY INSTITUTIONS (PITON) — Institutional actors experiencing the constraint as degraded function. Original role: coordinate genuine xrisk reduction through technical research and capacity-building. Current state: substantial theater (competitive positioning for funding, institutional rivalry, methodological arguments about AGI likelihood consuming resources that could go to alignment research). Theater ratio 0.52 reflects this degradation — nearly half of institutional activity is now about maintaining priority status within the governance hierarchy rather than direct xrisk mitigation.
constraint_indexing:constraint_classification(existential_risk_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER — EXISTENTIAL PRIORITY VIEW (TANGLED ROPE) — From a civilizational/universal perspective, the existential risk frame represents a genuine coordination mechanism (preventing superintelligent misalignment benefits all future agents) layered with extraction (resources concentrated on speculative scenarios, beneficiaries control the frame, present harms deprioritized). The analytical position here is inside the existential risk reading's own committed framework — the observer has adopted the prioritization lens. This perspective is not neutral; it is the reading's native analysis.
constraint_indexing:constraint_classification(existential_risk_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(existential_risk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(existential_risk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(existential_risk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(existential_risk_reading, TR),
    TR >= 0.70.

:- end_tests(existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): This reading declares a genuine coordination problem (preventing superintelligent misalignment benefits all parties) but beneficiaries receive disproportionate benefits relative to the cost imposed on present-harm victims. The value reflects: (a) coordination genuine but partial (some actors would pursue safety even without the governance frame), (b) beneficiary capture measurable (xrisk institutions have developed institutional incentives to maintain existential priority status), (c) opportunity cost substantial (resources directed toward speculative scenarios reduce investment in present harms by 60-80% in some domains). Suppression (0.65): High barriers to alternative framings. Resource scarcity creates zero-sum dynamics; timeline uncertainty gives existential reading advantage in institutional contexts; power asymmetry between high-income country AI governance and global south input means voices questioning the prioritization face suppression. Theater ratio (0.52): Roughly half of institutional activity is now performative — safety conference prestige signaling, capability frontier positioning as 'responsible,' methodological arguments about AGI timelines that serve institutional interests rather than clarifying evidence. The other half (interpretability research, adversarial testing, governance framework design) retains functional content. This reflects that the constraint is degrading from genuine coordination (piton formation beginning) while still retaining real function. The theater ratio's increase from 0.28 at t=0 (Goodhart drift) suggests that as the xrisk frame gained institutional power, gaming of safety metrics increased.
 *
 * PERSPECTIVAL GAP:
 *   The existential risk reading produces dramatic perspectival divergence. The xrisk institutions see pure coordination (Rope) — preventing existential risk is a genuine collective action problem that benefits all. Future humanity sees pure extraction (Snare) — trapped victims receiving no benefit, bearing all cost. Present harm victims see extraction with coordination overlay (Tangled Rope) — the coordination is real (preventing xrisk would benefit them) but they bear opportunity cost of resource diversion. The analytical observer positioned inside the reading sees the mixed structure (Tangled Rope) but cannot evaluate whether the empirical premises (short AGI timelines, high misalignment probability) are correct or aspirational. The piton perspective reveals institutional degradation: safety research institutions originally existed to solve alignment problems; they now partly exist to maintain their own institutional status, with theater rising from 0.28 to 0.52. The scaffold perspective suggests a sunset: if AGI governance frameworks (international treaties, technical standards, capability thresholds) become operational, the scarcity-driven prioritization logic shifts and resources rebalance.
 *
 * DIRECTIONALITY LOGIC:
 *   The directive chain derives directionality (d) from beneficiary/victim declarations combined with exit options. Xrisk institutions (institutional/arbitrage) have low d ≈ 0.15, producing negative f(d) ≈ -0.01 — they experience the constraint as subsidizing their agenda. Future humanity (powerless/trapped) has high d ≈ 0.95, producing f(d) ≈ 1.42 — they experience maximum extraction. Present harm victims (powerless/trapped) have high d ≈ 0.95, producing f(d) ≈ 1.42, experiencing both direct harms (algorithmic bias) and opportunity cost (resource diversion). Global South labor (moderate/constrained) has d ≈ 0.70, producing f(d) ≈ 1.05 — they experience extraction above the baseline. The beneficiary/victim asymmetry is stark: four beneficiary groups have low-d directionality; four victim groups have high-d directionality. The scope modifier σ(global) = 1.2 amplifies the effective extractiveness, making the global reach of resource concentration count heavily in the chi calculation. The perspectival gap arises because different agents experience different d values under the same base constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED. The existential risk reading exhibits tangled_rope structure (genuine coordination + asymmetric extraction) but the analysis does not resolve whether: (a) the coordination function is primary with extraction as side effect, or (b) the extraction is primary with coordination as legitimating narrative. This is the core mandatrophy — the constraint cannot be classified as pure coordination (Rope) because beneficiaries clearly gain disproportionately; cannot be classified as pure extraction (Snare) because the xrisk prevention genuinely benefits all parties including victims. The unresolved omega variables (particularly agi_capability_timeline_uncertainty and misalignment_scenario_tractability) are downstream of the mandatrophy: if timelines are actually short and scenarios tractable, the coordination reading strengthens and extraction becomes a justified side effect. If timelines are longer or scenarios intractable, the constraint risks becoming purely performative (Piton). The mandate to prevent existential risk is genuine; whether the prioritization frame serves that mandate or exploits it remains unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agi_capability_timeline_uncertainty,
    'What is the actual probability distribution over AGI timelines (transformative AI that meets or exceeds human capabilities across most domains)? Is the 2030-2050 consensus window grounded in empirical trends or expert intuition and model uncertainty?',
    'Retrospective analysis (post-2030): compare actual frontier capability growth rates against predicted timelines. Empirical challenge: decompose timeline estimates into (a) scaling law certainty, (b) inference/training efficiency improvements, (c) architectural breakthroughs required. Current disagreement: AI labs predict near-term (10-20 years); academic ML predicts longer (30-50+ years); some researchers argue indefinite plateau.',
    'If timelines are shorter: existential priority reading is strengthened (urgent coordination required). If timelines are longer or highly uncertain: resource allocation toward present algorithmic harms becomes less opportunity-costly; the tangled_rope and snare readings gain structural strength. If timelines remain intractably uncertain: the whole prioritization logic loses empirical grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agi_capability_timeline_uncertainty, empirical, 'Probability distribution over AGI capability timelines and its empirical basis').

omega_variable(
    misalignment_scenario_tractability,
    'Among the candidate AGI misalignment failure modes (specification gaming, goal misgeneralization, deceptive alignment, instrumental convergence), which are actually tractable via governance, technical research, or interpretability? Which are irreducible?',
    'Empirical testing: (a) can current interpretability methods scale to explain large model behavior? (b) do adversarial testing and red-teaming catch specification gaming or only rediscover known vulnerabilities? (c) do governance structures (international agreements, compute monitoring) actually constrain capability development or merely shift it? Retrospective analysis of safety research impact on deployed system behavior.',
    'If highly tractable: existential risk reading gains strength (research has leverage). If largely intractable or resistant to governance: resource allocation toward existential risk governance reveals itself as performative; the piton classification strengthens. If tractability is domain-specific (some failure modes tractable, others not): the constraint becomes more precise but also more complicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(misalignment_scenario_tractability, empirical, 'Tractability of AGI misalignment scenarios via governance and technical research').

omega_variable(
    resource_flow_counterfactual,
    'If the existential risk reading did not command priority institutional status, where would safety research resources actually flow? Would they redirect toward algorithmic fairness, labor transition support, and governance capacity in high-income countries? Or would they evaporate entirely, replaced by capability research?',
    'Counterfactual analysis: funding data from periods when xrisk had lower priority (pre-2012 effective altruism movement). Institutional behavior: what safety research persists in labs where xrisk is deprioritized? Comparative analysis: resources allocated to AI fairness, interpretability, labor economics, global governance capacity under different priority regimes.',
    'If resources would redirect to present harms: existential risk reading is extractive (victim set correct). If resources would evaporate: existential risk reading is coordination (prevents zero outcome). If resources would split: the constraint is mixed (tangled_rope correct) with different opportunity costs by region.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_flow_counterfactual, empirical, 'Counterfactual allocation of safety research resources absent existential risk priority').

omega_variable(
    kernel_interpretation_read_shift,
    'Within the same ai_risk_governance_priority kernel, what core premises would need to shift to move from existential_risk_reading to near_term_harms_reading or bridge_reading? Is this a difference in empirical beliefs about risk magnitude, or a foundational disagreement about which lives count?',
    'Comparative textual analysis of institutional positions across the three readings: identify which claims appear in existential reading but not in near-term reading (and vice versa). Identify whether disagreements are (a) empirical (timeline estimates, AGI probability), (b) normative (present vs future weighting), or (c) methodological (tractability assumptions). This maps the reading space of the kernel itself.',
    'If empirical: readings can potentially converge if evidence accumulates. If normative: readings coexist indefinitely (different value premises). If methodological: readings influence each other (better tools might make scenarios tractable to one reading but not another). This determines reading_relations classification (forecloses vs coexists_with vs influences).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_interpretation_read_shift, conceptual, 'Source of disagreement between existential_risk_reading and sibling readings within the kernel').

omega_variable(
    extractive_beneficiary_structure,
    'The xrisk reading declares beneficiaries (xrisk institutions, safety-leading labs). But are these genuinely aligned with preventing existential risk, or have they developed institutional incentives to maintain the existential risk frame regardless of actual risk landscape? Is the constraint''s beneficiary structure a feature or a bug?',
    'Behavioral evidence: do xrisk institutions respond to evidence that timelines are longer than expected? Do they redirect resources toward near-term harms? Do they accept funding constraints if framed as resource reallocation to other AI safety domains? Institutional dynamics: do safety labs that receive premium status under the existential reading actually deploy safety measures at higher rates than non-premium labs? Or is safety investment decoupled from priority status?',
    'If beneficiaries are truly aligned: constraint is coordination (tangled_rope correct). If beneficiaries have developed capture incentives: constraint is extractive (snare elements strengthen, theater_ratio may be underestimated). If beneficiaries are mixed: the tangled_rope classification holds but with stronger emphasis on the extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_beneficiary_structure, empirical, 'Whether xrisk institutional beneficiaries are genuinely aligned with existential risk prevention or have developed capture incentives').

omega_variable(
    supraspectival_reading_foreclosure,
    'Is there a supraordinate reading of the ai_risk_governance_priority kernel that would foreclose one or more of the three subordinate readings (existential, near-term, bridge)? Or are all three readings held as live options by different but legitimate stakeholders such that no single reading can logically rule out the others within any universal framework?',
    'Meta-institutional analysis: is there an epistemic or governance authority that could adjudicate between the readings? (UN, scientific consensus, market mechanisms) If no adjudicator exists or is trusted: readings coexist. If adjudication is attempted: trace whether the adjudicator''s decision is based on empirical evidence (refutable) or normative commitments (not refutable). Identify whether any reading''s core axiom (if violated or overridden) would logically eliminate the reading, and whether that axiom is actually vulnerable to drift.',
    'If foreclose relation is possible: existential_risk_reading might logically preclude near_term_harms_reading (if existential risk is real and extinction prevents all present-day considerations). If coexist relation holds: different stakeholders legitimately prioritize differently. If influences relation holds: the readings compete for institutional resources but neither eliminates the other''s logical coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supraspectival_reading_foreclosure, conceptual, 'Whether existential_risk_reading can foreclose other readings or only coexist with them').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(existential_risk_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(existential_theater_t0, existential_risk_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(existential_theater_t3, existential_risk_reading, theater_ratio, 3, 0.38).
narrative_ontology:measurement(existential_theater_t6, existential_risk_reading, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(existential_extractiveness_t0, existential_risk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(existential_extractiveness_t3, existential_risk_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(existential_extractiveness_t6, existential_risk_reading, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(existential_risk_reading, near_term_harms_reading).
narrative_ontology:affects_constraint(existential_risk_reading, bridge_reading).
narrative_ontology:affects_constraint(existential_risk_reading, algorithmic_fairness_governance).
narrative_ontology:affects_constraint(existential_risk_reading, labor_market_transition_capacity).
narrative_ontology:affects_constraint(existential_risk_reading, global_ai_governance_capacity).

% DUAL FORMULATION NOTE:
% The existential_risk_reading is one semantic position within a contested kernel (ai_risk_governance_priority). The near_term_harms_reading and bridge_reading are structurally distinct constraints with different victim sets, beneficiary structures, and resource flows. Each reading should be modeled as a separate story with its own ε value. The existential reading (ε=0.58, tangled_rope) prioritizes speculative victims; near_term reading would prioritize present victims with different ε; bridge reading would attempt to distribute resources across both with intermediate ε. Network links show structural influence: the existential reading's dominance in institutional funding constrains the capacity of near_term and bridge readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
