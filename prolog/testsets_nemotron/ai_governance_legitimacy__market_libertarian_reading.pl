% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: AI Governance Legitimacy — Market Libertarian Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint story represents the market libertarian reading of AI
 *   governance legitimacy: the claim that legitimate authority over AI
 *   systems derives exclusively from voluntary exchange and property rights,
 *   with innovation flourishing when unencumbered by collective mandates. The
 *   reading treats property rights as pre-political natural law — a Mountain
 *   constraint that would persist regardless of enforcement. The encyclical's
 *   subsidiarity principle is accepted as supporting decentralization, but
 *   its solidarity demands are rejected as illegitimate coercion. Enforcement
 *   operates through contract law, private arbitration, and reputational
 *   mechanisms rather than state mandate. The reading claims Mountain status
 *   (natural law) while identifiable beneficiaries (entrepreneurs, investors,
 *   high-autonomy individuals) and victims (those lacking market power,
 *   monopsony workers, coordination-failure-exposed populations) exist — this
 *   is a false summit candidate.
 *
 * KEY AGENTS:
 *   - tech_entrepreneurs: Primary beneficiary (institutional/arbitrage) — captures value from unencumbered innovation
 *   - venture_investors: Primary beneficiary (institutional/arbitrage) — extracts returns from property-rights-protected ventures
 *   - high_autonomy_individuals: Beneficiary (organized/mobile) — exercises meaningful exit options in competitive markets
 *   - market_powerless_communities: Primary victim (powerless/trapped) — bears coordination failures without market leverage
 *   - monopsony_labor_workers: Primary victim (powerless/constrained) — faces asymmetric bargaining with no credible exit
 *   - coordination_failure_exposed_populations: Victim (moderate/constrained) — suffers externalities no individual can price
 *   - magisterial_authority: Excluded (institutional/analytical) — claims legitimacy from doctrinal interpretation
 *   - democratic_polity: Excluded (institutional/analytical) — claims legitimacy from public reason and consent
 *   - technocratic_expertise: Excluded (institutional/analytical) — claims legitimacy from optimization outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.25).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.15).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "AI Governance Legitimacy — Market Libertarian Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '230640cc-eefb-4635-aa01-2ea89d149fb9').
narrative_ontology:cs_kernel_codification('230640cc-eefb-4635-aa01-2ea89d149fb9', formalized).
narrative_ontology:cs_authority_grounding('230640cc-eefb-4635-aa01-2ea89d149fb9', extraction).
narrative_ontology:cs_reading_relation('230640cc-eefb-4635-aa01-2ea89d149fb9', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('230640cc-eefb-4635-aa01-2ea89d149fb9', ai_governance_legitimacy__democratic_pluralist_reading, forecloses).
narrative_ontology:cs_reading_relation('230640cc-eefb-4635-aa01-2ea89d149fb9', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_axiom('230640cc-eefb-4635-aa01-2ea89d149fb9', foundational, property_rights_are_prepolitical_natural_law).
narrative_ontology:cs_axiom_status(property_rights_are_prepolitical_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('230640cc-eefb-4635-aa01-2ea89d149fb9', property_rights_are_prepolitical_natural_law, deontological).
narrative_ontology:cs_axiom('230640cc-eefb-4635-aa01-2ea89d149fb9', foundational, solidarity_demands_are_illegitimate_coercion).
narrative_ontology:cs_axiom_status(solidarity_demands_are_illegitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('230640cc-eefb-4635-aa01-2ea89d149fb9', solidarity_demands_are_illegitimate_coercion, deontological).
narrative_ontology:cs_reference_frame('230640cc-eefb-4635-aa01-2ea89d149fb9', classical_liberal_property_order).
narrative_ontology:cs_drift_state('230640cc-eefb-4635-aa01-2ea89d149fb9', platform_capitalism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('230640cc-eefb-4635-aa01-2ea89d149fb9', '2026-08-03T14:22:17Z').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, tech_entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, venture_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, market_powerless_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, monopsony_labor_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, coordination_failure_exposed_populations).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, property_rights_as_prepolitical).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, voluntary_exchange_as_legitimacy_source).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, exit_options_protect_dignity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Found and scale AI ventures under property-rights protection. Capture value from unencumbered innovation — no collective mandates to comply with, no solidarity taxes to pay. Exit globally via capital mobility and jurisdictional arbitrage. The constraint's enforcement (contract law, private arbitration) protects their claim on returns.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, tech_entrepreneurs, beneficiary,
    institutional, biographical, arbitrage, global).

% Deploy capital into AI ventures protected by strong property rights. Extract returns through equity appreciation and exit events. The constraint's legitimacy framework (voluntary exchange) ensures their contracts are enforceable without political interference. Exit via portfolio reallocation across jurisdictions and asset classes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, venture_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% High-skill knowledge workers with portable human capital. Exercise meaningful exit options — can choose employers, start ventures, or work independently. Benefit from competitive labor markets for AI talent. Dignity protected through ability to walk away. Constraint's reputational mechanisms reward their contributions.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    organized, biographical, mobile, global).

% Communities lacking capital, technical skills, or political connections to influence AI deployment. Bear coordination failures — algorithmic discrimination, infrastructure neglect, environmental externalities — with no market leverage to price or prevent them. Property rights regime offers no recourse; exit requires resources they don't have. Constraint's operation externalizes costs onto them.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, market_powerless_communities, payer,
    powerless, generational, trapped, regional).

% Workers in labor markets dominated by few AI-intensive employers (platform gig work, warehouse automation, data annotation). Face asymmetric bargaining — employer sets terms, worker accepts or loses livelihood. 'Exit options' are nominal: alternative employers use same algorithmic management. Constraint's property-rights framework protects employer's algorithmic control as property; worker's dignity is not protected.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, monopsony_labor_workers, payer,
    powerless, biographical, constrained, national).

% Populations exposed to AI-driven coordination failures no individual can solve: algorithmic amplification of misinformation, market manipulation, systemic bias in critical infrastructure (credit, hiring, policing). Some political voice but insufficient to overcome concentrated AI power. Constraint's voluntary-exchange framework treats these as externalities — not priced, not prevented. Exit means opting out of digital society.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, coordination_failure_exposed_populations, payer,
    moderate, biographical, constrained, global).

% Claims legitimate interpretive authority over Catholic Social Doctrine (common good, subsidiarity, solidarity, universal destination of goods). Reads the encyclical as requiring subordination of economic freedom to these principles. Excluded by this reading's axiom that property rights are pre-political and solidarity demands are coercion. Would object that market outcomes violate human dignity.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, magisterial_authority, excluded,
    institutional, civilizational, analytical, universal).

% Claims legitimacy from consent of the governed and public reason. Seeks to balance diverse values through transparent political processes. Excluded by this reading's axiom that collective mandates are illegitimate encroachments on voluntary exchange. Would object that AI governance requires democratic accountability, not just market exit.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, democratic_polity, excluded,
    institutional, generational, analytical, national).

% Claims authority from technical competence and demonstrated performance. Seeks to maximize aggregate welfare and innovation within feasible constraints. Excluded by this reading's axiom that ethical constraints beyond property rights are illegitimate. Would object that unencumbered markets produce suboptimal AI safety and alignment outcomes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, technocratic_expertise, excluded,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates AI development resources through voluntary exchange and price signals, protecting innovators' property rights so they capture returns and bear risks. Solves the problem of who decides what AI gets built and deployed by replacing political authority with market discipline.
% TRANSFER_FUNCTION: Moves control over AI development trajectories and capture of AI-generated value from political/collective processes to property-holding entrepreneurs and investors. Moves coordination-failure costs (algorithmic harm, labor displacement, infrastructure externalities) from AI developers to market-powerless communities and workers.
% ABSENT_VOICES: Magisterial authority (Catholic Social Doctrine), democratic polity (public reason), and technocratic expertise (welfare optimization) are structurally excluded — their legitimacy claims are ruled out by the property-rights axiom. They would object that dignity requires more than exit, that solidarity is not coercion, and that markets fail to coordinate AI safety. They are absent because the constraint defines them as illegitimate.
% DISAPPEARANCE_RATIONALE: If the property-rights/voluntary-exchange constraint vanished overnight, AI governance would reorganize around competing legitimacy claims: magisterial authority would assert doctrinal constraints, democratic polities would impose regulatory mandates, technocrats would optimize for welfare metrics. The world rearranges because multiple excluded seats have institutional capacity to fill the vacuum.
% FOUNDING_PROBLEM: Coordinating AI development without political capture — preventing states, churches, or technocrats from directing innovation toward their preferred ends. The market libertarian reading was built to solve this by making property rights and voluntary exchange the sole legitimate authority.
% FOUNDING_PROBLEM_CORROBORATION: Beneficiaries (tech entrepreneurs, investors) attest the problem is live — political capture of AI is an active threat. Victims (market-powerless communities, monopsony workers) and excluded authorities (magisterial, democratic, technocratic) attest the problem is dead or transformed — market concentration now reproduces the capture the founding problem feared, but through private power. Independent AI ethics researchers and labor economists outside the beneficiary set corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint's claimed operation is voluntary exchange — no direct transfer from victims to beneficiaries is authored as the constraint's mechanism. Suppression is low (0.15) because the reading denies coercion; enforcement is contractual/reputational, not state-backed violence. Theater ratio is minimal (0.10) because the coordination function (market allocation) is presented as genuine. Accessibility collapse is high (0.88) because alternatives (collective mandates, political oversight) are structurally excluded by the property-rights axiom — they appear as rights violations, not options. Resistance is near-zero (0.08) because the constraint presents itself as the natural order; opposition is framed as error, not resistance. The measurement series shows slight extractiveness creep and theater growth over 20 time units as market concentration deepens.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (entrepreneurs, investors, high-autonomy individuals), this is a Mountain — property rights are natural law, markets coordinate without coercion. From the victim seats (market-powerless, monopsony workers, coordination-failure-exposed), the same structure operates as a Snare: exit options are nominal, not real; property rights enforce extraction; coordination failures are externalized. The engine computes this divergence from the declared structural data — the claimed_type (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (tech_entrepreneurs, venture_investors, high_autonomy_individuals) are declared as collecting from the constraint's operation — they hold institutional/organized power with arbitrage/mobile exit, placing their derived directionality near 0.0 (full beneficiary). Victims (market_powerless_communities, monopsony_labor_workers, coordination_failure_exposed_populations) are declared as bearing costs — they hold powerless/moderate power with trapped/constrained exit, placing derived directionality near 1.0 (full target). The excluded seats (magisterial, democratic, technocratic) are not coordinated by this constraint; their exclusion is the point. Directionality derivation runs from these declarations; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims its mandate (protecting property rights and voluntary exchange) is live and natural. Mandatrophy would arise if the coordination function (efficient allocation via markets) atrophied while the property-rights framework persisted — e.g., if monopoly rents replaced competitive returns, or if exit options became theoretical for all but the most powerful. The slight extractiveness creep in measurements (0.18 → 0.25) and theater growth (0.05 → 0.10) over the interval suggests early mandatrophy dynamics: the arrangement increasingly serves incumbents rather than coordinating exchange. The founding problem (coordinating AI development without political capture) is contested — beneficiaries say it's live; victims say it's dead and the arrangement persists as rent extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    false_summit_natural_law_vs_constructed,
    'Is the property-rights framework a genuine natural law (Mountain) or a constructed constraint that benefits identifiable agents (Tangled Rope/Snare)?',
    'Cross-cultural and historical analysis: if property-rights regimes vary systematically with power distributions and the ''natural law'' claim emerges only when beneficiaries control discourse, the Mountain claim is constructed. Comparative study of AI governance outcomes under different property regimes.',
    'If constructed, the constraint reclassifies from Mountain to Tangled Rope (coordination + asymmetric extraction) or Snare (pure extraction) via the false_summit_mountain signature. The beneficiaries/victims structure would become the classification driver rather than the natural-law claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_vs_constructed, conceptual, 'Natural-law vs. constructed ambiguity for a Mountain with declared beneficiaries').

omega_variable(
    exit_option_reality_for_victims,
    'Are the exit options declared for victim groups (trapped/constrained) structurally real, or does the constraint''s operation render them nominal?',
    'Empirical measurement of actual mobility: job-switching rates, community relocation capacity, coordination-failure mitigation access for the declared victim groups. If exit is theoretically available but practically inaccessible, the constraint''s effective suppression is higher than authored.',
    'If exit is nominal, victim directionality shifts toward 1.0 (full target), effective extraction rises, and the constraint''s per-seat classification for victims moves from Mountain toward Snare/Tangled Rope. The Mountain claim becomes less tenable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_option_reality_for_victims, empirical, 'Whether declared exit options for victims are real or nominal').

omega_variable(
    coordination_function_vs_extraction_boundary,
    'Does the market coordination function genuinely solve AI governance problems, or does it primarily allocate gains to beneficiaries while externalizing coordination failures to victims?',
    'Counterfactual analysis: compare AI safety, alignment, and distribution outcomes under market-libertarian regimes vs. regimes with collective mandates. If collective mandates produce better coordination outcomes for the same problems, the market''s coordination claim is weakened.',
    'If the coordination function is illusory, the constraint loses its Rope/Tangled Rope coordination component and becomes pure extraction (Snare) or degraded coordination (Piton). The Mountain claim collapses entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_vs_extraction_boundary, empirical, 'Whether market coordination genuinely solves the founding problem or masks extraction').

omega_variable(
    reading_relations_structure,
    'What is the structural relationship between this market libertarian reading and its sibling readings of the ai_governance_legitimacy kernel?',
    'Analyze whether the core premises are logically contradictory (forecloses), merely held by different parties (coexists_with), or create structural pressure (influences). The property-rights-as-prepolitical axiom forecloses magisterial authority (which subordinates property to common good) and democratic authority (which subjects property to public reason) within a single framework. It influences technocratic optimization by setting the boundary conditions for what optimization can touch.',
    'Determines cs_structure.reading_relations values. Foreclosure with magisterial and democratic readings means they cannot coexist in one governance framework; coexistence with technocratic reading means market libertarianism sets the arena within which technocratic optimization operates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_structure, conceptual, 'Structural relationship of this kernel reading to its siblings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_tr_t5, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_tr_t10, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_tr_t15, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_be_t5, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 5, 0.21).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_be_t10, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_be_t15, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_su_t5, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_su_t10, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_su_t15, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement(ai_governance_legitimacy__market_libertarian_reading_su_t20, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__market_libertarian_reading, 0.15).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).

% DUAL FORMULATION NOTE:
% This constraint is the market_libertarian_reading of the ai_governance_legitimacy kernel. It differs structurally from siblings: ε ≈ 0.25 (low) vs. magisterial ε ≈ 0.45 (moderate extraction via doctrinal compliance), democratic ε ≈ 0.35 (moderate via participatory costs), technocratic ε ≈ 0.55 (high via optimization overhead). Beneficiaries/victims invert across readings: this reading's beneficiaries are magisterial/democratic victims and vice versa. The kernel is the contested legitimacy ground; each reading instantiates a different constraint with different ε, different parties, different type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
