% ============================================================================
% CONSTRAINT STORY: carbon_offset_additionality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carbon_offset_additionality, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: carbon_offset_additionality
 *   human_readable: Carbon Offset Additionality Verification Constraint
 *   domain: climate/environmental_markets
 *
 * SUMMARY:
 *   Carbon offset additionality is a structural constraint governing whether
 *   emissions reduction projects represent genuine climate benefit or merely
 *   transfer emissions-accounting liability to offset purchasers. The
 *   constraint requires that offset projects produce emissions reductions
 *   'additional to what would have happened anyway' — the counterfactual
 *   baseline. This constraint exhibits the full range of DR classifications
 *   because additionality verification sits at the intersection of
 *   epistemological limits (unmeasurable counterfactuals), market incentives
 *   (beneficiaries profit from non-verification), and power asymmetries
 *   (developing country communities cannot verify claims made about their
 *   territories). The theater ratio (0.81) reflects that offset methodologies
 *   are extensively documented and procedurally legitimate — they create an
 *   appearance of scientific rigor through baseline calculations, leakage
 *   modeling, and investment analysis — yet the underlying additionality
 *   determination is largely unverifiable. The constraint has degraded over
 *   time as offset markets have grown: early projects in the CDM faced
 *   selective scrutiny, while later projects exploit methodological loopholes
 *   faster than standards can close them. Suppression is high (0.68) because
 *   participation in offset schemes is structurally asymmetric: beneficiaries
 *   (project developers, emitters, brokers, standard-setters) control
 *   verification procedures and baseline-setting authority, while victims
 *   (climate system, developing country communities, future generations) have
 *   no exit option and no mechanism to contest additionality claims after
 *   carbon credits are issued.
 *
 * KEY AGENTS:
 *   - Climate System and Future Generations: Primary victim (powerless/trapped) — receives non-additional offsets as if they were real reductions; no mechanism to reject false credits
 *   - Developing Country Communities: Primary victim (powerless/trapped) — promised benefits receive minimal compensation; minimal dispute resolution capacity; trapped by information asymmetry and geographic constraints
 *   - Offset Project Developers: Primary beneficiary (institutional/arbitrage) — capture carbon credit revenue; control methodology application; high exit options (can exit for other projects)
 *   - Regulated Carbon Emitters: Secondary beneficiary (powerful/mobile) — use offsets to meet compliance targets without deep emissions reductions; high exit options (can lobby for weaker standards, relocate)
 *   - Offset Brokers and Intermediaries: Beneficiary (institutional/arbitrage) — extract transaction fees; capture markup between verified and unverified credits
 *   - Offset Auditors and Verifiers: Mixed (moderate/constrained) — provide verification service but revenue depends on project approval; constrained by client dependence
 *   - Climate Advocacy Organizations: Mixed (organized/constrained) — provide oversight pressure but also benefit from offset market growth and corporate partnerships
 *   - Regulatory Bodies: Mixed (institutional/arbitrage) — maintain standard-setting authority but benefit from offset market expansion; perpetuate theater through procedural legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carbon_offset_additionality, 0.58).
domain_priors:suppression_score(carbon_offset_additionality, 0.68).
domain_priors:theater_ratio(carbon_offset_additionality, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carbon_offset_additionality, extractiveness, 0.58).
narrative_ontology:constraint_metric(carbon_offset_additionality, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(carbon_offset_additionality, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carbon_offset_additionality, tangled_rope).
narrative_ontology:human_readable(carbon_offset_additionality, "Carbon Offset Additionality Verification Constraint").
narrative_ontology:topic_domain(carbon_offset_additionality, "climate/environmental_markets").

domain_priors:requires_active_enforcement(carbon_offset_additionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carbon_offset_additionality, offset_project_developers).
narrative_ontology:constraint_beneficiary(carbon_offset_additionality, carbon_credit_purchasers).
narrative_ontology:constraint_beneficiary(carbon_offset_additionality, compliance_market_intermediaries).
narrative_ontology:constraint_victim(carbon_offset_additionality, climate_mitigation_effectiveness).
narrative_ontology:constraint_victim(carbon_offset_additionality, developing_country_beneficiaries).
narrative_ontology:constraint_victim(carbon_offset_additionality, future_climate_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE SYSTEM (SNARE) — Cannot exit the verification trap. Receives counterfeit emissions reductions while atmospheric CO2 accumulates. No mechanism to reject false offsets; bears full cost of non-additionality. Maximum extraction — abstract future suffering cannot organize or advocate.
constraint_indexing:constraint_classification(carbon_offset_additionality, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING COUNTRY OFFSET BENEFICIARIES (SNARE) — Structurally trapped in carbon credit schemes. Local communities promised climate finance and development benefits receive minimal compensation while project developers extract carbon credit revenues. No capacity to verify additionality claims or exit arrangements. No dispute resolution mechanism they can access. Trapped by geography, resource constraints, and information asymmetry.
constraint_indexing:constraint_classification(carbon_offset_additionality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: OFFSET AUDITORS (TANGLED ROPE) — Constrained by revenue dependence on offset project clients who pay for audits. Also benefit from the offset market's existence — their professional services, career advancement, and institutional resources depend on offset expansion. Mixed: genuine verification function exists alongside incentive to approve projects. High switching cost (reputational damage, client loss) constrains candid assessment.
constraint_indexing:constraint_classification(carbon_offset_additionality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: OFFSET DEVELOPERS & BROKERS (ROPE) — Net beneficiaries with full exit options. Capture carbon credit revenue, develop project portfolios, exploit regulatory arbitrage (weaker standards in some jurisdictions). See the constraint as coordination: communicating project impact enables compliance and financing. Experience extraction as favorable — the asymmetry runs toward them.
constraint_indexing:constraint_classification(carbon_offset_additionality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATED EMITTERS (ROPE) — Powerful institutions with mobile exit options (can relocate, can shift production, can lobby for weaker standards). Experience the constraint as enabling coordination: offsets allow compliance without deep emissions reductions. Net beneficiary — the constraint subsidizes their decarbonization narrative. Low effective extraction because they have high agency and high benefit.
constraint_indexing:constraint_classification(carbon_offset_additionality, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY SYSTEM (PITON) — Maintains additionality verification protocols that are largely performative. Methodology documents (CDM methodologies, VCS standards, Gold Standard) specify additionality tests, but enforcement is theater: baseline projections are unverifiable counterfactuals; leakage calculations depend on speculative models; investment additionality tests can be gamed with creative counterfactuals. The regulatory system persists through institutional inertia and political convenience — offsets allow the appearance of climate action without disrupting economic growth. Theater ratio (0.81) reflects extensive documentation of procedures that don't reliably prevent non-additionality.
constraint_indexing:constraint_classification(carbon_offset_additionality, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: CLIMATE ADVOCACY ORGS (TANGLED ROPE) — Organized agents (Oxfam, Carbon Trust, Forest Trends) provide some verification function and reputational pressure on projects. Also benefit from the offset market's existence — their funding, policy influence, and institutional existence depend on carbon market growth. Constrained by need for industry partnerships and funding from climate-concerned corporates. Mixed: genuine oversight co-exists with institutional dependence on the market continuing.
constraint_indexing:constraint_classification(carbon_offset_additionality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / EPISTEMOLOGICAL LIMIT (MOUNTAIN) — From a civilizational perspective, additionality verification is structurally impossible: the counterfactual (what would have happened without the project) is inherently unobservable and untestable. No empirical method can verify whether a project is additional or would have occurred anyway. The constraint appears as an immutable epistemological limit — you cannot verify what-did-not-happen. However, this naturalization obscures the structural fact that non-additionality is instrumentally convenient for all beneficiaries.
constraint_indexing:constraint_classification(carbon_offset_additionality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carbon_offset_additionality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carbon_offset_additionality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carbon_offset_additionality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carbon_offset_additionality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(carbon_offset_additionality, TR),
    TR >= 0.70.

:- end_tests(carbon_offset_additionality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint systematically produces counterfeit emissions reductions that beneficiaries monetize through carbon credit sales. The extraction is sustained because the counterfactual baseline is unverifiable and baseline-setting authority is controlled by beneficiaries. Extraction rises over time as offset markets mature and sophisticated players learn to navigate additionality tests. Non-additionality rates in published meta-analyses range from 30-70% depending on project type — meaning 30-70% of carbon credits represent zero or negative emissions reductions. Suppression (0.68): High. Multiple mechanisms prevent victims from exiting or contesting non-additionalconstraints. Developing country communities lack technical capacity and institutional access to challenge baseline calculations. Future generations have no voice in present offset purchasing. Climate scientists publish critiques but lack authority in market governance. The regulatory system reinforces suppression through procedural legitimacy — detailed methodologies create an appearance of rigor that preempts deeper scrutiny. Theater ratio (0.81): Very high. Offset verification is performative. Methodologies document baseline selection, leakage modeling, and investment additionality tests in extensive detail, but the documentation is theater. Counterfactuals are inherently unobservable; leakage assumptions are speculative; investment additionality tests can be gamed with creative financial narratives. The theater has increased over time as standard-setters have developed more elaborate methodological guidance in response to criticism — more documentation creates an appearance of tighter control without changing the fundamental epistemological problem.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how a single structural fact (counterfactual additionality is unverifiable) produces radically different classifications across perspectives. The beneficiary sees rope: coordination between project developers and compliance markets enables efficient emissions accounting. The regulatory system sees piton: the ritual is degraded but persists through institutional inertia. The auditor sees tangled rope: genuine verification function exists alongside financial dependence on project approval. The climate system sees snare: receives counterfeit reductions while bearing the full cost of non-additionalrity. The analytical observer risks seeing mountain: additionality is epistemically impossible, therefore immutable — but this naturalizes a contingent institutional choice (weak baseline tests) as an unresolvable limit. The perspectival gap reveals that non-additionality is systematically convenient for all beneficiaries — none have incentive to develop stronger verification methods.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status combined with exit options. Project developers as beneficiaries with arbitrage exit options experience low d (favorable directionality) — they can exit to other projects if this market sours. Regulated emitters as beneficiaries with mobile options experience similarly low d. The climate system as victim with trapped exit experiences maximal d (1.0) — no escape from receiving non-additional credits. Developing country communities as victims with trapped exit also experience near-maximal d. Auditors as constrained agents with mixed benefits/costs experience moderate d — they have some exit agency but their career and revenue depend on the offset market. The beneficiary capture of directionality-setting authority is reflected in how baseline assumptions are systematically set: initial baselines are conservative (favoring non-additionalrity) but are routinely revised upward during project development, increasing credit generation in favor of developers.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that the constraint combines genuine coordination (projects do emit verified reductions to the extent they claim) with systematic extraction (most projects are non-additional, and beneficiaries profit by monetizing counterfeit reductions). The tangled rope classification is correct: the constraint requires active enforcement (verification audits, baseline selection, methodological compliance) to maintain the appearance of coordination, yet that enforcement mechanism is structurally compromised by beneficiary control of audit procedures and baseline-setting. The constraint avoids collapse into pure snare because some offset projects are genuinely additional — renewable energy projects in jurisdictions without subsidies, forest protection in areas facing imminent development, methane capture at sites with strong economic incentives to expand. But the proportion of truly additional projects is much lower than markets assume. The mandatrophy is resolved by the theater_ratio trend: as theater has increased (more elaborate methodologies), extractiveness has increased (weaker empirical additionality). This is the signature of a constraint sustaining itself through procedural legitimacy rather than functional performance. The constraint does not degrade into piton (total inertia) because beneficiaries actively maintain the offset market apparatus through new standards, new methodologies, and renewed investor confidence — but the maintenance is performative, not corrective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_verifiability,
    'Is additionality fundamentally unverifiable (epistemological barrier) or merely difficult to verify due to institutional neglect (contingent engineering problem)?',
    'Development of proxy mechanisms for additionality that don''t rely on counterfactual baseline. Historical comparison: did renewable energy projects priced at X become additional when renewable costs dropped below baseline? Can technology adoption trajectories falsify additionalityprofiles?',
    'If epistemological: mountain classification is justified, constraint is immutable. If contingent: the mountain is a false summit, masking engineerable institutional failures. Standard-setters have chosen cheap counterfactual tests over expensive outcome verification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_verifiability, conceptual, 'Whether additionality verification is epistemically impossible or institutionally neglected').

omega_variable(
    leakage_quantification_accuracy,
    'How accurate are the leakage displacement calculations embedded in offset methodologies? Do conservativeness claims (0.5x project impact) reflect real empirical precision or provide cover for unquantified uncertainty?',
    'Post-project empirical evaluation: track where displaced activity actually relocated. Compare methodological leakage assumptions to observed emissions patterns. Meta-analysis of leakage studies across offset project types.',
    'If accurate: some offsets are likely additional even with methodological uncertainty. If inaccurate by >2x: most tropical forest and energy project offsets are non-additional. Theater ratio rises if empirical displacement is much larger than conservative factors assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leakage_quantification_accuracy, empirical, 'Accuracy of leakage displacement modeling in offset methodologies').

omega_variable(
    investment_additionality_gaming,
    'What proportion of offset projects would have been economically viable without carbon credit revenue? Can investment additionality tests reliably distinguish projects whose financial case depends on offsets from projects that claim offset-dependence strategically?',
    'Retrospective financial analysis: examine project IRRs, debt structures, and investor compositions. Track how methodology guidance on acceptable financial thresholds evolved. Analyze whether tighter investment tests reduce project approval rates without changing realized emissions outcomes.',
    'If high gaming rate (>60%): investment additionality is largely unenforceable. Non-additionality rates are highest in this category. Extractiveness rises because beneficiary capture of non-additional revenue becomes the dominant mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investment_additionality_gaming, empirical, 'Proportion of projects gaming investment additionality criteria').

omega_variable(
    regulatory_standard_choice_mechanism,
    'Why do offsetting standards use weak additionality tests (baseline & additionality methodologies) rather than outcome-based verification (post-project measurement of actual avoided emissions)? Is this a cost-benefit judgment or a structural preference for unverifiable claims?',
    'Comparative institutional analysis: why do some regulatory regimes (California, EU) allow outcome-based verification while others (VCS, Gold Standard) resist it? What are the cost differentials? Is outcome-based verification technically possible for major offset categories?',
    'If cost-driven: higher-cost outcome methods could be mandated with budget constraints. If preference-driven: the regulatory choice reveals beneficiary capture — weak tests are intentional. Theater ratio interpretation shifts from ''verification is hard'' to ''verification is avoided.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_standard_choice_mechanism, preference, 'Why regulatory standards avoid outcome-based additionality verification').

omega_variable(
    baseline_creep_rate,
    'Do baseline scenarios (what emissions would occur without the project) systematically drift upward over time, inflating additionality claims? Can projects adjust baselines retrospectively to increase credit generation?',
    'Longitudinal analysis of baseline revisions: compare initial baseline to final approved baseline for large project cohorts. Track whether grid carbon intensity baselines, business-as-usual growth assumptions, or technology cost baselines increase after project approval. Statistical analysis of revision direction.',
    'If upward drift >20%: systematic bias in favor of projects. Non-additionality is systematic rather than random. Suppression increases because project developers capture baseline-setting authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseline_creep_rate, empirical, 'Systematic upward drift in project baseline scenarios').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carbon_offset_additionality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cooff_tr_t0, carbon_offset_additionality, theater_ratio, 0, 0.62).
narrative_ontology:measurement(cooff_tr_t3, carbon_offset_additionality, theater_ratio, 3, 0.71).
narrative_ontology:measurement(cooff_tr_t6, carbon_offset_additionality, theater_ratio, 6, 0.81).
narrative_ontology:measurement(cooff_tr_t9, carbon_offset_additionality, theater_ratio, 9, 0.85).

% Extraction over time
narrative_ontology:measurement(cooff_be_t0, carbon_offset_additionality, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cooff_be_t3, carbon_offset_additionality, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(cooff_be_t6, carbon_offset_additionality, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(cooff_be_t9, carbon_offset_additionality, base_extractiveness, 9, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carbon_offset_additionality, resource_allocation).
narrative_ontology:boltzmann_floor_override(carbon_offset_additionality, 0.18).
narrative_ontology:affects_constraint(carbon_offset_additionality, climate_policy_effectiveness).
narrative_ontology:affects_constraint(carbon_offset_additionality, corporate_carbon_accountability).
narrative_ontology:affects_constraint(carbon_offset_additionality, developing_country_climate_finance).

% DUAL FORMULATION NOTE:
% Carbon offset additionality decomposition: The constraint can be split into two structurally distinct claims: (1) technical additionality — whether a specific project avoids emissions relative to its baseline (ε=0.52, Tangled Rope); (2) regulatory additionality — whether the offset verification system prevents non-additional projects from being credited (ε=0.68, Tangled Rope with higher theater). This story addresses the combined constraint as experienced in compliance markets. Decomposition enables separate analysis of technical failure (poor baseline models) vs institutional failure (beneficiary control of verification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(carbon_offset_additionality, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
