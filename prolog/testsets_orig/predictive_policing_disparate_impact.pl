% ============================================================================
% CONSTRAINT STORY: predictive_policing_disparate_impact
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_predictive_policing_disparate_impact, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: predictive_policing_disparate_impact
 *   human_readable: Predictive Policing Disparate Impact on Marginalized Communities
 *   domain: criminal_justice/algorithmic_discrimination
 *
 * SUMMARY:
 *   Predictive policing systems deployed in the United States create a
 *   structural constraint that concentrates law enforcement surveillance and
 *   enforcement on historically marginalized communities through algorithmic
 *   amplification of biased training data. The constraint operates as a
 *   self-reinforcing feedback loop: historic disparities in policing produce
 *   biased arrest data; algorithms trained on this data recommend increased
 *   enforcement in the same communities; increased enforcement generates more
 *   arrests; these arrests retrain the algorithm with amplified bias. The
 *   victims (marginalized communities) are trapped geographically and
 *   administratively; they cannot exit the targeting. Law enforcement
 *   agencies benefit through liability displacement (decisions attributed to
 *   algorithms rather than officers) and resource concentration. Technology
 *   vendors benefit through system licensing, ongoing updates, and lock-in
 *   effects. The constraint exhibits all six DR types from different observer
 *   positions, revealing how institutional capture operates through
 *   algorithmic legitimation. The theater ratio (0.58) reflects that fairness
 *   metrics and bias audits perform accountability without preventing
 *   disparate impact — agencies comply with fairness metrics while the
 *   underlying targeting disparity persists through feedback loop
 *   amplification and metric gaming.
 *
 * KEY AGENTS:
 *   - Marginalized Communities: Primary victims (powerless/trapped) — geographically concentrated, cannot exit algorithmic targeting; bear full cost of increased enforcement
 *   - Criminal Justice Reformers: Secondary victims (moderate/identity_locked) — structurally mobile but identity-fused with reform narrative; their work becomes extractive as their expertise legitimates systems they critique
 *   - Law Enforcement Agencies: Primary beneficiaries (institutional/arbitrage) — capture crime reduction credit, displace liability to algorithms, reduce discretion claims
 *   - Technology Vendors: Secondary beneficiaries (institutional/arbitrage) — generate recurring revenue, create lock-in through data integration, globalize market presence
 *   - Community Advocacy Organizations: Organized victims (organized/constrained) — mobilize collective defense and documentation; also have their advocacy absorbed into regulatory theater
 *   - Algorithmic Accountability Advocates: Organized agents building exit pathways (organized/constrained) — developing fairness mandates, impact assessment requirements, and alternative approaches
 *   - Fairness Certification Apparatus: Institutional theater (institutional/arbitrage) — maintains mathematical fairness metrics that cannot prevent actual disparate impact
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing algorithmic bias as inherent rather than contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(predictive_policing_disparate_impact, 0.68).
domain_priors:suppression_score(predictive_policing_disparate_impact, 0.72).
domain_priors:theater_ratio(predictive_policing_disparate_impact, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(predictive_policing_disparate_impact, extractiveness, 0.68).
narrative_ontology:constraint_metric(predictive_policing_disparate_impact, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(predictive_policing_disparate_impact, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(predictive_policing_disparate_impact, snare).
narrative_ontology:human_readable(predictive_policing_disparate_impact, "Predictive Policing Disparate Impact on Marginalized Communities").
narrative_ontology:topic_domain(predictive_policing_disparate_impact, "criminal_justice/algorithmic_discrimination").

domain_priors:requires_active_enforcement(predictive_policing_disparate_impact).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(predictive_policing_disparate_impact, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(predictive_policing_disparate_impact, technology_vendors).
narrative_ontology:constraint_victim(predictive_policing_disparate_impact, marginalized_communities).
narrative_ontology:constraint_victim(predictive_policing_disparate_impact, criminal_justice_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OVER-POLICED RESIDENTS (SNARE) — Trapped in neighborhoods targeted by predictive algorithms with no exit option. Increased stops, surveillance, and arrests driven by models trained on historical biased data. Cannot exit the geographic constraint or the algorithmic targeting. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(predictive_policing_disparate_impact, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CRIMINAL JUSTICE REFORMERS (SNARE via identity_locked) — Structurally mobile (could abandon reform work) but identity-fused with criminal justice reform movement. Cannot exit their professional and ideological commitment to fairness even as the system absorbs their critiques into legitimation narratives. The constraint extracts their labor and moral authority while neutralizing their reform potential through algorithmic fairness theater.
constraint_indexing:constraint_classification(predictive_policing_disparate_impact, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMUNITY ADVOCACY ORGS (TANGLED ROPE) — Organized agents with constrained exits (funding dependencies, network effects). Experience genuine coordination function (mobilizing collective defense, documenting disparate impacts) alongside asymmetric extraction (their data and expertise fuel algorithm audits that legitimize the systems they critique; their advocacy becomes regulatory input that tech vendors incorporate into compliance theater).
constraint_indexing:constraint_classification(predictive_policing_disparate_impact, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LAW ENFORCEMENT AGENCIES (ROPE) — Net beneficiary with arbitrage capacity (can switch vendors, implement or defund systems). Coordinate enforcement strategy through algorithmic tools while capturing crime reduction credit and liability displacement. Experience the constraint as pure coordination: algorithms guide resource allocation, reduce decision discretion claims, enable data-driven narratives.
constraint_indexing:constraint_classification(predictive_policing_disparate_impact, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TECHNOLOGY VENDORS (ROPE) — Net beneficiary with global arbitrage capacity (can sell to other jurisdictions, retract from markets with regulation). Experience constraint as pure coordination: algorithms bundle law enforcement workflow, create lock-in through data integration, generate revenue from recurring licensing and model updates. Minimal experienced extraction relative to value captured.
constraint_indexing:constraint_classification(predictive_policing_disparate_impact, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALGORITHMIC ACCOUNTABILITY MOVEMENT (SCAFFOLD) — Organized agents building exit pathways through regulatory requirements (impact assessments, bias audits, transparency mandates). See predictive policing as a temporary coordination failure with a sunset: mathematical approaches to fairness, algorithmic transparency requirements, and hardware-agnostic replacement systems are maturing. High organizational agency and declining effectiveness of the traditional extraction mechanism — as auditing norms spread, algorithmic legitimation narratives lose force. Theater ratio declining as external validation requirements increase.
constraint_indexing:constraint_classification(predictive_policing_disparate_impact, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: FAIRNESS CERTIFICATION APPARATUS (PITON) — Academic fairness metrics (bias detection tools, impact assessment frameworks) persist largely as performance theater. The mathematical tools (disparate impact ratios, equalized odds) cannot detect systemic gaming (training data poisoning, feedback loop amplification, outcome measure manipulation). The certification apparatus maintains itself through institutional inertia — it offers the appearance of verification without capacity to prevent the extraction it claims to audit. Theater ratio 0.72 — the metrics are comprehensive but orthogonal to actual disparate impact.
constraint_indexing:constraint_classification(predictive_policing_disparate_impact, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, there is a temptation to see this as an inherent property of algorithmic systems: any sufficiently complex predictive model will embed historical biases, and no audit can fully eliminate this. This perspective risks naturalizing what is actually a contingent institutional choice — to deploy known-biased systems with legitimation theater rather than to refrain from deployment. The engine's false summit detector identifies this as naturalization of a policy choice.
constraint_indexing:constraint_classification(predictive_policing_disparate_impact, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(predictive_policing_disparate_impact_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(predictive_policing_disparate_impact, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(predictive_policing_disparate_impact, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(predictive_policing_disparate_impact, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(predictive_policing_disparate_impact, TR),
    TR >= 0.70.

:- end_tests(predictive_policing_disparate_impact_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint extracts substantial value from marginalized communities through concentration of enforcement, arrest inflation, and criminal record accumulation. The value is captured by law enforcement (through metrics, funding, political legitimation) and vendors (through licensing and lock-in). Extractiveness increased from 0.42 to 0.68 as systems matured and feedback loops amplified biases — early deployments operated on historical data alone; mature systems operate on poisoned data streams that accelerate disparate impact. Suppression (0.72): Very high. Victims face compound barriers: geographic immobility (housing market discrimination concentrates marginalized populations), administrative barriers (policing prevents exit mobility), technological opacity (algorithms are proprietary), and legitimation narratives (fairness audits create appearance of control). The suppression is not purely external — some component is internalized through legitimation narratives that frame algorithm recommendations as objective and unavoidable. Theater ratio (0.58): Moderate-high and increasing. Fairness metrics (bias audits, disparate impact ratios, equalized odds) are mathematically comprehensive but orthogonal to actual disparate impact because vendors can game metric selection (comply with mandated metrics while violating alternative fairness criteria) and manipulate outcome measures (redefining what counts as crime or a successful prediction).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across all six types from identical base properties. Marginalized residents see pure extraction (Snare) — maximum experienced exploitation with no coordination benefit. Community organizations see mixed coordination and extraction (Tangled Rope) — their defensive work coordinates collective resistance but is absorbed into regulatory theater. Law enforcement agencies see pure coordination (Rope) — algorithms coordinate enforcement strategy with minimal experienced extraction. Technology vendors see pure coordination (Rope) — algorithms bundle workflow with lock-in effects. The algorithmic accountability movement sees a temporary problem with solutions (Scaffold) — fairness audits, transparency mandates, and alternative technologies are building exit pathways. The fairness certification apparatus sees its own degraded ritual (Piton) — metrics persist through institutional inertia despite evidence of ineffectiveness. The civilizational analytical observer risks seeing an inherent property of algorithmic systems (Mountain) — that all predictive systems embed biases. This false summit is revealed by the structural data: disparate impact is not inherent to prediction; it results from specific institutional choices to deploy known-biased systems with legitimation theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position: power level, exit capacity, and extraction flow. Powerless trapped residents experience maximum d (0.95), producing high f(d) and thus high χ. Institutional beneficiaries with arbitrage capacity experience low d (0.10-0.20), producing negative f(d) and thus negative χ (the constraint subsidizes them). Organized agents with constrained exits (community orgs, accountability advocates) experience moderate d (0.55-0.65), producing moderate f(d) and moderate χ. Identity-locked reformers are structurally mobile but experientially trapped; their d derives from victim status (they bear the extraction of being extracted from by the systems they critique) combined with constrained exit (identity fusion prevents walking away), producing d around 0.75 and f(d) around 1.20. The engine's directionality pipeline produces the measured extraction values that differentiate powerless experience (maximum) from institutional experience (minimal) from organized experience (moderate).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that all six types are legitimate perspectival readings that together reveal the full extraction structure. The question 'Is this a snare or a tangled rope?' has no single answer — it depends on your structural position. For trapped residents: snare (maximum extraction). For community organizations: tangled rope (mixed coordination and extraction). For law enforcement: rope (net benefit). For vendors: rope (net benefit). For accountability advocates: scaffold (remediable). For fairness metrics: piton (degraded theater). For the analytical observer: mountain is a FALSE summit — the constraint naturalizes a contingent institutional choice. The mandatrophy is not resolved by picking the 'correct' type but by recognizing that the distributional gap between victim and beneficiary experience (snare vs. rope) is the defining feature of extraction. When different agents experience the same constraint as snare/tangled rope/rope simultaneously, the constraint is operating as organized extraction with institutional legitimation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feedback_loop_amplification_mechanism,
    'Does the constraint operate primarily through initial biased training data or through feedback loop amplification that accelerates disparate impact over time?',
    'Longitudinal analysis of algorithmic predictions vs. actual crime patterns; comparison of disparate impact metrics across early deployment (years 1-3) vs. mature systems (years 5+); intervention studies with algorithmic retraining using debiased data',
    'If primarily training data bias: constraint is remediable through data curation and external auditing. If primarily feedback amplification: constraint is self-reinforcing and requires system discontinuation, not reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_loop_amplification_mechanism, empirical, 'Whether disparate impact is driven by biased training data or self-reinforcing feedback loops').

omega_variable(
    algorithmic_gaming_by_law_enforcement,
    'To what extent do law enforcement agencies deliberately manipulate outcomes (arrest targets, crime classifications) to game the algorithmic inputs and preserve system legitimacy?',
    'Analysis of arrest patterns before vs. after algorithmic deployment; comparison of crime classifications with independent victim surveys; interviews with officers about deliberate data gaming; detection of statistical breaks that suggest outcome manipulation',
    'If significant gaming: the constraint includes active conspiracy to maintain disparate impact despite knowledge. If minimal gaming: the constraint operates through passive feedback loops rather than deliberate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_gaming_by_law_enforcement, empirical, 'Extent of deliberate manipulation of algorithmic inputs by law enforcement').

omega_variable(
    reformability_of_fairness_metrics,
    'Can mathematical fairness constraints (disparate impact thresholds, equalized odds, demographic parity) be implemented in live systems without vendors circumventing through metric selection or outcome measure gaming?',
    'Comparative analysis of jurisdictions with strong fairness mandates vs. minimal requirements; longitudinal tracking of metric compliance vs. actual disparate impact; detection of metric substitution (complying with mandated metric while gaming alternative metrics)',
    'If remediable: algorithmic accountability movement''s scaffold perspective is structural and sunset is real. If unremediable: fairness metrics are pure theater and the constraint requires system discontinuation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformability_of_fairness_metrics, empirical, 'Whether mathematical fairness constraints can prevent disparate impact in deployed systems').

omega_variable(
    identity_lock_in_reform_community,
    'Is the criminal justice reform community''s commitment to algorithmic fairness rooted in instrumental belief that fairness is achievable, or in identity fusion with the reform narrative itself?',
    'Analysis of reform rhetoric: presence of sunset clauses or discontinuation scenarios vs. exclusive focus on improving fairness metrics; behavioral shifts when fairness interventions demonstrably fail; willingness to recommend full system discontinuation vs. continued advocacy for reform',
    'If instrumental: reformers will pivot to discontinuation advocacy if fairness proves unachievable. If identity-locked: reformers will continue extractive fairness work even when evidence suggests futility, maintaining the constraint''s legitimation narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_reform_community, conceptual, 'Whether reform commitment is instrumental or identity-constituted').

omega_variable(
    jurisdictional_heterogeneity_in_disparate_impact,
    'Does disparate impact vary systematically across jurisdictions based on system design choices, or is it invariant to local implementation details?',
    'Comparative analysis of disparate impact metrics across municipalities using same vendor, vs. same municipality using different vendors; correlation between implementation choices (model architecture, hyperparameters, audit frequency) and disparate impact magnitude',
    'If heterogeneous and controllable: disparate impact is a constraint design flaw remediable through better architecture. If invariant: disparate impact is inherent to predictive policing itself and requires discontinuation, not design improvement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jurisdictional_heterogeneity_in_disparate_impact, empirical, 'Whether disparate impact varies with implementation or is invariant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(predictive_policing_disparate_impact, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pred_pol_tr_t0, predictive_policing_disparate_impact, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pred_pol_tr_t3, predictive_policing_disparate_impact, theater_ratio, 3, 0.48).
narrative_ontology:measurement(pred_pol_tr_t6, predictive_policing_disparate_impact, theater_ratio, 6, 0.58).
narrative_ontology:measurement(pred_pol_tr_t9, predictive_policing_disparate_impact, theater_ratio, 9, 0.6).

% Extraction over time
narrative_ontology:measurement(pred_pol_be_t0, predictive_policing_disparate_impact, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pred_pol_be_t3, predictive_policing_disparate_impact, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(pred_pol_be_t6, predictive_policing_disparate_impact, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(pred_pol_be_t9, predictive_policing_disparate_impact, base_extractiveness, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(predictive_policing_disparate_impact, enforcement_mechanism).
narrative_ontology:affects_constraint(predictive_policing_disparate_impact, algorithmic_bias_in_loan_underwriting).
narrative_ontology:affects_constraint(predictive_policing_disparate_impact, criminal_record_employment_discrimination).
narrative_ontology:affects_constraint(predictive_policing_disparate_impact, housing_discrimination_algorithmic_steering).

% DUAL FORMULATION NOTE:
% Predictive policing disparate impact is downstream of biased historical data but represents a distinct constraint with its own feedback loop structure. The upstream constraint (historical biased policing data) has different ε reflecting past institutional choices; the downstream constraint (algorithmic amplification through feedback loops) has higher ε reflecting the self-reinforcing nature of the system. Separating these stories enables distinct policy interventions: data curation addresses the upstream constraint; system discontinuation addresses the downstream feedback loop constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(predictive_policing_disparate_impact, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
