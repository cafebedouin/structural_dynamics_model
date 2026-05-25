% ============================================================================
% CONSTRAINT STORY: value_extraction_plateau
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_value_extraction_plateau, []).

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
 *   constraint_id: value_extraction_plateau
 *   human_readable: The Law of Diminishing Predation
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Digital platforms have entered a mature extraction phase where marginal
 *   increases in commission rates, algorithmic suppression, and rent-seeking
 *   overhead produce diminishing returns on control. The dependent producers,
 *   end users, and gig labor supply face successively higher extraction
 *   barriers (15-40% commissions, algorithmic demotion, data harvesting,
 *   rating manipulation) yet remain trapped by network effects and switching
 *   costs. The platform operator captures institutional benefits from the
 *   coordination function while maximizing extraction. Regulatory authorities
 *   experience mixed coordination-extraction tension. The venture capital
 *   system perpetuates a degraded funding ritual despite declining unit
 *   economics. The analytical observer risks naturalizing this extraction
 *   plateau as an immutable law of digital economics when it is actually a
 *   contingent institutional equilibrium maintained by suppression
 *   mechanisms. The constraint exhibits the defining characteristic of
 *   diminishing predation: extraction is approaching a hard ceiling where
 *   further increases trigger producer quality collapse, algorithmic
 *   backlash, regulatory intervention, or competitive displacement.
 *
 * KEY AGENTS:
 *   - Dependent Producers: Primary victims (powerless/trapped) — sellers face 15-30% commissions, algorithmic demotion, mandatory platform payment; rebuild customer base on exit; captured
 *   - End Users: Primary victims (powerless/trapped) — face escalating take rates, algorithmic ranking bias, data harvesting; high switching costs trap despite alternatives
 *   - Gig Labor Supply: Secondary victims (moderate/constrained) — receive declining per-task compensation while platform takes 25-40%; geographic arbitrage and skill specificity constrain exit
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — captures coordination tax and rent-seeking surplus; high exit optionality enables pricing and market pivots
 *   - Regulatory Authority: Mixed stakeholder (organized/constrained) — sees both coordination benefits and extraction harm; constrained by lobbying and legal complexity; active enforcement exists but resource-limited
 *   - Venture Capital System: Institutional actor (institutional/arbitrage) — maintains performative funding ritual (Series rounds) despite declining ROI; treats regulation as temporary friction; inertia-driven
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent equilibrium as thermodynamic law of surplus extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(value_extraction_plateau, 0.68).
domain_priors:suppression_score(value_extraction_plateau, 0.72).
domain_priors:theater_ratio(value_extraction_plateau, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(value_extraction_plateau, extractiveness, 0.68).
narrative_ontology:constraint_metric(value_extraction_plateau, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(value_extraction_plateau, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(value_extraction_plateau, snare).
narrative_ontology:human_readable(value_extraction_plateau, "The Law of Diminishing Predation").
narrative_ontology:topic_domain(value_extraction_plateau, "economic/technological").

domain_priors:requires_active_enforcement(value_extraction_plateau).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(value_extraction_plateau, platform_operator).
narrative_ontology:constraint_victim(value_extraction_plateau, dependent_producers).
narrative_ontology:constraint_victim(value_extraction_plateau, end_users).
narrative_ontology:constraint_victim(value_extraction_plateau, labor_supply).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT PRODUCER (SNARE) — Sellers on the platform face commission rates of 15-30%, algorithmic demotion as punishment for price-setting, and mandatory use of platform payment systems. Exit requires rebuilding entire customer base on alternative platforms with similar or worse terms. No meaningful exit option despite repeated attempts by competing platforms. Maximum experienced extraction.
constraint_indexing:constraint_classification(value_extraction_plateau, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: END USER (SNARE) — Faces escalating take rates embedded in final prices, algorithmic ranking that favors high-margin items, and data harvesting with no compensation. Switching costs (lost reviews, wishlists, recommendation history) trap users despite superior alternatives existing. The platform throttles service quality to force premium tier adoption. No exit.
constraint_indexing:constraint_classification(value_extraction_plateau, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LABOR SUPPLY (SNARE) — Gig workers receive declining per-task compensation while platform takes 25-40% commission. Algorithmic account suspension and rating manipulation create unstable income. Geographic arbitrage to lower-wage regions reduces bargaining power. Some exit capacity (seeking alternative gigs) but constrained by skill specificity and platform switching costs.
constraint_indexing:constraint_classification(value_extraction_plateau, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Experiences the extraction mechanism as a coordination solution: matching buyers and sellers, handling payment infrastructure, providing discovery algorithms. The platform sees its commission as coordination tax. High exit optionality (can pivot to new markets, adjust pricing strategy, lobby regulators). Net beneficiary experiencing the constraint as functional infrastructure.
constraint_indexing:constraint_classification(value_extraction_plateau, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (TANGLED ROPE) — Experiences both coordination (platforms solve matching problems, enable commerce) and extraction (rent-seeking behavior, consumer harm, labor exploitation). Regulatory capacity is constrained by lobbying pressure and threat of service relocation. Active enforcement exists (FTC investigations, EU regulations) but faces resource limits and legal complexity. Mixed experience: genuine coordination benefit but asymmetric extraction.
constraint_indexing:constraint_classification(value_extraction_plateau, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: VENTURE CAPITAL SYSTEM (PITON) — Continues funding extraction-maximizing platforms despite declining ROI and increasing regulatory friction. The VC funding ritual (Series A, B, C milestones) persists as performative growth theater long after unit economics become unsustainable. The system maintained by institutional inertia: investors expect platform exits, regulation is treated as temporary friction, and failed exit predictions are rationalized away. Extraction function has atrophied (newer platforms fail faster), but VC ritual persists.
constraint_indexing:constraint_classification(value_extraction_plateau, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, extraction economics suggests a hard ceiling: platforms cannot extract more than the net surplus they create. At high commission rates (>35%), producer defection and quality collapse reduce total volume and make extraction self-limiting. This perspective sees the plateau as a natural law — extraction is thermodynamically constrained by the surplus function itself. However, the structural data (high suppression, active enforcement) reveals this naturalizes what is actually a contingent institutional equilibrium.
constraint_indexing:constraint_classification(value_extraction_plateau, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(value_extraction_plateau_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(value_extraction_plateau, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(value_extraction_plateau, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(value_extraction_plateau, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(value_extraction_plateau, TR),
    TR >= 0.70.

:- end_tests(value_extraction_plateau_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The platform captures 15-40% of transaction value across dependent producers and end users through multiple rent-extraction mechanisms: commission scaling, algorithmic ranking bias favoring high-margin items, mandatory payment system lock-in, data monetization. The extractiveness is measured at 0.68 rather than higher values (0.75+) because the extraction rate is approaching saturation: further increases trigger producer exodus and quality collapse. The trajectory (0.38→0.68 over 9 periods) shows extraction climbing toward its asymptote but decelerating—characteristic of diminishing predation. Suppression (0.72): High. Dependent producers face multiple suppression mechanisms: algorithmic demotion for price-setting independence, account suspension threats, rating manipulation that destroys seller reputation, switching costs from customer base rebuild requirements. End users face service quality throttling and recommendation system manipulation forcing premium tier adoption. Labor faces account deactivation and income instability. Suppression is not maximal (0.85+) because some escape routes exist (competitive platforms, though inadequate) and regulatory pressure is rising. Theater ratio (0.48): Moderate-low. Unlike traditional extraction mechanisms, platform extraction is largely functional rather than performative. Commissions ostensibly pay for infrastructure, algorithms, and payment handling. The theater emerges not from false claims about function but from inflated cost justifications (algorithms are cheaper than claimed commission rates suggest) and opacity (users and producers cannot see actual cost structure). Theater is rising (0.32→0.48) as extraction mechanism becomes more abstract and harder to verify.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by structural position. The platform operator sees a Rope: their commission is coordination tax for matching, payment infrastructure, algorithmic discovery—a functional mechanism with beneficiaries. Dependent producers see a Snare: their extraction is asymmetric predation with no exit option and no coordination benefit they couldn't obtain from alternative platforms with lower commissions. Regulators see Tangled Rope: they experience both coordination value (platform solves genuine matching problem) and extraction harm (consumer surplus loss, labor exploitation); their constrained exit (cannot eliminate the platform) forces them to tolerate mixed extraction. The venture capital system sees a Piton: the institutional ritual of funding platform growth persists (Series A, B, C, IPO) despite degraded unit economics and regulatory uncertainty—the VC machine is inertia-driven theater, no longer functional. The analytical observer risks seeing a Mountain: extraction is constrained by thermodynamics (cannot extract more than surplus created), so the plateau is natural law. The structural data (high suppression, active enforcement requirements) reveals this is false: the extraction plateau is maintained by contingent suppression mechanisms, not immutable surplus physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operator (beneficiary, institutional power, arbitrage exit): Low directionality d ≈ 0.15. The operator benefits from the extraction mechanism and retains high exit optionality (can pivot products, adjust commission rates, lobby regulators). The sigmoid f(d) produces negative or minimal effective extraction from the operator's perspective—they see the constraint as a coordination mechanism generating surplus they capture. Dependent producers (victims, powerless, trapped exit): High directionality d ≈ 0.92. Producers are both victims of extraction and trapped by network effects. The sigmoid f(d) produces maximum experienced extractiveness chi from their perspective. Regulatory authority (mixed beneficiary/victim, organized, constrained exit): Moderate directionality d ≈ 0.58. Regulators benefit from the platform's coordination function but experience extraction asymmetry (consumer harm, labor exploitation). Constrained exit (cannot shut down the platform without harming commerce) yields intermediate d. The derived chi reflects mixed perception: genuine coordination benefit partially offset by extraction concerns.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: This constraint resolves the coordination-vs-extraction ambiguity by decomposing the platform's function into two structurally distinct roles. (1) COORDINATION FUNCTION: Matching buyers and sellers, payment infrastructure, discovery algorithms—genuinely valuable. Baseline extractiveness for pure coordination would be 0.05-0.15 (Rope). (2) RENT-SEEKING OVERLAY: Commission scaling beyond infrastructure costs, algorithmic demotion of price-competitive sellers, mandatory ecosystem lock-in, data monetization without compensation—predatory. Baseline extractiveness for pure predation would be 0.72+ (Snare). The observed extractiveness of 0.68 reflects that the platform has optimized its rent layer while still maintaining minimal coordination functionality (otherwise whole system collapses). The snare classification is justified by: (a) high measured suppression (0.72), (b) asymmetric extraction toward powerful beneficiary (platform operator), (c) lack of genuine beneficiary status for victims (producers gain nothing from 25% commission they wouldn't get from 10% commission on alternative platform), (d) active enforcement (algorithmic punishment for defection). The constraint is pure extraction dressed in coordination clothing—the mandatrophy is resolved by recognizing that the coordination function is instrumentalized to enable extraction, not the reverse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    surplus_collapse_threshold,
    'At what commission rate do dependent producers cease finding the platform economically viable, causing quality collapse and platform exodus?',
    'Historical analysis of platform commission increases and corresponding producer retention curves; cross-platform comparison of viability thresholds; econometric modeling of producer surplus vs platform take',
    'If threshold < 25%: extraction is already at plateau, further increases trigger collapse. If threshold > 40%: significant extraction potential remains untapped, platform has room to increase predation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surplus_collapse_threshold, empirical, 'Critical commission rate for producer viability collapse').

omega_variable(
    algorithmic_retaliation_effectiveness,
    'Can platforms maintain effective suppression through algorithmic demotion and account suspension without triggering regulatory intervention or competitive displacement?',
    'Regulatory action outcomes (FTC, EU); cross-platform retention rates after enforcement actions; emergence and success of alternative platforms with transparent algorithms',
    'If retaliation remains effective: suppression remains at 0.72, snare classification stable. If regulatory/competitive pressure succeeds: suppression drops below 0.60, classification shifts toward tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_retaliation_effectiveness, empirical, 'Whether algorithmic suppression can persist against regulation').

omega_variable(
    network_effect_irreversibility,
    'Are network effects (incumbent advantage from size and ecosystem lock-in) irreversible, or can new platforms with superior terms bootstrap competing networks?',
    'Historical precedent analysis (Instagram vs Orkut, Discord vs Teamspeak); time-to-equilibrium for entrant platforms; econometric measurement of switching cost elasticity',
    'If irreversible: incumbent platform extraction is truly trapped (snare persists indefinitely). If reversible: extraction plateau represents unstable equilibrium vulnerable to competitive displacement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_irreversibility, empirical, 'Reversibility of network effects and competitive displacement').

omega_variable(
    producer_coalition_capacity,
    'Can dependent producers organize collective defection (coordinated boycott or migration) to counter platform predation?',
    'Historical analysis of producer union organizing; cross-platform comparison of collective action capacity; game-theoretic modeling of defection coordination with network heterogeneity',
    'If coalitions succeed: powerless agent power may upgrade to organized, snare classification shifts toward tangled rope with credible exit. If coordination fails: snare persists with high suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(producer_coalition_capacity, empirical, 'Whether producer coalitions can organize collective defection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(value_extraction_plateau, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vep_tr_t0, value_extraction_plateau, theater_ratio, 0, 0.32).
narrative_ontology:measurement(vep_tr_t3, value_extraction_plateau, theater_ratio, 3, 0.38).
narrative_ontology:measurement(vep_tr_t6, value_extraction_plateau, theater_ratio, 6, 0.44).
narrative_ontology:measurement(vep_tr_t9, value_extraction_plateau, theater_ratio, 9, 0.48).

% Extraction over time
narrative_ontology:measurement(vep_be_t0, value_extraction_plateau, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(vep_be_t3, value_extraction_plateau, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(vep_be_t6, value_extraction_plateau, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(vep_be_t9, value_extraction_plateau, base_extractiveness, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(value_extraction_plateau, resource_allocation).
narrative_ontology:affects_constraint(value_extraction_plateau, algorithmic_opacity_extraction).
narrative_ontology:affects_constraint(value_extraction_plateau, network_effect_lock_in).
narrative_ontology:affects_constraint(value_extraction_plateau, venture_capital_ponzi_scaling).

% DUAL FORMULATION NOTE:
% The platform extraction mechanism decomposes into three structurally distinct constraints: (1) algorithmic_opacity_extraction (ε≈0.45, Tangled Rope)—the information asymmetry enabling ranking manipulation, (2) network_effect_lock_in (ε≈0.35, Rope)—the coordination problem of customer base, (3) venture_capital_ponzi_scaling (ε≈0.62, Snare)—the institutional extraction layer where VC funding finances rent-seeking rather than product development. The observed constraint value_extraction_plateau (ε=0.68) reflects the composite of all three operating simultaneously. The plateau emerges when all three components reach their structural limits concurrently: algorithmic opacity hits regulatory scrutiny, network effects face competitive displacement, and VC funding evaporates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(value_extraction_plateau, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
