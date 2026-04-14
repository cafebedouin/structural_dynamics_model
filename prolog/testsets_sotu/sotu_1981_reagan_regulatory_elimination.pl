% ============================================================================
% CONSTRAINT STORY: sotu_1981_reagan_regulatory_elimination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1981_reagan_regulatory_elimination, []).

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
 *   constraint_id: sotu_1981_reagan_regulatory_elimination
 *   human_readable: Regulatory Elimination Initiative (Reagan 1981)
 *   domain: regulatory/economic_policy
 *
 * SUMMARY:
 *   The 1981 Reagan regulatory elimination initiative represents a
 *   coordinated policy shift to systematically review and remove regulations
 *   estimated to add $100 billion annually to consumer prices and suppress
 *   productivity growth. The constraint operates as a structural reallocation
 *   of authority from regulatory bodies back to market mechanisms, treating
 *   deregulation as the primary recovery tool for economic stagnation. This
 *   constraint is diagnostically rich because it exhibits genuine
 *   coordination function (unified market clarity, reduced compliance burden,
 *   enabled market competition) alongside asymmetric extraction (removal of
 *   protections from constituencies — workers, environment, consumers — who
 *   cannot veto the elimination). The measurement trajectory shows
 *   extractiveness rising from 0.35 to 0.62 over ten years as the policy's
 *   extractive mechanisms become clearer: initial theory emphasizes
 *   coordination and efficiency gains, but realized outcomes concentrate
 *   benefits on large producers while costs accumulate on powerless
 *   constituencies. Theater ratio remains moderate (0.48-0.51) because
 *   deregulation paradoxically reduces performative activity relative to
 *   regulatory regimes — the extraction mechanism is more direct and less
 *   ritualized, making the constraint's asymmetry more visible rather than
 *   concealed in procedural theater.
 *
 * KEY AGENTS:
 *   - Manufacturing Sector: Primary beneficiary (institutional/arbitrage) — captures compliance cost reductions and market expansion opportunity during period of reduced regulatory constraint
 *   - Agricultural Producers: Primary beneficiary (institutional/arbitrage) — reduction of environmental and labor regulations enables cost-cutting and scale expansion
 *   - Small Business Merchants: Secondary beneficiary (moderate/constrained) — benefit from reduced compliance burden but face competitive pressure from larger firms better positioned to capitalize on deregulation
 *   - Environmental Protection Constituencies: Primary victim (powerless/trapped) — lose legal standing and regulatory protections without compensation or veto power; bear full cost of ecological externalities
 *   - Worker Safety Constituencies: Primary victim (powerless/trapped) — lose occupational safety standards and enforcement mechanisms; face direct health hazard risk without exit option
 *   - Consumer Protection Constituencies: Primary victim (powerless/trapped) — lose product safety and quality standards; bear cost of market failures and asymmetric information without protective mechanism
 *   - Regulatory Agencies: Institutional actor (institutional/arbitrage) — nominally maintain oversight function despite budget/enforcement reduction; see their own capacity as degraded
 *   - Labor Union Organizations: Organized actor (organized/constrained) — benefit from market-driven wage pressure (union negotiating strength increases) while losing occupational safety protections (membership health hazard increases)
 *   - Consumer Safety Advocates: Organized actor (organized/constrained) — face suppression of protective standards but retain media access and long-term political strategy for regulatory restoration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1981_reagan_regulatory_elimination, 0.58).
domain_priors:suppression_score(sotu_1981_reagan_regulatory_elimination, 0.52).
domain_priors:theater_ratio(sotu_1981_reagan_regulatory_elimination, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1981_reagan_regulatory_elimination, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1981_reagan_regulatory_elimination, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sotu_1981_reagan_regulatory_elimination, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1981_reagan_regulatory_elimination, tangled_rope).
narrative_ontology:human_readable(sotu_1981_reagan_regulatory_elimination, "Regulatory Elimination Initiative (Reagan 1981)").
narrative_ontology:topic_domain(sotu_1981_reagan_regulatory_elimination, "regulatory/economic_policy").

domain_priors:requires_active_enforcement(sotu_1981_reagan_regulatory_elimination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1981_reagan_regulatory_elimination, manufacturing_sector).
narrative_ontology:constraint_beneficiary(sotu_1981_reagan_regulatory_elimination, agricultural_producers).
narrative_ontology:constraint_beneficiary(sotu_1981_reagan_regulatory_elimination, small_business_merchants).
narrative_ontology:constraint_victim(sotu_1981_reagan_regulatory_elimination, environmental_protection_constituencies).
narrative_ontology:constraint_victim(sotu_1981_reagan_regulatory_elimination, worker_safety_constituencies).
narrative_ontology:constraint_victim(sotu_1981_reagan_regulatory_elimination, consumer_protection_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENVIRONMENTAL PROTECTION CONSTITUENCIES (SNARE) — Trapped by asymmetric removal of constraints that protected ecosystems and public health. No legislative veto power, no exit from regulatory sphere, bear full cost of deregulation without proportional voice in elimination process. Classified as snare: high suppression, no coordination benefit, pure extraction of previously protected rights.
constraint_indexing:constraint_classification(sotu_1981_reagan_regulatory_elimination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKER SAFETY CONSTITUENCIES (SNARE) — Trapped by removal of occupational safety standards and enforcement mechanisms. Face direct health hazard exposure without exit option (workplace participation is economic necessity). No veto power over regulatory elimination. Classified as snare: extraction of previously guaranteed protections, high suppression via economic necessity, minimal coordination benefit.
constraint_indexing:constraint_classification(sotu_1981_reagan_regulatory_elimination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATED INDUSTRIES / MID-SCALE MANUFACTURERS (TANGLED ROPE) — Experiences genuine coordination function (market clarity, unified standards) alongside asymmetric extraction. While benefiting from reduced compliance costs, bears burden of competitive adaptation to lower-cost competitors, shifting internal cost structures. Constrained exit: cannot opt out of deregulation-driven market realignment. Classified as tangled rope: real coordination gain (market clarity, level playing field) alongside real extraction (competitive pressure to capture market gains through cost-cutting elsewhere).
constraint_indexing:constraint_classification(sotu_1981_reagan_regulatory_elimination, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE MANUFACTURING & AGRICULTURAL CORPORATIONS (ROPE) — Primary beneficiaries experiencing the constraint as pure coordination mechanism: unified reduction of compliance burden enables market coordination around cost savings. Exit option (arbitrage) available: can relocate operations to less-regulated jurisdictions if desired, but deregulation makes this unnecessary. Extraction runs toward this agent. Classified as rope: significant beneficiary experience, minimal suppression, coordination function authentic.
constraint_indexing:constraint_classification(sotu_1981_reagan_regulatory_elimination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSUMER SAFETY ADVOCACY ORGANIZATIONS (SCAFFOLD) — Organized agents (consumer unions, public health advocates) perceive the deregulation initiative as temporary removal of protections with eventual sunset through crisis or organized political recovery. Low effective extraction because organized actors have agency, media platforms, and long-term exit strategy (regulatory restoration after demonstrable harm). Classified as scaffold: sees temporary suppression with eventual restoration path, organization provides leverage against extraction.
constraint_indexing:constraint_classification(sotu_1981_reagan_regulatory_elimination, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY AGENCIES (PITON) — Maintain performative regulatory functions despite mandate reduction: agencies continue paperwork, data collection, and nominal oversight even as enforcement capacity and budgets are eliminated. Theater persists as institutional inertia — the regulatory apparatus sees itself as degraded but persists through lack of alternatives. Classified as piton: theater_ratio high (continued symbolic regulation despite reduced function), low extraction because institutional actors can arbitrage across jurisdictions or reframe their work.
constraint_indexing:constraint_classification(sotu_1981_reagan_regulatory_elimination, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: LABOR UNION ORGANIZATIONS (TANGLED ROPE) — Organized actors experiencing both coordination and extraction. Genuine coordination function: deregulation creates pressure for cost control that strengthens union negotiating position (workers become cost concern for employers). Simultaneous extraction: loss of occupational safety standards increases member health hazard risk without proportional compensation. Constrained exit: cannot exit labor market. Classified as tangled rope: asymmetric enforcement of protective standards while benefiting from market-driven wage competition.
constraint_indexing:constraint_classification(sotu_1981_reagan_regulatory_elimination, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, market efficiency and cost reduction are natural laws: regulations inherently suppress productivity, and their removal is restoration of natural equilibrium rather than policy choice. This perspective naturalizes the deregulation initiative as inevitable economic law. However, structural data contradicts: identifiable beneficiaries, measured costs on victims, policy choice dimension all suggest false summit. Engine false summit detector will identify naturalization of contingent policy as artificial mountain.
constraint_indexing:constraint_classification(sotu_1981_reagan_regulatory_elimination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1981_reagan_regulatory_elimination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1981_reagan_regulatory_elimination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1981_reagan_regulatory_elimination, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1981_reagan_regulatory_elimination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1981_reagan_regulatory_elimination, TR),
    TR >= 0.70.

:- end_tests(sotu_1981_reagan_regulatory_elimination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint represents genuine cost reduction for beneficiaries (estimated $100 billion consumer price reduction is real), but this cost reduction is achieved by externalization rather than efficiency gain — the costs are shifted to constituencies (workers, environment, consumers) without compensation. The base extractiveness reflects the magnitude of this externalization. Measurement trajectory (0.35→0.62) indicates that initial optimistic theory of efficiency gains gradually revealed as extraction mechanism: as implementation proceeds, uncompensated costs to victims accumulate and become visible. Suppression (0.52): Moderate-high. Beneficiaries face no suppression (they can exit into market mechanisms or relocate operations). Victims face high suppression: workers cannot exit the labor market, environmental constituencies cannot exit ecological exposure, consumers cannot exit markets. The aggregate suppression reflects that half the constraint's population faces extreme suppression while half faces none. Theater ratio (0.48): Moderate-low. Unlike regulatory regimes heavy in procedural legitimacy, deregulation operates through direct market mechanisms with less performative content. The constraint's legitimation is efficiency rhetoric rather than procedural theater. Claimed type (tangled rope) is accurate: the constraint combines genuine coordination function (market clarity, unified rules, enabled competition) with asymmetric extraction (cost externalization to powerless constituencies). This is not pure snare (which would be extraction without coordination) nor pure rope (which would be coordination without extraction).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival gap across agent positions. Large institutional beneficiaries with arbitrage options perceive rope (pure coordination mechanism enabling market efficiency). Powerless constituencies with trapped exit perceive snare (pure extraction of previously protected rights). Organized victims perceive scaffold (temporary suppression with eventual restoration path via political organizing). Regulatory agencies perceive piton (degraded ritual lacking enforcement capacity). The analytical observer risks perceiving mountain (natural law of market efficiency) but structural data reveals false summit: beneficiaries exist, extraction is measured, policy choice is evident. The gap between beneficiary and victim perspectives is structurally necessary given the constraint's asymmetric incidence: cost reduction for one agent requires cost externalization from another. This is not a failure of classification but evidence that the constraint's core function is redistribution rather than coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position: beneficiaries with arbitrage options (can exit into deregulated markets or relocate) experience low d (~0.15 for institutional beneficiaries), producing negative or low effective extraction chi. Victims with trapped exit (workers must work, environmental constituencies cannot exit exposure, consumers must participate in markets) experience high d (~0.90-0.95), producing high effective extraction chi. Organized victims (labor unions, consumer advocates) with constrained but meaningful exit experience moderate-high d (~0.65-0.75), producing moderate effective extraction chi. The pipeline computes these from beneficiary/victim declarations plus exit options, producing the perspectival gap: beneficiaries see rope (coordination), victims see snare (extraction), observers see tangled rope (mixture). The directionality override logic applies if any institutional actor is captured by the extraction mechanism despite apparent beneficiary status — mid-scale manufacturers facing competitive pressure from larger deregulated competitors experience higher d than their nominal beneficiary status suggests, warranting override to d~0.35-0.45.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that all eight perspectives are legitimate readings of the same constraint from different structural positions. The constraint IS rope from the beneficiary perspective and IS snare from the victim perspective, not because one classification is 'correct' but because the constraint's core function is asymmetric redistribution. The tangled rope claimed type resolves the mandatrophy by refusing to collapse the asymmetry: the constraint has real coordination content (unified market rules, enabled competition) AND real extraction content (uncompensated cost externalization). The measurement trajectory showing rising extractiveness (0.35→0.62) indicates that the coordination function was oversold and extraction mechanism was underestimated in initial theory. The false summit at the analytical perspective reveals the mandatrophy's deepest structure: observers risk naturalizing policy choices as inevitable laws. The resolution is not 'choose one type' but 'map the presheaf over observation sites': the constraint is a roof covering all six types simultaneously, indexed by agent position. The organizability question (whether victims can coalesce to resist or restore) becomes the critical variable determining whether the constraint remains tangled rope or devolves to snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_quantification,
    'What is the true cost of externalities (environmental degradation, worker illness, consumer harm) that regulations prevented versus the estimated $100 billion consumer price impact of regulation?',
    'Longitudinal health outcome studies post-deregulation; environmental damage assessments; healthcare cost increases attributable to reduced protections; comparison to pre-regulation baseline',
    'If externality costs exceed compliance costs: constraint is snare (pure extraction from uncompensated victims). If externality costs are minimal: constraint is rope (genuine coordination gain). If externality costs are comparable: constraint is tangled rope (mixed coordination/extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_quantification, empirical, 'Quantification of externality costs versus claimed $100B regulation price burden').

omega_variable(
    organizational_substitution,
    'Do market mechanisms actually replace regulatory protection functions, or do they eliminate protection without substitution?',
    'Tracking of alternative protection mechanisms: industry self-regulation effectiveness, private certification, litigation-based deterrence, market reputation mechanisms; comparison of pre- and post-deregulation safety outcomes',
    'If substitution occurs: constraint is genuine rope (coordination of protection via market mechanisms). If no substitution: constraint is snare (pure extraction of protections without replacement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_substitution, empirical, 'Whether market mechanisms substitute for eliminated regulatory protections').

omega_variable(
    incidence_distribution,
    'Who actually bears the cost reduction burden? Is it distributed as labor cost reduction (wage suppression), capital cost reduction (profit increase), or price reduction (consumer benefit)?',
    'Wage trend analysis; profit margin analysis; price trend analysis for deregulated sectors; income distribution changes post-deregulation',
    'If labor bears cost (wage suppression): victims are workers, constraint is snare from labor perspective. If consumers bear cost (inadequate price reduction): victims are consumers, constraint is snare from consumer perspective. If incidence unclear: strong indicator of extraction being captured by capital/management.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incidence_distribution, empirical, 'Distribution of cost reduction benefits among labor, capital, and consumers').

omega_variable(
    market_competition_concentration,
    'Does deregulation enable genuine market competition or does it accelerate concentration and enable dominant firms to suppress rivals?',
    'Herfindahl index tracking in deregulated sectors; M&A activity and concentration trends; pricing power analysis; entry barrier changes',
    'If competition increases: constraint approaches rope (genuine market coordination). If concentration increases: constraint becomes snare (extraction via monopoly power enabled by deregulation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_competition_concentration, empirical, 'Impact of deregulation on market competition versus concentration').

omega_variable(
    regulatory_restoration_feasibility,
    'After deregulation, can regulatory frameworks be restored if harms emerge, or does institutional capacity decay make restoration prohibitively costly?',
    'Analysis of regulatory agency capacity retention; institutional knowledge loss; political economy of re-regulation; comparison to other countries'' re-regulatory cycles',
    'If restoration is feasible (scaffold logic holds): constraint is temporary with sunset path. If restoration is infeasible (institutional decay is permanent): constraint is snare (irreversible extraction). If uncertain: strong signal that deregulation imposes option value risk on future generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_restoration_feasibility, empirical, 'Feasibility of regulatory restoration after deregulation-driven institutional decay').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1981_reagan_regulatory_elimination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu81_tr_t0, sotu_1981_reagan_regulatory_elimination, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sotu81_tr_t3, sotu_1981_reagan_regulatory_elimination, theater_ratio, 3, 0.42).
narrative_ontology:measurement(sotu81_tr_t6, sotu_1981_reagan_regulatory_elimination, theater_ratio, 6, 0.48).
narrative_ontology:measurement(sotu81_tr_t10, sotu_1981_reagan_regulatory_elimination, theater_ratio, 10, 0.51).

% Extraction over time
narrative_ontology:measurement(sotu81_be_t0, sotu_1981_reagan_regulatory_elimination, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu81_be_t3, sotu_1981_reagan_regulatory_elimination, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(sotu81_be_t6, sotu_1981_reagan_regulatory_elimination, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(sotu81_be_t10, sotu_1981_reagan_regulatory_elimination, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1981_reagan_regulatory_elimination, resource_allocation).
narrative_ontology:affects_constraint(sotu_1981_reagan_regulatory_elimination, labor_market_deregulation).
narrative_ontology:affects_constraint(sotu_1981_reagan_regulatory_elimination, environmental_standards_rollback).
narrative_ontology:affects_constraint(sotu_1981_reagan_regulatory_elimination, financial_sector_deregulation_cascade).

% DUAL FORMULATION NOTE:
% The regulatory elimination constraint is upstream of sector-specific deregulation cascades. The general coordination mechanism (unified rule reduction) affects specific sectors differently: labor deregulation creates snare-like outcome for workers; environmental deregulation creates snare-like outcome for ecosystems; financial deregulation creates tangled-rope outcome for retail investors (coordination through market efficiency + extraction through information asymmetry). Each sector-specific constraint has its own epsilon and perspectives, linked via network.affects_constraints to the parent regulatory elimination constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1981_reagan_regulatory_elimination, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
