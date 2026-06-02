% ============================================================================
% CONSTRAINT STORY: temporal_scarcity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_scarcity, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temporal_scarcity
 *   human_readable: The "Scoop Economy" in Digital Media
 *   domain: technological/social
 *
 * SUMMARY:
 *   The scoop economy in digital media creates a structural tension between
 *   the real-time publication requirements of algorithmic amplification and
 *   the epistemic requirements for verification and synthesis. The constraint
 *   exhibits the classic tangled rope signature: genuine coordination
 *   function (rapid information dissemination) combined with asymmetric
 *   extraction (accuracy degradation, attention harvesting from audiences
 *   unable to verify). The temporal pressure is not exogenous — it is
 *   endogenously constructed by algorithmic amplification systems that reward
 *   speed over accuracy, creating a prisoner's dilemma where individual
 *   publishers cannot unilaterally slow down without losing audience share.
 *   The theater ratio (0.68) reflects that editorial standards persist as
 *   ritual (fact-checking departments, corrections sections, ethical codes)
 *   but have atrophied in function — they are applied retroactively through
 *   retractions and apologies, not preventatively. The constraint's
 *   extractiveness (0.52) is moderate-high because the scoop economy does
 *   deliver genuine value (real-time information access) alongside its
 *   extractive costs (compromised accuracy, attention harvesting). This
 *   moderate extractiveness distinguishes it from pure snares (debt traps,
 *   coercive labor) while still qualifying it as a significant asymmetric
 *   extraction mechanism.
 *
 * KEY AGENTS:
 *   - News Aggregators: Primary beneficiary (institutional/arbitrage) — capture attention and engagement without incurring verification costs; experience zero penalty for accuracy degradation
 *   - First Publishers: Secondary beneficiary (institutional/arbitrage) — claim scoop status and competitive advantage during real-time publication window; face reputational costs only if cascade failures accumulate
 *   - Attention Platforms (Algorithm Curators): Infrastructure beneficiary (institutional/arbitrage) — algorithmic amplification systems profit from engagement velocity regardless of accuracy; actively incentivize speed over synthesis
 *   - News Consumers: Primary victim (powerless/trapped) — trapped in real-time publication cycle; cannot exit information access without losing competitive awareness; no meaningful verification capacity
 *   - Information Quality: Structural victim (powerless/trapped) — abstract collective good that cannot organize or exit; bears full cost of verification deficit
 *   - Institutional Journalism: Secondary victim (moderate/constrained) — trapped by competitive dynamics; cannot invest in verification without losing scoop races; erosion of differentiation advantage (accuracy and depth)
 *   - Distributed Verification Communities: Organized agents (organized/constrained) — building alternative verification pathways (blockchain attestation, crowdsourced fact-checking, algorithmic credibility scoring) that represent sunset mechanism
 *   - Editorial Standards Bodies: Institutional ritual-keepers (institutional/arbitrage) — maintain performative standards apparatus while lacking enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_scarcity, 0.52).
domain_priors:suppression_score(temporal_scarcity, 0.58).
domain_priors:theater_ratio(temporal_scarcity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_scarcity, extractiveness, 0.52).
narrative_ontology:constraint_metric(temporal_scarcity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(temporal_scarcity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_scarcity, tangled_rope).
narrative_ontology:human_readable(temporal_scarcity, "The \"Scoop Economy\" in Digital Media").
narrative_ontology:topic_domain(temporal_scarcity, "technological/social").

domain_priors:requires_active_enforcement(temporal_scarcity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_scarcity, news_aggregators).
narrative_ontology:constraint_beneficiary(temporal_scarcity, first_publishers).
narrative_ontology:constraint_beneficiary(temporal_scarcity, attention_platforms).
narrative_ontology:constraint_victim(temporal_scarcity, news_consumers).
narrative_ontology:constraint_victim(temporal_scarcity, information_quality).
narrative_ontology:constraint_victim(temporal_scarcity, institutional_journalism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEWS CONSUMER (SNARE) — Trapped in the real-time publication cycle. Cannot exit news consumption without losing information access. No meaningful verification possible before story reaches them. Experiences pure extraction: attention is harvested without corresponding accuracy guarantees. Maximum d (≈0.95) — full target of extraction.
constraint_indexing:constraint_classification(temporal_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL JOURNALIST (TANGLED ROPE) — Constrained by competitive pressure to publish immediately or lose the scoop. Also benefits from the real-time attention apparatus that amplifies their work. The constraint both extracts (forced publication without verification) and coordinates (enables rapid information dissemination). Career incentives are asymmetric — scoops reward advancement; cautious verification rewards nothing.
constraint_indexing:constraint_classification(temporal_scarcity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NEWS AGGREGATOR (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: algorithmic curation and rapid republishing solve the collective action problem of 'who surfaces the most recent information?' Faces no penalty for inaccuracy; benefits from traffic amplification. Extraction runs toward this agent. Low d (≈0.15) — net beneficiary with arbitrage exit.
constraint_indexing:constraint_classification(temporal_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL NEWS ORGANIZATIONS (SNARE) — Trapped by competitive real-time dynamics. Cannot slow publication without losing audience share. Also cannot verify at scale without incurring costs that competitors avoid. The scoop economy extracts institutional resources (staff, investigative budget) while eliminating their information quality advantage. Organized but structurally trapped — the coordination mechanism has become a mechanism of extraction.
constraint_indexing:constraint_classification(temporal_scarcity, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: EDITORIAL STANDARDS BODIES (PITON) — Traditional journalism ethics (fact-checking, source verification, cooling-off periods) persist as ritual but have atrophied in function. Editorial review is now performative — stories are published first, 'standards' applied retroactively through corrections and retractions. Theater ratio reflects the gap between professed standards and actual practice. The institutional apparatus maintains theatrical compliance without enforcement.
constraint_indexing:constraint_classification(temporal_scarcity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DISTRIBUTED VERIFICATION COMMUNITIES (SCAFFOLD) — Organized agents (fact-checkers, citizen archives, decentralized verification networks) see the scoop economy as a temporary coordination failure with a sunset: blockchain-based content attestation, distributed fact-checking bots, and algorithmic credibility scoring are building alternative verification pathways. The constraint declines as these alternatives mature. Modest sunset (15-25 years for mainstream adoption).
constraint_indexing:constraint_classification(temporal_scarcity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the scoop economy exhibits genuine coordination function (rapid information dissemination) combined with asymmetric extraction (accuracy degradation, attention harvesting). Neither pure law nor pure coordination — the constraint is institutionally contingent and partially remediable. The analytical perspective sees both the beneficial real-time signal and the extractive accuracy cost.
constraint_indexing:constraint_classification(temporal_scarcity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_scarcity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_scarcity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_scarcity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(temporal_scarcity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(temporal_scarcity, TR),
    TR >= 0.70.

:- end_tests(temporal_scarcity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The scoop economy extracts accuracy from the epistemic commons while distributing attention and engagement value upward to aggregators and platforms. The extraction is not maximal (0.70+) because genuine coordination value is provided — real-time information dissemination is a real capability. However, the value is asymmetrically distributed: audiences receive speed but not accuracy assurance; publishers receive engagement but not verification resources; institutions lose competitive advantage without gaining efficiency. The temporal pressure is endogenously amplified by algorithmic systems, not exogenously necessary. Suppression (0.58): Moderate-high. Significant barriers to exit include: competitive necessity of real-time participation (cannot slow down without losing audience), algorithmic amplification economics (speed-optimized systems), platform design (encourages rapid republication, penalizes fact-checking delays), and structural knowledge asymmetry (audiences cannot verify before consumption). However, suppression is not absolute — some institutional journalists maintain verification practices despite cost; some audiences develop critical consumption habits; alternative verification pathways exist but are not mainstream. Theater ratio (0.68): High and increasing. Editorial standards (fact-checking departments, ethics codes, correction processes) persist as institutional ritual but have atrophied in preventative function. The ritual is performative: stories are published first (theater), standards are invoked retroactively (corrections, retractions). The theater ratio has increased from 0.35 (when traditional editorial gates still functioned) to 0.68 (current state) as real-time publication has become default practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how indexical classification reveals structural power asymmetries invisible in single-perspective analysis. The aggregator sees rope (coordination with no extraction felt). The consumer sees snare (extraction without coordination value). The journalist sees tangled rope (mixed). The analytical observer sees tangled rope (asymmetric extraction with genuine coordination function). The perspectives are not measurement artifacts — they reflect real structural differences in power, exit capacity, and benefit flow. The gap is resolved not by averaging or hierarchizing, but by recognizing that the constraint operates differently for different agents. The mandatrophy is resolved by the tangled rope classification: the constraint is not pure coordination (rope) nor pure extraction (snare), but a hybrid that genuinely serves coordination while asymmetrically extracting.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural power and exit capacity. News aggregators occupy low d (≈0.15): institutional power + arbitrage exit + beneficiary status. They can choose participation terms; they profit from the constraint; they can exit or reposition without cost. News consumers occupy high d (≈0.95): powerless status + trapped exit + victim status. They cannot exit news consumption without losing information access; they bear full cost of accuracy degradation; they have no escape. Institutional journalists occupy moderate d (≈0.55): moderate power + constrained exit + mixed beneficiary/victim status. They benefit from real-time amplification but suffer extraction of verification resources. The sigmoid f(d) translates d into experienced extractiveness multiplier: low d (beneficiaries) experience negative or minimal χ; high d (trapped victims) experience amplified χ. This produces the perspectival divergence in classification types across the same base extractiveness score.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH TANGLED ROPE: The constraint resolves mandatrophy by combining genuine coordination function (real-time information dissemination) with asymmetric extraction (accuracy degradation, attention harvesting without verification). The analytical observer perceives both functions simultaneously: the scoop economy IS a coordination mechanism (it solves the real problem of rapid information distribution) AND an extraction mechanism (it asymmetrically distributes costs and benefits). This is not measurement ambiguity or perspective relativism — it is structural reality. The tangled rope classification is correct because: (1) genuine beneficiaries exist (aggregators, first publishers) who benefit from the constraint, (2) genuine victims exist (consumers, information quality, institutional journalism) who bear costs, (3) the mechanism requires active enforcement (algorithmic amplification, publication velocity pressure, competitive dynamics) to maintain the asymmetry, and (4) suppression is significant (0.58) because exit options are limited and verification costs are structural. The mandatrophy dissolves because we have a legitimate hybrid type that describes what is actually happening: coordination overlaid with extraction, not coordination disguised as extraction or vice versa.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accuracy_threshold_definition,
    'At what point does real-time publication without verification constitute actionable misinformation rather than legitimate speed-of-truth optimization?',
    'Longitudinal tracking of correction rates, retraction cascades, and downstream harms from unverified claims; analysis of false positive rates by publication latency',
    'If threshold < 1 hour: most breaking news is misinformation (classification remains Snare). If threshold > 24 hours: current practices may be defensible as fast-enough coordination (Rope from some perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accuracy_threshold_definition, conceptual, 'Definition of actionable misinformation threshold in real-time publishing').

omega_variable(
    audience_demand_endogeneity,
    'Does the scoop economy reflect genuine consumer demand for immediate information, or does algorithmic amplification artificially construct that demand?',
    'A/B testing of publishing latency with and without algorithmic amplification; surveys of consumer preferences when default/algorithmic bias is removed; analysis of engagement curves for verified vs unverified stories',
    'If demand is genuine: Rope classification gains credibility — the constraint solves a real coordination problem. If demand is constructed: Snare classification is correct — the constraint manufactures scarcity that wouldn''t exist without algorithmic amplification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(audience_demand_endogeneity, empirical, 'Whether real-time demand is endogenous or algorithmically constructed').

omega_variable(
    verification_scalability,
    'Can distributed verification (crowdsourcing, AI-assisted fact-checking, decentralized attestation) actually scale to match publication volume without creating new asymmetries or gatekeeping?',
    'Pilot deployment of distributed verification at scale; measurement of false positive rates in crowdsourced vs professional fact-checking; analysis of economic incentives for participation',
    'If scalable: scaffold sunset is realistic — decentralized verification becomes the exit pathway. If not scalable: scaffold perspective is aspirational, and the scoop economy persists as structural extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_scalability, empirical, 'Whether distributed verification can scale without creating new gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_scarcity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_scarcity_tr_t0, temporal_scarcity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(temp_scarcity_tr_t5, temporal_scarcity, theater_ratio, 5, 0.52).
narrative_ontology:measurement(temp_scarcity_tr_t10, temporal_scarcity, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(temp_scarcity_be_t0, temporal_scarcity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(temp_scarcity_be_t5, temporal_scarcity, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(temp_scarcity_be_t10, temporal_scarcity, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_scarcity, information_standard).
narrative_ontology:affects_constraint(temporal_scarcity, algorithmic_amplification).
narrative_ontology:affects_constraint(temporal_scarcity, verification_bottleneck).
narrative_ontology:affects_constraint(temporal_scarcity, attention_economy_extraction).

% DUAL FORMULATION NOTE:
% The scoop economy is decomposable into upstream constraint (algorithmic amplification that creates temporal pressure) and downstream constraint (accuracy-speed tradeoff in editorial decision-making). This story focuses on the temporal scarcity mechanism itself. The upstream constraint (algorithmic amplification) has different ε and would be separately analyzed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
