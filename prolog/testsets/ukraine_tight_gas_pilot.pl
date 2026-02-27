% ============================================================================
% CONSTRAINT STORY: ukraine_tight_gas_pilot
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ukraine_tight_gas_pilot, []).

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
 *   constraint_id: ukraine_tight_gas_pilot
 *   human_readable: Ukraine Tight Gas Pilot Project Framework
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   Ukraine's tight gas pilot project with Expert Petroleum represents a
 *   state-sponsored energy security initiative that functions simultaneously
 *   as technology transfer coordination and as asymmetric rent extraction.
 *   The constraint emerges from Ukraine's structural energy vulnerability
 *   (Russian gas dependency during geopolitical tension) combined with
 *   domestic capacity gaps in tight gas extraction technology. The exclusive
 *   partnership solves a genuine coordination problem — tight gas requires
 *   specialized equipment and expertise Ukraine lacks — but the exclusivity
 *   mechanism also forecloses alternative pathways (competitive bidding,
 *   multi-partner licensing, domestic producer participation) that could
 *   achieve the same technical outcome at lower extraction cost. Theater
 *   ratio (0.58) reflects the dual narrative: the partnership is presented as
 *   technical necessity (coordination frame) while embedding monopoly profit
 *   protection (extraction frame). The constraint exhibits all six types
 *   across different perspectives, revealing how the same structural
 *   arrangement can appear as coordination to beneficiaries, extraction to
 *   consumers, and contingent institutional theater to observers.
 *
 * KEY AGENTS:
 *   - Naftogaz Leadership: Primary beneficiary (institutional/arbitrage) — captures monopoly rents and service contract revenues during exclusive pilot period
 *   - Expert Petroleum: Primary beneficiary (institutional/arbitrage) — receives exclusive territorial licensing, profit share, and long-term service contracts; can exit to other markets
 *   - Ukrainian Energy Consumers: Primary victim (powerless/trapped) — pay implicit subsidies through restricted supply and monopoly pricing; cannot exit energy system
 *   - Competing Domestic Gas Producers: Secondary victim (moderate/constrained) — excluded from tight gas development zone; constrained exit options
 *   - Ukrainian Government: Organized actor (organized/constrained) — experiences mixed coordination (energy security gain) and extraction (rent leakage to partners)
 *   - Post-Soviet Bureaucracy: Institutional inertia (institutional/arbitrage) — perpetuates state monopoly through performative justifications rather than functional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ukraine_tight_gas_pilot, 0.52).
domain_priors:suppression_score(ukraine_tight_gas_pilot, 0.65).
domain_priors:theater_ratio(ukraine_tight_gas_pilot, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, extractiveness, 0.52).
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ukraine_tight_gas_pilot, tangled_rope).
narrative_ontology:human_readable(ukraine_tight_gas_pilot, "Ukraine Tight Gas Pilot Project Framework").
narrative_ontology:topic_domain(ukraine_tight_gas_pilot, "economic/geopolitical").

domain_priors:requires_active_enforcement(ukraine_tight_gas_pilot).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, naftogaz_leadership).
narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, expert_petroleum).
narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, ukrainian_government_treasury).
narrative_ontology:constraint_victim(ukraine_tight_gas_pilot, ukrainian_energy_consumers).
narrative_ontology:constraint_victim(ukraine_tight_gas_pilot, competing_gas_producers).
narrative_ontology:constraint_victim(ukraine_tight_gas_pilot, regional_energy_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENERGY CONSUMERS (SNARE) — Domestic consumers bear full cost of exclusive partnership arrangement through licensing restrictions and limited domestic supply competitiveness. Cannot exit energy system. Bear extraction costs of monopoly rents that accumulate to Naftogaz leadership and Expert Petroleum. Maximum experienced extraction — no advocacy mechanism, no alternative supply pathway during pilot phase.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING PRODUCERS (SNARE) — Excluded from tight gas development zone by exclusive partnership agreement. Cannot access reserves. Face constrained exit: switching to different hydrocarbon extraction requires capital redeployment but is possible. Suppression is high — regulatory exclusion prevents market entry; suppression is structural, not circumstantial.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UKRAINIAN GOVERNMENT (TANGLED ROPE) — Experiences constraint as mixed coordination-extraction. The partnership solves a genuine technical problem: tight gas extraction requires specialized expertise Ukraine lacks domestically. Coordination benefit: access to technology transfer, training, and production scaling. But also extracts: Expert Petroleum captures disproportionate profit share; long-term licensing agreements create path dependency. Government is constrained by geopolitical necessity (energy security vs Russian dependency) — exit to alternative partnerships is possible but costly. Active enforcement required: exclusive license must be legally protected and renewed.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXPERT PETROLEUM (ROPE) — Experiences constraint as pure coordination. Solves technical and capital deployment problem: brings specialized equipment and expertise to unlock reserves. Net beneficiary: receives licensing exclusivity, profit share, and service contract obligations. Has arbitrage exit: can walk away if terms become unfavorable or redirect capital to other markets. Extraction runs toward this agent from system. Low experienced extraction due to institutional power and arbitrage exit options.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SOVIET-ERA BUREAUCRACY (PITON) — Naftogaz organizational structure and incentives are largely vestigial: state-owned monopoly maintained through inertia and political convenience, not functional necessity. The exclusive partnership with Expert Petroleum is justified using technical necessity (tight gas requires foreign expertise) but is substantially performative theater — disguising rent extraction as technology transfer. Theater ratio is elevated because the 'pilot project' rhetoric obscures bilateral profit-sharing. The bureaucratic structure persists because privatization is politically infeasible, not because state ownership is functionally justified. Piton classification: degraded coordination mechanism maintained by institutional inertia.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global perspective, the constraint combines genuine energy security coordination (Ukraine reducing Russian gas dependency) with asymmetric rent extraction (Expert Petroleum and Naftogaz leadership capturing monopoly profits). The coordination is real and necessary. The extraction is real and deliberate. Both are structural properties of the same arrangement. Suppression is high because alternative pathways (competitive bidding, domestic producer participation, technology licensing to multiple firms) are foreclosed by the exclusive partnership design. Theater ratio (0.58) reflects mixed performative and functional content: genuine technical necessity combined with rent-justifying narrative.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ukraine_tight_gas_pilot_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ukraine_tight_gas_pilot, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ukraine_tight_gas_pilot, TR),
    TR >= 0.70.

:- end_tests(ukraine_tight_gas_pilot_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The exclusive partnership captures profit that could be distributed across multiple licensees or returned to consumers through competitive pricing. The tight gas market is genuine and valuable (estimated 4-6 trillion cubic meters in accessible reserves), but the profit split systematically advantages Expert Petroleum and Naftogaz leadership. Theater ratio increase (0.42→0.58) reflects Goodhart drift: as the partnership matures, justificatory rhetoric (technology transfer, energy security) becomes more performative — public framing emphasizes development benefits while actual profit structures remain opaque. Suppression (0.65): High. The exclusive license forecloses alternative pathways. Regulatory barriers prevent competing bidders from accessing the same reserves. Career incentives within Naftogaz are structured to protect the partnership. Consumer pricing lacks transparency mechanisms to reveal extraction costs. Suppression is structural — the exclusive design itself suppresses alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Expert Petroleum sees rope (pure coordination solving a technical problem). Naftogaz leadership sees rope (capturing monopoly rents justified by technical necessity). Ukrainian government sees tangled rope (genuine energy security gain mixed with profit leakage). Competing producers see snare (excluded from reserves by regulatory fiat). Consumers see snare (restricted supply enabling monopoly pricing). Soviet-era bureaucracy sees piton (performative technology transfer masking rentier inertia). The analytical observer sees tangled rope (both coordination and extraction are structurally real). The perspectival gap reveals that the same structural arrangement genuinely provides benefits (technology transfer, energy security) while systematically extracting from those who cannot exit (consumers, competing producers). The gap also reveals the false summit risk: framing the partnership as 'natural necessity' (mountain view) would naturalize what is actually a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position within the extraction flow. Expert Petroleum has institutional power and arbitrage exit (can redirect capital elsewhere), so the derivation produces low d — they experience the constraint as beneficial coordination. Naftogaz leadership, while nominally a state institution, has institutional power and effective arbitrage (can renegotiate partnership terms or seek other partnerships), producing low-moderate d. Ukrainian government sits at the pivot: constrained exit (energy security necessity limits alternatives), some power (can modify terms but faces geopolitical constraints), producing moderate d around 0.5. Competing domestic producers are powerless relative to the state monopoly, with exit constrained by capital requirements — producing moderate-high d. Energy consumers are powerless and trapped by the energy system itself — producing high d approaching 1.0. The directionality spread (from -0.12 for Expert Petroleum arbitrage to 1.4+ for trapped consumers) produces the perspectival gap: beneficial coordination for beneficiaries, pure extraction for consumers.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing that tangled rope classification is correct precisely because it acknowledges both genuine coordination (energy security, technology transfer) and asymmetric extraction (monopoly rents, foreclosed alternatives). The false mountain would claim the exclusive partnership is 'natural' and 'technically necessary,' erasing the distributional choices embedded in the design. The false snare would deny the genuine energy security coordination function. The tangled rope acknowledges both: the partnership solves a real problem AND redistributes surplus toward beneficiaries. The exclusive design is a choice, not a technical requirement — alternative models (competitive bidding, non-exclusive licensing, technology transfer obligations on multiple partners) could provide the same coordination benefits at lower extraction cost. The omegas identify the key factual ambiguities that would resolve whether the current design is minimally extractive (tangled rope confirmed) or maximally extractive (snare confirmed): technology transfer effectiveness, profit fairness, license exclusivity necessity, and consumer subsidy incidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_effectiveness,
    'Does the exclusive partnership actually transfer tight gas extraction expertise to Ukrainian firms, or does it perpetuate foreign dependence on Expert Petroleum?',
    'Post-pilot assessment of Ukrainian technical capacity: personnel trained, domestic firms capable of independent extraction, knowledge transfer documented. Comparison with alternative licensing models (non-exclusive multi-partner) used in comparable countries.',
    'If effective transfer: tangled rope classification confirmed, with sunset path toward independence. If ineffective: constraint reclassifies toward pure extraction snare; perpetuates foreign extraction with Ukrainian consumer cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_effectiveness, empirical, 'Whether tight gas expertise is actually transferred to Ukrainian capacity').

omega_variable(
    profit_sharing_fairness,
    'Is the profit split between Expert Petroleum and Naftogaz reflective of actual capital/risk contributions, or does it embed hidden extraction masquerading as fair partnership?',
    'Comparative contract analysis: profit allocation vs industry standards for similar joint ventures; cost accounting for Expert Petroleum''s capital investment vs Naftogaz''s resource access; market valuation of exclusive territorial rights.',
    'If fair allocation: tangled rope confirmed. If extraction-embedded: classification shifts toward snare for consumers and competing producers; extraction coefficient rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(profit_sharing_fairness, empirical, 'Whether profit-sharing reflects fair risk/capital allocation').

omega_variable(
    exclusive_license_necessity,
    'Is the exclusive partnership structurally necessary for tight gas development, or is exclusivity a rent-protection mechanism disguised as technical necessity?',
    'Comparative case study: tight gas development in Poland, Romania, and other post-Soviet states. Analysis of whether non-exclusive competitive models achieved same extraction rates and technology development. Modeling of tight gas extraction with multiple licensed firms.',
    'If necessary: justifies suppression and extraction as coordination requirement. If unnecessary: suppression becomes purely extractive; constraint reclassifies toward higher-extraction snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusive_license_necessity, empirical, 'Whether exclusive partnership is technically necessary or rentier design').

omega_variable(
    consumer_subsidy_implicit,
    'Do Ukrainian domestic energy consumers bear implicit subsidies to Expert Petroleum and Naftogaz leadership through restricted supply and monopoly pricing, or is domestic supply adequately decoupled from partnership profits?',
    'Price comparison: domestic gas prices under partnership vs international benchmarks; cost allocation analysis between export contracts and domestic supply; tracing of partnership profits back to consumer price markup.',
    'If subsidy is significant: snare classification for consumers is confirmed with high extraction. If decoupled: tangled rope classification strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_subsidy_implicit, empirical, 'Whether consumers implicitly subsidize partnership extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ukraine_tight_gas_pilot, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(utgp_tr_t0, ukraine_tight_gas_pilot, theater_ratio, 0, 0.42).
narrative_ontology:measurement(utgp_tr_t3, ukraine_tight_gas_pilot, theater_ratio, 3, 0.52).
narrative_ontology:measurement(utgp_tr_t6, ukraine_tight_gas_pilot, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(utgp_be_t0, ukraine_tight_gas_pilot, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(utgp_be_t3, ukraine_tight_gas_pilot, base_extractiveness, 3, 0.46).
narrative_ontology:measurement(utgp_be_t6, ukraine_tight_gas_pilot, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ukraine_tight_gas_pilot, resource_allocation).
narrative_ontology:affects_constraint(ukraine_tight_gas_pilot, ukrainian_energy_security_dependency).
narrative_ontology:affects_constraint(ukraine_tight_gas_pilot, post_soviet_naftogaz_monopoly).
narrative_ontology:affects_constraint(ukraine_tight_gas_pilot, russian_gas_leverage_over_ukraine).

% DUAL FORMULATION NOTE:
% The tight gas pilot is downstream of broader Ukrainian energy security constraints but represents a distinct structural arrangement. The upstream energy dependency constraint has higher extractiveness and pure extraction (snare) characteristics; the tight gas pilot combines genuine coordination (technical necessity) with rent extraction (exclusive licensing). The two constraints are linked: successful tight gas development reduces Russian leverage (upstream snare severity decreases), but only if the tight gas coordination actually transfers technology rather than perpetuating foreign dependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ukraine_tight_gas_pilot, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
