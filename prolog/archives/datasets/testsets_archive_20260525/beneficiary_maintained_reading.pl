% ============================================================================
% CONSTRAINT STORY: beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beneficiary_maintained_reading, []).

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
 *   constraint_id: beneficiary_maintained_reading
 *   human_readable: Market Dominance Actively Defended by Incumbent Capital Holders
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   Market dominance by incumbent capital holders is actively defended
 *   through a combination of network effects, intellectual property
 *   enforcement, regulatory positioning, predatory pricing, and strategic
 *   acquisition. This constraint exists at the intersection of genuine
 *   coordination (established firms provide stable platforms, supply chains,
 *   and investment certainty) and extractive rent protection (barriers to
 *   entry that exceed coordination requirements, suppression of technological
 *   alternatives, regulatory capture that prevents efficient market
 *   restructuring). The constraint story instantiates the
 *   BENEFICIARY_MAINTAINED reading of the contested kernel
 *   'market_naturalization' — treating dominant market structures as outcomes
 *   of identifiable incumbent actions, policy choices, and enforcement
 *   mechanisms rather than inevitable consequences of competitive dynamics or
 *   lapsed alternative pathways. Incumbent capital holders experience this as
 *   coordination; excluded entrepreneurs experience it as a snare; regulators
 *   experience it as a hybrid problem with both coordination and extraction
 *   components.
 *
 * KEY AGENTS:
 *   - Incumbent Capital Holders: Primary beneficiary (institutional/arbitrage) — dominant firms and their shareholders. Active defenders of market position through IP, lobbying, acquisition, pricing strategy.
 *   - Potential Market Entrants: Primary victim (powerless/trapped) — entrepreneurs, startups, alternative firms blocked from scaling past niche markets by systematic barriers.
 *   - Displaced Labor: Secondary victim (moderate/constrained) — workers displaced by consolidation or suppressed by labor-market concentration; lower wages, reduced mobility.
 *   - Smaller Competitors: Mixed agent (moderate/constrained) — can exist in constrained niches but dependent on incumbent platforms/supply chains; experience tangled rope.
 *   - Regulatory Coalition: Organized observer (organized/mobile) — antitrust authorities, consumer advocates, competition regulators. See both coordination value and extractive asymmetry.
 *   - Technological Disruptors: Organized future agent (powerful/mobile) — each generation brings technologies that bypass incumbent dominance (initially); scaffold perspective on current dominance.
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing incumbent dominance as law of markets rather than recognizing active maintenance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beneficiary_maintained_reading, 0.58).
domain_priors:suppression_score(beneficiary_maintained_reading, 0.68).
domain_priors:theater_ratio(beneficiary_maintained_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beneficiary_maintained_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(beneficiary_maintained_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(beneficiary_maintained_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(beneficiary_maintained_reading, "Market Dominance Actively Defended by Incumbent Capital Holders").
narrative_ontology:topic_domain(beneficiary_maintained_reading, "political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(beneficiary_maintained_reading, distributed).
narrative_ontology:cs_authority_grounding(beneficiary_maintained_reading, extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(beneficiary_maintained_reading, dominant_firm_networks).
narrative_ontology:constraint_victim(beneficiary_maintained_reading, potential_market_entrants).
narrative_ontology:constraint_victim(beneficiary_maintained_reading, displaced_labor).
narrative_ontology:constraint_victim(beneficiary_maintained_reading, competitive_pressure_absorbers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED ENTREPRENEUR (SNARE) — Faces systematically defended barriers to market entry: capital gatekeeping, regulatory capture, predatory pricing, patent thickets, network effects, and social capital exclusion. Exit options from the market are total — no way to compete meaningfully. The constraint extracts by preventing any alternative allocation of productive resources.
constraint_indexing:constraint_classification(beneficiary_maintained_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALLER COMPETITOR (TANGLED ROPE) — Can survive in niche markets or geographies where incumbent dominance is partial, but only through acceptance of constrained margins and technological dependence on incumbent-controlled platforms or supply chains. Experiences genuine coordination (access to networks, distribution channels) alongside extraction (asymmetric pricing, exclusionary terms, capacity suppression).
constraint_indexing:constraint_classification(beneficiary_maintained_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT CAPITAL HOLDER (ROPE) — Experiences the market dominance defense as coordination: maintaining proprietary networks, standards, supply chains, and customer relationships. The enforcement costs (lobbying, regulatory positioning, patent prosecution, acquisition of competitive threats) are perceived as necessary coordination overhead rather than extractive suppression. Maximum net beneficiary.
constraint_indexing:constraint_classification(beneficiary_maintained_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (TANGLED ROPE) — Antitrust authorities, consumer protection agencies, and open-market advocates see the constraint as both coordination (network effects, investment security, economies of scale generate genuine value) and extraction (defensive barriers extract rents and suppress innovation). Organized but with real constraints on their power to reshape market structure without destroying coordination value.
constraint_indexing:constraint_classification(beneficiary_maintained_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TECHNOLOGICAL DISRUPTION (SCAFFOLD) — Disruptive technologies (distributed networks, open-source production, digital platforms) create periodic windows where incumbent dominance is bypassed rather than overcome. The constraint is temporary — valid only until the next technology wave restructures competitive dynamics. Suppression is high but with an inherent sunset: the technology that beats incumbents one generation becomes the incumbent the next.
constraint_indexing:constraint_classification(beneficiary_maintained_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, market dominance by successful firms and capital concentration are treated as immutable features of capitalism itself: larger firms have structural advantages (economies of scale, network effects, access to capital) that cannot be overcome through competition. This perspective naturalizes incumbent defense as inevitable rather than actively maintained. False summit candidate — the observables (incumbent lobbying, patent thickets, acquisition strategy) reveal active maintenance, not natural law.
constraint_indexing:constraint_classification(beneficiary_maintained_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beneficiary_maintained_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(beneficiary_maintained_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(beneficiary_maintained_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint combines genuine coordination benefits (network effects, investment stability, innovation platforms) with extractive mechanisms (barriers to entry exceed coordination requirements, predatory pricing, IP thickets). The measurement trajectory shows rising extractiveness over 20 years: as digital platforms scale, network effects strengthen and barriers harden, increasing rent extraction. Suppression (0.68): High. Entrants face multiple coordinated barriers: capital gatekeeping, regulatory capture, patent prosecution, predatory pricing strategies, acquisition of competitive threats, and social capital exclusion. These barriers are not independent market outcomes but actively constructed. Theater ratio (0.55): Moderate. Incumbent defense includes both substantive coordination (genuine economies of scale, real network benefits) and performative elements (patent thickets with questionable innovation value, regulatory compliance theater, brand defense that exceeds functional differentiation). The ratio reflects that the coordination story is partially credible but embedded in extractive mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent capital holders perceive rope (coordination, network maintenance, investment security) because they control the network and benefit from exclusion. Potential entrants perceive snare (systematic exclusion with no exit) because they cannot overcome barriers through competitive effort. Smaller competitors perceive tangled rope (genuine access to platforms alongside constrained margins) because they can exist within the incumbent's ecosystem but not challenge dominance. Regulators perceive tangled rope at the organized level (both genuine coordination problems and extraction requiring oversight) because they see the full structure. Disruptors perceive scaffold (dominance is temporary, undermined by periodic technological shifts) because new platforms bypass incumbent networks. The analytical observer at the civilizational scope risks perceiving mountain (market dominance as inevitable consequence of competitive dynamics) — a false summit that naturalizes what is actively maintained.
 *
 * DIRECTIONALITY LOGIC:
 *   See logic_rationale and perspectival_gap for structural positioning and derived directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing between coordination value (network effects are real) and extraction (barriers exceed coordination needs). The beneficiary_maintained reading emphasizes that incumbent capital holders actively defend dominance through mechanisms beyond passive network effects — lobbying, IP thickets, predatory pricing, acquisition of threats. The constraint avoids both false summits (naturalizing as inevitable) and false rope classifications (treating all barriers as legitimate coordination). The tangled rope type reflects genuine mixed structure: incumbents coordinate, but their coordination is inseparable from rent extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_differentiation,
    'Is market dominance primarily an outcome of active beneficiary maintenance, lapsed alternative pathways, or hybrid mechanisms across different sectors?',
    'Comparative institutional analysis across sectors: industries with active incumbent defense (pharma, telecommunications) vs. industries with lapsed alternatives (agriculture consolidation) vs. hybrid dynamics (software/platforms). Historical counterfactual: what market structures would persist if incumbent defense stopped.',
    'This constraint represents the BENEFICIARY_MAINTAINED reading. Sibling readings (lapsed_alternative_reading, hybrid_reading) would decompose extractiveness differently and attribute dominance to different causal mechanisms. If evidence shows lapsed alternatives dominate causation, this reading''s ε would be overstated; if hybrid dominates, the snare/rope perspectives would shift toward tangled_rope across all positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_differentiation, conceptual, 'Which reading of market naturalization kernel applies: beneficiary-maintained, lapsed-alternative, or hybrid').

omega_variable(
    enforcement_cost_boundary,
    'What proportion of incumbent defense consists of genuine coordination costs (economies of scale, network maintenance) vs. extractive enforcement (lobbying, predatory pricing, regulatory capture)?',
    'Cost decomposition analysis: allocation of incumbent R&D spending to innovation vs. defensive IP; comparison of lobbying expenditure across competitive vs. concentrated sectors; correlation between enforcement spending and suppression of particular entrant types.',
    'If enforcement costs > 50% of defense spending: snare classification for more perspectives. If < 30%: rope classification strengthens. Current tangled_rope classification assumes mixed 40-60 split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_boundary, empirical, 'Proportion of incumbent defense that is coordination cost vs. extractive enforcement').

omega_variable(
    alternative_market_viability,
    'Could alternative market structures (distributed ownership, commons-based production, platform cooperatives) deliver equivalent coordination functions with lower extraction?',
    'Pilot program analysis (open-source ecosystems, cooperative networks, distributed finance protocols); measurement of coordination effectiveness (innovation rate, capital efficiency, quality) vs. incumbent structures; identification of structural barriers that aren''t scale-related.',
    'If viable alternatives exist: market dominance is demonstrated choice, not necessity — ε and suppression should both increase (more active suppression of real threats). If alternatives fail at scale: some of the enforced barriers are legitimate (mountain-like coordination needs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_market_viability, empirical, 'Whether alternative market structures could deliver equivalent coordination with lower extraction').

omega_variable(
    false_summit_natural_law,
    'Is market dominance by incumbent capital treated as natural law rather than actively maintained institutional choice?',
    'Discourse analysis: frequency of naturalizing language (''survival of the fittest,'' ''natural consolidation,'' ''inevitable outcomes'') vs. causal attribution to specific incumbent actions (lobbying, IP strategy, acquisition, predatory pricing). Historical analysis: did dominant firms become dominant through technical superiority alone, or through defensive strategies applied to technical advantage?',
    'The mountain perspective represents the false-summit candidate. If naturalizing language dominates policy discourse, the constraint''s actual extractiveness is obscured. If causal attribution to incumbent action dominates, the snare perspective becomes more salient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether market dominance is naturalized as inevitable law or recognized as actively maintained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beneficiary_maintained_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bene_tr_t0, beneficiary_maintained_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bene_tr_t10, beneficiary_maintained_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(bene_tr_t20, beneficiary_maintained_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(bene_be_t0, beneficiary_maintained_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bene_be_t10, beneficiary_maintained_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(bene_be_t20, beneficiary_maintained_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beneficiary_maintained_reading, resource_allocation).
narrative_ontology:affects_constraint(beneficiary_maintained_reading, regulatory_capture_institutional).
narrative_ontology:affects_constraint(beneficiary_maintained_reading, intellectual_property_rent_extraction).
narrative_ontology:affects_constraint(beneficiary_maintained_reading, labor_market_concentration).
narrative_ontology:affects_constraint(beneficiary_maintained_reading, technology_platformization).

% DUAL FORMULATION NOTE:
% Market dominance constraint family: This reading (beneficiary_maintained_reading) assumes incumbent capital holders actively defend through identifiable mechanisms. The lapsed_alternative_reading decomposes dominance into path-dependent institutional collapse (separate constraint). The hybrid_reading integrates sector-level variation. Each reading has its own ε and beneficiary/victim structure; together they model the contested kernel 'market_naturalization.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
