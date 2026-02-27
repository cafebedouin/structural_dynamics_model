% ============================================================================
% CONSTRAINT STORY: strategic_deep_sea_rare_earth_mining
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strategic_deep_sea_rare_earth_mining, []).

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
 *   constraint_id: strategic_deep_sea_rare_earth_mining
 *   human_readable: Strategic Deep-Sea Mining for Rare Earth Minerals
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Strategic deep-sea rare earth mining represents a hybrid constraint
 *   combining legitimate geopolitical supply-chain resilience (coordination
 *   function) with asymmetric ecological extraction and capacity-based
 *   exclusion of developing nations. Japan's technological leadership in
 *   deep-sea exploration, combined with state backing through JOGMEC, creates
 *   a window of technological monopoly—advanced mining companies and
 *   regulatory bodies experience coordination benefits, while developing
 *   coastal nations, artisanal mining communities, and marine ecosystems face
 *   extraction with limited alternatives. The constraint exhibits tangled
 *   rope structure: genuine coordination (reducing Chinese REE monopoly)
 *   layered atop asymmetric extraction (environmental costs, technological
 *   gatekeeping, EEZ resource access). Theater component (58%) reflects the
 *   performative elements of environmental impact assessment and ISA
 *   regulatory processes that sometimes mask continued extraction. The
 *   constraint's evolution depends critically on technological substitution
 *   (REE recycling maturation) and international governance enforcement—if
 *   both accelerate, the deep-sea mining window narrows and the scaffold
 *   sunset mechanism becomes real. If governance remains performative and
 *   substitution stalls, the constraint hardens into pure snare for powerless
 *   actors.
 *
 * KEY AGENTS:
 *   - Japanese State (JOGMEC): Primary beneficiary (institutional/arbitrage) — strategic goal is REE supply independence and geopolitical leverage
 *   - Advanced Electronics Manufacturers: Secondary beneficiary (institutional/arbitrage) — gain supply chain resilience and Chinese dependency reduction
 *   - Developing Coastal Nations: Primary victim (powerless/trapped) — lack capital and technology to access their own EEZ resources
 *   - Deep-Sea Marine Ecosystems: Primary victim (powerless/trapped) — no exit option; bear ecological costs
 *   - Artisanal Mining Communities: Secondary victim (organized/constrained) — face technological displacement and wage suppression
 *   - International Seabed Authority (ISA): Institutional actor (institutional/constrained) — develops mining code but lacks enforcement mechanisms
 *   - Deep-Sea Mining Regulation Coalition: Organized agents (organized/constrained) — NGOs, small island states building moratoriums and alternatives
 *   - Chinese Terrestrial REE Monopoly: Institutional actor (institutional/constrained) — degraded constraint losing functional necessity (piton)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strategic_deep_sea_rare_earth_mining, 0.52).
domain_priors:suppression_score(strategic_deep_sea_rare_earth_mining, 0.65).
domain_priors:theater_ratio(strategic_deep_sea_rare_earth_mining, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, extractiveness, 0.52).
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strategic_deep_sea_rare_earth_mining, tangled_rope).
narrative_ontology:human_readable(strategic_deep_sea_rare_earth_mining, "Strategic Deep-Sea Mining for Rare Earth Minerals").
narrative_ontology:topic_domain(strategic_deep_sea_rare_earth_mining, "geopolitical/economic").

domain_priors:requires_active_enforcement(strategic_deep_sea_rare_earth_mining).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strategic_deep_sea_rare_earth_mining, japanese_state_industrial_policy).
narrative_ontology:constraint_beneficiary(strategic_deep_sea_rare_earth_mining, advanced_electronics_manufacturers).
narrative_ontology:constraint_beneficiary(strategic_deep_sea_rare_earth_mining, geopolitical_supply_chain_resilience).
narrative_ontology:constraint_victim(strategic_deep_sea_rare_earth_mining, deep_sea_marine_ecosystems).
narrative_ontology:constraint_victim(strategic_deep_sea_rare_earth_mining, developing_coastal_nations).
narrative_ontology:constraint_victim(strategic_deep_sea_rare_earth_mining, global_environmental_commons).
narrative_ontology:constraint_victim(strategic_deep_sea_rare_earth_mining, artisanal_mining_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING COASTAL NATIONS (SNARE) — Coastal developing nations face extraction through exclusion from technological capacity and capital requirements for deep-sea operations. Trapped by geology (REE deposits in all EEZs) and resource constraints (cannot fund exploration/extraction alone). Japan's technological monopoly on deep-sea mining creates asymmetric access to ocean resources. d≈0.92, f(d)≈1.40, σ=1.1 → χ≈0.80.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DEEP-SEA MARINE ECOSYSTEMS (SNARE) — Abyssal ecosystems have no exit option and cannot organize. Bear full extraction cost through habitat destruction, sediment plumes, noise pollution, and species loss. Zero alternatives for ecosystem resilience. d≈0.98, f(d)≈1.42, σ=1.2 → χ≈0.83.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ARTISANAL MINING COMMUNITIES (TANGLED ROPE) — Land-based artisanal miners in Southeast Asia benefit from REE demand (employment, market access via Japanese buyers) but face extraction through technological displacement and wage suppression as deep-sea supply scales. Organized enough to mobilize but constrained by capital barriers and regulatory capture. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.51.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ADVANCED ELECTRONICS MANUFACTURERS (ROPE) — Benefit from supply chain resilience and reduced Chinese dependency. Experience the constraint as coordination: access to Japanese-controlled rare earths improves negotiating position vs. Chinese monopoly. No extraction felt because manufacturers have arbitrage options (shift suppliers, source from multiple nations, diversify technologies). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL ENVIRONMENTAL GOVERNANCE BODIES (TANGLED ROPE) — Coordination function: ISA (International Seabed Authority) develops mining code to manage deep-sea extraction. Extraction mechanism: Japan's technological capacity and state backing create de facto enforcement asymmetry; ISA rules are negotiated with institutional power leverage favoring technical pioneers. Constrained by lack of enforcement mechanisms for high-seas mining. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DEEP-SEA MINING REGULATION COALITION (SCAFFOLD) — Organized agents (environmental NGOs, small island states, progressive governments) are building regulatory frameworks with sunset logic: moratoriums on high-seas mining (Nauru-sponsored ISA rules), EEZ protection agreements, and alternative technology development (REE recycling, synthetic substitutes). Theater ratio remains moderate (58%) because regulatory codification has genuine structural function—not merely performative. d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.19. Sunset mechanism: technological alternatives mature (REE recycling reaches 60%+ recovery) or synthetic substitutes reduce demand → deep-sea extraction loses economic justification.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: LAND-BASED RARE EARTH MINING OLIGOPOLY (PITON) — China's terrestrial REE monopoly (97% global supply pre-2020, still 60%+ after diversification efforts) is a degraded constraint. Technological shift toward ocean resources is eroding its function. The oligopoly persists through institutional inertia (supply relationships, refining infrastructure, geopolitical leverage) despite declining structural necessity. theater_ratio=0.58 indicates moderate performative content — oligopoly maintains pricing power through narrative of scarcity despite emerging alternatives. As deep-sea and recycling mature, terrestrial oligopoly becomes increasingly theatrical.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalist view, rare earth scarcity is a natural fact: these elements exist in limited concentration, extraction requires industrial infrastructure, and geographical uneven distribution is a geological constraint. However, structural data (ε=0.52, suppression=0.65, theater=0.58) contradicts mountain classification. The constraint is not about scarcity itself but about strategic control of extraction technology and EEZ governance—contingent institutional arrangements, not natural law. This is a false summit: naturalizing geopolitical competition as resource inevitability.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strategic_deep_sea_rare_earth_mining_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strategic_deep_sea_rare_earth_mining, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(strategic_deep_sea_rare_earth_mining, TR),
    TR >= 0.70.

:- end_tests(strategic_deep_sea_rare_earth_mining_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Base extraction reflects the asymmetric technological capacity (Japan can mine, developing nations cannot), ecological costs borne by non-beneficiaries, and strategic resource control. Trajectory shows rise from 0.28 → 0.52 as deep-sea mining matures from research to commercial deployment. Not extreme (>0.66) because extraction is geographically limited (EEZs, not open ocean initially) and technological alternatives are emerging. Suppression (0.65): High. Multiple barriers: capital requirements eliminate independent developing nation access, environmental regulations limit where mining occurs, technological opacity prevents local competition, and geopolitical leverage constrains negotiating power. Developing coastal nations have theoretically sovereign EEZ rights but practically cannot exercise them. Theater ratio (0.58): Moderate. Environmental impact assessments, ISA regulatory processes, and sustainability claims have genuine structural function—they do establish baselines and monitoring. However, performative elements exist: impact assessments often underestimate long-term ecological damage, ISA codes lack enforcement teeth, and sustainability framing obscures resource extraction as environmental stewardship.
 *
 * PERSPECTIVAL GAP:
 *   The tangled rope classification emerges from dramatic perspectival divergence. Japanese state and electronics manufacturers experience genuine coordination (rope perspective)—supply chain resilience solves a real collective action problem. ISA and regulation coalition see coordination with extraction overlay (tangled rope)—they're building governance mechanisms that both enable and constrain mining. Artisanal miners and developing nations experience pure extraction (snare)—technological gatekeeping traps them without benefit. Marine ecosystems experience irreversible snare—they have no capacity to exit or organize. The scaffold perspective from the regulation coalition is forward-looking: if technological alternatives and regulatory frameworks mature, the constraint's extraction mechanism becomes time-limited. The piton perspective on terrestrial monopoly reflects that the oligopoly's function is eroding—deep-sea mining undermines the rationale for scarcity-based pricing that sustained it. The analytical observer's mountain view (natural resource scarcity as inevitable) is a false summit: scarcity is real, but the extraction mechanism is institutional (technological monopoly, EEZ governance), not geological.
 *
 * DIRECTIONALITY LOGIC:
 *   Japanese state & electronics manufacturers: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net positive; they can exit (diversify suppliers, shift technologies) with manageable cost. Developing coastal nations: Victims + trapped → d≈0.92, f(d)≈1.40. Trapped by geography (REE deposits in all EEZs) and capital constraints (cannot fund exploration independently). High effective extraction. Marine ecosystems: Victims + trapped → d≈0.98, f(d)≈1.42. Absolute extraction—no capacity to exit or negotiate. Artisanal miners: Victims + constrained → d≈0.68, f(d)≈1.05. Constrained by technological displacement but have some organizational capacity and market alternatives. ISA/governance bodies: Institutional + constrained → d≈0.55, f(d)≈0.75. Constrained by lack of enforcement mechanisms but have coordination function. Regulation coalition: Organized + constrained → d≈0.35, f(d)≈0.28. Lower extraction because coalition has agency and sees sunset pathway. Terrestrial monopoly: Institutional + constrained → d≈0.50 (piton derivation differs—theater gate applies). Theater indicates performative maintenance, not extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through structural decomposition: the constraint combines (1) legitimate coordination—reducing Chinese REE dependency solves a real collective action problem, and (2) asymmetric extraction—technological gatekeeping and environmental externalities create uncompensated costs. Both functions are real and necessary to explain the perspectives. Japan genuinely solves a coordination problem (beneficial rope from electronics manufacturers' perspective); simultaneously, developing nations genuinely experience extraction (snare perspective). The hybrid tangled_rope classification prevents mischaracterizing this as either pure coordination (which would miss the environmental/capacity asymmetry) or pure extraction (which would miss the real supply-chain resilience benefit). The scaffold perspective's sunset logic prevents indefinite classification as extraction: if technological alternatives materialize and governance enforcement succeeds, the extraction window closes and the constraint transforms into temporary scaffolding. The piton perspective on terrestrial monopoly shows how constraints degrade: as deep-sea mining and recycling mature, the Chinese monopoly's functional scarcity erodes, leaving only inertial pricing power. This avoids the error of treating the monopoly as a timeless natural law (mountain) or perpetual snare when it is actually transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecological_impact_threshold,
    'What ecological damage rate from deep-sea mining triggers irreversible ecosystem collapse vs. manageable impact?',
    'Long-term abyssal ecosystem monitoring; modeling of sediment plume dispersion and benthic recovery timescales; comparison of impact zones across pilot mining sites',
    'If threshold exceeded: mountain classification for ecosystem protection becomes valid (irreversible = natural law). If manageable: victims switch to constrained (not trapped), potentially shifting snare perspectives to tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecological_impact_threshold, empirical, 'Ecological damage threshold for deep-sea mining impacts').

omega_variable(
    technological_substitution_rate,
    'Will REE recycling and synthetic substitutes mature fast enough to eliminate deep-sea mining economic rationale before significant extraction occurs?',
    'Tracking recovery rates for REE recycling; R&D timelines for synthetic alternatives (permanent magnets, phosphors); cost curves for recycling vs. virgin extraction',
    'If substitutes mature (10-15 years): scaffold sunset is real, extraction mechanism becomes time-limited. If maturation delayed (25+ years): deep-sea mining becomes permanent extraction constraint, snare classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_substitution_rate, empirical, 'Timeline for REE technological substitution viability').

omega_variable(
    japanese_state_capacity_limitation,
    'Does Japan''s state capacity and capital budget constrain deep-sea mining scale, or is technology transfer to private actors inevitable?',
    'Analysis of JOGMEC (Japan Oil, Gas and Metals National Corporation) funding trajectory; tracking of licensing to private mining companies; comparison with terrestrial mining privatization patterns',
    'If state retains monopoly: extraction remains constrained by Japan''s strategic patience and regulatory alignment. If technology diffuses: extraction scales unpredictably, victims face snare classification across multiple state extractors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(japanese_state_capacity_limitation, empirical, 'Japanese state''s sustained capacity for deep-sea mining monopoly').

omega_variable(
    international_governance_enforcement,
    'Will ISA mining code establish enforceable environmental baselines, or will it become performative theater masking continued extraction?',
    'Monitoring compliance with ISA environmental standards; tracking penalty enforcement and sanctions against violators; analysis of regulatory capture risk from mining interests',
    'If enforcement succeeds: governance bodies'' tangled_rope classification confirmed, extraction is regulated. If theater persists: ISA becomes a piton—its rules are performative, actual constraint is geopolitical power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_governance_enforcement, conceptual, 'ISA governance code enforcement strength').

omega_variable(
    geopolitical_competition_escalation,
    'Does deep-sea mining drive competition for EEZ control that increases military tension, or remains contained within economic/diplomatic channels?',
    'Tracking naval deployments in disputed EEZ areas; analysis of diplomatic incidents related to mining claims; monitoring arms buildup in coastal states',
    'If military escalation: constraint expands beyond economic extraction to security competition (new snare dimension for developing nations). If contained: economic extraction mechanism remains primary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_competition_escalation, empirical, 'Risk of military escalation from deep-sea mining competition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strategic_deep_sea_rare_earth_mining, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsrare_tr_t0, strategic_deep_sea_rare_earth_mining, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dsrare_tr_t5, strategic_deep_sea_rare_earth_mining, theater_ratio, 5, 0.5).
narrative_ontology:measurement(dsrare_tr_t10, strategic_deep_sea_rare_earth_mining, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(dsrare_be_t0, strategic_deep_sea_rare_earth_mining, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dsrare_be_t5, strategic_deep_sea_rare_earth_mining, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(dsrare_be_t10, strategic_deep_sea_rare_earth_mining, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strategic_deep_sea_rare_earth_mining, resource_allocation).
narrative_ontology:affects_constraint(strategic_deep_sea_rare_earth_mining, chinese_rare_earth_monopoly).
narrative_ontology:affects_constraint(strategic_deep_sea_rare_earth_mining, artisanal_mining_labor_extraction).
narrative_ontology:affects_constraint(strategic_deep_sea_rare_earth_mining, environmental_cost_externalization).

% DUAL FORMULATION NOTE:
% Strategic deep-sea mining is downstream of Chinese REE monopoly (which created the supply resilience motivation) and upstream of artisanal mining disruption (technological displacement). The constraint's ε=0.52 reflects the institutional extraction layered onto the coordination function. Upstream constraint (chinese_rare_earth_monopoly) has higher ε (≈0.60, snare) due to monopolistic rent extraction; downstream constraint (artisanal_mining_labor_extraction) has lower ε (≈0.35, tangled_rope) reflecting direct labor but reduced exclusivity as supply diversifies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(strategic_deep_sea_rare_earth_mining, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
