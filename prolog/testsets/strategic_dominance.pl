% ============================================================================
% CONSTRAINT STORY: strategic_dominance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strategic_dominance, []).

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
 *   constraint_id: strategic_dominance
 *   human_readable: Strategic Dominance in Competitive Systems
 *   domain: general/strategic_interaction
 *
 * SUMMARY:
 *   Strategic dominance emerges when one agent in a competitive system
 *   accumulates sufficient power, resources, or control mechanisms to dictate
 *   terms, establish norms, and extract rents from subordinate agents. This
 *   constraint appears across domains: market concentration, organizational
 *   hierarchies, geopolitical spheres of influence, and scientific field
 *   dominance. The mechanism combines genuine coordination (the dominant
 *   agent organizes the field and establishes incentives) with asymmetric
 *   extraction (dominance captures rents and suppresses alternatives). The
 *   constraint exhibits all six classification types depending on
 *   perspective: pure extraction to trapped subordinates (Snare),
 *   coordination to dominant agents (Tangled Rope or Rope), emergent
 *   hierarchy to the analytical observer (Mountain), degraded competition
 *   norms to regulatory systems (Piton), and changeable structure to
 *   organized coalitions (Rope). The measurement data shows escalating
 *   extractiveness and rising theater ratio — the dominance consolidates over
 *   time and increasingly relies on performative mechanisms (regulatory
 *   compliance theater, market legitimacy narratives) rather than functional
 *   necessity.
 *
 * KEY AGENTS:
 *   - Dominant Agent: Primary beneficiary (institutional/arbitrage) — controls resources, information, and norm-setting; extracts rents during dominance window
 *   - Subordinate Agents: Primary victims (powerless/trapped, moderate/constrained) — face resource barriers, information disadvantage, exit costs, and suppression from dominance enforcement
 *   - Competitive Field Integrity: Secondary victim (powerless/trapped) — abstract collective good bearing cost of false competition, market inefficiency, reduced innovation
 *   - Coalition of Subordinates: Organized actors (organized/mobile) — when formed, can convert Snare to Rope through collective exit capacity
 *   - Regulatory Enforcement Apparatus: Institutional actor (institutional/arbitrage) — nominally constrains dominance but increasingly performs rather than enforces
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent strategic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strategic_dominance, 0.68).
domain_priors:suppression_score(strategic_dominance, 0.72).
domain_priors:theater_ratio(strategic_dominance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strategic_dominance, extractiveness, 0.68).
narrative_ontology:constraint_metric(strategic_dominance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(strategic_dominance, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strategic_dominance, snare).
narrative_ontology:human_readable(strategic_dominance, "Strategic Dominance in Competitive Systems").
narrative_ontology:topic_domain(strategic_dominance, "general/strategic_interaction").

domain_priors:requires_active_enforcement(strategic_dominance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strategic_dominance, dominant_agent).
narrative_ontology:constraint_victim(strategic_dominance, subordinate_agents).
narrative_ontology:constraint_victim(strategic_dominance, competitive_field_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE COMPETITOR (SNARE) — Trapped within a competitive field where the dominant agent controls resource access, information asymmetries, and norms. Exit is structurally impossible without abandoning the domain entirely. Experiences maximum extraction and suppression with minimal coordination benefit. The dominant position creates cascading disadvantages: reduced bargaining power, information disadvantage, resource scarcity, norm enforcement against challenge.
constraint_indexing:constraint_classification(strategic_dominance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EMERGING CHALLENGER (SNARE) — High-cost exit from the competitive field through resource investment, time horizon, and reputational establishment. Can perceive the dominant structure and recognize extraction but cannot escape without prohibitive sacrifice. The suppression mechanisms include norm enforcement, resource gatekeeping, and coalitional pressure from the dominant agent's allies.
constraint_indexing:constraint_classification(strategic_dominance, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT AGENT (TANGLED ROPE) — Experiences genuine coordination benefit: their dominance organizes the competitive field, establishes clear hierarchies and incentives that enable market-making, standard-setting, and ecosystem development. Simultaneously extracts through control of key resources, information gating, and norm enforcement. The coordination function is real — the field operates more efficiently under their dominance — but generates asymmetric rents that flow upward. Arbitrage options enable flexible repositioning if dominance is threatened.
constraint_indexing:constraint_classification(strategic_dominance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COALITION OF SUBORDINATES (ROPE) — When subordinate agents organize collectively, the constraint becomes a coordination mechanism with minimal extraction. Coalition formation enables exit from individual dominance through collective action: pool resources, share information, establish counter-norms, and create alternative competitive structures. Perception of the constraint as Rope (changeable, soluble through coordination) emerges when subordinates achieve organization.
constraint_indexing:constraint_classification(strategic_dominance, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY ENFORCEMENT APPARATUS (PITON) — Antitrust, competition, and regulatory frameworks are nominally designed to prevent strategic dominance from crystallizing into permanent extraction. Yet these regulatory mechanisms are increasingly performative: they enforce cosmetic changes (behavioral remedies, divestiture theater) while leaving the structural dominance intact. The apparatus maintains the fiction of competitive markets while lacking enforcement power or willingness to challenge genuine dominance. Theater ratio reflects gap between regulatory mandate and actual outcome.
constraint_indexing:constraint_classification(strategic_dominance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, strategic dominance appears as an immutable property of competitive systems: in any multi-agent competitive environment, some agents will accumulate power, establish hierarchies, and extract rents from subordinates. This is presented as inherent to competition itself — hierarchy and extraction are seen as natural laws of strategic interaction. However, this perspective naturalizes what may be a contingent institutional arrangement, risking false summit classification.
constraint_indexing:constraint_classification(strategic_dominance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strategic_dominance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strategic_dominance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strategic_dominance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strategic_dominance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(strategic_dominance, TR),
    TR >= 0.70.

:- end_tests(strategic_dominance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, escalating. The dominant agent captures the majority of competitive value through control mechanisms, information asymmetries, and norm enforcement. The escalation from 0.42 to 0.68 over the measurement interval reflects dominance consolidation — initial extraction is moderate (dominance must still prove its coordination value to maintain legitimacy), but as subordinates become locked in, extraction increases without risking exit because subordinates have no alternative. Theater ratio (0.58): Moderate-high, escalating. Early dominance relies on functional efficiency and genuine coordination. As dominance matures and becomes consolidated, it increasingly relies on narratives (market leadership, innovation, industry standards) that justify dominance through legitimacy theater rather than functional necessity. The rise from 0.35 to 0.58 reflects transition from functional to performative dominance. Suppression (0.72): High and relatively stable. Strategic dominance suppresses through multiple mechanisms: resource control (material barriers), norm enforcement (social barriers — competitors who challenge dominance face retaliation from allies), information asymmetry (dominant agent controls data and standard-setting), and identity capture (subordinate agents internalize inferiority narratives). The suppression is not rising because dominance establishes suppression mechanisms early and maintains them; they do not need to increase because the initial level is sufficient.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full classification range from identical base properties. The dominant agent experiences Tangled Rope or Rope — genuine coordination combined with justified rents. Subordinate agents experience Snare or Tangled Rope — extraction with minimal coordination benefit from their perspective. Organized coalitions experience Rope — the constraint becomes solvable through collective action. The regulatory apparatus experiences Piton — the regulatory enforcement mechanisms are increasingly performative, maintaining the fiction of competitive markets without functional force. The civilizational analytical observer risks Mountain classification — treating strategic dominance as an inevitable feature of competitive systems rather than as a contingent institutional outcome shaped by specific historical, legal, and power structures. The gap between the analytical false summit and the structural data is the core of the mandatrophy resolution.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position and exit capacity. Dominant agents with arbitrage options experience low or negative directionality (d ≈ 0.15-0.25) — they are beneficiaries and can reposition. Subordinate agents with trapped exit experience high directionality (d ≈ 0.90-0.98) — they bear maximum extraction. Moderate agents with constrained exit experience medium directionality (d ≈ 0.60-0.75) — they can perceive the extraction clearly but cannot escape without significant cost. Organized agents with mobile exit experience low-to-moderate directionality (d ≈ 0.35-0.50) — collective action reduces individual extraction through exit capacity. The regulatory apparatus with arbitrage options derives low directionality but fails the functional gate because its enforcement power is attenuated by capture or resource constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: Strategic dominance demonstrates the mandatrophy resolution through perspectival decomposition. The false natural law ('dominance is inevitable in competition') is revealed as institutional by showing that the same constraint classifies as Snare (extractive, contingent, solvable) from the subordinate perspective, Tangled Rope (mixed coordination-extraction, institutional) from the dominant perspective, and Rope (coordination, changeable) from the organized coalition perspective. No single type is 'true' — the presheaf of perspectives IS the truth. The dominance appears inevitable (Mountain) only when observing from a civilizational distance that abstracts away the specific power structures, norms, and coalitional dynamics that maintain it. Close observation from structural positions reveals it as an institutional constraint that can be reformed, dissolved, or redistributed depending on coalition strength and strategic intervention. The mandatrophy resolves by showing that the analytical observer's 'natural law' perspective is actually a high-abstraction perspective that misses the contingency visible at lower abstraction levels.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dominance_structural_vs_exploitative,
    'Is the observed dominance a structural equilibrium outcome of fair competition or an exploitative lock-in mechanism preventing legitimate competitors from entering?',
    'Historical analysis of competitive entry patterns; measurement of switching costs and barriers to entry; comparison of dominance distribution across similar competitive domains; analysis of whether dominance persists despite lower-cost alternatives existing',
    'If structural equilibrium: classification shifts toward Rope (coordination through dominance). If exploitative lock-in: classification strengthens toward Snare (pure extraction). Determines whether therapeutic intervention (coalition building) or structural reform (market restructuring) is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominance_structural_vs_exploitative, empirical, 'Whether dominance is structural outcome or exploitative lock-in').

omega_variable(
    suppression_mechanism_type,
    'Is suppression achieved through resource control (material barriers), norm enforcement (social barriers), information asymmetry (epistemic barriers), or internalized identity capture (cognitive barriers)?',
    'Analysis of exit cost composition: proportion attributable to material loss, reputation damage, identity dissolution, information disadvantage. Intervention experiment: remove one suppression type and measure remaining barriers.',
    'Purely material suppression can be addressed through direct resource redistribution or competitive deregulation. Norm-enforced or identity-captured suppression requires cognitive reframing and coalition-building. Misdiagnosis leads to interventions that address only surface barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_type, empirical, 'Composition of suppression mechanisms').

omega_variable(
    coordination_function_necessity,
    'Does the dominant agent''s coordinative function genuinely require their dominance, or is dominance orthogonal to the coordination it claims to provide?',
    'Comparison of coordination outcomes in systems with distributed power vs concentrated dominance; measurement of coordination efficiency gains attributable specifically to dominance; analysis of whether alternative coordination mechanisms achieve similar outcomes',
    'If coordination requires dominance: classification toward Tangled Rope (hybrid coordination-extraction). If coordination is orthogonal to dominance: classification toward Snare (pure extraction masquerading as coordination). Determines whether competitive decentralization destroys coordination or merely redistributes rent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether dominance is necessary for claimed coordination function').

omega_variable(
    coalition_formation_threshold,
    'What is the critical mass of organized subordinate agents required to convert Snare perception to Rope perception and achieve genuine exit capacity?',
    'Historical analysis of successful coalition movements against dominance; measurement of subordinate agent count, resource pooling, and norm-coordination required for successful challenge; comparison across domains with different dominance structures',
    'If threshold is low (small coalitions succeed): Rope perspective becomes accessible quickly. If threshold is high (large coalitions required): Snare locks in longer. Determines practical window for intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_formation_threshold, empirical, 'Critical mass for subordinate agent coalition formation').

omega_variable(
    natural_law_false_summit,
    'Is the mountain classification (strategic dominance as inherent to competition) a genuine natural law or a false summit naturalizing contingent institutional arrangements?',
    'Analysis of competitive systems without dominance (cooperative markets, highly fragmented industries, decentralized networks); comparison of dominance distribution across regulatory regimes; measurement of whether dominance emerges from free competition or from enforcement/norm-locking',
    'If genuine natural law: some degree of dominance is unavoidable. If false summit: dominance is policy-dependent and addressable through institutional redesign. Highest-stakes omega for strategic intervention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_false_summit, conceptual, 'Whether strategic dominance is natural law or contingent institution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strategic_dominance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stra_tr_t0, strategic_dominance, theater_ratio, 0, 0.35).
narrative_ontology:measurement(stra_tr_t5, strategic_dominance, theater_ratio, 5, 0.47).
narrative_ontology:measurement(stra_tr_t10, strategic_dominance, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(stra_be_t0, strategic_dominance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stra_be_t5, strategic_dominance, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(stra_be_t10, strategic_dominance, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strategic_dominance, resource_allocation).
narrative_ontology:affects_constraint(strategic_dominance, information_asymmetry_in_competition).
narrative_ontology:affects_constraint(strategic_dominance, norm_enforcement_mechanisms).
narrative_ontology:affects_constraint(strategic_dominance, resource_scarcity_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(strategic_dominance, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
