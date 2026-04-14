% ============================================================================
% CONSTRAINT STORY: incumbent_moat_entrenchment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incumbent_moat_entrenchment, []).

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
 *   constraint_id: incumbent_moat_entrenchment
 *   human_readable: Incumbent Moat Entrenchment
 *   domain: economic_structure/competition_dynamics
 *
 * SUMMARY:
 *   Incumbent moat entrenchment represents a fundamental tension in market
 *   capitalism: the same mechanisms that incentivize long-term investment and
 *   quality provision also enable rent extraction through reduced
 *   competition. A moat is a structural advantage that makes it costly or
 *   impossible for competitors to enter or gain market share. These can be
 *   economic (brand loyalty, switching costs, network effects, scale
 *   advantages, proprietary technology), regulatory (licenses, patents,
 *   exclusive contracts), or institutional (supplier relationships, ecosystem
 *   lock-in). The constraint is tangled because all durable moats serve both
 *   a coordination function (they enable firms to invest in quality,
 *   infrastructure, and long-term relationships) and an extraction function
 *   (they allow incumbent to charge prices above competitive levels and
 *   prevent better alternatives from entering). Extractiveness has increased
 *   over the measurement interval as digital platforms have enabled network
 *   effects and data advantages to create nearly impenetrable moats. Theater
 *   ratio remains moderate because moat entrenchment is structurally evident
 *   (entry barriers are visible), though firms engage in substantial
 *   narrative work ('innovation advantage,' 'customer preference') to
 *   legitimize what are often regulatory or network-effect barriers.
 *
 * KEY AGENTS:
 *   - Incumbent Firm: Primary beneficiary (institutional/arbitrage) — captures monopoly rents, long-term investment security, and ecosystem control
 *   - Potential Entrants: Primary victims (powerless/trapped) — face insurmountable barriers; entry is blocked or prohibitively costly
 *   - Niche Competitors: Secondary victims (moderate/constrained) — can survive in underserved segments but face constant extraction pressure and threat of predatory incumbent response
 *   - Downstream Ecosystem: Organized agents (organized/constrained) — suppliers, complementors, and customers benefit from ecosystem stability but experience extraction through bargaining power asymmetries
 *   - Consumers: Implicit victims (powerless/trapped in some markets) — reduced choice, higher prices, slower innovation than competitive counterfactual
 *   - Regulatory Authorities: Organized interveners (organized/constrained) — attempt to enforce competition through antitrust, interoperability requirements, and license conditions with sunset logic
 *   - Analytical Observer: Meta-perspective (analytical/analytical) — sees the tangled structure: moats are simultaneously legitimate (incentive alignment) and illegitimate (rent extraction)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incumbent_moat_entrenchment, 0.58).
domain_priors:suppression_score(incumbent_moat_entrenchment, 0.62).
domain_priors:theater_ratio(incumbent_moat_entrenchment, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incumbent_moat_entrenchment, extractiveness, 0.58).
narrative_ontology:constraint_metric(incumbent_moat_entrenchment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(incumbent_moat_entrenchment, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incumbent_moat_entrenchment, tangled_rope).
narrative_ontology:human_readable(incumbent_moat_entrenchment, "Incumbent Moat Entrenchment").
narrative_ontology:topic_domain(incumbent_moat_entrenchment, "economic_structure/competition_dynamics").

domain_priors:requires_active_enforcement(incumbent_moat_entrenchment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incumbent_moat_entrenchment, incumbent_firm).
narrative_ontology:constraint_victim(incumbent_moat_entrenchment, potential_entrants).
narrative_ontology:constraint_victim(incumbent_moat_entrenchment, consumer_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POTENTIAL ENTRANT (SNARE) — New firms face insurmountable barriers: incumbent's brand loyalty, switching costs, network effects, exclusive supplier contracts, regulatory capture, and capital requirements. The entrant cannot exit — entering the market IS the goal, but the moat makes entry prohibitively costly or impossible. Maximum extraction experienced: all upfront investment bears no return.
constraint_indexing:constraint_classification(incumbent_moat_entrenchment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NICHE COMPETITOR (TANGLED ROPE) — Mid-market or specialized entrant can survive by serving underserved segments, but faces extraction: incumbent threatens to drop price, replicate features, or use predatory tactics against niche segments. Coordination function exists (both firms benefit from market maturation, innovation ecosystems), but extraction is embedded: the niche competitor must constantly innovate to avoid head-to-head competition where the moat is impenetrable.
constraint_indexing:constraint_classification(incumbent_moat_entrenchment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FIRM (ROPE) — Experiences the moat as coordination: it enables long-term investment in brand, infrastructure, and ecosystem partnerships. The moat coordinates intertemporal incentives (firms can invest in quality knowing they won't be immediately undercut). The incumbent benefits from the structure but also sees itself as solving a market problem: without moats, firms would under-invest in quality or customer service.
constraint_indexing:constraint_classification(incumbent_moat_entrenchment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOWNSTREAM ECOSYSTEM (TANGLED ROPE) — Suppliers, complementors, and customers organize around the incumbent. Genuine coordination function: the ecosystem benefits from the incumbent's stability and platform investment. Extraction embedded: the incumbent extracts rents from suppliers through bargaining power, from complementors through platform control, and from consumers through reduced competition. Organized agents (trade associations, supplier networks) push back, creating hybrid dynamics.
constraint_indexing:constraint_classification(incumbent_moat_entrenchment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY INTERVENTION (SCAFFOLD) — Antitrust enforcement, open-access mandates, or interoperability requirements temporarily reduce the moat's effectiveness. These interventions are scaffolds with sunset clauses (often 5-10 years): patents expire, standards mature, new technologies disrupt the moat. The scaffold perspective sees moat entrenchment as a temporary problem solvable through time-limited enforcement. High suppression is tolerated because the intervention is designed to fade once structural conditions change.
constraint_indexing:constraint_classification(incumbent_moat_entrenchment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY COMPETITIVE ADVANTAGE (PITON) — Entrenchment mechanisms that once provided genuine competitive advantage (proprietary technology, brand trust, supply chain integration) become vestigial as industries mature or technology shifts. The moat persists through institutional inertia, marketing theater, and lock-in from installed base — but the functional advantage has degraded. A legacy moat in a disrupted industry is piton: maintained through theater and habit, not function.
constraint_indexing:constraint_classification(incumbent_moat_entrenchment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical view, moat entrenchment is a genuine hybrid: it serves legitimate coordination functions (incentivizing quality investment, enabling ecosystem stability) while simultaneously extracting rents through reduced competition. Neither pure extraction nor pure coordination — the structure is intrinsically tangled. The analytical perspective identifies mandatrophy: the tension between moat-as-coordination and moat-as-extraction is the actual structure of capitalist competition.
constraint_indexing:constraint_classification(incumbent_moat_entrenchment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incumbent_moat_entrenchment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incumbent_moat_entrenchment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incumbent_moat_entrenchment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incumbent_moat_entrenchment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incumbent_moat_entrenchment, TR),
    TR >= 0.70.

:- end_tests(incumbent_moat_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Incumbent captures monopoly rents and prevents entry, but the extraction is not total — potential entrants retain the option to enter at high cost (they are constrained, not trapped in the ultimate sense), and ecosystem partners benefit from some coordination. The value reflects the hybrid nature: genuine competitive advantage provides value, but incumbent extracts significantly more than would be possible in a competitive market. Suppression (0.62): High. Barriers to entry include capital requirements, switching costs, network effects, regulatory licensing, exclusive contracts, and customer lock-in. These are substantial but not absolute — the suppression is engineered, not immutable. Theater ratio (0.48): Moderate. Moat entrenchment is structurally visible (entry barriers are manifest), but firms conduct substantial narrative work presenting moats as innovations or customer preferences rather than regulatory/network barriers. Measurement trajectory shows entrenchment increasing over time as digital platforms amplify network effects and data advantages.
 *
 * PERSPECTIVAL GAP:
 *   The potential entrant sees a Snare — they face an impossible choice between not entering and entering at prohibitive cost with minimal return. The niche competitor sees Tangled Rope — they can survive by specializing but must innovate constantly to avoid head-to-head competition where the moat is impenetrable. The incumbent sees Rope — moats enable investment and ecosystem coordination. The organized ecosystem sees Tangled Rope — they benefit from platform stability but experience extraction through bargaining power. Regulatory intervention sees a Scaffold — antitrust and interoperability requirements are temporary fixes designed to fade as technology evolves. Legacy moats see a Piton — formerly functional advantages (proprietary tech, trusted brand) persist through inertia and marketing theater as the underlying functional advantage erodes. The analytical perspective identifies the true structure: Tangled Rope. Mandatrophy resolution: the tension between moat-as-coordination and moat-as-extraction IS the structure. Neither pure type is correct — the presheaf over different agent perspectives reveals that moat entrenchment intrinsically contains both functions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to extraction flow. The incumbent (institutional/arbitrage) experiences low d because they are the beneficiary with exit options (they can exit by selling, diversifying, or adapting). Potential entrants (powerless/trapped) experience high d because they are victims with no exit (entry is the goal, but the moat blocks it). Niche competitors (moderate/constrained) experience moderate d — they are victims but have partial exit (specialize further, serve different geography, accept lower margins). The ecosystem (organized/constrained) experiences moderate d — they benefit from coordination but experience extraction through bargaining power asymmetries. Regulatory intervention (organized/constrained) experiences moderate d from the incumbent's perspective (enforcement constrains the incumbent but doesn't eliminate the moat). The analytical observer (analytical/analytical) experiences moderate d because they see both costs and benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves mandatrophy by identifying the genuine hybrid structure. The incumbent's perspective (Rope) is not wrong — moats do enable quality investment and ecosystem coordination. The potential entrant's perspective (Snare) is not wrong — entry is structurally blocked. The scaffold perspective is not wrong — regulation can temporarily reduce moat effectiveness. The piton perspective is not wrong — legacy moats often persist through theater after functional advantage erodes. All are correct from their structural positions. The constraint is NOT 'which type is really correct?' but 'what structure generates these different perspectives?' Answer: Tangled Rope. The moat serves legitimate coordination (intertemporal incentives, ecosystem stability) while enabling extraction (monopoly rents, blocked entry). The analytical perspective should classify as Tangled Rope, not attempt to collapse into a single type. The mandatrophy is resolved by recognizing that the tension between coordination and extraction is the actual constraint structure, not a problem to be solved by choosing one side.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moat_legitimacy_threshold,
    'At what point does a competitive advantage cease being legitimate (rewards quality/efficiency) and become illegitimate (pure rent extraction)?',
    'Empirical test: pricing relative to marginal cost; rate of innovation; consumer surplus loss; historical counterfactual (what would prices/innovation be in competitive market?)',
    'If threshold is near moat formation: most entrenchment is illegitimate extraction (classification shifts toward Snare). If threshold is far: most entrenchment provides genuine coordination benefit (classification shifts toward Rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moat_legitimacy_threshold, preference, 'Threshold for distinguishing legitimate advantage from pure rent extraction').

omega_variable(
    technological_disruption_inevitability,
    'Is the moat''s sunset guaranteed by technological change, or can entrenched incumbents adapt to maintain moats across generations?',
    'Historical case studies: telecommunications (Bell System → post-divestiture → mobile), retail (Sears → Walmart → Amazon), search (AltaVista → Google → ?). Measure moat persistence across major technological transitions.',
    'If disruption is inevitable: moat entrenchment is temporary (Scaffold from most perspectives). If incumbents can maintain moats across transitions: entrenchment is structural (Snare or Tangled Rope persist).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_disruption_inevitability, empirical, 'Whether technological change guarantees moat disruption').

omega_variable(
    regulation_capture_feedback,
    'Does regulatory intervention to reduce moats strengthen or weaken the incumbent''s incentive to capture the regulator?',
    'Historical analysis of post-intervention incumbent behavior; measurement of regulatory capture intensity after antitrust enforcement; correlation between enforcement stringency and lobbying intensity',
    'If enforcement strengthens capture incentives: intervention creates meta-extraction (the moat shifts from economic to regulatory). If enforcement weakens capture: scaffold perspective is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulation_capture_feedback, empirical, 'Whether regulation strengthens incumbent''s capture incentives').

omega_variable(
    multi_sided_platform_moat_ambiguity,
    'In multi-sided platforms (network effects on both sides), is the moat a genuine coordination mechanism for both sides or extraction by the platform from one side or both?',
    'Price elasticity analysis by side; comparison of pricing to marginal cost by side; measurement of cross-subsidies; counterfactual analysis of what would happen without network effects',
    'If network effects genuinely benefit both sides: moat provides Rope-level coordination (both sides better off than without network). If one side bears the extraction: Tangled Rope or Snare depending on that side''s power and exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_sided_platform_moat_ambiguity, empirical, 'Whether multi-sided platform moats coordinate or extract').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incumbent_moat_entrenchment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moat_tr_t0, incumbent_moat_entrenchment, theater_ratio, 0, 0.32).
narrative_ontology:measurement(moat_tr_t5, incumbent_moat_entrenchment, theater_ratio, 5, 0.4).
narrative_ontology:measurement(moat_tr_t10, incumbent_moat_entrenchment, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(moat_be_t0, incumbent_moat_entrenchment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(moat_be_t5, incumbent_moat_entrenchment, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(moat_be_t10, incumbent_moat_entrenchment, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incumbent_moat_entrenchment, resource_allocation).
narrative_ontology:affects_constraint(incumbent_moat_entrenchment, network_effects_lock_in).
narrative_ontology:affects_constraint(incumbent_moat_entrenchment, switching_cost_accumulation).
narrative_ontology:affects_constraint(incumbent_moat_entrenchment, regulatory_capture_dynamics).

% DUAL FORMULATION NOTE:
% Incumbent moat entrenchment is upstream of several specific manifestations: network effects (digital platforms), switching cost dynamics (customer lock-in), and regulatory capture (barriers defended through regulation rather than economics). Each downstream constraint has its own ε value reflecting the specific mechanism; the parent constraint represents the general structural phenomenon. Decomposition: moat entrenchment (this story, ε=0.58) → network_effects_lock_in (ε=0.68, Snare for non-platforms) → switching_cost_accumulation (ε=0.52, Tangled Rope for mature markets).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(incumbent_moat_entrenchment, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
