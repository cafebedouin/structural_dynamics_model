% ============================================================================
% CONSTRAINT STORY: trade_secret_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trade_secret_law, []).

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
 *   constraint_id: trade_secret_law
 *   human_readable: Trade Secret Law (Information Ownership)
 *   domain: legal/economic
 *
 * SUMMARY:
 *   Trade secret law creates a structural tension between protecting
 *   investment incentives (coordination function) and enabling labor
 *   mobility, competitive entry, and cumulative innovation (extraction
 *   costs). The constraint operates across six distinct perspectives,
 *   revealing how the same legal mechanism produces fundamentally different
 *   experiences depending on structural position. Incumbent enterprises
 *   benefit from information control and experience the law as pure
 *   coordination. Departing employees face labor imprisonment through
 *   non-compete enforcement and trade secret litigation threats, experiencing
 *   maximum extraction. The startup ecosystem experiences mixed coordination
 *   (boundaries matter for competition) and extraction (barriers to entry
 *   require costly independent development). The cumulative innovation
 *   ecosystem experiences delayed access to intermediate knowledge, which
 *   slows industry-wide innovation while rewarding first-movers. Trade secret
 *   litigation is substantially performative — settlement and threat-based
 *   compliance dominate adjudicated cases, with actual damages awards
 *   remaining rare and calculable damages nearly impossible to establish. The
 *   constraint's extractiveness has increased over the 100-year interval as
 *   digital technologies, remote work, and cross-border employment have
 *   increased both the value of information control and the difficulty of
 *   maintaining secrets, leading to escalated enforcement theater (litigation
 *   threats, broad non-competes, aggressive discovery disputes).
 *
 * KEY AGENTS:
 *   - Incumbent Enterprises: Primary beneficiary (institutional/arbitrage) — captures information monopoly premium and R&D reward; shapes competitive boundaries
 *   - Departing Employees: Primary victim (powerless/trapped) — cannot exit trade secret constraints; faces career destruction from litigation risk and non-compete enforcement
 *   - Startup Founders: Secondary victim (moderate/constrained) — face barriers to enter incumbent perimeters; also benefit from protecting own trade secrets
 *   - Cumulative Innovation Ecosystem: Organized victim (organized/mobile) — delayed access to intermediate knowledge; higher costs for independent discovery; chilling effects on knowledge sharing
 *   - Legal Enforcement Apparatus: Institutional actor (institutional/arbitrage) — maintains performative trade secret litigation; high cost asymmetry favors incumbents; actual damages rarely awarded
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent property law as inherent market feature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trade_secret_law, 0.52).
domain_priors:suppression_score(trade_secret_law, 0.65).
domain_priors:theater_ratio(trade_secret_law, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trade_secret_law, extractiveness, 0.52).
narrative_ontology:constraint_metric(trade_secret_law, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(trade_secret_law, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trade_secret_law, tangled_rope).
narrative_ontology:human_readable(trade_secret_law, "Trade Secret Law (Information Ownership)").
narrative_ontology:topic_domain(trade_secret_law, "legal/economic").

domain_priors:requires_active_enforcement(trade_secret_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trade_secret_law, incumbent_enterprises).
narrative_ontology:constraint_beneficiary(trade_secret_law, knowledge_capital_holders).
narrative_ontology:constraint_victim(trade_secret_law, labor_mobility).
narrative_ontology:constraint_victim(trade_secret_law, competitive_entry).
narrative_ontology:constraint_victim(trade_secret_law, cumulative_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPARTING EMPLOYEE (SNARE) — Worker leaving an incumbent firm cannot exit trade secret constraints. Carrying experiential knowledge to a new employer or startup triggers injunctive relief, liquidated damages, and career destruction. Maximum suppression: non-compete agreements, trade secret litigation threat, and knowledge compartmentalization create de facto labor imprisonment. The constraint extracts by preventing talent mobility and wage competition.
constraint_indexing:constraint_classification(trade_secret_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STARTUP FOUNDER (TANGLED ROPE) — Faces significant barriers to exit incumbent trade secret perimeters but has some agency through clean-team development, independent discovery, and reverse engineering. Benefits from trade secret protection for their own information but also constrained by existing trade secrets they may encounter. Moderate suppression through discovery litigation risk and patent thickets. Mixed coordination (defines competitive boundaries) and extraction (raises barriers to entry).
constraint_indexing:constraint_classification(trade_secret_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT ENTERPRISE (ROPE) — Primary beneficiary with high exit options and arbitrage capacity. Trade secret law coordinates information governance and enables R&D investment by protecting market advantage. Experiences the constraint as pure coordination: defining what is confidential, establishing legitimate competitive boundaries, rewarding first-mover innovation. Net beneficiary with agency to shape competitive rules.
constraint_indexing:constraint_classification(trade_secret_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CUMULATIVE INNOVATION ECOSYSTEM (TANGLED ROPE) — Organized agents (startups, researchers, adjacent industries) experience trade secret law as mixed: coordination function (establishes boundaries, rewards investment) but also extraction through delayed access to intermediate knowledge, higher development costs to achieve independent discovery, and chilling effects on knowledge sharing. Benefits from innovation incentives but constrained by information silos. Has exit options (reverse engineering, clean-team development) but at significant cost.
constraint_indexing:constraint_classification(trade_secret_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGAL ENFORCEMENT APPARATUS (PITON) — Trade secret litigation is substantially performative: settlements and threat-based compliance dominate actual adjudicated cases. The system maintains theatrical enforcement (cease-and-desist letters, discovery disputes, threat of damages) but actual damages awards are rare and difficult to calculate. Enforcement persists through institutional inertia and cost asymmetry (incumbent defense budgets) despite low actual functional verification of misappropriation. The legal theater increases compliance cost without proportional harm prevention.
constraint_indexing:constraint_classification(trade_secret_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some information control is inherent to competitive advantage: the gap between knowledge and its public availability is a structural feature of how markets function. This perspective sees trade secret law as reflecting an immutable asymmetry — information asymmetry is a law of economics, not a contingent legal choice. However, the structural data contradicts this: the extraction values (0.52), suppression levels (0.65), and beneficiary/victim declarations reveal that trade secret law is a contingent institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(trade_secret_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trade_secret_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trade_secret_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trade_secret_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trade_secret_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trade_secret_law, TR),
    TR >= 0.70.

:- end_tests(trade_secret_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Trade secret law extracts value from employees (through labor mobility restrictions), competitors (through information barriers), and the cumulative innovation system (through delayed access to intermediate knowledge). However, extraction is not maximal because: (a) information decay occurs through technological obsolescence and inevitable independent discovery; (b) reverse engineering and clean-team development provide exit options; (c) some benefits flow to the broader innovation ecosystem through R&D investment incentives. The rising trend (0.35 → 0.52 over interval) reflects intensified enforcement and digital scale effects that have increased both secret value and enforcement complexity. Suppression (0.65): Moderate-high. Barriers to exit trade secret perimeters are substantial — non-compete agreements, broad injunctive relief, litigation cost asymmetry, and reputational damage from trade secret litigation create significant suppression. However, suppression is not total because clean-team development, reverse engineering, and the mobility of knowledge through supply chains and published research provide real (if expensive) alternatives. Theater ratio (0.58): Moderate. Trade secret litigation contains significant performative elements (threat-based settlement, discovery disputes focused on cost escalation) but actual verification of misappropriation is court-adjudicated and focused on tangible harms. The theater has increased as enforcement has become more aggressive despite low actual damages awards.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. The incumbent enterprise sees coordination (Rope) — trade secret law enables competitive boundaries and R&D investment. The departing employee sees extraction machinery (Snare) — legal threats and labor restrictions block mobility. The startup ecosystem sees mixed extraction and coordination (Tangled Rope) — boundaries enable competition but also raise entry costs. The cumulative innovation system sees delayed access (Tangled Rope) — both enabled and constrained. The legal system sees its own performative mechanism (Piton) — enforcing trade secrets through theater (discovery disputes, settlement pressure) that costs all parties without proportional verification. The analytical observer risks seeing natural economic law (Mountain) — information asymmetry inherent to markets — but the structural data reveals this is contingent law choice, not immutable economics. The perspectival gaps widen when considering labor mobility restrictions: are non-competes part of legitimate trade secret protection (incumbent/enterprise view) or labor imprisonment (employee view)?
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position in the extraction flow. Incumbent enterprises (beneficiaries with arbitrage options) experience low d — they set the rules and can opt out of trade secret constraints through disclosure if strategically preferable. Departing employees (trapped victims with no exit) experience high d — they cannot avoid knowledge restrictions without career destruction. Startup founders and the innovation ecosystem (moderate victims with constrained/mobile exits) experience moderate d — they have alternatives (reverse engineering, independent discovery, clean teams) but at significant cost, giving them some agency but substantial extraction experience. The legal system itself (institutional with arbitrage) experiences low d as beneficiary — it enforces on behalf of incumbents and can shape doctrine. The analytical observer at civilizational scale experiences d around 0.72 as observer (neither beneficiary nor victim but seeing full structure).
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL HYBRID: Trade secret law resolves mandatrophy by exhibiting genuine coordination function (enabling R&D investment, establishing competitive boundaries) alongside genuine extraction (restricting labor mobility, creating information barriers, delaying innovation diffusion). The coordination function is real — firms do invest more when confident of information control, and competitive boundaries do enable market function. The extraction is also real — employees cannot leave without career destruction, startups face barriers to entry, and the cumulative innovation system pays higher development costs. Neither function subsumes the other; both exist simultaneously. This is the canonical Tangled Rope: requires active enforcement (true), produces both coordination benefit (true) and asymmetric extraction (true), with beneficiaries (incumbents) and victims (employees, startups, cumulative system) occupying different structural positions. The mandatrophy is resolved by accepting that trade secret law simultaneously serves legitimate coordination and functions as extraction mechanism — the policy question is not which is 'real' but how to structure the law to maximize coordination while minimizing extraction (through shorter secret duration, stronger reverse-engineering rights, cleaner exit options, and innovation-friendly safe harbors).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    independent_discovery_boundary,
    'What constitutes genuine independent discovery versus inevitable derivation from the same underlying facts or principles?',
    'Comparative analysis of clean-team development costs vs. incumbent R&D costs; empirical tracking of whether independent discoveries cluster around same time intervals',
    'If independent discovery is rare/expensive: trade secret law functions as near-total extraction. If independent discovery is frequent: law functions primarily as coordination with moderate extraction overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independent_discovery_boundary, empirical, 'Whether independent discovery is genuinely possible or extraction is near-total').

omega_variable(
    information_decay_rate,
    'How quickly do trade secrets lose competitive value through technological change, employee turnover, and independent discovery?',
    'Historical analysis of trade secret litigation cases: ratio of cases won to cases filed; longitudinal tracking of secret value decay; comparison with patent protection duration effectiveness',
    'If decay is rapid (2-3 years): extraction period is naturally limited, scaffold-like sunset occurs organically. If decay is slow (10+ years): extraction is sustainable and snare-like long-term.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_decay_rate, empirical, 'Rate at which trade secrets lose competitive value').

omega_variable(
    knowledge_leakage_through_supply_chains,
    'Do trade secret barriers actually prevent innovation diffusion, or does knowledge transfer occur inevitably through supplier relationships, customer interfaces, and published research?',
    'Analysis of innovation timelines in industries with vs. without strong trade secret enforcement; tracking of how publicly available patent disclosures correlate with private trade secret innovation; supplier network analysis for knowledge flow patterns',
    'If leakage is inevitable: trade secret law is primarily theater (Piton). If barriers are effective: law functions as real extraction mechanism (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_leakage_through_supply_chains, empirical, 'Whether trade secret barriers actually prevent knowledge diffusion through supply chains').

omega_variable(
    startup_ecosystem_dependency,
    'Do startup formation rates and industry innovation actually depend on robust trade secret protection, or on other factors (venture capital, talent availability, market demand)?',
    'Comparative analysis of startup formation rates in jurisdictions with strong vs. weak trade secret enforcement; longitudinal tracking of innovation metrics pre/post trade secret law changes',
    'If dependent: trade secret law''s coordination function is genuine (Rope/Tangled Rope justified). If independent: law extracts without meaningful coordination benefit (Snare/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(startup_ecosystem_dependency, empirical, 'Whether innovation depends on strong trade secret enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trade_secret_law, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ts_tr_t0, trade_secret_law, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ts_tr_t50, trade_secret_law, theater_ratio, 50, 0.51).
narrative_ontology:measurement(ts_tr_t100, trade_secret_law, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(ts_be_t0, trade_secret_law, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ts_be_t50, trade_secret_law, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(ts_be_t100, trade_secret_law, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trade_secret_law, enforcement_mechanism).
narrative_ontology:affects_constraint(trade_secret_law, non_compete_agreements).
narrative_ontology:affects_constraint(trade_secret_law, patent_disclosure_tensions).
narrative_ontology:affects_constraint(trade_secret_law, innovation_ecosystem_velocity).

% DUAL FORMULATION NOTE:
% Trade secret law decomposes into two distinct structural claims: (1) the coordination function (legitimate information protection enabling R&D investment, ε ≈ 0.08, Mountain or Rope), and (2) the extraction mechanism (labor mobility restrictions and competitive barriers, ε ≈ 0.62, Snare/Tangled Rope). These are linked through enforcement — the same legal doctrine serves both functions, but their structural properties differ. The overall story presented here models the hybrid (ε = 0.52, Tangled Rope) because the enforcement mechanism unifies them. See downstream constraints for decomposition into pure coordination and pure extraction narratives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trade_secret_law, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
