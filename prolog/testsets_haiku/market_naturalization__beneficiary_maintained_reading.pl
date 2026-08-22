% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__beneficiary_maintained_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Dominance as Actively Maintained Incumbent Protection
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'market_naturalization': the reading that market dominance is actively
 *   defended by incumbent capital holders. The kernel admits multiple
 *   readings because 'market dominance' can be interpreted as (1) the result
 *   of ongoing incumbent defense (this reading), (2) the result of lapsed
 *   structural closure now merely inherited (lapsed_alternative_reading), or
 *   (3) a mixture of active and passive mechanisms (hybrid_reading). This
 *   story generates the constraint as structured under the
 *   beneficiary-maintained reading: high extractiveness, active suppression
 *   of alternatives, identifiable beneficiary class (incumbent capital
 *   holders and the apparatus that serves them), and victims (potential
 *   entrants, displaced workers, subordinated capital). The author commits to
 *   this reading as structurally true while acknowledging omegas that
 *   document the sibling readings and the irreducible framing variance. The
 *   kernel itself is a site of genuine contestation in political economy,
 *   institutional theory, and business analysis — no reading is marginal or
 *   fringe.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders: institutional power; beneficiary/agenda-setter; arbitrage-level exit because their wealth IS their market position
 *   - potential_market_entrants: powerless; payer/victim; trapped by patent thickets, regulatory barriers, capital requirements, and incumbent litigation
 *   - displaced_labor_cohorts: organized but moderate; payer/victim; constrained exit because alternative sectors are occupied or foreclosed
 *   - regulatory_capture_agents: institutional power; agenda-setter; identity-locked to incumbent-serving interpretation of law and regulation
 *   - subordinated_capital_holders: moderate power; payer/victim; their capital returns are suppressed by incumbent price-setting
 *   - consumer_coalitions: powerless; payer/excluded; identity-locked to branded incumbents (switching costs are often cognitive/identity costs, not just economic)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.78).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.81).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Dominance as Actively Maintained Incumbent Protection").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, '47048e61-537e-459c-ab74-4dd12188ff80').
narrative_ontology:cs_kernel_codification('47048e61-537e-459c-ab74-4dd12188ff80', formalized).
narrative_ontology:cs_authority_grounding('47048e61-537e-459c-ab74-4dd12188ff80', extraction).
narrative_ontology:cs_interpretation_layer_present('47048e61-537e-459c-ab74-4dd12188ff80').
narrative_ontology:cs_reading_relation('47048e61-537e-459c-ab74-4dd12188ff80', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('47048e61-537e-459c-ab74-4dd12188ff80', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('47048e61-537e-459c-ab74-4dd12188ff80', foundational, incumbent_maintenance_primary_causal_mechanism).
narrative_ontology:cs_axiom_status(incumbent_maintenance_primary_causal_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('47048e61-537e-459c-ab74-4dd12188ff80', incumbent_maintenance_primary_causal_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('47048e61-537e-459c-ab74-4dd12188ff80', foundational, identifiable_beneficiary_concentration).
narrative_ontology:cs_axiom_status(identifiable_beneficiary_concentration, holdable).
narrative_ontology:cs_axiom_grounding('47048e61-537e-459c-ab74-4dd12188ff80', identifiable_beneficiary_concentration, empirically_contingent).
narrative_ontology:cs_reference_frame('47048e61-537e-459c-ab74-4dd12188ff80', incumbent_protected_market_order).
narrative_ontology:cs_drift_state('47048e61-537e-459c-ab74-4dd12188ff80', contemporary_antitrust_and_platform_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('47048e61-537e-459c-ab74-4dd12188ff80', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, rent_collection_apparatus).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, potential_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, displaced_labor_cohorts).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, subordinated_capital_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_ideological_defenders).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, consumer_coalitions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and control established market positions. They benefit from market structure that prevents or slows new entrants and suppresses wage/price competition. Their capital is defended by regulatory capture, intellectual property enforcement, supply-chain lock-in, and network effects they control. They invest substantially in maintaining these defenses through litigation, lobbying, and institutional capture. Exit is effectively impossible — their wealth IS their market position.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter).

% The institutional infrastructure that enforces incumbency protection: intellectual property courts, regulatory agencies staffed by former incumbents, licensing systems, patent prosecution offices, trade negotiators bound to incumbent interests. Not an agent itself but the machinery through which incumbent preferences become enforceable rules.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, rent_collection_apparatus, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(market_naturalization__beneficiary_maintained_reading, rent_collection_apparatus).

% Face barriers to entry engineered to appear natural: patent thickets that block technological development paths, regulatory approval processes weighted toward incumbents, network effects that make entry capital-prohibitive, predatory pricing that incumbent players can sustain but entrants cannot. Many potential entrants never attempt entry because they correctly perceive the defensibility of incumbent positions. Those who try face litigation and exclusion.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, potential_market_entrants, payer,
    powerless, biographical, trapped, global).

% Bear costs through suppressed wage growth and employment precarity. Labor that would have moved into dynamic new entrant firms instead faces barriers (labor market power concentrated in incumbents, skill-mismatch from blocked sector transitions). They experience the constraint as naturalized scarcity rather than active closure — they blame themselves for 'lack of fit' rather than recognizing the organized defense that prevents alternative employment paths from opening.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, displaced_labor_cohorts, payer,
    organized, biographical, constrained, national).

% Own capital but operate in markets where incumbents have set the competitive parameters. They pay through suppressed returns on their invested capital — below what would obtain in a contestable market — because they operate subject to incumbent price-setting power and entry barriers that truncate their growth. They may recognize the extraction but lack the political power to challenge it; they may also mistake it for natural competitive disadvantage.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, subordinated_capital_holders, payer,
    moderate, biographical, constrained, global).

% Pay through higher prices, reduced product quality/innovation, and service lock-in. Entry barriers mean fewer competitors and less price pressure. Their identity as consumers is often identity-locked to specific branded incumbents ('I am an iPhone user,' 'I use this social network') in ways that make exit costly even when alternatives exist. They are excluded from policy conversations about market structure; their preferences are solicited only as market data, never as political voice.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, consumer_coalitions, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, consumer_coalitions, excluded).

% Government officials, judges, and agency staff who administer intellectual property, antitrust, and regulatory systems. Many are identity-locked to the incumbent-serving interpretation of these systems through professional training, career incentives, and ideological capture. They experience defending incumbent positions as 'applying the law correctly' rather than as active suppression. Their exit from this frame requires abandoning professional identity and institutional belonging.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, regulatory_capture_agents, agenda_setter,
    institutional, generational, identity_locked, national).

% Economists, business theorists, and technologists who propose genuinely different market structures (cooperative models, distributed ownership, platform commons, degrowth allocation). They are excluded from policy-setting conversations; their work is marginalized as ideological rather than scientific. Academic and publishing gatekeeping favors incumbent-friendly theory. They can publish and teach but cannot influence market structure itself.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, alternative_market_theorists, excluded,
    moderate, biographical, constrained, global).

% Intellectuals, economists, and commentators who defend incumbent market positions as natural, efficient, or inevitable. They benefit through professional standing, publication platforms, consulting contracts, and identity alignment with the defender role. Their worldview makes incumbent defense a moral and intellectual duty. Exit would require abandoning career identity and professional community.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_ideological_defenders, beneficiary,
    organized, biographical, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes stable expectations around property rights, investment security, and return on capital. Capital holders can invest confidently if they believe their advantages will be defended by institutions. The arrangement coordinates on 'how markets work' and 'what security looks like' — a genuine coordination problem in a world of competing property claims.
% TRANSFER_FUNCTION: Moves rents and restricted opportunities from potential entrants, displaced workers, and subordinated capital to incumbent holders and the regulatory apparatus that defends them. The transfer is executed through patent enforcement, licensing barriers, regulatory approval, litigation costs, and labor-market suppression. Capital surplus that would be competed away in a contestable market is instead captured as rent.
% ABSENT_VOICES: Potential entrants who never attempt entry because they perceive barriers as unchallengeable; workers in sectors foreclosed by incumbent control; theorists and practitioners of alternative market structures whose proposals are kept outside policy discourse; future generations whose technological and organizational possibilities are constrained by locked-in incumbency.
% DISAPPEARANCE_RATIONALE: If active incumbent defense ceased — if patents expired without renewal, if regulatory capture dissolved, if litigation threats against entrants were withdrawn — capital would face genuine competition within months. New entrants would occupy competitive niches, wages would rise as labor-market power dispersed, and prices would compress toward marginal cost. The entire capital structure would undergo revaluation. The incumbents depend on this constraint continuing; its disappearance is their existential threat.
% FOUNDING_PROBLEM: Early industrial and digital eras faced genuine uncertainty about whether capital would take the risks required to build infrastructure if investment could be expropriated or easily competed away. Patent systems and regulatory protection were established to guarantee returns on innovation and infrastructure investment.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent capital holders and mainstream business economists attest the founding problem remains live: without patent protection and regulatory stability, venture capital would not fund innovation and infrastructure. Alternative development economists and technology theorists (outside the benefiting parties) attest the founding problem is substantially solved for core sectors but the apparatus persists as rent protection — historical evidence shows innovation continues in patent-weak sectors, and technological diffusion accelerates when barriers fall. Empirical studies of patent efficacy from outside incumbent-funded research generally find patents protect rents rather than innovation in mature sectors.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint exhibits rising extractiveness (0.62→0.78 over the interval) because rents accumulate as incumbents invest in defense mechanisms — each successful litigation, each regulatory victory, each patent extension increases the cost-to-entry and therefore the spread between incumbent returns and competitive returns. Suppression rises and plateaus (0.68→0.81) because active defense requires sustained enforcement: litigation threats, regulatory lobbying, intellectual property prosecution, and denial of access to distribution networks. Theater ratio rises early (0.28→0.42 by t=15) then stabilizes because the apparatus develops elaborate justifications (innovation incentives, consumer safety, investment security) that displace from public view the naked extraction logic — stabilization at 0.42 indicates the theater has reached equilibrium (elaborate enough to be credible, spare enough to be operationally efficient). Accessibility collapse is moderate-high (0.68) because alternatives ARE technically and organizationally conceivable (cooperative ownership, platform commons, degrowth allocation) but appear economically impossible within the framework that incumbent institutions control — the alternatives are collapsed not by physical law but by institutional redefinition of what is 'rational' or 'viable'. Resistance is high (0.72) because potential entrants constantly probe the barriers, workers organize around wages and conditions, and alternative theorists publish and teach — resistance persists because the constraint is not natural, so it meets continuous friction.
 *
 * PERSPECTIVAL GAP:
 *   The seat-level divergence is acute and structural. From the incumbent_capital_holders and regulatory_capture_agents seats, the constraint appears as (at minimum) rope-class coordination — it stabilizes expectations, enables long-term investment, and solves a real collective-action problem around property rights. From their perspective the metrics should compute as moderate extraction with genuine coordination function. From the potential_entrants and displaced_labor_cohorts seats, the constraint appears as pure snare — they experience active exclusion, concentrated costs, and no coordination benefit. The engine should compute higher extraction and lower beneficiary payoff for these seats. The authored structural data (beneficiaries identified as incumbent holders; victims identified as entrants and workers; requires_active_enforcement: true) should drive the per-seat divergence in computed classification without requiring me to state it explicitly.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders derive d near 0.0 (full beneficiary) because: (1) they benefit from the constraint (rents), (2) they control the terms (agenda-setter role), (3) their exit is only arbitrage (they can move capital between markets but cannot leave the market-dominance dynamic itself). Potential entrants derive d near 1.0 (full target) because: (1) they bear the cost (blocked entry), (2) they have no structural control (trapped), (3) their exit is structural (they can only abandon the attempt). Displaced workers derive intermediate d (0.6-0.8 toward target) because: (1) they bear diffuse costs (wage suppression, employment precarity), (2) they have organized-but-limited power, (3) their exit is constrained (retraining takes time and capital). The directionality derivation follows from beneficiary/victim declarations + power atom + exit options without requiring overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem was genuine: early capital formation required institutions that guaranteed returns and prevented expropriation. The founding_problem_status is contested because: incumbent capital holders attest the problem remains live (venture capital still requires patent/regulatory protection), while alternative development economists attest the problem is substantially solved in most sectors (empirical evidence shows innovation in patent-weak sectors, and technological diffusion accelerates when barriers fall). The mandatrophy dynamic emerges because the apparatus persists while its justification has decayed — the warrant that initially motivated patent and regulatory systems (solving the under-investment problem) no longer applies uniformly. Some sectors remain under-invested absent protection; most sectors are over-protected relative to the original warrant. The constraint exhibits mandatrophy in two ways: (1) the founding coordination problem (securing capital investment) is mostly solved but the apparatus persists and expands, and (2) the apparatus develops an independent beneficiary class (incumbent capital holders, rent-collection bureaucracies, ideological defenders) that benefits from perpetuating the founding problem's framing even as the problem decays. This is the structural marker of mandatrophy: a problem-solving apparatus that now primarily solves the problem of how to maintain the apparatus itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_structural_maintenance,
    'Is market dominance actively maintained by intentional incumbent defense, or is it structurally reproduced once established — i.e., does it require ongoing incumbent agency or does it persist through institutional inertia alone?',
    'Comparative analysis of markets where incumbent defense investment changed (reduced legal/regulatory spending, or increased): if dominance decays when defense spending falls, active maintenance is primary; if dominance persists unchanged, structural reproduction is primary.',
    'If active maintenance is primary, the reading stands as authored — incumbent agency is the causal mechanism. If structural reproduction dominates, the constraint shifts toward piton (inertial, theatrical defense of a decaying function). The ''beneficiary-maintained'' reading assumes agency is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_vs_structural_maintenance, empirical, 'Causal determination: Does incumbent agency actively maintain dominance, or do institutions reproduce it without agency?').

omega_variable(
    kernel_framing_naturalization,
    'Is market dominance read as ''natural'' (emerging inevitably from efficiency or competition) or as ''constructed'' (sustained by deliberate institutional design benefiting identifiable classes)?',
    'The committer frame: this reading (beneficiary_maintained) asserts the constructed reading; the sibling lapsed_alternative_reading asserts naturalization under inertia; the hybrid_reading asserts mixed mechanisms. The kernel itself (market_naturalization) hosts the contest — no external resolution available. What resolves is WHICH reading the engine computes from the authored metrics and structural data.',
    'This omega documents the reading-level irreducibility: the beneficiary_maintained_reading INSTANTIATES a specific ε-invariant constraint (high extraction, active enforcement, identifiable beneficiaries) under the CONSTRUCTED framing. A different reading would author different ε, different beneficiaries, different stakeholder situations. The engine''s per-seat classification output is the apparatus''s verdict — which reading''s structure better fits the observed constraint dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_naturalization, conceptual, 'Kernel reading frame: Is dominance natural or constructed? This reading instantiates the constructed interpretation.').

omega_variable(
    beneficiary_identification_ambiguity,
    'Are incumbent capital holders the primary beneficiary, or is the beneficiary distributed across multiple classes (capital holders, rent-collection apparatus, ideological defenders, regulatory agencies)?',
    'Trace the actual flow of rents and enforcement resources: where do resources extracted from entrants, workers, and subordinated capital actually accumulate? If rent flows concentrate in incumbent hands, beneficiary identification holds. If rents disperse or are captured by the apparatus itself, the beneficiary structure shifts.',
    'If rents concentrate in incumbent hands, the reading''s Tangled Rope classification (coordination + asymmetric extraction to a named class) stands. If rents disperse, the constraint might be Scaffold (transitory apparatus) or Piton (diffuse theater). The beneficiary-maintained reading depends on identifiable, concentrated beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Where do extracted rents actually accumulate? Are beneficiaries identifiable and concentrated?').

omega_variable(
    reading_kinship_bifurcation,
    'Is this reading separable from the lapsed_alternative_reading and hybrid_reading, or do they represent points on a single continuum?',
    'Structural difference test: Do the three readings author genuinely different ε values, different beneficiary structures, or different suppression mechanisms — or do they differ only in narrative frame? If ε and structure are identical and only the causal story changes, they are the same constraint read differently (a framing variance). If ε and structure genuinely differ, they are three distinct constraints inhabiting one kernel.',
    'If distinct constraints: three separate JSON files, three separate compilations, three separate engine classifications. The network linking them reveals how beneficiary-maintained structure can coexist with lapsed structure in different sectors or regions. If framing variance: one constraint, one JSON, one engine output, but the omega documents how seat-level perception differs (payer seats read ''actively maintained'' because they experience defense; beneficiary seats read ''natural'' because they inherit advantage).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kinship_bifurcation, conceptual, 'Are the sibling readings structurally distinct constraints or narrative variants of one constraint?').

omega_variable(
    regulatory_capture_internalization,
    'To what degree is the suppression of alternatives internalized in regulatory agents and professionals (identity-locked to incumbent-serving interpretation) versus externally enforced through litigation and market barriers?',
    'Comparative study of regulatory regime change: when regulators with incumbent-serving professional identity exit (retirement, political transition), does suppression of alternatives persist at the same intensity, weaken, or shift mechanism? Post-exit trajectory reveals the balance between internalized commitment and external enforcement.',
    'High internalization means suppression persists even when external mechanisms weaken — the reading''s high suppression metric (0.81) would remain robust to external institutional change. Low internalization means weakening external enforcement quickly erodes suppression. This determines whether the constraint''s persistence depends primarily on incumbent maintenance (requires high internalization) or institutional structures (could persist under competing management).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_internalization, empirical, 'What fraction of suppression is internalized in professional identity versus externally enforced?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mkt_nat_bm_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mkt_nat_bm_tr_t5, market_naturalization__beneficiary_maintained_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(mkt_nat_bm_tr_t10, market_naturalization__beneficiary_maintained_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(mkt_nat_bm_tr_t15, market_naturalization__beneficiary_maintained_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(mkt_nat_bm_tr_t20, market_naturalization__beneficiary_maintained_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(mkt_nat_bm_tr_t25, market_naturalization__beneficiary_maintained_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(mkt_nat_bm_tr_t30, market_naturalization__beneficiary_maintained_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(mkt_nat_bm_tr_t35, market_naturalization__beneficiary_maintained_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(mkt_nat_bm_tr_t40, market_naturalization__beneficiary_maintained_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(mkt_nat_bm_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(mkt_nat_bm_be_t5, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement(mkt_nat_bm_be_t10, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(mkt_nat_bm_be_t15, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(mkt_nat_bm_be_t20, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(mkt_nat_bm_be_t25, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(mkt_nat_bm_be_t30, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(mkt_nat_bm_be_t35, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement(mkt_nat_bm_be_t40, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mkt_nat_bm_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(mkt_nat_bm_su_t5, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement(mkt_nat_bm_su_t10, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(mkt_nat_bm_su_t15, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement(mkt_nat_bm_su_t20, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(mkt_nat_bm_su_t25, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(mkt_nat_bm_su_t30, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement(mkt_nat_bm_su_t35, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 35, 0.81).
narrative_ontology:measurement(mkt_nat_bm_su_t40, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(market_naturalization__beneficiary_maintained_reading, 0.12).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__hybrid_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, innovation_patent_efficacy).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, regulatory_capture_institutional_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is one reading (beneficiary_maintained) of a contested kernel (market_naturalization). Two sibling readings exist as separate constraint stories: lapsed_alternative_reading (dominance persists through inertia, not active defense) and hybrid_reading (mixed mechanisms). The network links all three readings; the kernel_id/reading_id in cs_structure documents kinship. The three readings are NOT alternative measurements of a single constraint — they are structurally distinct constraints instantiated from different interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
