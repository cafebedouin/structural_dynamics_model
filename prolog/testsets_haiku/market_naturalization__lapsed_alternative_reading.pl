% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Institutional Closure
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the 'lapsed_alternative_reading' of the
 *   market_naturalization kernel. Market dominance—the structural advantage
 *   of established firms over would-be entrants—is modeled here as a PITON: a
 *   degraded institutional arrangement whose original coordination function
 *   (price discovery and decentralized allocation) is operationally sound,
 *   but whose persistence no longer requires active maintenance. Entry
 *   barriers have atrophied alternative pathways (cooperative production,
 *   municipalist distribution, peer networks) through non-use rather than
 *   active suppression. No identifiable beneficiary class consciously
 *   maintains the closure—incumbent firms benefit passively but do not defend
 *   the structure actively. The constraint persists through theatrical
 *   affirmation (market naturalism doctrine) and path-dependent expectations
 *   (everyone believes markets are inevitable) rather than through coercive
 *   enforcement. This reading explicitly COEXISTS with the
 *   beneficiary_maintained_reading (which argues incumbents actively defend
 *   dominance through predatory practices) and the hybrid_reading (which
 *   argues both lapsed elements and active maintenance operate
 *   simultaneously). The engine will compute divergent per-seat
 *   classifications: from the incumbent seat, the constraint looks like
 *   natural coordination requiring no maintenance; from the would-be entrant
 *   seat, it looks like entrapment; from the policy seat, it looks like
 *   evolution. The authored metrics show extractiveness declining over the
 *   interval (as natural law certification increases and active suppression
 *   becomes unnecessary), while theater ratio rises (as the arrangement
 *   substitutes narrative for enforcement). This metric trajectory is the
 *   diagnostic signature of a piton: performance of inevitability replacing
 *   structure of domination.
 *
 * KEY AGENTS:
 *   - incumbent_market_participants: Powerful firms benefiting from established market position without active defensive action; their advantage is inertial rather than maintained.
 *   - would_be_market_entrants: Powerless, identity-locked agents facing atrophied alternatives; they believe entry is impossible not because of active barriers but because markets are natural.
 *   - policy_makers: Institutional observers treating market dominance as evolutionary outcome rather than constructed constraint; they see no enforcement to regulate.
 *   - intellectual_tradition__market_naturalism: A non-agent doctrine vindicated by the constraint's operation; the tradition survives by explaining away evidence of institutional maintenance.
 *   - competing_economic_schools: Excluded voices with alternative institutional framings; their exclusion is epistemic rather than coercive.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.28).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Institutional Closure").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, 'ed92caab-cb06-4311-b057-b15ad9175957').
narrative_ontology:cs_kernel_codification('ed92caab-cb06-4311-b057-b15ad9175957', distributed).
narrative_ontology:cs_authority_grounding('ed92caab-cb06-4311-b057-b15ad9175957', extraction).
narrative_ontology:cs_interpretation_layer_present('ed92caab-cb06-4311-b057-b15ad9175957').
narrative_ontology:cs_reading_relation('ed92caab-cb06-4311-b057-b15ad9175957', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed92caab-cb06-4311-b057-b15ad9175957', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('ed92caab-cb06-4311-b057-b15ad9175957', foundational, market_dominance_requires_no_active_maintenance).
narrative_ontology:cs_axiom_status(market_dominance_requires_no_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('ed92caab-cb06-4311-b057-b15ad9175957', market_dominance_requires_no_active_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('ed92caab-cb06-4311-b057-b15ad9175957', foundational, alternatives_atrophied_through_non_use).
narrative_ontology:cs_axiom_status(alternatives_atrophied_through_non_use, holdable).
narrative_ontology:cs_axiom_grounding('ed92caab-cb06-4311-b057-b15ad9175957', alternatives_atrophied_through_non_use, empirically_contingent).
narrative_ontology:cs_reference_frame('ed92caab-cb06-4311-b057-b15ad9175957', market_dominance_as_lapsed_closure).
narrative_ontology:cs_drift_state('ed92caab-cb06-4311-b057-b15ad9175957', contemporary_period_2026, gap(stable, minor, false)).
narrative_ontology:cs_created_at('ed92caab-cb06-4311-b057-b15ad9175957', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).
:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts high (0.55 in 1800) when market dominance required active enforcement against feudalism, guilds, and mercantilist restriction. As the market framework becomes institutionally embedded, active suppression (suppression_requirement) declines from 0.65 to 0.12 by 2026. Extractiveness declines in parallel (to 0.28) because the constraint no longer extracts through coercion but through passive benefit from accumulated asymmetries. Theater rises sharply (from 0.25 to 0.68) as rhetoric about 'efficient markets,' 'natural selection,' and 'creative destruction' replaces the enforcement machinery that once defended the closure. At T=2026, the arrangement bears the hallmark piton signature: low extractiveness (coordination costs only), minimal suppression (no active force required), high theater (performative maintenance of the naturalist narrative), and stable resistance at a low floor (alternatives are invisible, not actively resisted). The measurement series is authored on a shared time grid (every metric at every time point) so temporal drift is unambiguous. The interval spans institutional consolidation (1800–2026) when market organization shifted from actively maintained exception to naturalized baseline.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent seat, market dominance is efficient equilibrium requiring no active maintenance—competitors lose because they are less efficient, not because barriers are constructed. From the would-be entrant seat, market dominance is entrapment encoded as inevitability—exit is impossible because alternatives have atrophied and beliefs about market naturalism lock identity. From the policy seat, market dominance is evolutionary outcome of competitive process—no regulation is required because the mechanism is self-correcting. From the heterodox-economist seat, market dominance is institutional lock-in maintained through doctrine—the naturalist narrative obscures the constructed character of the closure. The engine should compute PITON from incumbent and policy seats (low measured extraction supports the framing); SNARE-neighboring from would-be-entrant seat (trapped, identity-locked, high effective extraction despite low authored base); and contestation from excluded-voice seats (the arrangement is neither natural nor necessary—it is contingent on institutional choice). These divergences are the point: the constraint's classification is not unified but perspectivally fractured, and that fracture is the signal the reading instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent market participants derive d ≈ 0.15–0.25 (beneficiary with low coercion, mobile exit if threatened, powerful institutional position): they benefit but do not pay suppression costs; they are lightly bound to the constraint. Would-be entrants derive d ≈ 0.80–0.90 (trapped, identity-locked, powerless, high effective extraction despite low authored base): they pay suppression costs through atrophied alternatives and belief in naturalism; they are heavily bound. Policy makers derive d ≈ 0.50 (symmetric: benefit from stable allocation mechanism, bear costs of lost alternatives): they are analytical and do not experience the constraint materially. The intellectual tradition is declared as a non-agent beneficiary (agent=false) because it collects vindication but runs no machinery. The competing schools are excluded, not victims—they are not extracted from but rather driven out of policy discourse. The directionality spreads widely because the reading claims NO identifiable beneficiary maintaining the constraint: incumbents benefit but do not pay to maintain it; the constraint persists through institutional inertia and narrative, not through active defense. This is how a piton looks structurally: beneficiaries (if any) are passive; payers bear diffuse costs (atrophied alternatives); no one is extracted from acutely. The low base_extractiveness (0.28) reflects this: it is the coordination cost of market mechanism, not the rent collected by a defending agent.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED for this reading. The founding problem (coordination without central planning) is LIVE in the technical sense—price signals still perform their function—but DEAD in the historical sense: the problem that markets were built to solve (organizing pre-industrial economies without feudal hierarchy) no longer exists in developed economies with institutional infrastructure. The constraint persists not because the founding problem persists but because the institutional arrangements built to solve it have become self-perpetuating. The measurement series show extractiveness declining as the founding problem fades and the arrangement becomes increasingly theatrical. A true piton: the original problem is gone; the solution persists as performance. The mandatrophy_resolved flag should be TRUE for this reading, distinguishing it from the hybrid_reading (which argues the founding problem remains partly live) and the beneficiary_maintained_reading (which argues the problem is actively maintained through strategic action). The reading's resolution of mandatrophy is the marker of its ε-invariance: if the founding problem were still live, extractiveness would not decline; if active beneficiaries were defending dominance, suppression would not decline; both decline, so the reading stands alone in claiming lapsed closure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_pathway_atrophy_vs_active_suppression,
    'Are entry barriers to market dominance caused by atrophied alternatives (path-dependent institutional development, sunk costs in established firms) or by active suppression (incumbent firms defending dominance through predatory practices, exclusive dealing, regulatory capture)?',
    'Historical analysis of policy efforts to reduce barriers (antitrust, deregulation, subsidies for new entrants) and measurement of whether reduced policy barriers translate to successful entry, or whether structural barriers persist despite policy change. Institutional history of how alternatives (cooperatives, municipal enterprises, barter networks) lost viability—was it through active suppression or through non-use and legal infrastructure changes?',
    'If atrophy is primary, the constraint is a piton (this reading) with low extractiveness and minimal active enforcement. If suppression is primary, the constraint is a snare or tangled rope with high extractiveness and substantial enforcement. If both operate, the constraint is hybrid. The classification hinges entirely on which mechanism dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_atrophy_vs_active_suppression, empirical, 'Whether market dominance''s persistence is driven by passive atrophy or active defense.').

omega_variable(
    market_naturalism_as_cover_story,
    'Is the high theater_ratio (0.68)—the dominance of naturalist rhetoric over enforcement machinery—evidence that the constraint is lapsed and performatively maintained, or evidence that beneficiaries use naturalist narrative as a cover story for active suppression?',
    'Comparative institutional analysis: examine policy interventions (antitrust cases, regulatory reviews) that temporarily disrupt the naturalist frame and observe whether incumbents mount active defense or whether the constraint dissolves. Measure whether belief in market inevitability declines when natural-law framing is publicly contested (linguistic intervention and belief measurement).',
    'If theater indicates lapsed closure, the constraint continues as piton. If theater indicates cover story for active maintenance, the constraint reclassifies toward snare/tangled rope and the beneficiary_maintained_reading becomes more plausible. The theater ratio alone cannot distinguish these—the oracle is whether active defense emerges when the naturalist narrative is disabled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_naturalism_as_cover_story, conceptual, 'Whether high theater signals genuine lapse or strategic narrative deployment by active beneficiaries.').

omega_variable(
    identity_lock_mechanism__belief_vs_structural,
    'Is the identity_locked exit option for would-be entrants caused by belief in market inevitability (a cognitive/cultural lock) or by structural dependency on established market infrastructure (actual economic lock)? These require different remedies and imply different constraint structures.',
    'Offer experimental cohorts (entrepreneurs, policy makers) exposure to heterodox economic framings and alternative institutional models (worker cooperatives, municipal enterprises, platform cooperatives); measure whether belief-shift changes entry behavior or whether structural barriers prevent entry despite belief shift. Qualitative interviews with would-be entrants to distinguish ''I could never succeed'' (structural belief) from ''I could not survive without access to established distribution/finance'' (economic fact).',
    'If lock is primarily belief-based and remediable through narrative reframing, the constraint''s effective suppression is lower than authored and could shift toward rope if coordination function is highlighted. If lock is structural and belief is secondary, the constraint remains piton/snare depending on whether incumbents defend actively. If both, the constraint is hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism__belief_vs_structural, empirical, 'Whether identity-lock on entrants is cognitive or structural.').

omega_variable(
    coordination_function_vs_dominance_structure_independence,
    'Can the market''s genuine coordination function (price discovery, decentralized allocation) operate without market dominance—that is, are dominance structure and coordination function technically separable, or does coordination require some degree of market concentration to function?',
    'Examine actual alternative markets (cryptocurrency networks, peer-to-peer platforms, municipal enterprises) that perform coordination without dominance; measure allocation efficiency and price stability. Test whether regulatory fragmentation (breaking up dominant firms) causes coordination breakdown or merely redistributes advantage.',
    'If separable, the coordination function justifies the constraint but dominance itself is pure extraction over-and-above coordination necessity—reclassifies toward tangled rope (coordination + extraction both present). If inseparable, dominance is structurally necessary to coordination and the low measured extractiveness is justified—piton classification holds. The question directly addresses whether this reading''s core claim is true: that the constraint solves a real problem while persisting through lapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_vs_dominance_structure_independence, empirical, 'Whether market dominance is technically necessary for market coordination or contingent to it.').

omega_variable(
    kernel_reading_contention__lapsed_vs_maintained,
    'This constraint is one of three readings of the market_naturalization kernel. The three readings predict different per-seat classifications and different enforcement signatures. Can observational data distinguish which reading is structurally correct, or do all three remain live interpretations?',
    'Design an intervention that would produce different predictions across readings: e.g., antitrust enforcement that reduces market concentration and offers subsidies to new entrants. Under lapsed_alternative_reading, barriers should persist (alternatives are atrophied, not suppressed); under beneficiary_maintained_reading, barriers should weaken significantly (suppression is active and can be disrupted); under hybrid_reading, barriers should weaken partially (both mechanisms relax). Measure entry rates, new-firm survival, and incumbent defensive behavior post-intervention.',
    'If the intervention clearly favors one reading''s predictions, the kernel contest is resolved and this reading may be superseded or confirmed. If all three readings survive—each with coherent sub-populations for whom it is true—then market dominance is genuinely underdetermined at the kernel level and all three constraint stories remain live, differing in their locus of operation rather than in a unified constraint''s type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contention__lapsed_vs_maintained, conceptual, 'Whether the kernel_reading contest is resolvable empirically or whether all three readings remain live incompatible framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 1800, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1800, market_naturalization__lapsed_alternative_reading, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(mark_tr_t1900, market_naturalization__lapsed_alternative_reading, theater_ratio, 1900, 0.38).
narrative_ontology:measurement(mark_tr_t1950, market_naturalization__lapsed_alternative_reading, theater_ratio, 1950, 0.52).
narrative_ontology:measurement(mark_tr_t1990, market_naturalization__lapsed_alternative_reading, theater_ratio, 1990, 0.63).
narrative_ontology:measurement(mark_tr_t2010, market_naturalization__lapsed_alternative_reading, theater_ratio, 2010, 0.66).
narrative_ontology:measurement(mark_tr_t2026, market_naturalization__lapsed_alternative_reading, theater_ratio, 2026, 0.68).

% Extraction over time
narrative_ontology:measurement(mark_be_t1800, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(mark_be_t1900, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1900, 0.48).
narrative_ontology:measurement(mark_be_t1950, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1950, 0.38).
narrative_ontology:measurement(mark_be_t1990, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(mark_be_t2010, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement(mark_be_t2026, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2026, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1800, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1800, 0.65).
narrative_ontology:measurement(mark_su_t1900, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1900, 0.48).
narrative_ontology:measurement(mark_su_t1950, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement(mark_su_t1990, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(mark_su_t2010, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement(mark_su_t2026, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2026, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__lapsed_alternative_reading, 0.18).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% The market_naturalization kernel decomposes into three ε-invariant readings: lapsed_alternative_reading (this story, ε ≈ 0.28—coordination cost only, no active maintenance); beneficiary_maintained_reading (ε likely 0.65+—high extraction from active incumbent defense); hybrid_reading (ε likely 0.45–0.55—both lapsed and active elements). Each reading generates a different per-seat classification because they represent different causal structures for the same observed dominance. The constraint_id naming distinguishes readings at the authoring level (constraint_id = kernel_id + reading_id) so the engine can track them separately. All three should be authored as distinct files and linked via network.affects_constraints so the corpus captures the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
