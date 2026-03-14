% ============================================================================
% CONSTRAINT STORY: consensus_governance_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consensus_governance_capture, []).

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
 *   constraint_id: consensus_governance_capture
 *   human_readable: Consensus Governance Capture
 *   domain: political_economy/institutional_governance
 *
 * SUMMARY:
 *   Consensus governance systems—those requiring unanimous or supermajority
 *   agreement for decisions—create a structural asymmetry between blocking
 *   power and exit capacity. The constraint begins as a genuine coordination
 *   mechanism: by requiring consensus, governance systems protect minority
 *   interests and prevent tyranny of the majority. However, as blocking
 *   coalitions organize and coordination costs rise, the same mechanism
 *   transforms into an extraction apparatus where organized minorities
 *   capture decision-making through veto threats, while excluded minorities
 *   and disempowered stakeholders face increasing suppression and reduced
 *   voice. This constraint exhibits the full range of Deferential Realism
 *   types depending on the observer's structural position, making it a
 *   diagnostic exemplar for how consensus governance can degrade from rope to
 *   snare. The measurements show extractiveness and theater ratio both
 *   increasing over the 30-unit interval, indicating institutional drift:
 *   genuine consensus coordination has degraded into performative consensus
 *   theater maintained by veto coalitions.
 *
 * KEY AGENTS:
 *   - Excluded Minorities: Primary victims (powerless/trapped) — face binding consensus requirements; cannot exit without loss of rights or community membership
 *   - Consensus Block Holders: Primary beneficiaries (institutional/arbitrage) — organized minorities with credible veto capacity; can exit to parallel structures if consensus breaks
 *   - Constrained Stakeholders: Secondary victims (moderate/constrained) — participate in consensus but face high exit costs; experience mixed extraction and coordination
 *   - Democratic Reform Coalition: Organized reformers (organized/mobile) — see consensus governance as temporary; advocate for majoritarian or weighted-voting alternatives
 *   - Collective Welfare: Victim (powerless/trapped) — abstract common good that cannot organize or exit; bears costs of decision paralysis and veto gridlock
 *   - Consensus Ideal Rhetorical Framework: Institutional actor (institutional/arbitrage) — maintains mythology of inclusive governance despite actual veto mechanics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consensus_governance_capture, 0.58).
domain_priors:suppression_score(consensus_governance_capture, 0.65).
domain_priors:theater_ratio(consensus_governance_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consensus_governance_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(consensus_governance_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(consensus_governance_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consensus_governance_capture, tangled_rope).
narrative_ontology:human_readable(consensus_governance_capture, "Consensus Governance Capture").
narrative_ontology:topic_domain(consensus_governance_capture, "political_economy/institutional_governance").

domain_priors:requires_active_enforcement(consensus_governance_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consensus_governance_capture, consensus_block_holders).
narrative_ontology:constraint_beneficiary(consensus_governance_capture, procedural_gatekeepers).
narrative_ontology:constraint_victim(consensus_governance_capture, disempowered_minorities).
narrative_ontology:constraint_victim(consensus_governance_capture, excluded_stakeholders).
narrative_ontology:constraint_victim(consensus_governance_capture, collective_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MINORITY (SNARE) — Powerless agents trapped within consensus governance systems cannot exit or veto. The requirement for unanimity or supermajority consensus transforms into a permission structure favoring status quo preferences. Minorities are locked into accepting majority-endorsed outcomes or face expulsion. Maximum extraction — no alternatives, no voice, no escape.
constraint_indexing:constraint_classification(consensus_governance_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTRAINED STAKEHOLDER (TANGLED ROPE) — Moderate-power agents within consensus systems face high cost to exit (relocation, community loss, economic dependency) but retain some negotiating capacity. The constraint genuinely coordinates collective action while simultaneously extracting disproportionate concessions. Exit is theoretically possible but practically constrained by social and economic ties.
constraint_indexing:constraint_classification(consensus_governance_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONSENSUS BLOCK HOLDER (ROPE) — Institutional actors with blocking power (organized minorities, veto coalitions, procedural gatekeepers) experience the constraint as pure coordination. They have arbitrage options (exit to parallel governance structures, defection to alternative coalitions) and use these as credible threats. The constraint enables them to shape collective decisions proportional to their bargaining capacity without coercion. Net beneficiary.
constraint_indexing:constraint_classification(consensus_governance_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC REFORM COALITION (SCAFFOLD) — Organized advocates for majoritarian or weighted voting systems see consensus governance as a temporary institutional arrangement with a sunset. As democratic norms and proportional representation mechanisms mature globally, consensus requirements become obsolete or morph into supermajority rules with clear thresholds. Low effective extraction because organized agents see an exit path and institutional momentum toward reform.
constraint_indexing:constraint_classification(consensus_governance_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE CONSENSUS IDEAL (PITON) — At civilizational scope, consensus governance is presented as a primordial ideal of collective legitimacy and inclusive decision-making. The actual functional content has atrophied — supermajority rules and veto coalitions replace genuine consensus — yet the rhetorical commitment to 'inclusive governance' persists through institutional inertia. Theater ratio reflects the gap between consensus mythology and consensus practice. The mechanism is maintained because alternatives haven't fully replaced it, not because consensus mechanics actually function.
constraint_indexing:constraint_classification(consensus_governance_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / POWER-BALANCING VIEW (TANGLED ROPE) — From an informed analytical position, consensus governance does provide genuine coordination benefits (preventing tyranny of the majority, protecting minority interests, incentivizing inclusion) while simultaneously enabling extraction through blocking power. The constraint is structurally stable because both coordination and extraction are real and interdependent. Dismantling the consensus requirement would solve the snare for minorities but destroy the rope for all parties. This is mandatrophy resolved: the constraint's mixed nature is the source of its persistence.
constraint_indexing:constraint_classification(consensus_governance_capture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consensus_governance_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consensus_governance_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consensus_governance_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consensus_governance_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consensus_governance_capture, TR),
    TR >= 0.70.

:- end_tests(consensus_governance_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Consensus governance captures value through control over veto — organized minorities can extract concessions, block redistributive policies, and force compromise on terms favorable to blocking coalitions. The value reflects that extraction operates through veto power (high) but not through total exclusion (lower than pure snare). Suppression (0.65): High. Multiple barriers prevent exit: formal exclusion (loss of rights), social exclusion (community ostracism), economic dependence (tied to the governance territory), and cognitive/identity barriers (internalization of consensus myth as legitimate). Theater ratio (0.68): High. Actual consensus governance relies on veto coalitions and blocking power, but the rhetoric maintains the fiction of inclusive deliberation and collective will-formation. Committee procedures, extended deliberation, and consensus-seeking theater mask the power dynamics driving decisions. Over the 30-unit measurement interval, both extractiveness and theater increase, indicating that as blocking coalitions stabilize and coordination costs rise, the system increasingly relies on performative consensus to maintain legitimacy while actual decision-making is driven by veto threat dynamics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint resolves the six-type spectrum through structural position. The beneficiary's Rope reflects their actual experience: consensus enables their veto power and protects their interests. The victim's Snare reflects their actual experience: they are locked into accepting outcomes they oppose or face expulsion. The analytical observer's Tangled Rope reflects the structural truth: both coordination (genuine minority protection) and extraction (veto rents) are real and codependent. The constraint cannot be dismantled without destroying both functions simultaneously — you cannot preserve minority protection without veto power, and you cannot eliminate veto power without eliminating minority protection. This is mandatrophy resolved: the constraint's persistence is not a design failure but a structural fact about how power-sharing systems work.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural position relative to the extraction flow. Consensus block holders have d ≈ 0.15 (beneficiaries with arbitrage exit) — they control veto capacity and can exit to alternative systems; f(d) is low/negative, meaning they experience negative effective extraction (they benefit). Excluded minorities have d ≈ 0.95 (victims with trapped exit) — they bear extraction with no escape; f(d) is maximum (~1.42), meaning they experience maximum effective extraction relative to their power level. Constrained stakeholders have d ≈ 0.65 (victims with constrained exit) — they face high exit costs but retain some negotiating capacity; f(d) is moderate (~1.00). The scope modifier σ(S) scales these values: at national scope σ=1.0, the effective extraction χ = ε × f(d) × σ(S) reflects the full unscaled extractiveness. At global scope (if consensus governance were a worldwide norm), σ=1.2 would amplify extraction. The piton classification derives from the high theater ratio (0.68), not from high d or f(d) — it reflects institutional inertia maintaining performative consensus despite atrophied functional coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: Consensus governance capture presents the core mandatrophy paradox: how can the same mechanism be both a protective coordination device (rope protecting minorities) and an extractive apparatus (snare enabling veto-coalition rents)? The answer is that BOTH are true simultaneously, and their coexistence is structural. Consensus governance genuinely protects minorities who would otherwise be exterminated or expelled by majorities. Simultaneously, consensus governance concentrates decision-making power in organized veto coalitions that extract rents through blocking. These functions are codependent: you cannot have minority protection without veto power, and organized veto power generates rents. The analytical perspective (Tangled Rope) recognizes this codependence as the key to the constraint's persistence. Institutional reformers (Scaffold perspective) argue for alternative minority-protection mechanisms (constitutional courts, proportional representation, veto thresholds with clear supermajority rules) that would decouple protection from rent extraction. The measurements show increasing theater (0.42 → 0.68) over the interval, indicating that as veto coalitions stabilize and extract more rents, the system increasingly relies on performative consensus mythology to maintain legitimacy. This theater increase signals drift from genuine coordination toward extraction-theater hybrid — the Piton perspective becomes increasingly empirically accurate as the system ages.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_threshold_definition,
    'What constitutes ''genuine consensus'' versus ''manufactured consent'' in governance systems?',
    'Empirical measurement of blocking-power distribution, veto frequency, and preference intensity across decision cycles. Analysis of whether minorities can credibly threaten to collapse consensus and at what cost.',
    'If consensus is genuine (blockers have real leverage): Rope classification accurate for block holders. If manufactured (blockers lack credible threat): Snare classification for all parties including apparent block holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_threshold_definition, empirical, 'Whether consensus is structurally genuine or performatively manufactured').

omega_variable(
    exit_option_distribution,
    'Are exit costs symmetrical across all governance participants, or do minorities face structurally higher costs than majorities?',
    'Comparative analysis of switching costs: costs for minorities to exit (relocation, loss of rights, economic dependence) versus costs for majority coalitions to revert to majoritarian rules. Measurement of de facto mobility versus de jure rights.',
    'If symmetric: stronger Rope classification across perspectives. If asymmetric: Snare and Tangled Rope classifications confirmed; minorities are systematically trapped regardless of formal consensus requirements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_distribution, empirical, 'Whether exit costs are distributed symmetrically across participants').

omega_variable(
    blocking_power_concentration,
    'Is blocking power concentrated in organized minorities or distributed across many small actors? Does concentration determine whether consensus functions as coordination or extraction?',
    'Herfindahl-Hirschman Index (HHI) analysis of veto power; measurement of blocking coalition size; tracking of veto coalition persistence and composition change over time.',
    'If highly concentrated (few organized blockers): extraction mechanism dominates; captured consensus. If distributed (many small actors): coordination mechanism dominates; functional consensus. Concentration change over time signals institutional drift from coordination to capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(blocking_power_concentration, empirical, 'Distribution of blocking power across participants').

omega_variable(
    preference_intensity_measurability,
    'Can preference intensity be credibly measured and weighted in consensus systems, or does consensus default to equal weighting regardless of intensity?',
    'Comparison of stated preferences, revealed preferences (through voting patterns, financial stakes, exit willingness), and actual allocation outcomes. Analysis of whether high-intensity minority preferences move outcomes.',
    'If measurable and weighted: Rope classification supported (genuine coordination). If equal weighting or unmeasurable: Snare classification likely (minority intensity ignored; blockers have veto but no intensity weighting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preference_intensity_measurability, empirical, 'Whether preference intensity is measurable and weighted in consensus outcomes').

omega_variable(
    institutional_capture_markers,
    'Do consensus governance systems show observable signs of capture by organized minorities (regulatory capture, veto fatigue, decision paralysis)?',
    'Time-series analysis of decision velocity, blocked-proposal frequency, veto coalition overlap, and outcome stability. Comparison with majoritarian governance baseline from similar domains.',
    'If capture markers present: Snare classification confirmed; consensus governance has degraded from coordination to extraction mechanism. If absent: Rope classification more likely; consensus remains functionally protective of minority interests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_markers, empirical, 'Observable markers of institutional capture in consensus systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consensus_governance_capture, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cgc_tr_t0, consensus_governance_capture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cgc_tr_t10, consensus_governance_capture, theater_ratio, 10, 0.55).
narrative_ontology:measurement(cgc_tr_t20, consensus_governance_capture, theater_ratio, 20, 0.68).
narrative_ontology:measurement(cgc_tr_t30, consensus_governance_capture, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(cgc_be_t0, consensus_governance_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cgc_be_t10, consensus_governance_capture, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(cgc_be_t20, consensus_governance_capture, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cgc_be_t30, consensus_governance_capture, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consensus_governance_capture, enforcement_mechanism).
narrative_ontology:affects_constraint(consensus_governance_capture, regulatory_capture).
narrative_ontology:affects_constraint(consensus_governance_capture, veto_point_proliferation).
narrative_ontology:affects_constraint(consensus_governance_capture, minority_rights_protection).

% DUAL FORMULATION NOTE:
% Consensus governance capture has two structurally distinct constraint stories: (1) consensus_governance_capture (this story) — the institutional mechanism by which blocking power enables extraction; (2) consensus_mythology (separate story, ε=0.72, Snare) — the rhetorical frame that naturalizes veto coalitions as 'inclusive governance.' The mythology story has higher extractiveness because it operates at identity level (agents internalize consensus as legitimacy marker) rather than institutional level. Both stories link: consensus_governance_capture is upstream; consensus_mythology is downstream and maintains the institutional constraint through cognitive capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consensus_governance_capture, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
