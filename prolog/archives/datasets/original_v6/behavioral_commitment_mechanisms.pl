% ============================================================================
% CONSTRAINT STORY: behavioral_commitment_mechanisms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_commitment_mechanisms, []).

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
 *   constraint_id: behavioral_commitment_mechanisms
 *   human_readable: Behavioral Commitment Mechanisms as Coordination and Extraction
 *   domain: behavioral_economics/social_psychology/organizational_behavior
 *
 * SUMMARY:
 *   Behavioral commitment mechanisms are devices, systems, or social
 *   structures that lock agents into future courses of action by raising the
 *   cost of deviation. These range from contracts and pledges to reputation
 *   systems, sunk-cost deployments, and social norms. The constraint exhibits
 *   the full spectrum of DR types depending on the observer's structural
 *   position. A commitment mechanism is simultaneously a solution to
 *   coordination problems (enabling agents to commit to group projects), a
 *   source of extraction (locking agents into terms favorable to designers),
 *   a performative ritual whose power has degraded (piton), a target of
 *   organized resistance (scaffold), and arguably an inevitable feature of
 *   any society solving temporal consistency problems (mountain). The
 *   extractiveness trajectory (0.35 → 0.58) reflects accumulation of
 *   extraction mechanisms onto what began as pure coordination structures.
 *   The theater ratio (0.45 → 0.68) shows how commitment mechanisms
 *   increasingly rely on performative maintenance rather than functional
 *   necessity as alternatives mature.
 *
 * KEY AGENTS:
 *   - Committed Agent: Primary victim (powerless/trapped) — locked into commitment structures by sunk costs, reputation, and psychological identity fusion; cannot exit without severe cost
 *   - Partially Aware Agent: Secondary victim (moderate/constrained) — aware of both coordination benefits and extraction risks; faces costly exit but can exercise it
 *   - Mechanism Designer: Primary beneficiary (institutional/arbitrage) — creates and maintains commitment structures; captures value from commitment-locked agents; high exit optionality
 *   - Commitment Beneficiary: Secondary beneficiary — party who benefits from target agent's committed behavior (employer, creditor, group coordinator)
 *   - Countercommitment Coalition: Organized resistance (organized/constrained) — labor unions, consumer advocates, alternative platform designers building exit pathways and lower-commitment alternatives
 *   - Traditional Loyalty Apparatus: Institutional inertia (institutional/arbitrage) — older commitment systems maintained through routine despite reduced functional necessity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable laws of human coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_commitment_mechanisms, 0.58).
domain_priors:suppression_score(behavioral_commitment_mechanisms, 0.62).
domain_priors:theater_ratio(behavioral_commitment_mechanisms, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_commitment_mechanisms, extractiveness, 0.58).
narrative_ontology:constraint_metric(behavioral_commitment_mechanisms, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(behavioral_commitment_mechanisms, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_commitment_mechanisms, tangled_rope).
narrative_ontology:human_readable(behavioral_commitment_mechanisms, "Behavioral Commitment Mechanisms as Coordination and Extraction").
narrative_ontology:topic_domain(behavioral_commitment_mechanisms, "behavioral_economics/social_psychology/organizational_behavior").

domain_priors:requires_active_enforcement(behavioral_commitment_mechanisms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behavioral_commitment_mechanisms, mechanism_designer).
narrative_ontology:constraint_beneficiary(behavioral_commitment_mechanisms, commitment_beneficiary).
narrative_ontology:constraint_victim(behavioral_commitment_mechanisms, committed_agent).
narrative_ontology:constraint_victim(behavioral_commitment_mechanisms, alternative_preference_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE COMMITTED AGENT (SNARE) — An agent whose future behavior has been locked in through mechanism design, sunk costs, or public commitment. Trapped by past decisions that constrain present options. Cannot exit the commitment without severe reputational, financial, or psychological cost. Experiences the mechanism as pure extraction — their autonomy is structured away.
constraint_indexing:constraint_classification(behavioral_commitment_mechanisms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PARTIALLY AWARE AGENT (TANGLED ROPE) — Aware that commitment mechanisms serve coordination functions (e.g., pledges enable group projects) but also recognizes extraction risk. Exit is costly but possible (reputation damage, financial penalty, relational friction). Sees both genuine coordination benefit AND asymmetric extraction layered on top. Mixed perception reflects mixed structural reality.
constraint_indexing:constraint_classification(behavioral_commitment_mechanisms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE MECHANISM DESIGNER (ROPE) — Organization, platform, or system designer that implements commitment mechanisms (precommitment apps, pledge systems, loyalty programs, employment contracts). Benefits from commitment structures that solve collective action problems. Experiences extraction flow toward them. High exit optionality — can redesign, pivot, or exit the market. Net beneficiary at low cost.
constraint_indexing:constraint_classification(behavioral_commitment_mechanisms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE COUNTERCOMMITMENT COALITION (SCAFFOLD) — Social movements, labor organizing, consumer protection advocates building alternative commitment structures (union contracts with exit clauses, consumer right-to-cancel laws, ethical AI design principles). Organized, constrained by institutional resistance, but have clear sunset logic: as alternatives mature (portable benefits, universal basic income, liquid labor markets), the extractive commitment mechanisms lose leverage. Temporary coordination function with declining suppression.
constraint_indexing:constraint_classification(behavioral_commitment_mechanisms, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE TRADITIONAL LOYALTY APPARATUS (PITON) — Older commitment structures (lifetime employment contracts, brand loyalty through inertia, family obligation systems) persist through institutional inertia despite declining functional coordination benefit. Theater ratio high: much of the perceived commitment is performative maintenance of outdated norms. The mechanism 'works' but primarily through habit and ritual rather than through effective coordination or genuine incentive alignment.
constraint_indexing:constraint_classification(behavioral_commitment_mechanisms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, all social coordination requires some commitment mechanism — precommitment is a fundamental feature of any society that must solve temporal consistency problems. Commitment mechanisms are inevitable structural features of human coordination, not contingent institutional arrangements. This perspective risks naturalizing what are actually designed, contestable mechanisms as immutable laws of social organization.
constraint_indexing:constraint_classification(behavioral_commitment_mechanisms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_commitment_mechanisms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(behavioral_commitment_mechanisms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(behavioral_commitment_mechanisms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(behavioral_commitment_mechanisms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(behavioral_commitment_mechanisms, TR),
    TR >= 0.70.

:- end_tests(behavioral_commitment_mechanisms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Commitment mechanisms impose significant constraints on agents' behavioral freedom. The value reflects that extraction is real and substantial (behavioral locking, opportunity cost, psychological cost) but not maximally severe — some agents can exit at high but surmountable cost, and some commitment mechanisms do solve genuine coordination problems. The trajectory (0.35 → 0.58) shows extraction accumulation: mechanisms initially introduced as coordination solutions have layers of extraction added as designers learn to exploit commitment logic. Suppression (0.62): High. Multiple barriers prevent exit: financial penalties, reputational damage, psychological identity fusion, institutional lock-in, and often legal constraints. Committed agents experience limited alternatives, though not total closure. Theater ratio (0.68): High and increasing. Much of commitment mechanism operation is performative: status signaling, ritual reaffirmation of identity, maintenance of credibility through public displays. The ratio's increase reflects that as technology enables lower-cost exit (gig economy, instant switching), traditional commitment mechanisms increasingly rely on theater (brand identity, social proof, aspirational identity) rather than structural lock-in to maintain force.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence from identical base metrics. The mechanism designer sees coordination (Rope) — they are solving temporal consistency problems and enabling collective action. The countercommitment coalition sees a temporary problem with clear sunset (Scaffold) — portable benefits, universal basic income, and liquid labor markets are creating alternative coordination pathways that do not require commitment locking. The traditional loyalty apparatus sees its own degraded ritual (Piton) — lifetime employment, brand loyalty, and family obligation systems persist through habit despite weakened functional necessity. The partially aware agent sees mixed coordination and extraction (Tangled Rope) — commitment mechanisms do enable some group projects but also lock them into unfavorable terms. The committed agent sees pure extraction (Snare) — their freedom has been structured away with minimal coordination benefit flowing to them. The civilizational observer risks seeing a natural law (Mountain) — 'all coordination requires commitment' — but the structural data reveals this as naturalization of contestable design choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its directionality from agent power, exit options, and beneficiary/victim status. The committed agent (powerless/trapped) experiences maximum extraction flow toward designers — high d value, high f(d). The mechanism designer (institutional/arbitrage) experiences low or negative extraction — they are the beneficiary, low d, negative f(d). The partially aware agent (moderate/constrained) occupies the middle ground — higher d than designers but lower than trapped agents. The countercommitment coalition (organized/constrained) has organized power that moderates experienced extraction despite constrained exit. Directionality is derived structurally from these position parameters; the beneficiary/victim declarations (mechanism_designer and commitment_beneficiary as beneficiaries; committed_agent and alternative_preference_holders as victims) anchor the pipeline to empirical relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that commitment mechanisms are genuinely hybrid structures: they coordinate AND extract simultaneously. The Tangled Rope classification is not a compromise or average of Rope and Snare but a description of the actual structure — the mechanism solves a real coordination problem (agents need ways to commit to group projects) AND creates extraction opportunities (designers profit from locking agents in). Both elements are essential. The perspectival gap is not a sign of ambiguity but of structural reality: from the designer's perspective, the coordination function dominates (Rope); from the committed agent's perspective, the extraction dominates (Snare); from a balanced institutional view (Tangled Rope), both are visible. The theater ratio (0.68) indicates that the mechanism increasingly operates through performative maintenance rather than structural necessity, but this does not change the underlying hybrid structure — performance of commitment is still a form of coordination, just less efficient and more theater-laden.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'How much of the commitment mechanism''s function is genuine coordination (solving collective action problems) vs extractive control (locking agents into terms that benefit designers)?',
    'Counterfactual analysis: would the coordination problem be solved by alternative mechanisms with lower commitment intensity? Comparison of commitment depth required vs commitment depth actually imposed.',
    'If primarily coordination: classify as Rope or Scaffold (depending on sunset presence). If primarily extraction: classify as Snare or Tangled Rope (depending on mixed presence). This distinction determines whether commitment mechanisms are socially necessary vs socially parasitic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether commitment mechanisms primarily serve coordination or extraction').

omega_variable(
    internalization_vs_coercion_suppression,
    'Is the measured suppression (0.62) structural (external barriers to exit) or internalized (the agent believes exit is shameful, disloyal, or identity-breaking)?',
    'Post-exit behavior tracking: if agents who exit the commitment mechanism continue to behave as though committed (guilt, identity disruption, self-imposed penalties), suppression is partially internalized. Exit trajectory analysis distinguishes structural from psychological barriers.',
    'If internalized: effective suppression is higher than the structural measure suggests — agents carry the commitment mechanism with them after formal exit. If structural: suppression drops with exit. Distribution between internalized and structural suppression determines whether countercommitment work must focus on structural alternatives (Scaffold pathway) or identity/psychological liberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_coercion_suppression, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    commitment_mechanism_temporal_asymmetry,
    'Do commitment mechanisms impose different constraints at different temporal horizons? (e.g., contract locks immediate behavior but not civilizational norms; conversely, social shame is weak at immediate but powerful at biographical/generational horizons)',
    'Comparative temporal analysis: measure exit barriers at immediate vs biographical vs generational horizons. Some commitment mechanisms lose power if agent can endure short-term penalty; others only work if backed by civilizational norm structures.',
    'If temporal asymmetry exists: the constraint''s classification should vary by time_horizon perspective (which it does in the perspectival set). The mechanism''s extractiveness may be high at biographical but low at immediate or vice versa. This affects whether Scaffold exits are viable — some alternatives work only for longer time horizons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commitment_mechanism_temporal_asymmetry, empirical, 'Temporal asymmetry in commitment mechanism strength').

omega_variable(
    designer_agency_vs_structural_inevitability,
    'Are commitment mechanisms designed choices by identifiable beneficiaries, or are they emergent features of coordination that arise regardless of designer intent?',
    'Historical and comparative analysis: do societies without explicit commitment mechanism designers develop commitment constraints anyway? Are extractive features explicable as designer intent or as unintended consequences of coordination structures?',
    'If designed choice: responsibility and accountability lie with designers; alternatives are possible through institutional redesign. If structural inevitability: commitment extraction may be unavoidable cost of coordination; policy focus shifts to mitigation rather than elimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designer_agency_vs_structural_inevitability, conceptual, 'Whether commitment mechanisms are designed or inevitable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_commitment_mechanisms, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(behav_commit_tr_t0, behavioral_commitment_mechanisms, theater_ratio, 0, 0.45).
narrative_ontology:measurement(behav_commit_tr_t3, behavioral_commitment_mechanisms, theater_ratio, 3, 0.58).
narrative_ontology:measurement(behav_commit_tr_t6, behavioral_commitment_mechanisms, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(behav_commit_be_t0, behavioral_commitment_mechanisms, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(behav_commit_be_t3, behavioral_commitment_mechanisms, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(behav_commit_be_t6, behavioral_commitment_mechanisms, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_commitment_mechanisms, attachment_coordination).
narrative_ontology:affects_constraint(behavioral_commitment_mechanisms, sunk_cost_fallacy_institutional).
narrative_ontology:affects_constraint(behavioral_commitment_mechanisms, reputation_system_lock_in).
narrative_ontology:affects_constraint(behavioral_commitment_mechanisms, psychological_identity_fusion_binding).

% DUAL FORMULATION NOTE:
% Behavioral commitment mechanisms decompose into three downstream constraints: (1) sunk_cost_fallacy_institutional — the structural logic of cost-irrelevance that nonetheless psychologically binds agents; (2) reputation_system_lock_in — the mechanism by which public commitment creates exit barriers; (3) psychological_identity_fusion_binding — the identity-level locking where the commitment becomes constitutive of the agent's self-concept. Each has different ε and different measurement profiles. The present story models the unified constraint; downstream stories model the component mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(behavioral_commitment_mechanisms, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
