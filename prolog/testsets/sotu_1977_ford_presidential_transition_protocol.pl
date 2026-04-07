% ============================================================================
% CONSTRAINT STORY: sotu_1977_ford_presidential_transition_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1977_ford_presidential_transition_protocol, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1977_ford_presidential_transition_protocol
 *   human_readable: Orderly Constitutional Presidential Transition (Ford-Carter Handoff, 1977)
 *   domain: governance/constitutional_mechanism
 *
 * SUMMARY:
 *   The orderly constitutional presidential transition from Gerald Ford to
 *   Jimmy Carter in 1977 represents a foundational coordination mechanism of
 *   U.S. democratic governance. This constraint operates as pure rope: no
 *   agent is trapped, no suppression is required, and no concentrated
 *   extraction occurs. Instead, all major institutional actors (outgoing and
 *   incoming executives, opposition parties, military, press, civil service)
 *   benefit from the protocol that normalizes power rotation, preserves
 *   constitutional legitimacy, and prevents alternatives such as military
 *   coup, electoral suppression, or authoritarian consolidation. The Ford
 *   administration inherited office through Vice Presidential succession
 *   following Nixon's resignation and faced questions about its legitimacy;
 *   the orderly handoff to Carter despite Ford's defeat in 1976 demonstrated
 *   that constitutional succession operates independent of incumbent
 *   preference. This constraint is post-Watergate institutional recovery — a
 *   reassertion of constitutional norms over executive unilateralism. The
 *   theater_ratio remains low (0.15) because the transition mechanism is
 *   substantively functional: power changes hands, opposition parties assume
 *   roles, press freedom continues, military remains subordinate. Minimal
 *   performative content; maximum coordination content.
 *
 * KEY AGENTS:
 *   - Ford Administration: Institutional beneficiary (institutional/arbitrage) — benefits from orderly exit that preserves legitimacy and avoids authoritarian alternative
 *   - Carter Campaign/Incoming Administration: Institutional beneficiary (institutional/arbitrage) — gains power through constitutional mechanism rather than force or suppression
 *   - Democratic Opposition/Carter Party: Moderate beneficiary (moderate/mobile) — guaranteed right to contest, campaign, and take power through elections; no suppression during transition
 *   - Press/Journalism: Moderate beneficiary (moderate/mobile) — freedom of press continues across administrations; no censorship or consolidation of media control
 *   - Civil Service: Institutional beneficiary (institutional/arbitrage) — continuity of law and procedure despite change of executive; subordination to law rather than persons
 *   - Military: Institutional beneficiary (powerful/arbitrage) — coordination mechanism explicitly specifies civilian control; subordination to constitutional authority is their functional role
 *   - Congress: Powerful institutional actor (powerful/arbitrage) — coordinates power transfer through certification of election results, impeachment/succession checks, appropriations continuity
 *   - Analytical Observer: Sees global/civilizational pure coordination benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1977_ford_presidential_transition_protocol, 0.08).
domain_priors:suppression_score(sotu_1977_ford_presidential_transition_protocol, 0.02).
domain_priors:theater_ratio(sotu_1977_ford_presidential_transition_protocol, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1977_ford_presidential_transition_protocol, extractiveness, 0.08).
narrative_ontology:constraint_metric(sotu_1977_ford_presidential_transition_protocol, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(sotu_1977_ford_presidential_transition_protocol, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sotu_1977_ford_presidential_transition_protocol, accessibility_collapse, 0.05).
narrative_ontology:constraint_metric(sotu_1977_ford_presidential_transition_protocol, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1977_ford_presidential_transition_protocol, rope).
narrative_ontology:human_readable(sotu_1977_ford_presidential_transition_protocol, "Orderly Constitutional Presidential Transition (Ford-Carter Handoff, 1977)").
narrative_ontology:topic_domain(sotu_1977_ford_presidential_transition_protocol, "governance/constitutional_mechanism").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1977_ford_presidential_transition_protocol, democratic_legitimacy).
narrative_ontology:constraint_beneficiary(sotu_1977_ford_presidential_transition_protocol, international_confidence).
narrative_ontology:constraint_beneficiary(sotu_1977_ford_presidential_transition_protocol, opposition_parties).
narrative_ontology:constraint_beneficiary(sotu_1977_ford_presidential_transition_protocol, press_freedom).
narrative_ontology:constraint_beneficiary(sotu_1977_ford_presidential_transition_protocol, executive_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OUTGOING ADMINISTRATION (ROPE) — Ford administration benefits from orderly transition protocol that preserves legitimacy and prevents military intervention. Exit via constitutional term limit is not extraction but coordination: the mechanism ensures smooth power transfer, judicial continuity, and institutional preservation. No suppression; no trapped agents. Pure coordination function.
constraint_indexing:constraint_classification(sotu_1977_ford_presidential_transition_protocol, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: INCOMING ADMINISTRATION (ROPE) — Carter administration benefits from constitutional legitimacy and peaceful transition protocol. Takes power through orderly handoff rather than force. No extraction; pure coordination. The mechanism benefits both parties by removing alternative pathways (military coup, electoral suppression) that would be costlier and riskier.
constraint_indexing:constraint_classification(sotu_1977_ford_presidential_transition_protocol, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: OPPOSITION PARTIES (ROPE) — Democratic opposition to Ford administration experiences the constitutional transition protocol as coordination: the mechanism guarantees their right to contest elections, campaign freely, and take power through ballots rather than suppression. No extraction; full mobility to participate in next election cycle. The constraint enables their agency.
constraint_indexing:constraint_classification(sotu_1977_ford_presidential_transition_protocol, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PRESS AND CIVIL SOCIETY (ROPE) — Freedom of press and civil liberties persist across transition. The protocol coordinates continuation of constitutional protections rather than extraction of them. Journalists, advocacy groups, and civic institutions maintain exit options and voice throughout. Pure coordination; zero suppression.
constraint_indexing:constraint_classification(sotu_1977_ford_presidential_transition_protocol, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MILITARY INSTITUTIONAL STRUCTURE (ROPE) — Armed forces coordinate orderly transition via subordination to civilian constitutional authority. The protocol is explicitly a coordination mechanism: it specifies that military follows civilian orders during power transfer. No extraction of military power; the constraint is their functional role. Mutual benefit through institutional integrity.
constraint_indexing:constraint_classification(sotu_1977_ford_presidential_transition_protocol, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From civilizational scope, the orderly constitutional transition is pure coordination infrastructure. No beneficiary extracts from victims; all stakeholders benefit from preventing alternatives (coup, civil war, authoritarian consolidation). The constraint is a public good: democratic stability, international confidence, institutional continuity. Zero concentrations of extraction.
constraint_indexing:constraint_classification(sotu_1977_ford_presidential_transition_protocol, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1977_ford_presidential_transition_protocol_tests).
:- end_tests(sotu_1977_ford_presidential_transition_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint distributes benefits widely with no concentrated extraction. The outgoing administration gains orderly exit; the incoming administration gains legitimacy; opposition parties gain guarantees; the military gains civilian control clarity; the press gains freedom. Each institutional actor benefits from alternatives being prevented (coup, suppression, breakdown). If any actor attempted to extract via the transition protocol — e.g., Ford attempting to cancel elections, incoming Carter administration consolidating power — the constraint would fail. Success requires mutual restraint. The low extractiveness reflects that mutual restraint is the primary mechanism, not coercion. Suppression (0.02): Negligible. The constraint explicitly does not suppress opposition, does not restrict press, does not prevent party competition. The opposite: it guarantees these freedoms. Minimal suppression reflects that the mechanism is constitutional (formal rules), not coercive (force backing rules). Theater ratio (0.15): Low. The transition protocol is substantively functional, not performative. Elections happen; losers cede power; winners take office; institutions continue. Minimal ceremony; maximum substantive transfer. Low theater distinguishes this rope from pitons, which would have theater_ratio > 0.70.
 *
 * PERSPECTIVAL GAP:
 *   All six perspectives classify this constraint identically as pure rope. The perspectival gap is minimal because the constraint's function aligns incentives: all agents benefit from constitutional coordination. This uniformity is diagnostic — it confirms the constraint is pure coordination with zero extraction. If perspectives diverged significantly (some seeing snare or tangled rope), it would indicate hidden asymmetries. The consistency across powerless, moderate, powerful, institutional, and analytical agents suggests the constraint is genuinely non-extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derivation for this rope is straightforward: beneficiaries with arbitrage or mobile exit options experience zero to negative effective extraction (d ranges 0.05-0.35). No victim groups exist; no trapped agents. The sigmoid f(d) produces uniformly low extraction modifiers. The constraint distributes coordination benefits, not coercive costs. Beneficiaries are not exploiting victims; they are mutually agreeing to operate within constitutional bounds. This symmetry of interest is the signature of rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy present. The constraint has clear rope classification across all perspectives with no ambiguity about whether it is coordination (yes) or extraction (no). The omega variables address robustness under future stress (post-Watergate consensus fragility, norm vs. law stability) but do not call the current classification into question. The constraint works as designed: peaceful power rotation, constitutional legitimacy, no suppression, no trapped agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    post_watergate_legitimacy_fragility,
    'Does the Ford-Carter transition''s smoothness depend on contingent post-Watergate consensus about constitutional restoration, or is it a robust institutional feature independent of trust recovery?',
    'Comparative historical analysis: examine transitions in periods of lower institutional trust (e.g., 2020-2021, 2024-2025); measure military subordination and opposition party cooperation under higher polarization stress',
    'If contingent on trust: the constraint is actually fragile and could degrade to snare or tangled_rope under sustained institutional crisis. If robust: the ropeness is stable across political stress levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_watergate_legitimacy_fragility, empirical, 'Whether constitutional transition robustness depends on post-Watergate legitimacy consensus').

omega_variable(
    structural_vs_cultural_norm_stability,
    'Is the peaceful transition enforced by constitutional law and military training, or primarily by cultural norms and incumbent willingness to cooperate? How would the constraint behave under a leader who contests the legitimacy of election outcome?',
    'Constitutional law analysis; interview military leadership about enforcement protocols; test case analysis (2020 certification challenges, 2024-2025 transition stress); scenario modeling of contested outcome procedures',
    'If structural enforcement is weak: the constraint is a piton (maintained by norm, not law). If robust: the constraint is genuinely rope (enforced). This distinction determines whether the constraint survives norm erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_cultural_norm_stability, empirical, 'Whether peaceful transition is structurally enforced or norm-dependent').

omega_variable(
    international_confidence_measurement,
    'How much of the extractiveness reduction and rope classification derives from the constraint''s function in generating international confidence vs. from genuine domestic coordination?',
    'Time-series analysis of foreign investment, currency stability, and geopolitical risk premiums correlated with transition smoothness; counterfactual modeling of transition failure impact on international markets and alliances',
    'If international confidence is primary beneficiary: the constraint is coordination across borders (global scope rope). If domestic coordination is primary: the constraint is national scope rope. Scope affects χ via σ(S) — different classification territories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_confidence_measurement, empirical, 'Proportion of constraint function devoted to international confidence vs domestic coordination').

omega_variable(
    transition_protocol_enforcement_gaps,
    'What specific enforcement mechanisms prevent a sitting president from nullifying election results, delaying transition, or refusing to cede power? Are these mechanisms adequate to resist a determined executive?',
    'Constitutional law analysis; identification of enforcement actors (Congress, courts, military, civil service); stress testing under scenarios of executive defiance (document seizure, refusal to vacate, foreign military pressure); comparison with other democracies'' transition safeguards',
    'If enforcement gaps exist: the constraint degrades toward snare (depending on who could exploit the gap). If enforcement is robust: the ropeness is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_protocol_enforcement_gaps, empirical, 'Adequacy of constitutional enforcement mechanisms against executive defiance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1977_ford_presidential_transition_protocol, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ford_trans_theater_t0, sotu_1977_ford_presidential_transition_protocol, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ford_trans_theater_t60, sotu_1977_ford_presidential_transition_protocol, theater_ratio, 60, 0.12).
narrative_ontology:measurement(ford_trans_theater_t120, sotu_1977_ford_presidential_transition_protocol, theater_ratio, 120, 0.15).

% Extraction over time
narrative_ontology:measurement(ford_trans_extract_t0, sotu_1977_ford_presidential_transition_protocol, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(ford_trans_extract_t60, sotu_1977_ford_presidential_transition_protocol, base_extractiveness, 60, 0.07).
narrative_ontology:measurement(ford_trans_extract_t120, sotu_1977_ford_presidential_transition_protocol, base_extractiveness, 120, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1977_ford_presidential_transition_protocol, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1977_ford_presidential_transition_protocol, electoral_suppression_prevention).
narrative_ontology:affects_constraint(sotu_1977_ford_presidential_transition_protocol, press_freedom_across_administrations).
narrative_ontology:affects_constraint(sotu_1977_ford_presidential_transition_protocol, military_civilian_control).
narrative_ontology:affects_constraint(sotu_1977_ford_presidential_transition_protocol, constitutional_checks_separation_of_powers).

% DUAL FORMULATION NOTE:
% This constraint is a foundational coordination mechanism that depends on upstream constraints (electoral system legitimacy, constitutional rule of law, military professionalism) and enables downstream constraints (opposition party function, press freedom, civil service continuity). Network relationships trace institutional dependencies rather than causal extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
