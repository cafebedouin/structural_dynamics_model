% ============================================================================
% CONSTRAINT STORY: israel_surplus_vote_agreements
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_surplus_vote_agreements, []).

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
 *   constraint_id: israel_surplus_vote_agreements
 *   human_readable: Surplus-Vote Agreements (Bader-Ofer Method)
 *   domain: political/electoral_systems
 *
 * SUMMARY:
 *   The Bader-Ofer surplus-vote agreement system in Israeli elections creates
 *   a structural hybrid between coordination mechanism and extraction
 *   apparatus. Two or more parties sign pre-election agreements to pool
 *   surplus votes (votes that exceed the divisor threshold for a single seat)
 *   for joint seat allocation. Formally, this solves a coordination problem:
 *   it reduces wasted votes and enables predictable coalition formation.
 *   Structurally, however, it concentrates seat allocation power among
 *   threshold-adjacent parties that can negotiate agreements, while
 *   marginalizing parties below the threshold or without coalition partners.
 *   The constraint exhibits genuine coordination function (reduced
 *   fragmentation, predictable seat distribution) alongside asymmetric
 *   extraction (voter waste externality, power concentration, mandatory
 *   coalition loyalty). The mechanism's theater ratio has remained low (0.35)
 *   because surplus allocation is a transparent, rule-based procedure — but
 *   the rising extractiveness (0.32 → 0.38) reflects increasing coalition
 *   negotiation complexity and the strategic use of surplus agreements to
 *   manipulate final seat outcomes beyond pure vote efficiency.
 *
 * KEY AGENTS:
 *   - Coalition Formation Agents (major parties, institutional power): Primary beneficiary — use surplus agreements to engineer predictable coalition math and marginalize competitors
 *   - Small Parties with Surplus Votes (moderate power, constrained exit): Secondary beneficiary but also victim — benefit from surplus pooling but lose autonomy to coalition partners
 *   - Parties Below Threshold or Without Partners (powerless, trapped): Primary victim — voters suffer complete exclusion despite participating in elections
 *   - Voter Representation Equity (abstract structural good, powerless): Secondary victim — systematic waste of votes from unpartnered parties, information asymmetry about coalition compacts
 *   - Electoral Commission (institutional power, arbitrage exit): Enforcer/administrator — maintains the constraint through regulatory implementation
 *   - Electoral Reform Movement (organized power, constrained exit): Secondary observer — perceives surplus agreements as temporary patch pending systemic reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_surplus_vote_agreements, 0.38).
domain_priors:suppression_score(israel_surplus_vote_agreements, 0.42).
domain_priors:theater_ratio(israel_surplus_vote_agreements, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, extractiveness, 0.38).
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_surplus_vote_agreements, tangled_rope).
narrative_ontology:human_readable(israel_surplus_vote_agreements, "Surplus-Vote Agreements (Bader-Ofer Method)").
narrative_ontology:topic_domain(israel_surplus_vote_agreements, "political/electoral_systems").

domain_priors:requires_active_enforcement(israel_surplus_vote_agreements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_surplus_vote_agreements, coalition_formation_agents).
narrative_ontology:constraint_beneficiary(israel_surplus_vote_agreements, small_parties_with_surplus_votes).
narrative_ontology:constraint_victim(israel_surplus_vote_agreements, excluded_parties).
narrative_ontology:constraint_victim(israel_surplus_vote_agreements, voter_representation_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED PARTY (SNARE) — Parties below threshold or without surplus-vote partners face complete exclusion from seat allocation despite receiving votes. Their voters bear extraction (wasted votes) with no exit option and no representation in seat bargaining. Maximum experienced extraction.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER PARTY (TANGLED ROPE) — Parties just above threshold experience mixed effects. The system enables coalition formation (coordination benefit) but constrains their autonomy through mandatory surplus agreements. They retain some bargaining power (which parties to partner with) but within a structured framework that extracts coalition loyalty. Constrained exit — they must partner or face marginalization.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COALITION ARCHITECT (ROPE) — Major parties and coalition designers benefit from surplus agreements as pure coordination mechanism. The system solves the fragmentation problem: by bundling parties for surplus allocation, it reduces waste and enables predictable coalition math. No extraction experienced — the mechanism serves their interests directly.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTORAL REFORM MOVEMENT (SCAFFOLD) — Reform-oriented political actors and civil society organizations perceive surplus agreements as a temporary coordination patch on a fragmented system. The constraint has a built-in sunset: if and when proportional representation is reformed or electoral thresholds are restructured, surplus agreements become obsolete. The mechanism is functional but acknowledged as transitional.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL COMMISSION (PITON) — The administrative apparatus that implements surplus agreements maintains the constraint through regulatory inertia. The system persists because it is embedded in law and procedure, even as political actors routinely negotiate around it or bypass it with coalition pre-agreements. The functional purpose (reducing waste) is increasingly achieved through backroom coalition negotiations that render the formal surplus-agreement mechanism performative.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global/comparative perspective, surplus agreements represent a hybrid mechanism solving coordination problems (fragmentation, wasted votes) while simultaneously extracting from voters and excluded parties (concentration of seat allocation power, voter waste externality, marginalization of threshold groups). The constraint is neither pure coordination nor pure extraction — it is structurally hybrid.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_surplus_vote_agreements_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_surplus_vote_agreements, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_surplus_vote_agreements, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(israel_surplus_vote_agreements, TR),
    TR >= 0.70.

:- end_tests(israel_surplus_vote_agreements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The surplus-agreement system reduces vote waste through legitimate allocation mechanics (coordination benefit), but simultaneously concentrates power among parties able to negotiate pre-election coalitions. Excluded parties experience total extraction (wasted votes → zero seats). Included small parties experience mixed effects: their votes count, but they lose strategic autonomy to coalition terms. The extractiveness value reflects the asymmetry — it is neither pure coordination (which would require all parties to benefit equally) nor pure extraction (which would eliminate the coordination benefit entirely). The rising trajectory (0.32 → 0.38) indicates increasing strategic sophistication in using surplus agreements to engineer seat distributions beyond vote efficiency. Suppression (0.42): Moderate. Barriers to alternative coalition structures include the legal requirement for pre-election registration, the strategic advantage of early coalition commitment, and the marginalization threat facing unpartnered parties. However, suppression is not extreme — parties can and do negotiate alternative partnerships, and the system permits open negotiation of agreement terms. Theater ratio (0.35): Low. The surplus allocation mechanism itself is transparent and rule-based. The rising theater (0.28 → 0.35) reflects increasing performative coalition marketing — parties may announce agreements for strategic effect or symbolic coalition-building beyond the mechanical surplus calculation.
 *
 * PERSPECTIVAL GAP:
 *   The coalition architect sees pure coordination — a transparent mechanism solving fragmentation. The excluded party sees pure extraction — complete marginalization despite legitimate electoral participation. The mid-tier party sees a mixed mechanism — enabling seat allocation (coordination) while mandating coalition loyalty (extraction). The electoral commission sees regulatory procedure — the administrative reality of implementing allocation rules. The reform movement sees a temporary patch — surplus agreements are pragmatic solutions pending electoral system restructuring. The analytical observer sees the structural hybrid: the same mechanism that reduces waste also concentrates power. This perspectival divergence is not mere interpretation — it reflects real differences in how the constraint operates across the party system. The gap is largest between powerless excluded parties (snare) and institutional coalition architects (rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the party's structural position relative to surplus allocation. Major coalition-forming parties (institutional power, arbitrage exit) experience low or negative extraction — the mechanism serves their coalition needs directly. The beneficiary group is institutionally defined (coalition architects who negotiate agreements). Small parties with surplus votes (moderate power, constrained exit) occupy the hybrid middle — they gain seat allocation but lose autonomy, deriving moderate to high directionality. The victim group (excluded parties and dispersed voters) has no exit — trapped agents with zero coalition bargaining power experience maximum directionality. The constraint's extractiveness is scaled by scope (national) and the sigmoid directionality function across perspectives. The analytical observer perceives the constraint from a civilizational/global comparative viewpoint, revealing the hybrid structure that local perspectives cannot fully see.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The surplus-agreement system is authentically hybrid. It possesses genuine coordination function (addresses fragmentation, reduces vote waste, enables predictable coalition math) AND simultaneous asymmetric extraction (marginalizes threshold groups, concentrates power, imposes coalition loyalty). The constellation of base properties confirms this: extractiveness moderate (0.38, not low like pure coordination, not high like pure extraction); suppression moderate-high (0.42, indicating real constraints but not total coercion); requires_active_enforcement (true, coalition negotiation and allocation must be actively managed); beneficiaries (coalition architects who benefit from predictability); victims (excluded parties who lose votes to waste). The mandatrophy is resolved by recognizing that the constraint serves BOTH functions structurally. It is not mislabeled extraction masquerading as coordination, nor mislabeled coordination that accidentally extracts. It is a intentional hybrid mechanism where coordination enables and justifies the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_manipulation_scope,
    'Does the surplus-agreement system primarily reduce vote waste (coordination benefit) or primarily concentrate power among threshold-adjacent parties (extraction mechanism)?',
    'Comparative time-series analysis of wasted votes before/after surplus agreement formation; seat allocation counterfactual modeling; party entry/exit dynamics in threshold region',
    'If waste reduction dominates: Rope classification strengthens. If power concentration dominates: Snare and Tangled Rope classifications strengthen. Classification outcome depends on empirical surplus magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_manipulation_scope, empirical, 'Whether surplus agreements primarily benefit waste reduction or power concentration').

omega_variable(
    voter_intent_capture,
    'Do voters casting ballots for surplus-linked parties fully understand and consent to the pre-election coalition compact, or are they functionally bound by agreements they did not author?',
    'Voter surveys on awareness of surplus agreements at ballot time; analysis of post-election seat allocation surprise (between expected and actual outcomes); interviews with party leadership on coalition negotiation timing relative to public disclosure',
    'If voters understand and consent: Consent-based coordination (Rope). If voters are surprised by outcomes: Information asymmetry extraction (Snare/Tangled Rope). This omega determines whether suppression is imposed or emergent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voter_intent_capture, empirical, 'Voter awareness and consent regarding surplus agreement coalition compacts').

omega_variable(
    alternative_surplus_mechanisms,
    'Would alternative vote-waste reduction mechanisms (modified thresholds, adjusted divisor methods, open-list voting) produce equivalent coordination benefits without the extraction and marginalization costs?',
    'Comparative electoral system analysis; simulation of alternative methods applied to recent election data; international best-practice review of proportional representation fragmentation solutions',
    'If effective alternatives exist: Tangled Rope or Scaffold (temporary solution). If surplus agreements are near-optimal: Rope (necessary coordination). This omega determines whether the constraint is fundamentally hybrid or contingently so.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_surplus_mechanisms, conceptual, 'Whether alternative waste-reduction mechanisms could replace surplus agreements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_surplus_vote_agreements, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isva_tr_t0, israel_surplus_vote_agreements, theater_ratio, 0, 0.28).
narrative_ontology:measurement(isva_tr_t5, israel_surplus_vote_agreements, theater_ratio, 5, 0.32).
narrative_ontology:measurement(isva_tr_t10, israel_surplus_vote_agreements, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(isva_be_t0, israel_surplus_vote_agreements, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(isva_be_t5, israel_surplus_vote_agreements, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(isva_be_t10, israel_surplus_vote_agreements, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_surplus_vote_agreements, resource_allocation).
narrative_ontology:affects_constraint(israel_surplus_vote_agreements, israeli_electoral_threshold_system).
narrative_ontology:affects_constraint(israel_surplus_vote_agreements, coalition_fragmentation_barrier).

% DUAL FORMULATION NOTE:
% Surplus-vote agreements are structurally downstream of the electoral threshold system and proportional representation fragmentation. They function as a tactical coordination mechanism within the constraint of multi-party fragmentation. The network link captures that threshold effects create the structural problem (vote waste) that surplus agreements partially solve, while simultaneously enabling the extraction (power concentration) that the threshold system permits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israel_surplus_vote_agreements, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
