% ============================================================================
% CONSTRAINT STORY: israel_surplus_vote_agreements
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: political/electoral
 *
 * SUMMARY:
 *   The Bader-Ofer surplus-vote agreement system in Israeli elections
 *   exemplifies a hybrid constraint that combines genuine coordination
 *   (helping small parties cross the threshold) with extractive outcomes
 *   (capturing marginal votes, disadvantaging excluded competitors,
 *   concentrating coalition-forming power). Two parties sign a pre-election
 *   agreement to pool surplus votes and allocate them as a single list if
 *   either fails to clear the electoral threshold independently.
 *   Structurally, the agreement solves the fragmentation problem inherent in
 *   proportional representation: small parties can guarantee Knesset seats by
 *   aligning with larger partners. But it also functions as an extraction
 *   mechanism: voter votes can be absorbed into coalition negotiations,
 *   marginal parties lose autonomy, and the coalition-forming majority gains
 *   leverage. The constraint has intensified over decades as Israeli politics
 *   has become more fragmented and coalition-building more dominant
 *   (theater_ratio rising from 0.35 to 0.58). The suppression reflects real
 *   barriers: a small party cannot withdraw from an agreement without risking
 *   failure to enter the Knesset; voters cannot recover their representation
 *   once absorbed into a coalition bloc.
 *
 * KEY AGENTS:
 *   - Marginal Voters: Victims (powerless/trapped) — cast ballots for small parties unaware of or unable to prevent surplus-vote absorption; no exit option once vote is cast
 *   - Small Coalition Parties: Mixed (moderate/constrained) — benefit from agreement (guaranteed Knesset seats) but lose negotiating power and autonomy; constrained exit (withdrawal risks threshold failure)
 *   - Coalition-Forming Majority: Primary beneficiary (institutional/arbitrage) — gains predictable post-election allies, efficient vote pooling, coalition numerics guarantees; arbitrage available (can negotiate with multiple partners)
 *   - Excluded Parties: Victims (moderate/mobile) — parties not in surplus agreements are disadvantaged when larger competitors pool votes; some mobility (can form rival coalitions or campaign for threshold-crossing legitimacy)
 *   - Electoral Reform Coalition: Organized reformers (organized/mobile) — civil society, think tanks, and reform-minded politicians see surplus agreements as a workaround to be phased out through systemic reform (threshold adjustment, open lists); exit path available
 *   - Central Elections Committee: Institutional administrator (institutional/constrained) — maintains mechanism through legal framework; constrained by path dependency (changing electoral system requires legislation); piton perspective
 *   - Analytical Observer: Sees false natural law (analytical/analytical) — risks naturalizing coalition fragmentation as inherent rather than contingent institutional outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_surplus_vote_agreements, 0.38).
domain_priors:suppression_score(israel_surplus_vote_agreements, 0.52).
domain_priors:theater_ratio(israel_surplus_vote_agreements, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, extractiveness, 0.38).
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_surplus_vote_agreements, tangled_rope).
narrative_ontology:human_readable(israel_surplus_vote_agreements, "Surplus-Vote Agreements (Bader-Ofer Method)").
narrative_ontology:topic_domain(israel_surplus_vote_agreements, "political/electoral").

domain_priors:requires_active_enforcement(israel_surplus_vote_agreements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_surplus_vote_agreements, smaller_coalition_parties).
narrative_ontology:constraint_beneficiary(israel_surplus_vote_agreements, coalition_forming_majorities).
narrative_ontology:constraint_victim(israel_surplus_vote_agreements, marginal_voters).
narrative_ontology:constraint_victim(israel_surplus_vote_agreements, excluded_parties).
narrative_ontology:constraint_victim(israel_surplus_vote_agreements, proportional_representation_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL VOTER (SNARE) — A voter casting a ballot for a small party that fails to meet the threshold or whose votes are absorbed into a coalition agreement has no exit option. The surplus-vote agreement functions as a trap: the voter cannot recover their representation once committed. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52. High extraction from voters whose party was used as a stepping stone.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL COALITION PARTY (TANGLED ROPE) — Experiences the agreement as coordination (pooling votes to cross the threshold) but also as extraction (subordination to larger partner, loss of negotiating power post-election, potential absorption into government without cabinet seats). Constrained exit: leaving the agreement risks failing to enter Knesset. d≈0.62, f(d)≈0.88, σ=1.0 → χ≈0.33. Mixed coordination and extraction.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COALITION-FORMING MAJORITY (ROPE) — Larger parties entering surplus agreements experience coordination: efficiently consolidating bloc votes, securing predictable post-election allies, and guaranteeing coalition numerics. Arbitrage available: can form agreements with multiple partners, shop for optimal terms. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.03. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTORAL REFORM COALITION (SCAFFOLD) — Organized civil society groups and reformers see the surplus-vote agreement as a temporary workaround — a coordination mechanism that addresses fragmentation but with a built-in sunset as electoral reform (threshold adjustment, open-list systems, or direct proportionality) matures. d≈0.35, f(d)≈0.33, σ=1.0 → χ≈0.13. Low effective extraction because reformers have agency and see an alternative pathway.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL COMMISSION (PITON) — The electoral machinery (Israel's Central Elections Committee) maintains the surplus-vote mechanism largely through institutional inertia. Functionally, the mechanism solves fragmentation, but the commission's role is increasingly performative: administering rules that parties circumvent (through informal understandings, post-election defections, or alliance reshuffling). theater_ratio=0.58 reflects moderate performative content. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.28.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a civilizational view, surplus agreements might appear as an immutable response to the inherent tension between proportional representation and coalition formation. But the structural data (ε=0.38, suppression=0.52) contradicts the mountain gate. The constraint is not a law of nature — it is a contingent institutional design choice. The false summit reveals that 'inevitable fragmentation' naturalizes what is actually extractive coalition engineering.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.38): Moderate-high. The surplus-vote agreement system extracts value from marginal voters by absorbing their votes into coalition blocs. The extraction is not total (it is legal, disclosed, and parties benefit from guaranteed Knesset seats) but significant. Small parties gain Knesset representation they could not achieve alone, but lose autonomy and negotiating power — a trade-off that resembles extraction. Over the interval, extractiveness has risen from 0.22 to 0.38 as Israeli politics became more fragmented, making agreements more frequent and coalition leverage greater. Suppression (0.52): Moderate-high. Barriers to escaping the system include: (1) small parties cannot viably compete without agreements due to fragmentation, (2) voters cannot withdraw votes after casting them, (3) transparency and enforcement of agreement terms is limited, (4) post-election defection from agreements is possible but costly. Theater ratio (0.58): Moderate-high. The electoral process preserves performative legitimacy: parties publicly campaign as independent entities while secretly negotiating surplus agreements; voters are told they are choosing a party while their votes are being pooled into blocs; the electoral commission administers a system that parties circumvent through informal understandings. Theater has increased over time as coalition engineering has become more sophisticated and less transparent.
 *
 * PERSPECTIVAL GAP:
 *   The marginal voter sees a snare (trapped, no exit, extraction). The small coalition party sees tangled rope (coordination benefit offset by extraction cost). The majority sees rope (pure coordination). The reformer sees a temporary scaffold with a sunset. The electoral commission sees a piton (degraded mechanism sustained by inertia). The civilizational observer risks seeing a natural law of proportional systems but is corrected by the structural data: the system's extractiveness (0.38) and suppression (0.52) reveal contingent design choices, not immutable laws. The perspectival gap is primarily between the beneficiary (majority) and the victim (marginal voter), mediated by the small coalition party's ambiguous position.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginal Voters: Victim + trapped → d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52. Maximum extraction. Votes are absorbed without consent or recovery mechanism. Small Coalition Parties: Beneficiary + constrained → d≈0.62, f(d)≈0.88, σ=1.0 → χ≈0.33. Mixed because they benefit (guaranteed Knesset seats) but are constrained (cannot exit without losing seats). Coalition-Forming Majority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.03. Net beneficiary with negative effective extraction — they experience coordination. Excluded Parties: Victim + mobile → d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40. Harmed by competitors' agreements but mobile enough to form rival coalitions. Electoral Reform Coalition: Organized + mobile → d≈0.35, f(d)≈0.33, σ=1.0 → χ≈0.13. Low effective extraction; reformers have agency. Electoral Commission: Institutional + constrained → d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.22. Piton classification comes from theater_ratio gate (≥0.70 not met, but performative content is notable).
 *
 * MANDATROPHY ANALYSIS:
 *   The Bader-Ofer system resolves mandatrophy by clarifying what is coordination and what is extraction. The coordination function is genuine: small parties that would fail the threshold independently do benefit from pooling votes to reach the Knesset, and larger parties efficiently consolidate support. But this coordination is contaminated by extraction: marginal voters are not informed of vote pooling, post-election defections from agreements undermine negotiating commitments, and coalition-forming majorities use agreements as leverage rather than principled partnerships. The tangled rope classification confirms both are present. The false summit (mountain perspective) is ruled out: the constraint is not a law of fragmentation but a design choice that could be reformed (lower threshold, open lists, proportional adjustments) to reduce extraction while maintaining coordination. The theater ratio increasing from 0.35 to 0.58 indicates growing performative content — Goodhart drift where the electoral process's legitimating function (appearing democratic and transparent) has been decoupled from its coordination function (actually allocating seats fairly). Reform scenarios (lower threshold, mandatory transparency, pre-election voter notification) would reduce theater and extractiveness together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voter_intent_capture,
    'Does a surplus-vote agreement represent voter intent (voting as a bloc for coalition purposes) or voter capture (voting for a small party believing it retains autonomy)?',
    'Post-election survey data on voter awareness of surplus agreements; comparison of stated coalition preferences vs actual agreement terms; analysis of party messaging during campaign',
    'If voter intent: agreement is coordination (rope-like). If voter capture: agreement is extraction (snare-like). Changes victim classification from ''excluded_parties'' to ''voters_denied_proportional_outcome''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voter_intent_capture, empirical, 'Whether surplus agreements represent voter intent or voter capture').

omega_variable(
    post_election_defection_rate,
    'How frequently do coalition partners defect from post-election arrangements negotiated via surplus agreements?',
    'Historical analysis of coalition governments: parties that signed surplus agreements, their negotiating position post-election, rate of cabinet-seat allocation, and defection to opposition during government term',
    'High defection rate (>30%): agreement extraction is enforceable only by threat/surprise, raising suppression. Low defection rate (<10%): agreement functions as genuine coordination, lowering effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_election_defection_rate, empirical, 'Rate of post-election defection from surplus-vote coalition arrangements').

omega_variable(
    threshold_pressure_mechanism,
    'Does the 3.25% electoral threshold (or prior variants) function as a natural filtering mechanism or as an extractive tool that surplus agreements circumvent?',
    'Comparison of election outcomes under different threshold levels; analysis of surplus agreements as threshold-bypass mechanism; demographic analysis of voters whose votes are ''lost'' to threshold failures',
    'If threshold is natural filter: surplus agreements coordinate to cross it legitimately. If threshold is artificial barrier: surplus agreements are extraction workaround. Changes ε valuation and suppression assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_pressure_mechanism, empirical, 'Whether electoral threshold is natural or extractive mechanism').

omega_variable(
    transparency_enforcement_gap,
    'Are surplus-vote agreements disclosed to voters in sufficient detail before the election, and what enforcement mechanisms ensure agreement terms are honored post-election?',
    'Audit of party websites, media disclosures, and legal filings for agreement terms; analysis of Central Elections Committee enforcement capacity; case studies of disputed or violated agreements',
    'High transparency + enforcement: suppression and theater_ratio lower (more like rope). Low transparency + weak enforcement: suppression and theater_ratio higher (more like snare). Directly affects classification confidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transparency_enforcement_gap, empirical, 'Transparency and enforcement of surplus-vote agreement terms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_surplus_vote_agreements, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surp_tr_t0, israel_surplus_vote_agreements, theater_ratio, 0, 0.35).
narrative_ontology:measurement(surp_tr_t5, israel_surplus_vote_agreements, theater_ratio, 5, 0.5).
narrative_ontology:measurement(surp_tr_t10, israel_surplus_vote_agreements, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(surp_be_t0, israel_surplus_vote_agreements, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(surp_be_t5, israel_surplus_vote_agreements, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(surp_be_t10, israel_surplus_vote_agreements, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_surplus_vote_agreements, resource_allocation).
narrative_ontology:affects_constraint(israel_surplus_vote_agreements, israeli_coalition_fragmentation).
narrative_ontology:affects_constraint(israel_surplus_vote_agreements, electoral_threshold_mechanism).
narrative_ontology:affects_constraint(israel_surplus_vote_agreements, proportional_representation_fidelity).

% DUAL FORMULATION NOTE:
% The Bader-Ofer surplus-vote agreement is downstream of two structural constraints: (1) electoral fragmentation (multiple parties competing for representation), and (2) the need for coalition formation to reach government. Upstream, the 3.25% threshold mechanism filters parties, and upstream of that, the proportional representation principle ideally allocates seats by vote share. The surplus-vote agreement exists in the gap between fragmentation and coalition necessity. If the upstream constraint is 'fragmentation is unavoidable in Israeli politics' (ε could be high), the surplus-vote system inherits that pressure. Each upstream constraint has its own structural status; the agreement is a secondary manifestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israel_surplus_vote_agreements, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
