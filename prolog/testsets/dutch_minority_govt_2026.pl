% ============================================================================
% CONSTRAINT STORY: dutch_minority_govt_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dutch_minority_govt_2026, []).

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
 *   constraint_id: dutch_minority_govt_2026
 *   human_readable: Dutch Minority Government External Support Agreement (2026)
 *   domain: political/parliamentary_governance
 *
 * SUMMARY:
 *   Following fragmented 2025 Dutch elections, the PVV (far-right, largest
 *   single party) cannot form a majority coalition due to widespread cordon
 *   sanitaire — most other parties refuse formal coalition partnership with
 *   the PVV. The structural solution is an external support agreement:
 *   opposition or satellite parties agree to support the PVV-led government
 *   on confidence votes and critical legislation in exchange for policy
 *   concessions and procedural influence, without formally joining the
 *   coalition. This creates a hybrid constraint: it solves the immediate
 *   collective action problem (allowing government formation when coalition
 *   is impossible), but it embeds extraction mechanisms (the supporting
 *   parties must perpetually trade concessions for continued support) and
 *   asymmetric institutional power (the government can threaten electoral
 *   reset). The constraint exhibits all six DR types from different
 *   perspectives, making it diagnostic of how external support agreements
 *   function as selective coordination-plus-extraction hybrids.
 *
 * KEY AGENTS:
 *   - PVV-Led Governing Coalition: Primary beneficiary (institutional/arbitrage) — benefits from external support that enables government formation and policy implementation; can threaten electoral reset if support withdrawn
 *   - Supporting Minority Party (likely one or two mid-sized parties): Primary victim (powerless/trapped) — trapped into perpetual support role; withdrawal risks electoral reset; constrained to perpetual concessions
 *   - Opposition Bloc: Secondary actor (moderate/constrained) — maintains legislative power to block government but cannot credibly use it without risking worse electoral outcome; gain some leverage through support party behavior
 *   - Electoral/Parliamentary Reform Coalition: Organized actors (organized/constrained) — civil society, reform parties, constitutional law experts; frame the support agreement as revealing electoral system pathology requiring reform
 *   - Parliamentary Norms Enforcement Apparatus: Institutional maintenance (institutional/arbitrage) — traditional conventions governing fair coalition formation persist performatively while being structurally bypassed
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent electoral outcomes as immutable properties of proportional representation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dutch_minority_govt_2026, 0.52).
domain_priors:suppression_score(dutch_minority_govt_2026, 0.65).
domain_priors:theater_ratio(dutch_minority_govt_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dutch_minority_govt_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(dutch_minority_govt_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dutch_minority_govt_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dutch_minority_govt_2026, tangled_rope).
narrative_ontology:human_readable(dutch_minority_govt_2026, "Dutch Minority Government External Support Agreement (2026)").
narrative_ontology:topic_domain(dutch_minority_govt_2026, "political/parliamentary_governance").

domain_priors:requires_active_enforcement(dutch_minority_govt_2026).
narrative_ontology:has_sunset_clause(dutch_minority_govt_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dutch_minority_govt_2026, pvv_governing_coalition).
narrative_ontology:constraint_beneficiary(dutch_minority_govt_2026, supporting_parliamentary_groups).
narrative_ontology:constraint_victim(dutch_minority_govt_2026, parliamentary_minority_protections).
narrative_ontology:constraint_victim(dutch_minority_govt_2026, policy_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUPPORTING MINORITY PARTY (SNARE) — Trapped into perpetual support role. Without external support, government collapses and fresh elections follow, carrying existential risk to the supporting party. Cannot credibly threaten withdrawal on specific policies without triggering government failure. Maximum extraction: support party must concede core positions while receiving minimal policy implementation, held hostage by the threat of electoral reset.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PVV-LED GOVERNING COALITION (ROPE) — Experiences the support agreement as coordination mechanism: external support solves the collective action problem of fragmented parliament while allowing the coalition to pursue its agenda. Exit option available (can force elections if support withdrawn) and can shop between potential supporters. Benefits from the agreement's efficiency; sees support as transactional coordination, not subordination.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGISLATIVE OPPOSITION BLOC (TANGLED ROPE) — Experiences mixed coordination and extraction. The support agreement creates a stable legislative counting frame (which opposition party has effective leverage), enabling opposition coordination. But the same agreement removes leverage points: if opposition credibly threatens no-confidence, they trigger elections they may lose worse. Constrained exit: opposition can organize around shared platform but cannot execute threats without accepting electoral risk.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTORAL REFORM MOVEMENT (SCAFFOLD) — Organized agents (reform coalitions, civil society bodies, constitutional law experts) see the support agreement as revealing a temporary pathology in the electoral system. Fragmentation producing ungovernable parliaments is the diagnosed problem; electoral reform (toward higher thresholds, mixed-member proportionality, or constructive no-confidence rules) is the sunset mechanism. The support agreement itself becomes leverage for reform: 'this gridlock proves we need better rules.' Theater low because the reform constituency openly frames this as systemic dysfunction requiring architectural change, not as normal governance.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PARLIAMENTARY NORMS ENFORCEMENT SYSTEM (PITON) — Traditional norms governing coalition formation and support arrangements (informal expectations of transparency, reciprocity, procedural fairness) are maintained performatively while being structurally bypassed. The 'support agreement' is a formal artifact meant to legitimize what would have been considered illegitimate (contingent governance) under previous constitutional conventions. The norm-enforcement apparatus (parliament's internal procedures, media scrutiny of coalition fairness, civil society accountability mechanisms) persists but with attenuated force — maintained by institutional inertia while the actual governing logic has shifted. Theater high because extensive procedural performance substitutes for substantive fairness.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN CLAIM) — From a civilizational perspective, parliamentary fragmentation producing minority government requiring external support is sometimes framed as an immutable law of proportional representation: 'proportional systems inherently produce fragmentation, fragmentation produces minority governments, minority governments require external support.' This perspective naturalizes the constraint. However, the structural data contradicts the mountain classification: this is a contingent outcome of specific electoral rules (threshold, district magnification, party strategy), not an irreducible limit of representation itself. Other proportional systems (Germany, Denmark) produce coalitions without external support arrangements. The 'natural law' framing masks a policy choice.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dutch_minority_govt_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dutch_minority_govt_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dutch_minority_govt_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dutch_minority_govt_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dutch_minority_govt_2026, TR),
    TR >= 0.70.

:- end_tests(dutch_minority_govt_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The governing coalition captures significant benefits (agenda control, legislative priority, ability to shape policy outcome space) while the supporting party bears costs (perpetual pressure to support government positions on critical votes despite preference disagreement, inability to signal independence or build alternative coalitions). The supporting party can extract some concessions (committee positions, policy promises, procedural influence) but lacks credible threat capacity sufficient to equalize the relationship. If the supporting party had high confidence it could win seats in an electoral reset, extractiveness would drop to 0.35-0.40 (moving toward pure Rope). Current Dutch polling suggests supporting parties lose in reset scenarios, trapping them in high-extraction positions. Suppression (0.65): High. The supporting parties face severe suppression of alternatives: formal coalition is impossible due to cordon sanitaire; direct opposition is suicidal (triggers electoral reset); media narrative frames support as either patriotic necessity or complicity in extremism, constraining their narrative space. No institutional mechanisms exist for supporting parties to credibly execute exit threats. Theater ratio (0.58): Moderate. The support agreement is partially performative (extensive procedural justification, norms-language about 'supporting democracy') and partially functional (the agreement genuinely enables government operation that could not otherwise exist). The theater has increased over the initial period as procedural complexity and normative justification accumulate around what is fundamentally a power-asymmetry arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The PVV's Rope classification (pure coordination, efficient solution) directly contradicts the supporting party's Snare classification (pure extraction, forced compliance). This gap is not measurement disagreement but structural: the two perspectives occupy opposite positions relative to the extraction flow. The same external support agreement appears as beneficial coordination to the beneficiary and coercive extraction to the victim. The opposition's Tangled Rope classification sits between these: the opposition faces both the coordination problem (parliament is fragmented) and extraction (their leverage is constrained). The reform movement's Scaffold classification introduces a time dimension absent from the other perspectives: external support is legitimate only as a temporary pathology revealing the need for electoral reform. The piton classification of traditional parliamentary norms reflects that fairness procedures persist even as fairness itself has degraded — the apparatus maintains itself through institutional inertia. The false mountain classification at the civilizational level shows how proportional representation systems are sometimes blamed for outcomes that are actually contingent policy choices: the cordon sanitaire (refusal to work with the PVV) is a political choice, not a logical consequence of proportional voting.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from structural position within the constraint. The governing coalition (beneficiary, institutional power, arbitrage exit) has low d (~0.20): they benefit from the arrangement and can exit via electoral reset if needed. The supporting party (victim, powerless or moderate power, trapped or constrained exit) has high d (~0.85-0.95): they bear asymmetric costs and lack credible exit options. The opposition (moderate power, constrained exit, neither pure beneficiary nor victim) has mid-range d (~0.60-0.70): they maintain some leverage through support party behavior but cannot directly execute threats. The reform coalition (organized power, constrained exit, long-term perspective) has moderate d (~0.50): they see both the dysfunction (victim-like) and the opportunity for systemic change (beneficiary-like). The analytical observer (analytical power, analytical exit, civilizational scope) has mid-high d (~0.72): the frame can either naturalize (benefiting elites) or reveal (benefiting reform), depending on which narrative dominates.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The support agreement is classified as Tangled Rope because it simultaneously exhibits coordination function (solves the governance problem) and asymmetric extraction (concentrates power in the governing coalition, extracts compliance from supporting parties). The mandatrophy arises from the question: 'Is this coordination with fairness constraints (Rope) or coordination with embedded extraction (Tangled Rope)?' The classification resolves mandatrophy by declaring both: coordination (genuine, benefits all parties relative to no-government baseline) and extraction (genuine, benefits governing coalition more than supporting parties, uses government collapse threat as enforcement mechanism). The Tangled Rope classification requires three elements: (1) genuine coordination function — external support does solve the parliamentary fragmentation problem (✓ beneficiaries declared as 'pvv_governing_coalition' and 'supporting_parliamentary_groups'); (2) asymmetric extraction — supporting parties bear disproportionate costs (✓ victims declared as 'parliamentary_minority_protections' and 'policy_coherence'); (3) active enforcement — the threat of electoral reset enforces support party compliance (✓ requires_active_enforcement: true). The Scaffold perspective (with sunset clause) suggests a potential resolution path: if electoral reform occurs, the support agreement becomes unnecessary and the extraction mechanism loses structural force. The false mountain at civilizational level reveals how systems theorists sometimes naturalize contingent policy outcomes (electoral thresholds, cordon sanitaire norms, coalition expectations) as immutable properties of proportional representation, when they are actually choices that could be unmade.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    support_agreement_credibility,
    'Will the supporting party(ies) actually execute threats to withdraw support, or is the threat fully non-credible (making extraction one-way)?',
    'Empirical observation of support party behavior under policy disagreement; willingness to risk government collapse on specific legislative votes; rate of actual defection or conditional support',
    'If threats are credible: extractiveness drops to 0.35-0.40, tangled rope from multiple perspectives. If threats are non-credible: extractiveness rises to 0.65+, snare classification from support party perspective hardens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(support_agreement_credibility, empirical, 'Whether supporting party can credibly threaten to withdraw support').

omega_variable(
    electoral_reset_risk_assessment,
    'If government collapses and elections are called, will the supporting party lose seat share relative to current parliament, or gain?',
    'Pre-election polling trends; comparative advantage analysis (which parties benefit from electoral reset); historical precedent from 2012-2017 cycles',
    'If supporting party gains in reset: exit option becomes mobile, extraction drops, classification shifts toward Rope from support perspective. If supporting party loses: exit remains trapped, extraction stays high, Snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_reset_risk_assessment, empirical, 'Electoral outcomes if government collapses').

omega_variable(
    opposition_defection_likelihood,
    'Can the governing coalition credibly break opposition unity by offering enough opposition members high-confidence escape routes (judicial appointments, committee positions, policy concessions) to produce an alternative majority?',
    'Tracking of defection offers and rates; comparison with historical Dutch coalition-switching patterns; strength of opposition party discipline',
    'If defection is high-risk and unlikely: opposition maintains cohesion, support agreement becomes necessary for stability, extraction sustained. If defection is likely: support agreement becomes optional, extraction decreases, government gains arbitrage options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opposition_defection_likelihood, empirical, 'Whether opposition defection can replace external support').

omega_variable(
    reform_timeline_credibility,
    'Will electoral reform actually occur within the current government''s term, or is the reform sunset purely aspirational?',
    'Tracking of reform proposal advancement through parliament; calendar timeline for constitutional amendment process; government commitment (legislative scheduling, coalition member positions)',
    'If reform is scheduled and achievable: Scaffold classification confirmed, sunset clause is structural. If reform is deferred beyond this government or contingent on future coalitions: Scaffold becomes aspirational (should be Tangled Rope or Snare), theater increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_timeline_credibility, empirical, 'Whether electoral reform will actually occur').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dutch_minority_govt_2026, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dutch_min_tr_t0, dutch_minority_govt_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dutch_min_tr_t2, dutch_minority_govt_2026, theater_ratio, 2, 0.5).
narrative_ontology:measurement(dutch_min_tr_t4, dutch_minority_govt_2026, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(dutch_min_be_t0, dutch_minority_govt_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dutch_min_be_t2, dutch_minority_govt_2026, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(dutch_min_be_t4, dutch_minority_govt_2026, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dutch_minority_govt_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(dutch_minority_govt_2026, dutch_electoral_fragmentation).
narrative_ontology:affects_constraint(dutch_minority_govt_2026, european_far_right_legitimation).

% DUAL FORMULATION NOTE:
% The support agreement is downstream of electoral fragmentation (itself downstream of voting rules and party strategy) but represents a distinct constraint on parliamentary governance. Electoral fragmentation produces the condition requiring external support; the support agreement is the institutional response that creates extraction mechanisms. They are linked but structurally distinct: fragmentation has ε=0.25 (a property of the electoral system itself, relatively immutable); support agreements have ε=0.52 (a contingent governance choice with higher degrees of freedom).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dutch_minority_govt_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
