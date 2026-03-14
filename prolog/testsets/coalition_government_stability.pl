% ============================================================================
% CONSTRAINT STORY: coalition_government_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coalition_government_stability, []).

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
 *   constraint_id: coalition_government_stability
 *   human_readable: Coalition Government Stability Constraint
 *   domain: political_economy/institutional_governance
 *
 * SUMMARY:
 *   Coalition government stability represents a hybrid constraint that
 *   simultaneously coordinates multiparty governance and extracts rents from
 *   excluded parties and voters. The constraint operates across multiple
 *   institutional levels: voters who select party portfolios, parties who
 *   negotiate coalitions, kingmaker parties who hold veto power, and the
 *   formal coalition agreement mechanisms that ritualize coordination. The
 *   extractiveness has increased over the measurement interval (0.42 → 0.58)
 *   as kingmaker parties have learned to weaponize coalition fragility for
 *   policy concessions. The theater ratio (0.65) reflects divergence between
 *   formal coalition agreements and actual policy implementation — parties
 *   publicly commit to coordination platforms they privately subvert through
 *   parliamentary maneuvering. The constraint exhibits genuine coordination
 *   benefits (preventing electoral chaos and enabling minority
 *   representation) while simultaneously suppressing the electoral mandates
 *   of excluded parties and distorting voter-intended policy outcomes through
 *   kingmaker extraction.
 *
 * KEY AGENTS:
 *   - Coalition Leadership (Majority Parties): Primary beneficiary (institutional/arbitrage) — forms government, distributes ministerial portfolios, controls agenda
 *   - Kingmaker Party: Secondary beneficiary (powerful/mobile) — holds pivotal vote; extracts disproportionate concessions through collapse threat
 *   - Voter Coalition: Primary victim (moderate/constrained) — receives stability coordination benefit but suffers mandate dilution and policy override
 *   - Excluded Minority Parties: Secondary victim (powerless/trapped) — cannot exit electoral system; face systematic exclusion from power despite significant vote share
 *   - Policy Coherence (Abstract Collective): Tertiary victim (powerless/trapped) — government policy reflects coalition compromise rather than any coherent mandate
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to proportional representation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coalition_government_stability, 0.58).
domain_priors:suppression_score(coalition_government_stability, 0.48).
domain_priors:theater_ratio(coalition_government_stability, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coalition_government_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(coalition_government_stability, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(coalition_government_stability, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coalition_government_stability, tangled_rope).
narrative_ontology:human_readable(coalition_government_stability, "Coalition Government Stability Constraint").
narrative_ontology:topic_domain(coalition_government_stability, "political_economy/institutional_governance").

domain_priors:requires_active_enforcement(coalition_government_stability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coalition_government_stability, coalition_leadership).
narrative_ontology:constraint_beneficiary(coalition_government_stability, median_party_kingmakers).
narrative_ontology:constraint_victim(coalition_government_stability, excluded_minority_parties).
narrative_ontology:constraint_victim(coalition_government_stability, voter_mandate_fidelity).
narrative_ontology:constraint_victim(coalition_government_stability, policy_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MINORITY PARTY (SNARE) — Trapped within electoral system; cannot exit short of franchise collapse. Bears full extraction via exclusion from power despite significant vote share. Kingmaker parties extract disproportionate concessions while maintaining veto threat. Suppression is structural: electoral system prevents third-party viability, and coalition arithmetic forces exclusion as punishment mechanism.
constraint_indexing:constraint_classification(coalition_government_stability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VOTER COALITION (TANGLED ROPE) — Receives coordination benefit (stable government replaces electoral chaos) but bears extraction through mandate dilution. Coalition agreements override voter intent on specific policies. Constrained exit: voters cannot easily coordinate against both major parties simultaneously, and defection to third parties carries spoiler risk. Genuine coordination function (stability) coexists with asymmetric extraction (policy divergence from what voters actually preferred).
constraint_indexing:constraint_classification(coalition_government_stability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COALITION LEADERSHIP (ROPE) — Primary beneficiaries. Experience constraint as pure coordination mechanism: coalition agreement enables government formation that individual parties could not achieve alone. Arbitrage exit (can form alternative coalitions or minority governments). Extraction flows toward this agent; they perceive the constraint as mutually beneficial bargaining rather than exploitation.
constraint_indexing:constraint_classification(coalition_government_stability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: KINGMAKER PARTY (TANGLED ROPE) — Holds pivotal position in coalition arithmetic; genuinely solves coordination problem (government wouldn't form without them) but also extracts rents through disproportionate policy concessions and ministerial portfolios. Mobile exit (could dissolve coalition and force new elections, shifting kingmaker status). Extraction mechanism is the credible threat to collapse government — benefits from maintaining instability risk as bargaining tool.
constraint_indexing:constraint_classification(coalition_government_stability, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: COALITION AGREEMENT RITUAL (PITON) — The formal coalition agreement and joint government program persist as institutional theater. Much of the documented coordination (written policy platforms, ministerial divisions) is performative; real coordination happens through behind-the-scenes negotiation. Theater ratio high (public agreements diverge from actual policy implementation). The ritual maintains legitimacy appearance despite degraded function — parties see their own agreements as frequently honored only in breach.
constraint_indexing:constraint_classification(coalition_government_stability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, coalition instability is inherent to multiparty systems: multiple parties with incompatible platforms must always negotiate, creating permanent potential for collapse. This perspective sees the constraint as an immutable property of proportional representation systems themselves. However, structural data contradicts mountain classification — variation across coalition systems and countries reveals the instability is contingent on political culture, veto player count, and institutional design, not a natural law.
constraint_indexing:constraint_classification(coalition_government_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coalition_government_stability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coalition_government_stability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coalition_government_stability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coalition_government_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coalition_government_stability, TR),
    TR >= 0.70.

:- end_tests(coalition_government_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The kingmaker party mechanism extracts rents through three channels: (1) disproportionate ministerial portfolios relative to vote share, (2) policy concessions on specific issues where they hold veto power, (3) maintenance of coalition fragility as a bargaining asset. However, extractiveness is not as severe as a pure snare (which would be 0.75+) because genuine coordination benefits exist — coalitions do prevent electoral stalemate and enable minority representation. The measurement trajectory shows accumulation of extraction over time (0.42 → 0.58) as parties refine strategies for leveraging kingmaker position. Suppression (0.48): Moderate. Excluded parties face structural barriers to inclusion (arithmetic of multiparty systems), but suppression is not total — parties can move between coalition and opposition, and voter turnout to third parties remains possible (though costly). Theater ratio (0.65): Moderate-high. Coalition agreements are performative — they create appearance of coordinated governance that actual parliamentary behavior often undermines. Party discipline within coalitions weakens as individual members extract rents from the coalition's survival requirement. The theater has increased over time as the gap between public commitments and private implementation has widened.
 *
 * PERSPECTIVAL GAP:
 *   Coalition stability exhibits maximum perspectival divergence across the six types: Rope (leadership), Tangled Rope (kingmakers and voters), Snare (excluded minorities), Piton (institutional ritual), Mountain (natural law view), and Snare (voter mandate). The gap between leadership (Rope) and excluded minorities (Snare) is maximal — they experience opposite classification outcomes from the same constraint. This gap reveals the fundamental asymmetry: governance coordination requires negotiation, but negotiation produces asymmetric extraction. The mountain perspective's false summit (naturalizing coalition instability as inherent) is revealed through the omega variables: cross-national variation and constitutional design contingency demonstrate the instability is institutional, not natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship: beneficiaries (coalition leaders, kingmakers) with arbitrage/mobile exit options experience low d (negative effective extraction — they benefit). Voters with constrained exit experience moderate d (they exit at cost through defection or non-voting). Excluded minorities with trapped exit experience high d (maximum extraction — they cannot exit the electoral system). The pipeline computes d automatically from beneficiary/victim status and exit capacity. Kingmakers occupy an intermediate position (d ≈ 0.40-0.50) because they simultaneously create and exploit the coordination problem — their exit option is mobile (can collapse coalition) but its exercise is costly (electoral competition re-opens, kingmaker status disappears). This ambiguity is captured in the kingmaker omega variable.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is between the coordination function (multiparty governance requires negotiation) and the extraction mechanism (negotiation produces kingmaker rents and mandate dilution). The constraint resolves mandatrophy by showing that both functions are real and coexistent: coalitions genuinely coordinate multiparty systems AND kingmakers genuinely extract rents. The distinction is not 'which function is real?' but 'how are extraction and coordination coupled?' The coupling is tight: the more fragmented the multiparty system (more potential kingmakers), the greater the extraction. The less fragmented (fewer kingmakers), the greater the extraction concentrates. No design escapes both functions simultaneously. The mandatrophy is irreducible unless the system is decomposed into separate stories: (1) multiparty coordination (ε ≈ 0.15, Rope), and (2) kingmaker rent extraction (ε ≈ 0.65, Snare). The tangled_rope classification (0.58) represents the empirical entanglement of these two mechanisms in actual coalition governments.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kingmaker_extraction_vs_coordination,
    'Is kingmaker party leverage a necessary coordination cost or pure extractive rent-seeking?',
    'Comparative analysis: measure policy concessions to kingmaker parties across multiple coalitions; correlate with whether coalition would form without them. If kingmakers are structurally necessary, some leverage is genuine coordination cost. If coalitions could form without them, leverage is rent extraction.',
    'If coordination: reclassify kingmaker perspective as Rope with high but justified asymmetry. If extraction: kingmaker perspective becomes Snare, increasing overall system extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kingmaker_extraction_vs_coordination, empirical, 'Whether kingmaker leverage is necessary coordination cost or pure extraction').

omega_variable(
    mandate_dilution_measurement,
    'How much of the policy divergence between coalition output and voter intent reflects necessary coordination compromise versus deliberate mandate overriding?',
    'Pre-election voter surveys on policy preferences; post-government implementation tracking; regression analysis of coalition agreement content vs actual legislation.',
    'If mostly compromise: extraction is moderate and justified; snare classification of excluded parties may be unfair. If deliberate override: extraction is asymmetric; victim classification of voters is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_dilution_measurement, empirical, 'Proportion of mandate dilution from compromise vs deliberate override').

omega_variable(
    coalition_collapse_trigger_mechanism,
    'Does coalition collapse result from genuine coordination failure or from intentional veto player destruction of coalition for electoral advantage?',
    'Historical analysis of coalition breakdown narratives; game-theoretic modeling of incentive structures; comparison of public rationales vs private incentives (where documentary evidence exists).',
    'If genuine failure: constraint is structural coordination problem. If intentional: constraint is partly extraction mechanism disguised as instability. Affects whether stabilization mechanisms are feasible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_collapse_trigger_mechanism, empirical, 'Whether coalition collapse is coordination failure or intentional extraction').

omega_variable(
    institutional_design_contingency,
    'Is coalition instability inherent to proportional representation or contingent on specific institutional design choices?',
    'Comparative cross-national analysis: stability rates across PR systems with different thresholds, runoff rules, investiture procedures, and dissolution powers. Analysis of whether constitutional reforms increase or decrease stability.',
    'If contingent: mountain classification is false summit; constraint is decomposable into design-choice stories, not a natural law. If inherent: mountain classification is correct; constraint is unavoidable feature of PR systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_design_contingency, empirical, 'Whether coalition instability is inherent or contingent on institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coalition_government_stability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coal_tr_t0, coalition_government_stability, theater_ratio, 0, 0.55).
narrative_ontology:measurement(coal_tr_t10, coalition_government_stability, theater_ratio, 10, 0.62).
narrative_ontology:measurement(coal_tr_t20, coalition_government_stability, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(coal_be_t0, coalition_government_stability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(coal_be_t10, coalition_government_stability, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(coal_be_t20, coalition_government_stability, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coalition_government_stability, enforcement_mechanism).
narrative_ontology:affects_constraint(coalition_government_stability, electoral_system_threshold_effects).
narrative_ontology:affects_constraint(coalition_government_stability, minority_party_viability).

% DUAL FORMULATION NOTE:
% Coalition stability decomposes into two structurally distinct constraints: (1) multiparty_coordination (ε ≈ 0.15, Rope) — the genuine problem of forming governments from multiple parties, and (2) kingmaker_rent_extraction (ε ≈ 0.65, Snare) — the mechanism by which pivotal parties extract policy concessions. This story represents their empirical entanglement in actual coalition systems. Separate stories with network links would enable precise measurement of which institutional reforms target coordination vs extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coalition_government_stability, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
