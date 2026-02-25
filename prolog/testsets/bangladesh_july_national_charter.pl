% ============================================================================
% CONSTRAINT STORY: bangladesh_july_national_charter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bangladesh_july_national_charter, []).

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
 *   constraint_id: bangladesh_july_national_charter
 *   human_readable: The July National Charter Referendum
 *   domain: political/constitutional_reform
 *
 * SUMMARY:
 *   Following the July 2024 uprising in Bangladesh, an interim government led
 *   by Muhammad Yunus proposed a 'July National Charter' to enact sweeping
 *   reforms. This charter, containing over 80 distinct constitutional
 *   amendments, was presented to the electorate as a single, binary 'Yes/No'
 *   referendum held alongside the 2026 general election. The core of this
 *   constraint is the bundling of numerous, complex issues into an
 *   indivisible package, forcing voters and political actors to accept or
 *   reject the entire reform agenda without nuance.
 *
 * KEY AGENTS:
 *   - Interim Government: Primary beneficiary (institutional/arbitrage) — sees the referendum as a necessary coordination tool to enact its reform agenda.
 *   - Individual Voters: Primary victim (powerless/trapped) — their ability to express nuanced political preference is extracted by the binary choice.
 *   - Incoming Elected Government (BNP): Institutional actor (institutional/constrained) — constrained by the popular mandate of a referendum it may not fully endorse.
 *   - Pro-Reform Coalition: Beneficiary (organized/mobile) — parties that view the charter as a vehicle for desired collective action.
 *   - Constitutional Deliberative Process: Abstract victim (powerless/trapped) — the quality of constitutional change is diminished by bypassing granular debate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bangladesh_july_national_charter, 0.55).
domain_priors:suppression_score(bangladesh_july_national_charter, 0.75).
domain_priors:theater_ratio(bangladesh_july_national_charter, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bangladesh_july_national_charter, extractiveness, 0.55).
narrative_ontology:constraint_metric(bangladesh_july_national_charter, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bangladesh_july_national_charter, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bangladesh_july_national_charter, tangled_rope).
narrative_ontology:human_readable(bangladesh_july_national_charter, "The July National Charter Referendum").
narrative_ontology:topic_domain(bangladesh_july_national_charter, "political/constitutional_reform").

domain_priors:requires_active_enforcement(bangladesh_july_national_charter).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bangladesh_july_national_charter, interim_government).
narrative_ontology:constraint_beneficiary(bangladesh_july_national_charter, reform_proponents_coalition).
narrative_ontology:constraint_victim(bangladesh_july_national_charter, individual_voters).
narrative_ontology:constraint_victim(bangladesh_july_national_charter, dissenting_political_parties).
narrative_ontology:constraint_victim(bangladesh_july_national_charter, constitutional_deliberative_process).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL VOTER (SNARE) — Forced into a binary choice on a package of 80+ complex reforms, suppressing any nuanced preference. The voter is trapped; they cannot vote for some reforms while rejecting others. The high suppression and extraction of political choice make this a Snare. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INTERIM GOVERNMENT (ROPE) — As the architect, the government sees the referendum as a pure coordination mechanism to overcome decades of political gridlock and implement necessary reforms. As a beneficiary with arbitrage exit (their mandate ends after the election), they experience negative effective extraction. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE INCOMING ELECTED GOVT (TANGLED ROPE) — The new government is constrained by the referendum's popular mandate, which it may not fully support. It benefits from the election framework but is a victim of the binding reform package. This mixed role and constrained exit result in a Tangled Rope classification. d≈0.6, f(d)≈0.87, σ=1.0 → χ≈0.48.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE PRO-REFORM COALITION (ROPE) — The 24 parties that signed the charter see it as a necessary tool for collective action. As organized beneficiaries with mobile exit (they could have refused to sign), they perceive it as a low-extraction coordination solution. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.005.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (TANGLED ROPE) — The observer sees both the genuine coordination function (breaking deadlock) and the asymmetric extraction (bundling suppresses dissent). The high base extractiveness and suppression, combined with a clear coordination goal, define a Tangled Rope. This is the system's claimed type. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bangladesh_july_national_charter_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bangladesh_july_national_charter, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bangladesh_july_national_charter, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bangladesh_july_national_charter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bangladesh_july_national_charter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55) is high because the bundling mechanism extracts political choice and nuance from voters. A voter who supports 90% of the reforms but vehemently opposes 10% has their consent for the former leveraged to pass the latter. Suppression (0.75) is very high because the all-or-nothing format completely eliminates alternative choices, such as voting on reforms individually or in smaller packages. Theater Ratio (0.20) is low, as the referendum has significant, tangible political consequences and is not primarily a performative act.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The interim government (beneficiary) sees a pure Rope, a tool to solve a national coordination problem. The individual voter (victim) experiences a Snare, trapped in a coercive choice that invalidates their specific preferences. The incoming government, caught between its own agenda and the referendum's mandate, perceives a Tangled Rope—a mix of coordination and coercion. This divergence highlights how the same political instrument can be perceived radically differently based on one's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural relationships. The Interim Government, as a beneficiary with arbitrage exit, has a very low 'd' value, resulting in negative effective extraction (Rope). The individual voter, as a trapped victim, has a very high 'd' value, leading to high effective extraction (Snare). The incoming government, with a mixed role and constrained exit, has a moderate 'd' value, placing it in the Tangled Rope category. The analytical perspective aligns with Tangled Rope, recognizing both the stated coordination goal and the coercive extractive mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a powerful example of resolving mandatrophy. Labeling the referendum as simply 'democratic reform' (Rope) would ignore the coercive extraction of choice from voters. Labeling it as pure 'authoritarian imposition' (Snare) would ignore the genuine coordination function it serves in a post-crisis political environment. The Tangled Rope classification correctly captures this duality: it is a mechanism that uses an extractive method (bundling) to achieve a coordinative end (reform), and its nature is fundamentally ambiguous without specifying the observer's perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_vs_political_mandate,
    'Is the referendum''s ''Yes'' vote legally binding on the new parliament, or is it only a politically persuasive mandate?',
    'A future ruling by the Bangladesh Supreme Court or the actions of the new parliament acting as a constituent assembly.',
    'If legally binding, the constraint''s suppression and extractiveness are confirmed, solidifying its Snare/Tangled Rope nature. If only political, it is a weaker constraint, potentially degrading to a Piton if ignored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_vs_political_mandate, empirical, 'Whether the referendum mandate is legally or merely politically binding.').

omega_variable(
    voter_intent_granularity,
    'Did the 68% ''Yes'' vote reflect specific approval for the 80+ bundled reforms, or was it a general vote for ''change'' and against the previous regime?',
    'Detailed post-election polling and analysis of voter motivations.',
    'If support was specific, the coordination function (Rope element) is stronger. If it was a general protest vote, the extractive function (Snare element) of bundling unrelated issues is more pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_intent_granularity, empirical, 'Granularity of voter intent behind the single ''Yes'' vote.').

omega_variable(
    selective_implementation_risk,
    'Will the new government with its two-thirds majority implement the charter faithfully, selectively, or not at all?',
    'Observing the legislative actions of the new parliament during its first two years.',
    'Selective implementation would reveal the true beneficiaries and victims of the charter, potentially altering the directionality calculations. Complete rejection would indicate the constraint''s failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_implementation_risk, empirical, 'Risk of the new government selectively implementing the charter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bangladesh_july_national_charter, 2024, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bang_tr_t0, bangladesh_july_national_charter, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bang_tr_t1, bangladesh_july_national_charter, theater_ratio, 1, 0.15).
narrative_ontology:measurement(bang_tr_t2, bangladesh_july_national_charter, theater_ratio, 2, 0.2).

% Extraction over time
narrative_ontology:measurement(bang_be_t0, bangladesh_july_national_charter, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bang_be_t1, bangladesh_july_national_charter, base_extractiveness, 1, 0.45).
narrative_ontology:measurement(bang_be_t2, bangladesh_july_national_charter, base_extractiveness, 2, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bangladesh_july_national_charter, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
