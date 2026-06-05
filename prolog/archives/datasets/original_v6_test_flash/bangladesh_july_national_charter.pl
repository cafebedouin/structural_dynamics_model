% ============================================================================
% CONSTRAINT STORY: bangladesh_july_national_charter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
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
 *   domain: political
 *
 * SUMMARY:
 *   The July National Charter Referendum, a bundle of 80 constitutional
 *   reforms proposed by the Muhammad Yunus-led interim government, was
 *   presented as a binary 'Yes/No' referendum alongside the February 12,
 *   2026, general elections. This constraint story examines the structural
 *   dynamics of this event, classifying its impact on different agents and
 *   identifying potential for both coordination and extraction.
 *
 * KEY AGENTS:
 *   - Muhammad Yunus-led Interim Government: Primary beneficiary (institutional/arbitrage)
 *   - Opposition Political Parties: Target (moderate/constrained)
 *   - General Electorate: Target (powerless/trapped)
 *   - Pro-Reform Civil Society Groups: Beneficiary (organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bangladesh_july_national_charter, 0.55).
domain_priors:suppression_score(bangladesh_july_national_charter, 0.65).
domain_priors:theater_ratio(bangladesh_july_national_charter, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bangladesh_july_national_charter, extractiveness, 0.55).
narrative_ontology:constraint_metric(bangladesh_july_national_charter, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bangladesh_july_national_charter, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bangladesh_july_national_charter, tangled_rope).
narrative_ontology:human_readable(bangladesh_july_national_charter, "The July National Charter Referendum").
narrative_ontology:topic_domain(bangladesh_july_national_charter, "political").

domain_priors:requires_active_enforcement(bangladesh_july_national_charter).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bangladesh_july_national_charter, muhammad_yunus_led_interim_government).
narrative_ontology:constraint_beneficiary(bangladesh_july_national_charter, pro_reform_civil_society_groups).
narrative_ontology:constraint_victim(bangladesh_july_national_charter, opposition_political_parties).
narrative_ontology:constraint_victim(bangladesh_july_national_charter, general_electorate).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL ELECTORATE (SNARE) - Faced with a binary choice on a complex set of reforms, and potentially pressured by the interim government's influence. Limited exit options due to the significance of the referendum and potential consequences. High suppression as the electorate's nuanced views are forced into a simple yes/no decision.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION POLITICAL PARTIES (TANGLED ROPE) - While opposing the reforms, they are constrained by the interim government's control and the potential backlash from opposing popular reforms. They benefit from increased visibility by taking a stand but also face suppression due to government control over media and potential legal repercussions. Extraction arises from curtailed political freedoms and limited campaigning abilities.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MUHAMMAD YUNUS-LED INTERIM GOVERNMENT (ROPE) - Benefits from the increased power and legitimacy gained if the reforms are approved. They see it as a means of implementing positive change. The referendum facilitates coordination between the government and the populace, enabling them to move forward with a clear mandate.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRO-REFORM CIVIL SOCIETY GROUPS (TANGLED ROPE) - Support the reforms and actively campaign for them. They benefit from increased visibility and influence if the reforms are passed, but are also subject to some suppression from dissenting groups. They have a mobile exit option by redirecting their support elsewhere if the referendum fails. A genuine coordination function is present because civil society's buy-in will be needed to implement the Charter.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) - Sees the referendum as a complex situation with both coordination and extraction elements. The long-term impact on democracy and governance is uncertain, with potential benefits and risks depending on the specific reforms and how they are implemented.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bangladesh_july_national_charter_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bangladesh_july_national_charter, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bangladesh_july_national_charter, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness: 0.55 - The referendum extracts consent from the general electorate, forcing them to accept or reject the entire package of 80 reforms. It also extracts political freedom from opposition parties, limiting their ability to campaign. Suppression: 0.65 - The binary choice suppresses alternative viewpoints and nuances within the electorate and political parties. The government's influence and control over media also contribute to suppression. Theater Ratio: 0.40 - The referendum serves a performative function, demonstrating public support for the government and its reform agenda. However, the underlying complexity of the reforms suggests this function is higher, and there is genuine effort to implement positive changes through genuine coordination. The complexity causes the ratio to be lower.
 *
 * PERSPECTIVAL GAP:
 *   The general electorate, particularly those with nuanced views or limited access to information, experiences the referendum as a snare. Opposition parties see it as a tangled rope, balancing opportunities for increased visibility with the constraints imposed by the interim government. The interim government views the referendum as a rope, a tool for coordinating action and legitimizing their authority. Pro-Reform Civil Society Groups see the situation also as a tangled rope, as their actions are helpful but may also lead to some forms of suppression. The analytical observer recognizes the combination of coordination and extraction, highlighting the trade-offs and potential unintended consequences of the process.
 *
 * DIRECTIONALITY LOGIC:
 *   The interim government benefits from increased power and legitimacy if the referendum passes, placing them in a beneficiary position. The opposition parties and general electorate face extraction in the form of limited political freedom and a forced choice. The specific design of the reforms may further extract from specific groups, depending on who benefits from specific portions of the package. The calculation for directionality appropriately adjusts for these variables. The government benefits, while some face limited freedom to campaign. This is weighed and factored into the logic for the overall rating.
 *
 * MANDATROPHY ANALYSIS:
 *   The referendum is not purely extractive because it includes elements of coordination and public choice. Distinguishing it from a pure extraction snare requires consideration of the reforms' content, the government's intentions, and the level of public understanding and free choice involved. The mandatrophy is handled by classifying the perspectives to highlight the variety of experiences. The perspective of the electorate helps emphasize the potential for the 'snare' type situation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_interim_government,
    'How legitimate is the interim government''s mandate to implement sweeping constitutional reforms via referendum, especially so close to a general election?',
    'Public opinion polls, expert legal analysis, post-referendum assessments of democratic processes.',
    'If mandate is weak: referendum seen as illegitimate power grab (more Snare-like). If mandate is strong: referendum seen as legitimate reform effort (more Rope-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_interim_government, conceptual, 'Legitimacy of the interim government''s mandate.').

omega_variable(
    reform_complexity_clarity,
    'How well do the 80 constitutional reforms (the full ''bundle'') lend themselves to a simple binary ''Yes/No'' vote?',
    'Analysis of the reforms themselves, public understanding assessments, expert legal commentary.',
    'If reforms are simple & clear: referendum is a fair decision. If reforms are complex: referendum is a misleading simplification (more Snare-like).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_complexity_clarity, conceptual, 'Clarity of individual impacts of 80-part package.').

omega_variable(
    potential_for_coercion,
    'To what extent were the general electorate able to vote freely, without undue external influence, persuasion, or threat?',
    'Election monitoring reports, post-referendum surveys, legal challenges to the vote.',
    'If vote was free: democratic choice is affirmed. If vote was coerced: referendum lacks democratic legitimacy (more Snare-like).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_for_coercion, empirical, 'Potential for coercion in election.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bangladesh_july_national_charter, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bang_tr_t0, bangladesh_july_national_charter, theater_ratio, 0, 0.3).
narrative_ontology:measurement(bang_tr_t6, bangladesh_july_national_charter, theater_ratio, 6, 0.4).
narrative_ontology:measurement(bang_tr_t12, bangladesh_july_national_charter, theater_ratio, 12, 0.5).

% Extraction over time
narrative_ontology:measurement(bang_be_t0, bangladesh_july_national_charter, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bang_be_t6, bangladesh_july_national_charter, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(bang_be_t12, bangladesh_july_national_charter, base_extractiveness, 12, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bangladesh_july_national_charter, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
