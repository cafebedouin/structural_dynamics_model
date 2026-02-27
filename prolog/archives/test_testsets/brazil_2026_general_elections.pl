% ============================================================================
% CONSTRAINT STORY: brazil_2026_general_elections
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brazil_2026_general_elections, []).

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
 *   constraint_id: brazil_2026_general_elections
 *   human_readable: 2026 Brazilian General Election Structure
 *   domain: political
 *
 * SUMMARY:
 *   The 2026 Brazilian General Election structure operates as a rigid
 *   constraint defined by the intense polarization between 'Bolsonarismo' and
 *   'Anti-Bolsonarismo' (often represented by 'Lulismo'). This binary
 *   simplifies the political landscape into a zero-sum conflict, suppressing
 *   alternative political projects and forcing voters into a 'lesser of two
 *   evils' calculation. The constraint is not merely the formal election
 *   rules, but the entire ecosystem of media narratives, party funding, and
 *   political alliances that enforces this dichotomy.
 *
 * KEY AGENTS:
 *   - Major Political Bloc Leadership: Primary beneficiaries (institutional/arbitrage) - Leaders of the two poles who consolidate power and resources.
 *   - Independent Voters: Primary victims (powerless/trapped) - Citizens whose range of political choice is severely constrained.
 *   - Third-Party Movements: Secondary victims (organized/constrained) - Political groups unable to gain traction due to the suppressive nature of the binary.
 *   - Legacy Media Outlets: Secondary beneficiaries (institutional/arbitrage) - Profit from the high-engagement, conflict-driven narrative.
 *   - Analytical Observer: External analyst (analytical/analytical) - Perceives the full hybrid nature of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazil_2026_general_elections, 0.6).
domain_priors:suppression_score(brazil_2026_general_elections, 0.85).
domain_priors:theater_ratio(brazil_2026_general_elections, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazil_2026_general_elections, extractiveness, 0.6).
narrative_ontology:constraint_metric(brazil_2026_general_elections, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(brazil_2026_general_elections, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brazil_2026_general_elections, tangled_rope).
narrative_ontology:human_readable(brazil_2026_general_elections, "2026 Brazilian General Election Structure").
narrative_ontology:topic_domain(brazil_2026_general_elections, "political").

domain_priors:requires_active_enforcement(brazil_2026_general_elections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brazil_2026_general_elections, major_political_bloc_leadership).
narrative_ontology:constraint_beneficiary(brazil_2026_general_elections, legacy_media_outlets).
narrative_ontology:constraint_victim(brazil_2026_general_elections, independent_voters).
narrative_ontology:constraint_victim(brazil_2026_general_elections, third_party_movements).
narrative_ontology:constraint_victim(brazil_2026_general_elections, policy_nuance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT VOTER (SNARE) — Trapped within a binary choice that does not represent their preferences. Their vote is extracted as support for the 'lesser evil', while genuine alternatives are suppressed. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.85. This high effective extraction meets the Snare threshold.
constraint_indexing:constraint_classification(brazil_2026_general_elections, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAJOR BLOC LEADERSHIP (ROPE) — Experiences the system as a pure coordination mechanism to consolidate power and govern. The binary structure simplifies messaging and funnels resources, appearing as an efficient tool. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. The negative extraction indicates a net beneficiary.
constraint_indexing:constraint_classification(brazil_2026_general_elections, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the coordination function (electing a government) and the severe extractive properties (suppression of alternatives, extraction of voter agency). The base metrics (ε=0.60, suppression=0.85) confirm a hybrid system. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.83.
constraint_indexing:constraint_classification(brazil_2026_general_elections, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THIRD-PARTY MOVEMENT (TANGLED ROPE) — Forced to operate within the constraining system, experiencing its extractive nature directly through funding barriers and lack of media access. Yet, they must engage with it as a coordination game to have any hope of influence. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.45.
constraint_indexing:constraint_classification(brazil_2026_general_elections, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: 'DUVERGER'S LAW' OBSERVER (MOUNTAIN) — This perspective misinterprets the contingent political polarization as an immutable law of two-party systems. It frames the binary as a natural, unavoidable outcome. The engine will flag this as a 'false summit' because the base properties (ε=0.60, suppression=0.85) violate the Mountain classification thresholds.
constraint_indexing:constraint_classification(brazil_2026_general_elections, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_2026_general_elections_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brazil_2026_general_elections, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_2026_general_elections, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brazil_2026_general_elections, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(brazil_2026_general_elections_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The system extracts political agency from voters and potential support from nascent political movements, channeling it towards the two established poles. Suppression (0.85): Very High. Electoral laws (e.g., performance barriers), party fund distribution, and dominant media narratives create formidable barriers for any third way. Theater Ratio (0.65): High. Substantive policy debate is often replaced by performative cultural warfare and loyalty signaling, focusing on personalities over governance plans.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the leadership of the two main blocs, the system is an effective Rope for coordinating power. For the independent voter, it is a Snare that forces a choice and extracts consent. For an aspiring third party, it is a Tangled Rope they must navigate, facing both its extractive barriers and its coordination rules. An academic might even misclassify it as a Mountain, an inevitable result of political 'laws', but this ignores the high degree of active enforcement and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Bloc Leadership, Media) have arbitrage exit options, leading to low directionality (d) and a Rope classification. Victims (Voters, Third Parties) are trapped or constrained, leading to high directionality and Snare/Tangled Rope classifications. The system's structure itself determines who benefits and who pays, and the classifications reflect these divergent structural realities.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves a potential mandatrophy by refusing to label the system as simply 'democratic' (Rope) or 'authoritarian' (Snare). The analytical classification of Tangled Rope correctly identifies that the system performs a genuine coordination function (selecting a government) while simultaneously being highly extractive and suppressive. The perspectival analysis shows that the 'Rope' and 'Snare' labels are not incorrect, but are incomplete truths reflecting valid but partial viewpoints from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    polarization_durability,
    'Is the current ''Bolsonarismo vs. Anti-Bolsonarismo'' polarization a durable structural feature of Brazilian politics or a transient phase tied to specific personalities?',
    'Analysis of electoral results post-2026, tracking the vote share of third parties and the emergence of new political cleavages.',
    'If durable, the Snare/Tangled Rope classifications are stable. If transient, the constraint may decay into a Piton or be replaced by a less extractive Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polarization_durability, empirical, 'Whether the current political polarization is a durable or transient feature.').

omega_variable(
    digital_media_impact,
    'Do digital media platforms and decentralized networks offer a viable path to break the binary, or do their algorithms inherently reinforce the two dominant poles?',
    'Network analysis of information flows during the 2026 campaign; correlation between social media engagement and support for non-polarized candidates.',
    'If they enable alternatives, the suppression score (0.85) is too high. If they reinforce the binary, suppression is accurate or even underestimated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_media_impact, empirical, 'The role of digital media in either reinforcing or breaking the political binary.').

omega_variable(
    null_vote_effect,
    'Is casting a null or blank vote a form of effective protest (a limited ''exit'') or a form of political surrender that legitimizes the victory of one of the two poles?',
    'Political theory analysis combined with empirical study of how high abstention/null vote rates are interpreted by political elites and media.',
    'If it''s an effective exit, the ''trapped'' status of voters is weakened, potentially lowering the perceived extraction. If it''s surrender, the Snare classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(null_vote_effect, conceptual, 'The structural meaning and effect of null/blank votes within the system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazil_2026_general_elections, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(braz_tr_t2018, brazil_2026_general_elections, theater_ratio, 2018, 0.5).
narrative_ontology:measurement(braz_tr_t2022, brazil_2026_general_elections, theater_ratio, 2022, 0.6).
narrative_ontology:measurement(braz_tr_t2026, brazil_2026_general_elections, theater_ratio, 2026, 0.65).

% Extraction over time
narrative_ontology:measurement(braz_be_t2018, brazil_2026_general_elections, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement(braz_be_t2022, brazil_2026_general_elections, base_extractiveness, 2022, 0.55).
narrative_ontology:measurement(braz_be_t2026, brazil_2026_general_elections, base_extractiveness, 2026, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brazil_2026_general_elections, resource_allocation).
narrative_ontology:affects_constraint(brazil_2026_general_elections, brazil_economic_policy_2027).
narrative_ontology:affects_constraint(brazil_2026_general_elections, brazil_judicial_appointments_2027).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
