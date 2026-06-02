% ============================================================================
% CONSTRAINT STORY: iran_hijab_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_hijab_law, []).

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
 *   constraint_id: iran_hijab_law
 *   human_readable: Mandatory Hijab Law in Iran
 *   domain: political/social/religious
 *
 * SUMMARY:
 *   The mandatory hijab law in the Islamic Republic of Iran represents a
 *   structural constraint enforced by the state through the morality police
 *   (Gasht-e Ershad) apparatus. The constraint extracts bodily autonomy,
 *   freedom of self-presentation, and behavioral conformity from women,
 *   religious minorities, and secular citizens. The law is framed as
 *   religious and moral necessity, but analysis reveals a primary extraction
 *   function with secondary coordination aspirations. The state benefits from
 *   compliance signaling, control over public space, and legitimation of
 *   enforcement institutions. The enforcement apparatus increasingly
 *   functions as a revenue mechanism and political control tool. Women and
 *   religious minorities experience the constraint as a Snare with no
 *   domestic exit options; the state experiences it as Tangled Rope (mixed
 *   coordination and extraction); the enforcement apparatus operates as a
 *   Piton (performative ritual maintained by institutional inertia); women's
 *   rights movements see it as a Scaffold with a political sunset. The
 *   2022-2023 'Woman, Life, Freedom' movement demonstrated the possibility of
 *   regime-change exit pathways, shifting the classification from inevitable
 *   to contingent on political sustainability.
 *
 * KEY AGENTS:
 *   - Women Subject to Enforcement: Primary victims (powerless/trapped) — bears full extraction cost of bodily autonomy and freedom of movement constraints
 *   - Religious Minorities: Secondary victims (powerless/trapped) — subject to same enforcement regime; forced to choose between religious authenticity and behavioral compliance
 *   - State Religious Authority: Primary beneficiary (powerful/mobile) — extracts compliance and behavioral conformity; gains legitimacy from enforcement
 *   - Morality Police Apparatus: Secondary beneficiary (organized/constrained) — maintains enforcement apparatus; generates revenue through fines; gains institutional legitimation
 *   - Women's Rights Coalition: Organized actors (organized/mobile) — see the constraint as politically vulnerable; mobilize exit pathways through protest and reform advocacy
 *   - Secular Iranian Citizens: Secondary victims (moderate/constrained) — experience conformity pressure and limited public self-expression options
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees extraction mechanism with weak coordination function; constraints function primarily as state control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_hijab_law, 0.68).
domain_priors:suppression_score(iran_hijab_law, 0.82).
domain_priors:theater_ratio(iran_hijab_law, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_hijab_law, extractiveness, 0.68).
narrative_ontology:constraint_metric(iran_hijab_law, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(iran_hijab_law, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_hijab_law, snare).
narrative_ontology:human_readable(iran_hijab_law, "Mandatory Hijab Law in Iran").
narrative_ontology:topic_domain(iran_hijab_law, "political/social/religious").

domain_priors:requires_active_enforcement(iran_hijab_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_hijab_law, state_religious_authority).
narrative_ontology:constraint_beneficiary(iran_hijab_law, morality_police_apparatus).
narrative_ontology:constraint_victim(iran_hijab_law, women_subject_to_enforcement).
narrative_ontology:constraint_victim(iran_hijab_law, religious_minorities).
narrative_ontology:constraint_victim(iran_hijab_law, secular_iranian_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN SUBJECT TO ENFORCEMENT (SNARE) — No meaningful exit option within Iran. Non-compliance triggers arrest, detention, fines, public humiliation, and social punishment. The constraint extracts bodily autonomy, freedom of movement, and self-presentation choices. Exits are severely constrained: emigration is costly and dangerous; staying requires complete behavioral conformance. Maximum experienced extraction.
constraint_indexing:constraint_classification(iran_hijab_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS MINORITIES (SNARE) — Non-Muslims subject to the same enforcement regime. The law extracts religious conformity or forces dissimulation. Exit options include emigration (costly) or strict behavioral compliance. No domestic exit mechanism. Bears full enforcement cost.
constraint_indexing:constraint_classification(iran_hijab_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE RELIGIOUS AUTHORITY (TANGLED ROPE) — Primary beneficiary. The law legitimates and enforces religious doctrine, extracting behavioral conformity. But the state also invests substantially in the enforcement apparatus and faces legitimacy costs when enforcement produces dissent. The constraint serves coordination (enforcing moral order) alongside extraction (controlling female bodies). High effective extraction for the state, but not frictionless — the state must actively maintain the system and faces periodic challenges.
constraint_indexing:constraint_classification(iran_hijab_law, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: MORALITY POLICE APPARATUS (PITON) — Institutional actor (Gasht-e Ershad and related enforcement structures). The apparatus ostensibly serves to enforce the law, but increasingly operates as a revenue-generating mechanism (fines for improper dress) and political tool for suppressing dissent. Theater ratio high: the enforcement ritual (patrols, checkpoints, public confrontations) is performative — the goal is visible control and compliance signaling, not elimination of uncovered hair. The apparatus persists through institutional inertia and patronage networks rather than functional necessity. Many officials privately acknowledge the law's ineffectiveness.
constraint_indexing:constraint_classification(iran_hijab_law, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WOMEN'S RIGHTS COALITION (SCAFFOLD) — Organized advocacy groups (both inside and outside Iran) treating the hijab mandate as a temporary institutional feature that can be dismantled through sustained protest, legal reform, or regime change. The 2022-2023 'Woman, Life, Freedom' movement demonstrated the possibility of an exit pathway. The coalition sees the constraint as politically vulnerable rather than inherent or immutable. Sunset logic applies: as international pressure increases, costs of enforcement rise, and internal dissent grows, the political sustainability of the mandate declines.
constraint_indexing:constraint_classification(iran_hijab_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: SECULAR IRANIAN CITIZENS (SNARE) — Non-enforcement victims. While not directly targeted by morality police (if compliant), the law constrains public self-expression and imposes conformity pressure. Exit options include emigration or behavioral compliance. Extraction is less severe than for women because state enforcement pressure is lower if public behavior conforms, but the underlying suppression (inability to live according to non-religious values publicly) remains.
constraint_indexing:constraint_classification(iran_hijab_law, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — External analytical frame. The constraint is a pure extraction mechanism with minimal coordination function: it extracts bodily autonomy, freedom of self-presentation, and behavioral conformity from entire population cohorts (women, religious minorities). The state's claimed coordination function (enforcing religious order) is weakly achieved — widespread non-compliance, circumvention, and private dissent indicate the coordination goal is not effectively served. The extraction function is clear: political control, compliance signaling, and enforcement apparatus legitimation. Suppression is high; exit options are structurally constrained.
constraint_indexing:constraint_classification(iran_hijab_law, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_hijab_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_hijab_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_hijab_law, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_hijab_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iran_hijab_law, TR),
    TR >= 0.70.

:- end_tests(iran_hijab_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts bodily autonomy, freedom of self-presentation, and behavioral conformity from entire population cohorts. The beneficiary (state religious authority) gains political control, compliance signaling, and enforcement apparatus legitimation. The extraction is not total (women can circumvent through private dress and border emigration for privileged populations) but is severe for the majority with limited exit options. The value reflects substantial but not absolute extraction. Suppression (0.82): Very High. Enforcement mechanisms include arrest, detention, fines, public humiliation, and social punishment. Alternative channels are blocked: emigration is costly and dangerous; domestic religious leadership does not provide shelter; social enforcement (family, community) reinforces state pressure. The suppression floor is set by the state monopoly on legitimate violence. Theater ratio (0.65): Moderate-High. The enforcement ritual (patrols, checkpoints, public confrontations) is performative — designed to signal state power and enforce compliance through visibility rather than internalization. Evidence of private non-compliance (underground fashion markets, behavioral variation in private spaces) indicates that much public compliance is simulated rather than internalized. The theater has increased as the enforcement apparatus has shifted toward revenue generation (fines) and political control rather than religious transformation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits sharp perspectival divergence from structural position. Women with no exit classify the constraint as Snare (pure extraction, maximum coercion). The state religious authority classifies it as Tangled Rope (mixed coordination and extraction, with some beneficiary experience of legitimacy and order). The morality police apparatus classifies it as Piton (performative ritual with degraded function, maintained by institutional inertia). The women's rights coalition classifies it as Scaffold (temporary political vulnerability with sunset logic). The analytical observer classifies it as Snare (extraction with weak coordination). The gap reflects different power positions and exit capacities: trapped agents see only extraction; beneficiaries see coordination benefits; the organized movement sees a political window; the apparatus sees ritual; the analytical frame sees structural extraction. No perspective sees it as Rope (pure coordination with minimal extraction), confirming the claim that the law's coordination function is weak.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position: power level, exit options, and beneficiary/victim status. Women subject to enforcement have no exit options (trapped) and bear maximum extraction costs (victims) — d derives toward 1.0, producing high f(d) and high experienced extractiveness. The state religious authority has exit options within its governance system (arbitrage alternatives) and benefits from the constraint (beneficiary) — d derives toward 0.0-0.2, producing lower experienced extraction for this agent. The morality police apparatus has constrained exit (organizational dependence on the law's enforcement) and mixed beneficiary/victim status (benefits from the apparatus but extracts enforcement costs) — d derives toward 0.4-0.5. The women's rights coalition has mobile exit options (can organize, advocate, emigrate) and victim status — d derives toward 0.55-0.70. The analytical observer at civilizational scope has only analytical exit and observer position — d derives from canonical fallback (0.73, f(d)≈1.15).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by distinguishing the claimed religious/coordination function from the actual extraction mechanism. The state claims the law enforces religious order and protects social morality (coordination narrative). Analysis reveals: (1) enforcement apparatus increasingly functions as revenue mechanism and political control tool, not religious instruction; (2) private non-compliance (underground fashion markets, behavioral variation) indicates coordination goal is weakly achieved; (3) suppression is achieved through state violence monopoly, not through moral persuasion or religious consensus; (4) beneficiary structure (state authority, enforcement apparatus) is asymmetric with victim structure (women, minorities, secular citizens), indicating extraction rather than mutual coordination. The theater ratio (0.65) indicates significant performative content — enforcement ritual serves visibility and compliance signaling rather than behavioral internalization. The mandatrophy is resolved by recognizing that the law's coordination function (enforcing moral order) is subordinate to and instrumentalized by its extraction function (state control, compliance signaling, apparatus legitimation). The law exhibits Snare characteristics (high extraction, high suppression, high f(d) for victims) despite claims to coordination necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_sustainability,
    'Can the state sustain mandatory hijab enforcement as dissent grows and international pressure increases?',
    'Empirical tracking of enforcement rates, arrest trends, fines collected, public compliance vs. actual behavior; correlation with regime legitimacy and international sanctions; comparison to enforcement trajectories in countries that attempted similar mandates',
    'If enforcement unsustainable: constraint transitions toward Scaffold (temporary). If enforcement persists despite costs: remains Snare with potential for regime violence escalation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Whether mandatory hijab enforcement is politically sustainable').

omega_variable(
    religious_coordination_vs_extraction,
    'To what extent does the hijab law serve legitimate religious coordination versus pure state control extraction?',
    'Theological analysis of religious justification; historical comparison to religious dress codes in other Muslim-majority societies; survey data on religious leadership support for the mandate vs. state coercion',
    'If coordination-dominant: reclassifies toward Tangled Rope from broader perspectives. If extraction-dominant: confirms Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_coordination_vs_extraction, conceptual, 'Whether hijab enforcement serves religious coordination or state extraction').

omega_variable(
    diaspora_exit_mechanism,
    'Does emigration represent a genuine exit option or a survival mechanism available only to privileged populations?',
    'Demographic analysis of emigration by class, education, gender; cost analysis of emigration pathways; tracking of those who attempt exit vs. those trapped',
    'If exit available to many: agent power upgrades for some populations; exit_options shifts from ''trapped'' to ''constrained''. If exit only for privileged: confirms ''trapped'' for majority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diaspora_exit_mechanism, empirical, 'Whether emigration provides meaningful exit from hijab enforcement').

omega_variable(
    private_compliance_simulation,
    'What proportion of public compliance is genuine internalization versus behavioral compliance masking private disagreement?',
    'Anonymous surveys of Iranian women; ethnographic studies; analysis of underground fashion markets and private dress practices; comparison of public vs. private behavior',
    'If mostly theater (private non-compliance): extraction mechanism is performative control rather than behavioral change. Supports Piton classification for enforcement apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_compliance_simulation, empirical, 'Extent of genuine vs. simulated compliance with hijab mandate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_hijab_law, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hijab_tr_t0, iran_hijab_law, theater_ratio, 0, 0.52).
narrative_ontology:measurement(hijab_tr_t15, iran_hijab_law, theater_ratio, 15, 0.6).
narrative_ontology:measurement(hijab_tr_t30, iran_hijab_law, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(hijab_be_t0, iran_hijab_law, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(hijab_be_t15, iran_hijab_law, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(hijab_be_t30, iran_hijab_law, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_hijab_law, enforcement_mechanism).
narrative_ontology:affects_constraint(iran_hijab_law, iranian_women_labor_market_participation).
narrative_ontology:affects_constraint(iran_hijab_law, religious_minority_status_iran).
narrative_ontology:affects_constraint(iran_hijab_law, state_surveillance_apparatus_iran).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iran_hijab_law, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
