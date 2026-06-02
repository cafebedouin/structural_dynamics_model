% ============================================================================
% CONSTRAINT STORY: brilliance_as_structural_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brilliance_as_structural_liability, []).

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
 *   constraint_id: brilliance_as_structural_liability
 *   human_readable: Brilliance as Structural Liability in Hierarchical Organizations
 *   domain: organizational_ethics/systems_theory/moral_psychology
 *
 * SUMMARY:
 *   Organizations facing systemic dysfunction exhibit a paradoxical pattern:
 *   the agents most capable of diagnosing and articulating structural
 *   problems are also those most likely to face career retaliation. Enhanced
 *   cognitive capacity — whether from intelligence, domain expertise, or
 *   systems-thinking ability — converts organizational visibility into
 *   liability. The brilliant subordinate sees the dysfunction clearly, can
 *   articulate it precisely, and thereby becomes a threat to institutional
 *   leadership invested in the status quo. This constraint operates across
 *   organizational types (corporate, academic, governmental, nonprofit) but
 *   with variable severity. The extraction mechanism is not the cognitive
 *   enhancement itself but the structural conversion of insight into
 *   retributive risk. The constraint exhibits high theater ratio because
 *   organizations maintain elaborate performance of meritocracy, ethical
 *   commitment, and openness to feedback while systematically punishing those
 *   who exercise these values authentically. Measurements show both theater
 *   and extraction increasing over the interval as organizations layer
 *   compliance rituals (ethics training, anonymous reporting systems,
 *   diversity statements) onto unchanged retributive structures.
 *
 * KEY AGENTS:
 *   - Enhanced Subordinate Agents: Primary victims (powerless to moderate / identity_locked to constrained) — possess cognitive capacity to recognize systemic injustice; face career retaliation when acting on this recognition; identity-locked variant has fused professional identity with organizational mission and cannot exit without abandoning self-concept
 *   - Institutional Leadership: Primary beneficiaries (institutional/arbitrage) — benefit from subordinate silence; maintain positional authority and resource control by suppressing systemic critique; can exit to equivalent positions elsewhere if challenged
 *   - Mediocre Compliant Actors: Secondary beneficiaries (moderate/mobile) — advance through non-threatening competence; benefit from removal of brilliant competitors who surface inconvenient truths
 *   - Organizational Epistemic Integrity: Abstract victim (powerless/trapped) — collective capacity for self-correction degraded by systematic removal of agents capable of accurate diagnosis
 *   - Professional Guilds: Mixed position (organized/mobile) — coordinate credentialing and standards but also enforce conformity norms that suppress dissent; experience constraint as tangled rope
 *   - Whistleblower Protection Coalition: Organized agents (organized/constrained) — building legal and financial infrastructure to reduce exit costs; see constraint as scaffold with sunset logic as protections mature
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function (organizations need hierarchy and authority) and extractive overlay (retribution against accurate diagnosis)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brilliance_as_structural_liability, 0.78).
domain_priors:suppression_score(brilliance_as_structural_liability, 0.82).
domain_priors:theater_ratio(brilliance_as_structural_liability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brilliance_as_structural_liability, extractiveness, 0.78).
narrative_ontology:constraint_metric(brilliance_as_structural_liability, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(brilliance_as_structural_liability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brilliance_as_structural_liability, snare).
narrative_ontology:human_readable(brilliance_as_structural_liability, "Brilliance as Structural Liability in Hierarchical Organizations").
narrative_ontology:topic_domain(brilliance_as_structural_liability, "organizational_ethics/systems_theory/moral_psychology").

domain_priors:requires_active_enforcement(brilliance_as_structural_liability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brilliance_as_structural_liability, institutional_leadership).
narrative_ontology:constraint_beneficiary(brilliance_as_structural_liability, mediocre_compliant_actors).
narrative_ontology:constraint_victim(brilliance_as_structural_liability, enhanced_subordinate_agents).
narrative_ontology:constraint_victim(brilliance_as_structural_liability, organizational_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(brilliance_as_structural_liability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

constraint_indexing:constraint_classification(brilliance_as_structural_liability, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(brilliance_as_structural_liability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

constraint_indexing:constraint_classification(brilliance_as_structural_liability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

constraint_indexing:constraint_classification(brilliance_as_structural_liability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

constraint_indexing:constraint_classification(brilliance_as_structural_liability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brilliance_as_structural_liability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brilliance_as_structural_liability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brilliance_as_structural_liability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brilliance_as_structural_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(brilliance_as_structural_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The constraint extracts from enhanced subordinates through multiple mechanisms: career stagnation, reputational damage, economic precarity, psychological harm from gaslighting and isolation, and opportunity cost of suppressed contribution. The extraction is not incidental but structural — the system requires subordinate silence to maintain leadership authority in the face of systemic dysfunction. Suppression (0.82): Very high. Barriers to exit and voice include economic dependency (employment, health insurance, housing costs), reputational risk (blacklisting, reference sabotage), legal barriers (non-disparagement clauses, arbitration agreements), psychological barriers (identity fusion with professional role, learned helplessness, fear of retaliation), and social barriers (isolation from peer support, normalization of dysfunction). Theater ratio (0.68): High. Organizations perform meritocracy (we reward excellence), ethical commitment (we welcome feedback), and openness (our door is always open) while systematically punishing those who take these performances literally. The theater has increased over the interval as organizations have added compliance rituals (mandatory ethics training, anonymous reporting hotlines, diversity and inclusion statements) that create the appearance of responsiveness without changing retributive structures. The measurements show theater rising from 0.45 to 0.68 as the gap between performed values and structural reality widens.
 *
 * PERSPECTIVAL GAP:
 *   The brilliant subordinate with identity_locked exit sees a snare — trapped by professional identity fusion, facing severe extraction, with no exit path that does not require abandoning their self-concept. The enhanced agent with constrained exit also sees a snare but with slightly lower experienced extraction because exit is materially possible at high cost. Institutional leadership sees rope — the constraint coordinates organizational stability by suppressing disruptive critique; they experience it as a legitimate authority-maintenance mechanism. Professional guilds see tangled rope — genuine coordination function (credentialing, standards) mixed with extractive overlay (conformity enforcement, dissent suppression). The whistleblower protection coalition sees scaffold — legal and financial infrastructure is maturing to reduce exit costs, creating a sunset path as protections strengthen over generational time. The analytical observer sees tangled rope — real coordination need (hierarchy, authority) mixed with extractive mechanism (retribution against diagnosis). The perspectival gap reveals that what leadership experiences as coordination (maintaining authority) is what subordinates experience as extraction (punishment for accurate perception).
 *
 * DIRECTIONALITY LOGIC:
 *   Enhanced subordinate agents are victims with identity_locked or constrained exit options, yielding high directionality values and high experienced extraction. The identity_locked variant cannot exit without abandoning professional identity — the agent has fused their self-concept with organizational mission or professional role, making departure psychologically equivalent to self-destruction. The constrained variant faces high material exit costs but could leave at significant personal price. Institutional leadership are beneficiaries with arbitrage exit options — they benefit from subordinate silence and can move to equivalent positions elsewhere if challenged, yielding low directionality and low or negative experienced extraction. Mediocre compliant actors are secondary beneficiaries with mobile exit options — they advance through non-threatening competence and benefit from removal of brilliant competitors. Professional guilds experience mixed directionality — they coordinate standards (beneficiary function) but also enforce conformity norms that suppress dissent (victim function). The analytical observer sees the full structure: genuine coordination need (organizations require hierarchy) overlaid with extractive mechanism (retribution against accurate diagnosis).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that the same structural phenomenon — enhanced cognitive capacity in hierarchical organizations — produces both genuine coordination function and severe extraction, depending on observational position. From institutional leadership perspective, suppressing systemic critique is a coordination mechanism: it maintains organizational stability, preserves authority structures, and prevents disruptive conflict. This is not mere rationalization — organizations genuinely require some degree of hierarchy and authority to function, and unlimited internal critique can paralyze decision-making. From enhanced subordinate perspective, the same suppression is pure extraction: accurate diagnosis is punished, systemic dysfunction persists, and the agent bears career and psychological costs for perceiving reality clearly. The analytical observer sees both: there is a real coordination problem (organizations need authority), but the solution has been captured by an extractive overlay (retribution extends far beyond what coordination requires). The constraint is tangled rope at the analytical level because both functions coexist in the same structure. The classification prevents mislabeling: calling it pure coordination (rope) erases the severe harm to brilliant subordinates and organizational epistemic integrity; calling it pure extraction (snare from all perspectives) erases the genuine coordination need that hierarchy serves. The tangled rope classification preserves both structural realities and makes the tradeoff explicit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_threshold_ambiguity,
    'What level of cognitive enhancement triggers the liability mechanism? Is there a threshold below which system visibility does not convert to retributive risk?',
    'Longitudinal tracking of organizational outcomes correlated with psychometric assessments; identification of inflection points where cognitive capacity predicts adverse career outcomes',
    'If threshold is low (e.g., 1 SD above mean): constraint affects large population, extraction is widespread. If threshold is high (e.g., 2+ SD): constraint affects only exceptional outliers, extraction is concentrated but severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_threshold_ambiguity, empirical, 'Cognitive threshold for triggering structural liability').

omega_variable(
    suppression_mechanism_composition,
    'What proportion of suppression is structural (economic dependency, legal barriers, geographic constraints) versus internalized (identity fusion with professional role, epistemic learned helplessness, fear of reputational damage)?',
    'Post-exit trajectory analysis: if suppression persists after structural barriers are removed (agent leaves organization but does not speak out), reclassify as partially internalized. Interview data on decision calculus of those who stayed silent vs those who spoke.',
    'If primarily structural: removing economic barriers (whistleblower funds, alternative employment pathways) reduces suppression. If primarily internalized: suppression persists even when exit is materially feasible, requiring identity-frame intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural vs internalized suppression composition').

omega_variable(
    retribution_attribution_problem,
    'Is observed career damage causally attributable to cognitive enhancement + system visibility, or is it confounded by correlated factors (personality traits, communication style, political skill deficits)?',
    'Controlled comparison of matched pairs: agents with similar cognitive profiles and system visibility but different organizational outcomes. Identification of mediating variables (e.g., does political skill buffer the liability?).',
    'If causal: brilliance itself is the liability, and the constraint is a genuine structural trap. If confounded: the liability is mediated by learnable skills, and the constraint is partly avoidable through strategic behavior.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retribution_attribution_problem, empirical, 'Causal attribution of retribution to cognitive enhancement').

omega_variable(
    organizational_type_variance,
    'Does the constraint operate uniformly across organizational types (corporate, academic, governmental, nonprofit), or do some institutional forms exhibit lower extraction?',
    'Cross-sector comparison of career trajectories for cognitively enhanced agents who surface systemic problems. Identification of organizational features (governance structure, funding model, mission orientation) that correlate with lower retribution rates.',
    'If uniform: constraint is a general feature of hierarchy. If variable: some organizational designs mitigate the liability, providing exit paths or reducing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_type_variance, empirical, 'Variance in constraint severity across organizational types').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brilliance_as_structural_liability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brilliance_tr_t0, brilliance_as_structural_liability, theater_ratio, 0, 0.45).
narrative_ontology:measurement(brilliance_tr_t3, brilliance_as_structural_liability, theater_ratio, 3, 0.55).
narrative_ontology:measurement(brilliance_tr_t6, brilliance_as_structural_liability, theater_ratio, 6, 0.62).
narrative_ontology:measurement(brilliance_tr_t10, brilliance_as_structural_liability, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(brilliance_be_t0, brilliance_as_structural_liability, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(brilliance_be_t3, brilliance_as_structural_liability, base_extractiveness, 3, 0.66).
narrative_ontology:measurement(brilliance_be_t6, brilliance_as_structural_liability, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(brilliance_be_t10, brilliance_as_structural_liability, base_extractiveness, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brilliance_as_structural_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(brilliance_as_structural_liability, institutional_epistemic_capture).
narrative_ontology:affects_constraint(brilliance_as_structural_liability, meritocracy_theater).
narrative_ontology:affects_constraint(brilliance_as_structural_liability, complicity_equilibrium).

% DUAL FORMULATION NOTE:
% This constraint is part of a family modeling organizational dysfunction. Related constraints include institutional_epistemic_capture (organizations lose capacity for self-correction), meritocracy_theater (performance of merit-based advancement masking political selection), and complicity_equilibrium (stable state where all actors collude in maintaining dysfunction). Each has distinct epsilon values reflecting different structural mechanisms, but all are linked through organizational hierarchy dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(brilliance_as_structural_liability, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
