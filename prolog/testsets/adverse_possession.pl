% ============================================================================
% CONSTRAINT STORY: adverse_possession
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adverse_possession, []).

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
 *   constraint_id: adverse_possession
 *   human_readable: Adverse Possession (Squatter's Rights)
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   Adverse possession is a legal doctrine where a person in possession of
 *   land owned by someone else may acquire valid title to it after a period
 *   of time, provided certain common law requirements are met. It is often
 *   colloquially called 'squatter's rights.' The doctrine is intended to
 *   promote the productive use of land and to resolve uncertainties in
 *   property titles. However, it creates a structural conflict between the
 *   rights of legal owners (especially those who are absentee or unaware) and
 *   the interests of the possessor and the state.
 *
 * KEY AGENTS:
 *   - Absentee Landowner: Primary victim (powerless/trapped) — loses their property without compensation after the statutory period.
 *   - Adverse Possessor: Primary beneficiary (organized/mobile) — gains title to property through systematic, open, and continuous use.
 *   - The State / Legal System: Institutional beneficiary (institutional/arbitrage) — benefits from clearer titles, a stable tax base, and productive land use.
 *   - Analytical Observer: Sees the dual function of coordination and extraction (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adverse_possession, 0.75).
domain_priors:suppression_score(adverse_possession, 0.8).
domain_priors:theater_ratio(adverse_possession, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adverse_possession, extractiveness, 0.75).
narrative_ontology:constraint_metric(adverse_possession, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(adverse_possession, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adverse_possession, tangled_rope).
narrative_ontology:human_readable(adverse_possession, "Adverse Possession (Squatter's Rights)").
narrative_ontology:topic_domain(adverse_possession, "economic/political/social").

domain_priors:requires_active_enforcement(adverse_possession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adverse_possession, adverse_possessors).
narrative_ontology:constraint_beneficiary(adverse_possession, the_state).
narrative_ontology:constraint_beneficiary(adverse_possession, title_insurers).
narrative_ontology:constraint_victim(adverse_possession, absentee_landowners).
narrative_ontology:constraint_victim(adverse_possession, heirs_unaware_of_property).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ABSENTEE LANDOWNER (SNARE) — For the original title holder who is unaware or unable to contest the occupation, the law is a pure extraction mechanism. Once the statutory period expires, their property is lost with no recourse. They are powerless and trapped. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.85.
constraint_indexing:constraint_classification(adverse_possession, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ADVERSE POSSESSOR (ROPE) — For the person claiming the land, the law is a pure coordination mechanism. It provides a clear, albeit lengthy, process for formalizing title to land they are actively using. They are organized (must meet strict criteria) and mobile (can abandon the claim). d≈0.40, f(d)≈0.40, σ=0.8 → χ≈0.24.
constraint_indexing:constraint_classification(adverse_possession, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE STATE (ROPE) — From the perspective of the legal system, this doctrine is a coordination tool to ensure land is used productively, resolve title uncertainties, and maintain a stable property tax base. The state benefits and can change the law at will (arbitrage). d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.09. Negative extraction indicates a net subsidy to the state's goals.
constraint_indexing:constraint_classification(adverse_possession, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The analytical view recognizes both the coordination function (clearing title, promoting land use) and the severe, asymmetric extraction from unaware owners. The high base extraction and suppression, combined with a clear public policy goal, make it a canonical Tangled Rope. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈1.04.
constraint_indexing:constraint_classification(adverse_possession, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: NATURAL LAW VIEW (MOUNTAIN) — This perspective frames the doctrine as an inevitable consequence of property rights: that ownership entails stewardship, and failure of stewardship naturally leads to forfeiture. It naturalizes a specific legal doctrine as an immutable law. The engine will flag this as a false summit, as the high ε and suppression values are inconsistent with a true Mountain.
constraint_indexing:constraint_classification(adverse_possession, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adverse_possession_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(adverse_possession, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adverse_possession, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(adverse_possession, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(adverse_possession_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. When the conditions are met, the extraction is total for the victim—they lose 100% of the property's value. The base value is set high to reflect this severity, triggering the mandatrophy resolution requirement. Suppression (0.80): High. While an attentive owner has many alternatives (e.g., eviction), the law completely suppresses the rights of an inattentive owner after the statutory period. The finality of the title transfer represents a high degree of coercion. Theater Ratio (0.10): Low. The legal process for adverse possession is highly functional and procedural, based on strict evidentiary tests rather than performative rituals.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the absentee landowner, the law is a Snare that legalizes theft. For the adverse possessor and the state, it is a Rope—a necessary and beneficial coordination mechanism for managing property. The analytical observer sees both functions simultaneously, classifying it as a Tangled Rope. This highlights how a single legal doctrine can be perceived as pure extraction or pure coordination depending on one's structural position relative to the property in question.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the adverse possessors who gain title and the state which gains a productive, tax-paying property. The victims are the original title-holders who lose their asset. The direction of extraction is clearly from the inactive owner to the active possessor, mediated and legitimized by the state. The state itself benefits from the overall system's function, making it an institutional beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by demonstrating a canonical Tangled Rope. To label adverse possession as purely a Snare ('legalized theft') ignores its genuine, state-sanctioned coordination function of clearing titles and encouraging land use. Conversely, to label it purely as a Rope ('economic efficiency tool') ignores the severe, non-consensual, and total extraction imposed upon the victim. The Tangled Rope classification correctly captures this dual nature, preventing misclassification in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_efficiency_gain,
    'Does adverse possession genuinely lead to more economically efficient land use, or does it primarily create legal conflict and transfer wealth without net social gain?',
    'Comparative economic analysis of jurisdictions with and without strong adverse possession laws, controlling for other factors in land development.',
    'If efficiency gains are high, the ''Rope'' perspective is strengthened. If gains are low or negative, the ''Snare'' perspective is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_efficiency_gain, empirical, 'Quantifying the net economic benefit of the adverse possession doctrine.').

omega_variable(
    hostility_intent_standard,
    'What level of intent satisfies the ''hostile'' possession requirement: a good-faith belief of ownership, an objective occupation without permission, or a bad-faith intent to steal?',
    'Jurisprudence analysis and clarification of legal standards across states.',
    'A good-faith standard reduces the perceived extractiveness, while a bad-faith standard increases it, shifting the classification between Tangled Rope and Snare for some observers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hostility_intent_standard, conceptual, 'The legal interpretation of ''hostile'' intent in possession claims.').

omega_variable(
    fairness_of_statutory_period,
    'What is a ''fair'' statutory period to balance the interests of an absentee owner against the goal of productive land use?',
    'Policy debate and legislative adjustment; there is no single empirical answer.',
    'Shorter periods (e.g., 5 years) make the constraint feel more like a Snare, while longer periods (e.g., 20-40 years) make it feel more like a Rope, as the owner has more time to act.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fairness_of_statutory_period, preference, 'The societal preference for the length of the statutory period for claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adverse_possession, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adve_tr_t0, adverse_possession, theater_ratio, 0, 0.1).
narrative_ontology:measurement(adve_tr_t10, adverse_possession, theater_ratio, 10, 0.1).
narrative_ontology:measurement(adve_tr_t20, adverse_possession, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(adve_be_t0, adverse_possession, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(adve_be_t10, adverse_possession, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(adve_be_t20, adverse_possession, base_extractiveness, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adverse_possession, resource_allocation).
narrative_ontology:affects_constraint(adverse_possession, property_title_certainty).
narrative_ontology:affects_constraint(adverse_possession, land_use_zoning).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
