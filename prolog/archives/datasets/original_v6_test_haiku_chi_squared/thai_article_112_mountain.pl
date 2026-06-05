% ============================================================================
% CONSTRAINT STORY: thai_article_112_mountain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thai_article_112_mountain, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: thai_article_112_mountain
 *   human_readable: Article 112 (Lèse-majesté Laws) as a Legal Mountain
 *   domain: political/social/legal
 *
 * SUMMARY:
 *   Article 112 of the Thai Criminal Code criminalizes insults, threats,
 *   defamation, or contempt directed at the Thai monarch, queen, heir, or
 *   regent. Enacted in 1908 and incorporated into every Thai constitution,
 *   the article has remained structurally invariant across democratic
 *   periods, military juntas, and constitutional reforms for 118 years. It
 *   functions as a Mountain constraint: an unchangeable, irreducible boundary
 *   of what Thai law permits. Unlike extraction mechanisms that benefit
 *   specific actors (snares), or coordination tools that can be adjusted
 *   (ropes), or temporary policies (scaffolds), Article 112 expresses a
 *   fundamental principle of the Thai legal order — the protection of the
 *   institution of the monarchy as a constitutional pillar. The constraint
 *   exhibits zero degrees of freedom: no Thai government, democratic or
 *   authoritarian, has successfully repealed or substantially weakened it.
 *   The accessibility_collapse (0.92) reflects that the boundary is fully
 *   defined — speakers can easily identify what utterances cross the line.
 *   The resistance (0.08) reflects minimal contestation within Thai law
 *   itself; the constraint is accepted as a fixed feature of legal
 *   possibility. Theater_ratio (0.15) is low because Article 112 enforcement,
 *   while selective and subject to political manipulation, does not depend on
 *   performative ritual or symbolic theater — the law itself is
 *   straightforward and its consequences are clear.
 *
 * KEY AGENTS:
 *   - The Thai Subject/Speaker: Powerless/trapped agent — cannot exit Thai jurisdiction and remains under the constraint regardless of political regime or personal beliefs
 *   - The Thai State Legal System: Institutional embodiment of the constraint — the Criminal Code itself, constitutional amendments, and judicial doctrines that reinforce Article 112
 *   - The Thai Monarchy: Implicit protected entity — beneficiary of the constraint in the sense that institutional protection flows to the crown, though the constraint is justified as structural necessity rather than institutional preference
 *   - The Analytical Observer: Civilizational perspective — evaluates whether Article 112 represents a universal principle of constitutional order or a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thai_article_112_mountain, 0.18).
domain_priors:suppression_score(thai_article_112_mountain, 0.03).
domain_priors:theater_ratio(thai_article_112_mountain, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thai_article_112_mountain, extractiveness, 0.18).
narrative_ontology:constraint_metric(thai_article_112_mountain, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(thai_article_112_mountain, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(thai_article_112_mountain, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(thai_article_112_mountain, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thai_article_112_mountain, mountain).
narrative_ontology:human_readable(thai_article_112_mountain, "Article 112 (Lèse-majesté Laws) as a Legal Mountain").
narrative_ontology:topic_domain(thai_article_112_mountain, "political/social/legal").

domain_priors:emerges_naturally(thai_article_112_mountain).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT UNDER THAI JURISDICTION (MOUNTAIN) — No exit option from the rule of law governing the Thai state. The constraint is experienced as an immutable boundary of what can be legally said or written. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.26. Mountain classification (ε=0.18, suppression=0.03) holds across power atoms.
constraint_indexing:constraint_classification(thai_article_112_mountain, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THAI STATE LEGAL SYSTEM (MOUNTAIN) — The Article 112 prohibition is constitutional law, embedded in the Thai Criminal Code since 1908. The constraint is structural to the Thai legal system itself. No internal mechanism can dislodge it without constitutional change. d≈0.00, f(d)≈-0.12, σ=1.0 → χ≈-0.02. Mountain classification invariant.
constraint_indexing:constraint_classification(thai_article_112_mountain, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal jurisprudential perspective, the constraint represents an irreducible structural feature of the Thai legal order: no competing principle can override the protection of the monarchy within Thai law. The constraint is not a policy choice that could be repealed — it is embedded in the constitutional identity of the Thai state. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.21. Mountain classification stable across all indexical positions.
constraint_indexing:constraint_classification(thai_article_112_mountain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_article_112_mountain_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(thai_article_112_mountain, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_article_112_mountain, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(thai_article_112_mountain, ExtMetricName, E),
    domain_priors:suppression_score(thai_article_112_mountain, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(thai_article_112_mountain),
    narrative_ontology:constraint_metric(thai_article_112_mountain, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(thai_article_112_mountain, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(thai_article_112_mountain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.18): Low. Article 112 is not framed as extraction from subjects to benefit an external party. It is framed as a structural boundary protecting the constitutional order itself. The monarchy is protected, but the constraint's justification is institutional continuity, not rent-seeking. Suppression (0.03): Negligible. The constraint does not rely on coercion or suppression of alternatives — it is a fixed legal boundary. Speakers understand the boundary clearly; no hidden mechanism forces compliance. Theater ratio (0.15): Very low. Article 112 enforcement is direct and unambiguous — violations carry clear penalties (3-15 years imprisonment, fines). The law does not require performative validation or symbolic theater to function. Enforcement may be selective across regimes, but the law itself is not performative. Accessibility collapse (0.92): Very high. The constraint is maximally defined. Thai speakers can identify protected speech categories with high precision. Resistance (0.08): Minimal. Within the Thai legal system, there is no structural resistance to Article 112 — all Thai governments since 1908 have maintained it. International criticism exists, but internal Thai legal resistance is negligible. Claimed type: Mountain. The structure satisfies all mountain gates: ε ≤ 0.25, suppression ≤ 0.05, emerges_naturally=true, accessibility_collapse ≥ 0.85, resistance ≤ 0.15.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify Article 112 as mountain. There is no perspectival gap — the constraint appears invariant across observer positions. This is the expected outcome for true mountains: the constraint does not change based on who observes it. A Thai subject, the Thai legal system itself, and an international analytical observer all recognize Article 112 as a fixed boundary. The convergence reflects that the constraint is not asymmetrically extractive (which would produce snare perspectives) nor primarily coordination (which would produce rope perspectives). The invariance is the diagnostic signature of a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Thai subject (powerless/trapped): d≈0.95. The subject cannot exit the jurisdiction and has no alternative legal framework. However, the constraint is not primarily exploitative — d's elevation reflects structural entrenchment, not active extraction directed at this agent. Thai state legal system (institutional/analytical): d≈0.00. The legal system experiences Article 112 as a foundational principle, not as extraction. Negative directionality reflects that the system sees itself as beneficiary of the constraint's constitutional force. Analytical observer (analytical/analytical): d≈0.72. The observer is neutral/external; directionality is high because the constraint operates at maximum definition, but the observer applies the analytical lens (canonical d≈0.73) rather than inhabiting an agency position within Thai law. All three vectors yield low to moderate effective extraction (χ ≤ 0.26) because base extraction is low and the constraint lacks the active enforcement mechanisms of snares or tangled ropes.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_permanence_vs_political_reform,
    'Is Article 112''s immutability a feature of Thai constitutional law (mountain) or a contingent outcome of political consolidation that could change with regime reform?',
    'Analysis of constitutional amendment procedures in Thailand; comparison with other states that have repealed lèse-majesté laws (Spain 1978, Sweden 1972); assessment of whether Thai constitutional structure itself makes repeal structurally impossible vs. merely politically infeasible',
    'If structurally immutable: mountain classification confirmed. If merely difficult to repeal: the constraint might be Tangled Rope (coordination + extraction) rather than mountain, with χ value sensitive to political dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_permanence_vs_political_reform, empirical, 'Whether Article 112''s permanence is constitutional structure or political contingency').

omega_variable(
    legal_independence_from_enforcement_discretion,
    'Does the mountain classification hold when enforcement discretion is high and capricious (varying by regime, political faction, proximity to monarchy)?',
    'Comparative analysis of Article 112 enforcement rates across Thai military regimes vs democratic interludes; assessment of whether the legal constraint''s structural properties remain invariant despite enforcement volatility',
    'If enforcement varies while the law remains unchanged: mountain classification correct — the legal constraint itself is fixed, enforcement is a separate phenomenon. If enforcement instability bleeds into legal status: might suggest Tangled Rope or Piton (theatrical law with inertial maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_independence_from_enforcement_discretion, conceptual, 'Independence of Article 112''s legal structure from enforcement discretion across regimes').

omega_variable(
    monarchy_as_beneficiary_vs_structural_limit,
    'Does Article 112 primarily benefit the Thai monarchy (making it a Snare with a privileged beneficiary) or does it express a structural limit on what Thai law can permit (mountain)?',
    'Analysis of Article 112''s legislative history and constitutional justification; comparison with international jurisprudence on protected speech categories; assessment of whether the constraint reflects a universal principle (e.g., preservation of constitutional order) or merely institutional preference',
    'If structural limit on speech: mountain confirmed. If primarily rent-seeking for monarchy: constraint should be reclassified as Snare with high beneficiary power and high victim suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monarchy_as_beneficiary_vs_structural_limit, conceptual, 'Whether Article 112 expresses a structural necessity or institutional preference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thai_article_112_mountain, 0, 118).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art112_tr_t0, thai_article_112_mountain, theater_ratio, 0, 0.1).
narrative_ontology:measurement(art112_tr_t55, thai_article_112_mountain, theater_ratio, 55, 0.15).
narrative_ontology:measurement(art112_tr_t118, thai_article_112_mountain, theater_ratio, 118, 0.15).

% Extraction over time
narrative_ontology:measurement(art112_be_t0, thai_article_112_mountain, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(art112_be_t55, thai_article_112_mountain, base_extractiveness, 55, 0.18).
narrative_ontology:measurement(art112_be_t118, thai_article_112_mountain, base_extractiveness, 118, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thai_article_112_mountain, enforcement_mechanism).
narrative_ontology:affects_constraint(thai_article_112_mountain, thai_digital_sedition_act_convergence).
narrative_ontology:affects_constraint(thai_article_112_mountain, thai_monarchy_protection_speech_doctrine).

% DUAL FORMULATION NOTE:
% Article 112 is a constraint family root. The Thai Digital Sedition Act (2007) extends the constraint to online speech; the broader monarchy protection doctrine in Thai jurisprudence uses Article 112 as its anchor. These are separate stories with higher ε values (reflecting extraction and suppression beyond Article 112 alone) but they are downstream of and structurally dependent on Article 112's immutability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
