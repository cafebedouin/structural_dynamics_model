% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling as Constitutionally Void under 14th Amendment Section 4 (Nullity Reading)
 *   domain: constitutional_law/fiscal_governance/political_economy
 *
 * SUMMARY:
 *   The 14th Amendment Section 4 (enacted 1868) declares: 'The validity of
 *   the public debt of the United States, authorized by law, including debts
 *   incurred for payment of pensions and bounties for services in suppressing
 *   the insurrection, shall not be questioned.' The statutory debt ceiling
 *   (enacted 1917, continuously revised) purports to cap the aggregate amount
 *   Treasury may borrow. The constitutional nullity reading holds that
 *   Section 4 mandates Treasury borrowing to satisfy all lawful
 *   appropriations, rendering any statutory ceiling void ab initio under
 *   constitutional supremacy doctrine. The constraint from this reading is
 *   not the debt ceiling itself (which is null) but the immutable
 *   constitutional requirement that validly appropriated obligations must be
 *   paid. This reading instantiates the narrowest logical path:
 *   constitutional supremacy + Section 4 mandatory language = Treasury
 *   borrowing is constitutionally required = statutory ceiling cannot bind.
 *   The nullity reading claims that the entire 110+ year practice of
 *   congressional debt ceiling votes is theater — a performative
 *   contradiction where Congress appropriates funds it refuses to authorize
 *   borrowing for, while Treasury executes the constitutionally mandated
 *   borrowing anyway. The rising theater ratio (0.40 → 0.95) models the
 *   accumulating recognition that the ritual persists despite theoretical
 *   legal nullity: the institutional theater increases as the logical
 *   contradiction becomes more explicit.
 *
 * KEY AGENTS:
 *   - Constitutional Supremacy Doctrine: Primary beneficiary (analytical/analytical) — the nullity reading vindicates supremacy as operative fact, not mere principle
 *   - Treasury Secretary: Mandatory executor (powerful/mobile) — constitutionally bound to borrow regardless of statutory ceiling; the nullity reading eliminates discretion
 *   - Congress: Theater-bound institutional actor (institutional/arbitrage) — votes on ceiling increases for political messaging while the votes are constitutionally irrelevant (piton perspective); benefits from maintaining the illusion of fiscal constraint
 *   - Creditors/Bond Market: Structural beneficiary (institutional/arbitrage) — the nullity reading guarantees that Treasury will borrow as needed, eliminating repudiation risk; eliminates default possibility
 *   - Taxpayers/Public: Abstract victim of theater (powerless/trapped) — subjected to political brinkmanship and shutdown threats despite constitutional guarantee of debt payment; the theater is performed on them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.95).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.95).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling as Constitutionally Void under 14th Amendment Section 4 (Nullity Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/fiscal_governance/political_economy").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'fb2942d9-70cf-42a3-b03f-a5f1aa0839f7').
narrative_ontology:cs_kernel_codification('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7', formalized).
narrative_ontology:cs_authority_grounding('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7', lineage).
narrative_ontology:cs_interpretation_layer_present('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7').
narrative_ontology:cs_reading_relation('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7', foundational, constitutional_supremacy_mandatory).
narrative_ontology:cs_axiom_status(constitutional_supremacy_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7', constitutional_supremacy_mandatory, deontological).
narrative_ontology:cs_axiom('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7', foundational, section_4_borrowing_mandate).
narrative_ontology:cs_axiom_status(section_4_borrowing_mandate, holdable).
narrative_ontology:cs_axiom_grounding('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7', section_4_borrowing_mandate, empirically_contingent).
narrative_ontology:cs_reference_frame('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7', constitutional_supremacy_doctrine).
narrative_ontology:cs_drift_state('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7', contemporary_debt_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb2942d9-70cf-42a3-b03f-a5f1aa0839f7', '2026-02-26T15:32:00Z').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL NULLITY (MOUNTAIN) — From the standpoint of constitutional supremacy and Marbury v. Madison supremacy doctrine, the debt ceiling statute is logically void because it purports to regulate conduct (Treasury borrowing to satisfy appropriations) that the 14th Amendment Section 4 has already constitutionally mandated. The statute cannot bind an agency to violate the Constitution. Extractiveness = 0.0 because there is no constraint operating — the debt ceiling is legally inoperative. Suppression = 0.0 because there are no alternative pathways suppressed; the Constitution mandates only one pathway (Treasury must borrow to satisfy lawful appropriations). This is mountain because the legal-logical relationship between constitutional supremacy and statutory void acts is an irreducible limit on what statutory law can do.
constraint_indexing:constraint_classification(statutory_debt_ceiling__constitutional_nullity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL THEATER (PITON) — Congress continues voting on debt ceiling increases despite the reading's claim that the statute is void. The ritual persists through institutional inertia and political theater: appropriating funds while refusing to authorize the borrowing needed to pay them is a performative contradiction that Congress maintains for political messaging. The constraint from Congress's perspective appears as a piton — a degraded institutional form where the primary function (regulating borrowing) has been theoretically evacuated but the ritual (voting, negotiation, brinkmanship) persists. Theater ratio 0.95 captures that the entire process is now ceremonial if the nullity reading is correct.
constraint_indexing:constraint_classification(statutory_debt_ceiling__constitutional_nullity_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY EXECUTOR (MOUNTAIN) — From the Treasury Secretary's perspective, if the constitutional nullity reading obtains, the debt ceiling imposes no binding constraint. The Treasury Secretary is constitutionally mandated to borrow as necessary to pay appropriated obligations. The statute claiming to cap borrowing is null — it constrains nothing and suppresses nothing. The Secretary's duty is constitutionally determined and constitutionally supreme. No exit; no alternative. This is mountain because the constitutional mandate is unchangeable by statute.
constraint_indexing:constraint_classification(statutory_debt_ceiling__constitutional_nullity_reading, mountain,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL SUPREMACY (MOUNTAIN) — The irreducible logical constraint is: Constitution > Statute in the hierarchy of legal authority (Marbury v. Madison, established 1803). If Section 4 of the 14th Amendment mandates that 'the validity of the public debt of the United States...shall not be questioned,' and Congress has appropriated funds, then the Treasury must borrow to satisfy those appropriations. A statute purporting to prevent this borrowing is void ab initio. This is not negotiable, not context-dependent, not alterable by assertion. The constraint is the logical structure of constitutional supremacy itself.
constraint_indexing:constraint_classification(statutory_debt_ceiling__constitutional_nullity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(statutory_debt_ceiling__constitutional_nullity_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statutory_debt_ceiling__constitutional_nullity_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, ExtMetricName, E),
    domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

test(piton_threshold) :-
    domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, TR),
    TR >= 0.70.

:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   NULLITY READING: Extractiveness = 0.0 (the constraint is legally inoperative). Suppression = 0.0 (there are no alternatives to suppress — the Constitution mandates one pathway only). Theater ratio = 0.95 (the entire institutional practice is ceremonial if the reading is correct — Congress votes on a void statute, Treasury ignores it and borrows anyway, creditors are guaranteed repayment by the Constitution, and the public sees political theater that is constitutionally irrelevant). This is MOUNTAIN because: (1) constitutional supremacy is an irreducible limit on what statutory law can do; (2) the logical relationship between Marbury v. Madison and Section 4 is immutable — it does not change with congressional will, presidential discretion, or electoral outcomes; (3) the constraint is the constitutional requirement itself, not any statutory impediment. The reading declares zero extractiveness because it rejects the premise that the debt ceiling extracts anything — it claims the ceiling is null and therefore neither constrains nor extracts. Beneficiary: constitutional supremacy doctrine (the nullity reading's central claim). Victims: technically none, because if the reading is correct, no constraint exists to victimize anyone. However, the institutional theater harms the public by subjecting them to unnecessary political brinkmanship (this is a secondary effect, not primary extraction from the constraint itself).
 *
 * PERSPECTIVAL GAP:
 *   This reading produces maximum perspectival divergence from the sibling readings. The coordination scaffold reading sees the debt ceiling as a temporary coordination problem (Congress and Treasury negotiating fiscal authority) with a sunset (as fiscal norms mature). The extraction snare reading sees the debt ceiling as pure extraction (Congress/creditors extract political authority over appropriations, targeting taxpayers and the public). The nullity reading rejects both: the constraint doesn't exist as a binding legal matter. From the nullity reading's perspective, the scaffold reading misdiagnoses a void statute as a coordination mechanism, and the snare reading misdiagnoses a null statute as an extraction mechanism. Both sibling readings incorrectly accept the debt ceiling's legal validity; the nullity reading rejects that validity at the root. The perspectival gap is not about different experiences of the same constraint, but about whether a constraint exists at all.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable in the traditional sense because extractiveness = 0.0. The constraint from this reading's perspective is not a relational mechanism (beneficiary vs. victim) but a constitutional declaration of what is legally mandatory. Treasury is not choosing to favor creditors over taxpayers — Treasury is executing the Constitution regardless of statutory obstacle. The beneficiary is constitutional supremacy doctrine itself (the reading vindicates the claim that Constitution > Statute). The piton perspective's institutional actors experience the theater, but the theater is not an extractive mechanism in this reading — it is a performative ritual that has been logically superseded. The nullity reading's claim is that there is no extraction to analyze because there is no binding constraint to produce extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by claiming extractiveness = 0.0, theater = 0.95. If the reading is correct, the constraint is NOT a snare (zero extractiveness, not high), NOT a tangled rope (zero coordination function, not mixed), NOT a scaffold (no sunset needed because the constraint is already null). It IS a mountain (constitutional supremacy is immutable) AND a piton (institutional theater persists despite functional nullity). The mandatrophy resolution depends on accepting that a constraint can have zero extractiveness while maintaining high theater — institutional actors perform the ritual while the logical constraint is void. This is diagnostically coherent: the institution is doing theater about something that doesn't exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    section_4_mandatory_interpretation,
    'Does the 14th Amendment Section 4 mandatorily require Treasury borrowing to satisfy all appropriated obligations, or does it merely prohibit repudiation of validly incurred debt?',
    'Constitutional interpretation via originalist analysis of Section 4''s text and historical context (1868); comparison with subsequent constitutional amendments and Supreme Court precedent on mandatory appropriations enforcement',
    'If mandatory: debt ceiling is void ab initio; extractiveness = 0.0. If merely prohibitory: debt ceiling can regulate new borrowing while remaining constitutionally consistent; extractiveness rises (constraint becomes either coordination problem or extraction mechanism). This is the reading''s foundational omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_4_mandatory_interpretation, conceptual, 'Whether Section 4 mandates borrowing or merely prohibits repudiation').

omega_variable(
    supremacy_doctrine_collapse,
    'If the debt ceiling is void, why does Congress continue voting on it and why have Presidents submitted to the constraint rather than Treasury simply executing unconstrained borrowing?',
    'Historical analysis of Treasury practice (2011-2023 debt ceiling standoffs); examination of presidential legal memo affirming or denying debt ceiling constitutionality; forensic documentation of actual borrowing authority Treasury exercised vs. claimed ceiling',
    'If Congress and Presidents acknowledge the void, the piton classification holds: ritual theater persists despite functional nullity. If Congress and Presidents claim the statute binds them despite constitutional argument, the nullity reading is rejected in practice and the constraint functions as either coordination (scaffold) or extraction (snare), not mountain. Practice legitimacy differs from logical supremacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supremacy_doctrine_collapse, empirical, 'Why institutional actors treat a void constraint as binding').

omega_variable(
    section_4_scope_ambiguity,
    'Does ''the validity of the public debt...shall not be questioned'' refer only to existing debt incurred under prior constitutional frameworks, or does it extend to any debt incurred under current lawful appropriations?',
    'Textual analysis: does ''public debt'' in Section 4 context mean debt already incurred (historical referent) or debt validly authorized going forward? Close reading of Section 5 enforcement clause and subsequent Supreme Court interpretations of debt scope.',
    'If historical-only: Section 4 does not mandate new borrowing for current appropriations; debt ceiling can be imposed prospectively without constitutional violation; constraint is coordinate or extractive, not void. If forward-looking: Treasury must borrow for all lawful appropriations; debt ceiling is void; constraint is mountain. This distinction maps directly to whether the reading is coherent or self-defeating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_4_scope_ambiguity, conceptual, 'Temporal scope of Section 4''s protection of public debt').

omega_variable(
    marbury_hierarchy_assumption,
    'Does Marbury v. Madison supremacy doctrine actually establish that all statutory provisions must bend to constitutional language, or does it establish only that courts resolve conflicts when they arise?',
    'Jurisprudential analysis: examination of cases where courts have affirmed statutory constraints despite constitutional tension (e.g., qualified immunity despite Fourth Amendment); analysis of where courts have invoked supremacy doctrine to void statutes directly vs. where courts have allowed statutory constraints to operate despite constitutional argument',
    'If supremacy is automatic: nullity reading is correct and debt ceiling is void. If supremacy is judicially contingent: nullity reading is a constitutional argument, not a constitutional fact; constraint remains binding until courts void it; the question is shifted from ''is the ceiling void?'' to ''would courts void the ceiling?''; this is now a snare or tangled_rope reading, not mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marbury_hierarchy_assumption, conceptual, 'Whether constitutional supremacy operates automatically or requires judicial enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1913, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debt_ceiling_theater_t0, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(debt_ceiling_theater_t15, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 15, 0.72).
narrative_ontology:measurement(debt_ceiling_theater_t30, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 30, 0.95).

% Extraction over time
narrative_ontology:measurement(debt_ceiling_extract_t0, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(debt_ceiling_extract_t15, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 15, 0.0).
narrative_ontology:measurement(debt_ceiling_extract_t30, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 30, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__extraction_snare_reading).

% DUAL FORMULATION NOTE:
% The statutory debt ceiling kernel decomposes into three structurally distinct readings with different extractiveness values and different constraint types. This reading (constitutional_nullity_reading, ε=0.0, Mountain) rejects the legal validity of the statutory ceiling, claiming it is void under constitutional supremacy. The scaffold reading (ε≈0.35, Tangled Rope) treats the ceiling as a valid coordination mechanism with sunset clauses in political norms. The snare reading (ε≈0.68, Snare) treats the ceiling as valid extraction mechanism. Each reading has its own beneficiary/victim structure and its own classification. The three readings coexist as different interpretive frameworks held by different institutional actors and legal theorists. See network edges for interdependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
