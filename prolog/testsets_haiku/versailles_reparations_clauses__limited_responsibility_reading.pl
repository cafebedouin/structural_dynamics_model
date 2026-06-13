% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__limited_responsibility_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations: Limited Responsibility Reading
 *   domain: international_relations/legal_history
 *
 * SUMMARY:
 *   The Treaty of Versailles imposed reparations on Germany, nominally to
 *   compensate victors for war costs. The limited responsibility reading
 *   argues that Article 231 (the 'war guilt' clause) is a legal formality
 *   establishing obligation, NOT a moral judgment of unlimited culpability.
 *   Reparations must align with German economic capacity; exceeding capacity
 *   reduces total collectible revenue and destabilizes Europe. This reading
 *   modulates Allied maximalism and reframes reparations as constrained
 *   extraction rather than punitive transfer. The constraint operates as a
 *   tangled rope: genuine coordination problem (distributing war costs
 *   sustainably) entangled with asymmetric extraction (German population
 *   bears concentrated costs). The divergence between claimed type
 *   (tangled_rope) and authored metrics (moderate-to-high extraction
 *   throughout) is deliberate—the reading claims
 *   coordination-with-extraction; the metrics measure the extraction
 *   dominance.
 *
 * KEY AGENTS:
 *   - German government: trapped payer, negotiates within capacity constraints
 *   - German population: powerless payer, identity-locked to national obligation, absorbs austerity
 *   - Allied creditor states: institutional beneficiaries, accept capacity moderation to secure payment flow
 *   - Occupied territories: organized beneficiary, claims bounded by viability principle
 *   - German industrial elites: powerful secondary payers, gain rhetorical leverage from capacity argument
 *   - League of Nations: agenda-setter, administers capacity-bounded payment schedules (Dawes Plan)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.58).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.52).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations: Limited Responsibility Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '44ef0922-996d-400a-b896-7b2e4784d1cc').
narrative_ontology:cs_kernel_codification('44ef0922-996d-400a-b896-7b2e4784d1cc', fixed_text).
narrative_ontology:cs_authority_grounding('44ef0922-996d-400a-b896-7b2e4784d1cc', extraction).
narrative_ontology:cs_interpretation_layer_present('44ef0922-996d-400a-b896-7b2e4784d1cc').
narrative_ontology:cs_reading_relation('44ef0922-996d-400a-b896-7b2e4784d1cc', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('44ef0922-996d-400a-b896-7b2e4784d1cc', versailles_reparations_clauses__repudiation_reading, influences).
narrative_ontology:cs_axiom('44ef0922-996d-400a-b896-7b2e4784d1cc', foundational, reparations_payments_must_not_exceed_sustainable_capacity).
narrative_ontology:cs_axiom_status(reparations_payments_must_not_exceed_sustainable_capacity, holdable).
narrative_ontology:cs_axiom_grounding('44ef0922-996d-400a-b896-7b2e4784d1cc', reparations_payments_must_not_exceed_sustainable_capacity, instrumental).
narrative_ontology:cs_axiom('44ef0922-996d-400a-b896-7b2e4784d1cc', foundational, article_231_establishes_legal_obligation_not_moral_unlimited_liability).
narrative_ontology:cs_axiom_status(article_231_establishes_legal_obligation_not_moral_unlimited_liability, holdable).
narrative_ontology:cs_axiom_grounding('44ef0922-996d-400a-b896-7b2e4784d1cc', article_231_establishes_legal_obligation_not_moral_unlimited_liability, conventional).
narrative_ontology:cs_reference_frame('44ef0922-996d-400a-b896-7b2e4784d1cc', capacity_bounded_obligations).
narrative_ontology:cs_drift_state('44ef0922-996d-400a-b896-7b2e4784d1cc', post_dawes_plan_institutionalization, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('44ef0922-996d-400a-b896-7b2e4784d1cc', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_states).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_population).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_capacity).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts high (0.72 in 1919: initial reparations demand of 132 billion gold marks far exceeds demonstrated German capacity) and falls through 1924 when the Dawes Plan operationalizes capacity-bounded schedules (extractiveness 0.62). It plateaus near 0.58–0.61 through the late 1920s as the schedules stabilize. The slight rise in 1930 reflects economic contraction reducing capacity without commensurate payment reduction. Suppression is high initially (0.68: enforced by occupation and threat of renewed military action) and falls as the Dawes Plan substitutes technical machinery for direct coercion (0.48 by 1928). Theater ratio rises from 0.35 to 0.44, indicating that over time an increasing share of enforcement activity is devoted to maintaining the appearance of capacity-based scheduling rather than collecting maximum available revenue. All metrics share one time grid so every series is complete at each examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the Allied institutional seat, the constraint is genuine coordination: a mechanism to extract war costs without destabilizing the debtor. From the German population seat, it is enforced extraction: austerity and sacrifice imposed by occupiers to extract resources beyond what autonomous choice would yield. From the economic analyst seat, it is a technical problem: can capacity-bounded schedules sustain payment? The engine computes directionality per seat; German powerless agents sit near d=1.0 (full targets), while Allied institutional beneficiaries sit near d=0.2 (beneficiaries with exit). The divergence is structural, not observational.
 *
 * DIRECTIONALITY LOGIC:
 *   German government and population are victims (high d, high extraction burden). Allied states and occupied territories are beneficiaries (low d, receive transfers). German industrial elites are secondary payers but mobile (d near 0.6–0.7: they bear costs but retain capital mobility options unavailable to the general population). The capacity-bounded framing gives German negotiators leverage to argue downward (a payer-side advantage), reducing d toward the moderate end for institutional German actors while powerless German citizens remain trapped near d=0.95. Exit options differentiate dramatically: Allied states can arbitrage toward other debtors or investment opportunities; German powerless populations are identity-locked to their national obligation.
 *
 * MANDATROPHY ANALYSIS:
 *   The limited responsibility reading prevents misclassification in two directions: (1) it resists calling the constraint a pure snare—there is genuine coordination (sustainable distribution of war costs) entangled with extraction; (2) it resists calling it pure rope—the extraction is substantial (German population bears concentrated costs), sustained by occupation force and treaty law, not by participant preference. The tangled_rope claim captures the hybrid: coordination function + asymmetric extraction + active enforcement. The measurement series tracks the progressive substitution of the Dawes Plan machinery (technical capacity assessment) for direct occupation coercion, raising theater_ratio—a sign that the coordination legitimacy story is carrying more of the enforcement load over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_vs_legal_article_231,
    'Does Article 231 ground a moral judgment of unlimited German culpability, or is it a legal formality establishing obligation only?',
    'Historical textual analysis of negotiation records; contemporary Allied statements on whether the clause asserted moral culpability or merely established a legal hook for reparations demands.',
    'If moral judgment: the punitive_liability_reading gains force and reparations demands should track moral responsibility (potentially unlimited). If legal formality only: capacity-bounded payments are justified and the limited_responsibility_reading is structurally sound.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_vs_legal_article_231, empirical, 'Article 231''s intended scope: moral or legal?').

omega_variable(
    capacity_assessment_contention,
    'Is the German economic capacity at which reparations schedules are set honestly assessed, or does the capacity-bounded framing mask Allied maximalism?',
    'Economic reconstruction: comparing published capacity assessments (Dawes Plan, League reports) against actual German economic data, production capacity, and trade flows. Testing whether scheduled payments track capacity or systematically exceed it.',
    'If honestly assessed: the constraint is a genuine tangled_rope (coordination + moderate extraction). If dishonestly inflated: extractiveness is higher than authored (0.58) and the constraint approaches snare (pure extraction disguised as capacity-based moderation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_assessment_contention, empirical, 'Is capacity assessment genuine or a cover for extraction maximalism?').

omega_variable(
    reading_dependence_on_treaty_legitimacy,
    'Does the limited_responsibility_reading require that the Versailles Treaty itself is legitimate, or is it compatible with the repudiation_reading''s treaty-legitimacy challenge?',
    'Logical analysis: If Germany has no obligation to pay any reparations (repudiation), then capacity-bounded payment schedules are moot. If Germany has an obligation (limited_responsibility), the treaty framework is legitimate at some level. These readings occupy different frames.',
    'If the limited_responsibility_reading requires treaty legitimacy, it forecloses repudiation within a single party''s framework (high constraint). If it is compatible with repudiation (capacity-based payments are a practical compromise regardless of treaty legitimacy), they coexist more freely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_dependence_on_treaty_legitimacy, conceptual, 'Whether limited_responsibility is compatible with rejecting treaty legitimacy.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of German resistance (0.52 baseline, falling to 0.48 by 1928) structural (occupation force, legal treaty enforcement) or internalized (German acceptance of responsibility norm)?',
    'Analyzing German political discourse, labor movements, and resistance intensity over the interval. If resistance drops after occupation forces withdraw (1930), suppression is primarily structural. If resistance persists, suppression has internalized components.',
    'If structural: post-exit suppression drops sharply and the constraint''s hold is occupation-dependent. If internalized: German society internalizes the responsibility narrative and the constraint persists without external coercion, shifting toward rope (coordination accepted).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Is suppression structural or internalized in German acceptance?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1919, 0.35).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1921, 0.38).
narrative_ontology:measurement(vers_tr_t1924, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1924, 0.42).
narrative_ontology:measurement(vers_tr_t1928, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1928, 0.44).
narrative_ontology:measurement(vers_tr_t1930, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1930, 0.48).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1932, 0.42).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1919, 0.72).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1921, 0.68).
narrative_ontology:measurement(vers_be_t1924, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1924, 0.62).
narrative_ontology:measurement(vers_be_t1928, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1928, 0.58).
narrative_ontology:measurement(vers_be_t1930, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1930, 0.61).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1932, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1919, 0.68).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1921, 0.64).
narrative_ontology:measurement(vers_su_t1924, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1924, 0.52).
narrative_ontology:measurement(vers_su_t1928, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1928, 0.48).
narrative_ontology:measurement(vers_su_t1930, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1930, 0.55).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1932, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__limited_responsibility_reading, 0.18).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% The contested kernel versailles_reparations_clauses has three structurally distinct readings: limited_responsibility (capacity-bounded payments), punitive_liability (moral responsibility permits unlimited claims), and repudiation (treaty is illegitimate; no binding obligation). Each reading instantiates a different ε (extraction level), beneficiary structure, and type. This file is the limited_responsibility reading. The three readings form a constraint family linked by network.affects_constraints in each member; they share a kernel (the treaty text and Article 231) but diverge in how they interpret that kernel and what constraints that interpretation instantiates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
