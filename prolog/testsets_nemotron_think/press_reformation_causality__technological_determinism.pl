% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Printing Press as Autonomous Enabling Technology Making Vernacular Scripture Spread and Reformation Success Inevitable
 *   domain: historical/technological/religious
 *
 * SUMMARY:
 *   This constraint story captures the technological determinist reading of
 *   the printing press's role in the Reformation: the press is treated as an
 *   autonomous enabling technology — a mountain-like physical constraint —
 *   that made the spread of vernacular scripture and the success of the
 *   Reformation inevitable. Human actors (reformers, printers, authorities)
 *   are cast as downstream responders to the technological affordances. The
 *   beneficiary structure of this narrative is obscured: who gains from
 *   portraying the Reformation as technologically inevitable? The reading
 *   itself does not name beneficiaries, but the omega variables surface the
 *   ambiguity.
 *
 * KEY AGENTS:
 *   - printing_press_technology: Primary constraint (mountain) — the physical apparatus and its diffusion
 *   - reformers: Downstream responders — their agency is constrained by the technology's affordances
 *   - printers_and_publishers: Downstream responders — economic actors operating within the technological logic
 *   - religious_authorities: Downstream responders — forced to react to the uncontrollable spread of vernacular scripture
 *   - historians_of_technology: Analytical observers — evaluate the determinist claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.15).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.1).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.15).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Autonomous Enabling Technology Making Vernacular Scripture Spread and Reformation Success Inevitable").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "historical/technological/religious").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, '10efcdc3-b5a2-4136-a9f9-cbe4ed3b5129').
narrative_ontology:cs_kernel_codification('10efcdc3-b5a2-4136-a9f9-cbe4ed3b5129', implicit).
narrative_ontology:cs_authority_grounding('10efcdc3-b5a2-4136-a9f9-cbe4ed3b5129', expertise).
narrative_ontology:cs_reading_relation('10efcdc3-b5a2-4136-a9f9-cbe4ed3b5129', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('10efcdc3-b5a2-4136-a9f9-cbe4ed3b5129', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_axiom('10efcdc3-b5a2-4136-a9f9-cbe4ed3b5129', foundational, printing_press_determines_reformation).
narrative_ontology:cs_axiom_status(printing_press_determines_reformation, holdable).
narrative_ontology:cs_axiom_grounding('10efcdc3-b5a2-4136-a9f9-cbe4ed3b5129', printing_press_determines_reformation, empirically_contingent).
narrative_ontology:cs_reference_frame('10efcdc3-b5a2-4136-a9f9-cbe4ed3b5129', technological_inevitability_thesis).
narrative_ontology:cs_drift_state('10efcdc3-b5a2-4136-a9f9-cbe4ed3b5129', contemporary_historical_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('10efcdc3-b5a2-4136-a9f9-cbe4ed3b5129', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, technological_determinism_of_reformation).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics reflect the reading's self-presentation: low extractiveness (0.15) because the technology is portrayed as a neutral enabler; low suppression (0.1) because inevitability requires no active enforcement; near-zero theater (0.05) because the narrative presents itself as straightforward historical fact; high accessibility_collapse (0.9) because alternatives (e.g., a Reformation without print) are treated as inconceivable; low resistance (0.1) because the determinist reading meets little resistance within its own framework. The claimed_type is mountain, consistent with the reading's assertion that the press operates like a natural law.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical observer seat, the constraint appears as a mountain. From the downstream responder seats (reformers, printers, authorities), the same historical process might be experienced as a tangled rope or snare — they faced strategic choices, extraction, and enforcement. The engine will compute per-seat classifications from the structural data; this commentary notes the expected divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading assigns the technology a beneficiary-like position (d ~ 0.0) because it is the source of enablement; human actors are targets (d ~ 1.0) because they bear the costs of adaptation. However, the reading obscures this directionality by presenting the technology as a neutral force. No beneficiaries or victims are declared in base_properties, reflecting the reading's own silence on distributional consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   The determinist reading may serve a mandatrophy function: it naturalizes a historical outcome that was in fact contingent on human choices, thereby obscuring the interests that benefited from that outcome. If the founding problem (explaining the Reformation's spread) is dead — because modern historiography emphasizes contingency — but the determinist narrative persists, it functions as a piton or false summit. The omega variables capture this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hidden_beneficiary_structure,
    'Does the technological determinist narrative conceal beneficiaries — such as later industrial capitalism, state centralization, or specific confessional interests — that gain from portraying the Reformation as an inevitable technological outcome?',
    'Genealogical analysis of who cites determinist accounts and for what political/ideological purposes; comparative study of alternative historiographies.',
    'If beneficiaries are identified, the mountain claim may be a false summit (FSM trigger), reclassifying the constraint as tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_beneficiary_structure, conceptual, 'Whether the determinist reading obscures a beneficiary structure that would reclassify the constraint.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the kernel ''press_reformation_causality'' best framed as a single historical causal claim, or as a family of distinct constraints (spectral universality vs eigenvector thermalization analog)?',
    'Decompose the kernel into separate constraint stories for each reading and test ε-invariance: does each reading have a stable ε across measurement bases?',
    'If the kernel decomposes cleanly, the current reading is one constraint among several; if not, the readings may be measurement bases of a single constraint, violating ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel admits a single ε or requires decomposition per the ε-invariance principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_reformation_techdet_tr_t0, press_reformation_causality__technological_determinism, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(press_reformation_techdet_tr_t0, observed).
narrative_ontology:measurement(press_reformation_techdet_tr_t50, press_reformation_causality__technological_determinism, theater_ratio, 50, 0.05).
narrative_ontology:measurement_basis(press_reformation_techdet_tr_t50, observed).
narrative_ontology:measurement(press_reformation_techdet_tr_t100, press_reformation_causality__technological_determinism, theater_ratio, 100, 0.05).
narrative_ontology:measurement_basis(press_reformation_techdet_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(press_reformation_techdet_be_t0, press_reformation_causality__technological_determinism, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(press_reformation_techdet_be_t0, observed).
narrative_ontology:measurement(press_reformation_techdet_be_t50, press_reformation_causality__technological_determinism, base_extractiveness, 50, 0.15).
narrative_ontology:measurement_basis(press_reformation_techdet_be_t50, observed).
narrative_ontology:measurement(press_reformation_techdet_be_t100, press_reformation_causality__technological_determinism, base_extractiveness, 100, 0.15).
narrative_ontology:measurement_basis(press_reformation_techdet_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(press_reformation_techdet_su_t0, press_reformation_causality__technological_determinism, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(press_reformation_techdet_su_t0, observed).
narrative_ontology:measurement(press_reformation_techdet_su_t50, press_reformation_causality__technological_determinism, suppression_requirement, 50, 0.1).
narrative_ontology:measurement_basis(press_reformation_techdet_su_t50, observed).
narrative_ontology:measurement(press_reformation_techdet_su_t100, press_reformation_causality__technological_determinism, suppression_requirement, 100, 0.1).
narrative_ontology:measurement_basis(press_reformation_techdet_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
