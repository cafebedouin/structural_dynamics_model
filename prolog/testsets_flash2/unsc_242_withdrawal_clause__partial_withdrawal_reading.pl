% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC 242 Withdrawal Clause: Partial Withdrawal Reading
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'partial withdrawal' reading of UNSC
 *   Resolution 242, which interprets the English indefinite article
 *   'territories' as allowing for discretionary withdrawal from some, but not
 *   all, occupied territories, balanced against the principle of secure
 *   boundaries. This reading converts textual ambiguity into negotiating
 *   leverage for the occupying power and mediating states, while claimant
 *   states and displaced populations bear the cost of indefinite occupation.
 *   The constraint is claimed as a Rope by its proponents (a flexible
 *   diplomatic tool) but operates as a Tangled Rope due to its asymmetric
 *   extraction and active enforcement of the occupying power's
 *   interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.45).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.6).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC 242 Withdrawal Clause: Partial Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, '957c1857-7db9-4b50-aaf0-ba8be3885418').
narrative_ontology:cs_kernel_codification('957c1857-7db9-4b50-aaf0-ba8be3885418', fixed_text).
narrative_ontology:cs_authority_grounding('957c1857-7db9-4b50-aaf0-ba8be3885418', lineage).
narrative_ontology:cs_interpretation_layer_present('957c1857-7db9-4b50-aaf0-ba8be3885418').
narrative_ontology:cs_reading_relation('957c1857-7db9-4b50-aaf0-ba8be3885418', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('957c1857-7db9-4b50-aaf0-ba8be3885418', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('957c1857-7db9-4b50-aaf0-ba8be3885418', foundational, indefinite_article_permits_discretion).
narrative_ontology:cs_axiom_status(indefinite_article_permits_discretion, holdable).
narrative_ontology:cs_axiom_grounding('957c1857-7db9-4b50-aaf0-ba8be3885418', indefinite_article_permits_discretion, conventional).
narrative_ontology:cs_axiom('957c1857-7db9-4b50-aaf0-ba8be3885418', foundational, secure_boundaries_principle_permits_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_principle_permits_retention, holdable).
narrative_ontology:cs_axiom_grounding('957c1857-7db9-4b50-aaf0-ba8be3885418', secure_boundaries_principle_permits_retention, instrumental).
narrative_ontology:cs_reference_frame('957c1857-7db9-4b50-aaf0-ba8be3885418', diplomatic_flexibility_framework).
narrative_ontology:cs_drift_state('957c1857-7db9-4b50-aaf0-ba8be3885418', contemporary_diplomatic_stalemate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('957c1857-7db9-4b50-aaf0-ba8be3885418', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, claimant_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the ambiguity of 'territories' (indefinite article) to justify phased or partial withdrawal, retaining strategic areas. Uses the 'secure boundaries' principle to frame its retention as defensive. Actively negotiates withdrawal terms, controlling the pace and scope.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter,
    institutional, generational, constrained, regional).

% Bear the cost of continued occupation and the lack of a clear, enforceable line for full withdrawal. Their claims for full territorial integrity are undermined by the indefinite language and the occupying power's interpretation. Their options are limited to diplomatic pressure and appeals to international bodies.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, claimant_states, payer,
    organized, generational, trapped, regional).

% Benefit from the flexibility the indefinite article provides, allowing for diplomatic solutions and phased agreements. They can leverage the ambiguity to broker deals that might not be possible under a maximal withdrawal interpretation, maintaining their influence in the region.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_states, beneficiary,
    institutional, biographical, mobile, global).

% Are directly impacted by the continued occupation and the lack of full withdrawal. Their right of return and self-determination are deferred or denied by the partial withdrawal reading. Their identity is often tied to their ancestral lands, making 'exit' from the situation a loss of self.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_populations, payer,
    powerless, generational, identity_locked, local).

% Analyze the legal implications of the indefinite article, the drafting history, and the 'secure boundaries' principle. They provide academic commentary on the legitimacy and consequences of this reading, influencing diplomatic discourse but not directly enforcing the constraint.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for negotiating territorial disputes by allowing for flexible interpretations of withdrawal, facilitating phased agreements and diplomatic engagement between conflicting parties.
% TRANSFER_FUNCTION: Transfers negotiating leverage and territorial control from claimant states to the occupying power and mediating states, by allowing for discretionary interpretation of withdrawal scope.
% ABSENT_VOICES: The maximal withdrawal advocates, who interpret the resolution as demanding full withdrawal from all occupied territories, are marginalized in negotiations that proceed under the partial withdrawal reading. Their arguments for territorial integrity are not given equal weight in the diplomatic process.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the occupying power would lose its primary legal justification for retaining strategic territories, forcing a re-evaluation of its position. Diplomatic negotiations would likely stall or shift dramatically towards maximal withdrawal demands, fundamentally altering the regional power balance.
% FOUNDING_PROBLEM: To establish a framework for peace in the Middle East following the 1967 Arab-Israeli War, balancing the need for withdrawal from occupied territories with the need for secure and recognized boundaries for all states in the region.
% FOUNDING_PROBLEM_CORROBORATION: The UN Security Council and various international mediators continue to reference UNSC 242 as the basis for peace negotiations, indicating the problem it sought to address remains unresolved. However, claimant states and displaced populations argue that the 'secure boundaries' aspect has been overemphasized to the detriment of withdrawal.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).
:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while it enables continued occupation, it also facilitates some diplomatic progress and partial withdrawals. Suppression (0.6) is significant as it actively suppresses alternative interpretations (maximal withdrawal) and the full claims of victim groups through diplomatic and legal maneuvering. Theater ratio (0.2) is low, as the diplomatic and legal processes are genuinely active, even if their outcomes are skewed. The temporal measurements show a slight increase in extractiveness and suppression over time as the interpretation solidified, with a recent slight decrease reflecting renewed diplomatic efforts.
 *
 * PERSPECTIVAL GAP:
 *   The occupying power and mediating states perceive this reading as a necessary and flexible diplomatic tool (a Rope), enabling peace processes. Claimant states and displaced populations, however, experience it as a mechanism for prolonged occupation and deferred justice (a Snare or Tangled Rope), where the 'flexibility' is primarily for the benefit of the occupying power. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying power and mediating states are beneficiaries, gaining flexibility and leverage. Claimant states and displaced populations are victims, bearing the costs of delayed or partial withdrawal. The constraint's structure allows the occupying power to maintain its position by leveraging the textual ambiguity, while mediators benefit from the diplomatic space it creates. Victims are trapped by the lack of a clear, enforceable mandate for full withdrawal.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_resolution,
    'Is the indefinite article ''territories'' in UNSC 242 genuinely ambiguous, or is its ambiguity a constructed interpretation to serve political ends?',
    'Comparative linguistic analysis of UN resolutions drafted in multiple languages (e.g., French definite article ''des territoires'' vs. English indefinite ''territories''), coupled with historical diplomatic records of drafting intent.',
    'If genuinely ambiguous, the partial withdrawal reading gains legitimacy as a valid interpretation. If constructed, it exposes the reading as a cover for extraction, potentially reclassifying it towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_resolution, empirical, 'Whether the textual ambiguity is inherent or manufactured.').

omega_variable(
    secure_boundaries_vs_territorial_integrity,
    'How should the principle of ''secure and recognized boundaries'' be balanced against the principle of ''inadmissibility of the acquisition of territory by war'' (territorial integrity)?',
    'International legal precedent from other post-conflict resolutions, advisory opinions from the International Court of Justice, or a new UN Security Council resolution clarifying the hierarchy of these principles.',
    'Prioritizing secure boundaries strengthens the partial withdrawal reading''s justification for retaining strategic territories. Prioritizing territorial integrity would undermine this reading, pushing towards maximal withdrawal and potentially reclassifying this constraint as more extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secure_boundaries_vs_territorial_integrity, conceptual, 'The conceptual tension between secure boundaries and territorial integrity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(unsc_tr_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(unsc_tr_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(unsc_tr_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(unsc_be_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(unsc_be_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(unsc_be_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(unsc_su_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(unsc_su_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(unsc_su_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the UNSC Resolution 242 withdrawal clause. Its interpretation of 'territories' as indefinite directly influences the maximal withdrawal reading (by contesting its premise) and the interpretive authority structure (by demonstrating the need for such an authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
