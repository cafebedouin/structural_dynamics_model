% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Constitutional Text: Legislative Sovereignty Reading
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint represents the 'legislative sovereignty' reading of a
 *   constitutional text, where parliament holds the ultimate authority in
 *   interpreting the constitution, with judicial review being advisory or
 *   subject to legislative override. This reading prioritizes majoritarian
 *   will and legislative flexibility over judicial entrenchment of rights.
 *   The constraint is claimed as a Rope, reflecting its function in
 *   coordinating governmental powers, but its metrics acknowledge the
 *   potential for extraction from minority groups and the judicial branch.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.3).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.4).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Constitutional Text: Legislative Sovereignty Reading").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, '99c42de0-e822-42e2-8072-cc7e9c26d923').
narrative_ontology:cs_kernel_codification('99c42de0-e822-42e2-8072-cc7e9c26d923', fixed_text).
narrative_ontology:cs_authority_grounding('99c42de0-e822-42e2-8072-cc7e9c26d923', lineage).
narrative_ontology:cs_interpretation_layer_present('99c42de0-e822-42e2-8072-cc7e9c26d923').
narrative_ontology:cs_reading_relation('99c42de0-e822-42e2-8072-cc7e9c26d923', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('99c42de0-e822-42e2-8072-cc7e9c26d923', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('99c42de0-e822-42e2-8072-cc7e9c26d923', foundational, legislative_will_is_supreme).
narrative_ontology:cs_axiom_status(legislative_will_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('99c42de0-e822-42e2-8072-cc7e9c26d923', legislative_will_is_supreme, conventional).
narrative_ontology:cs_axiom('99c42de0-e822-42e2-8072-cc7e9c26d923', secondary, constitutional_flexibility_is_desirable).
narrative_ontology:cs_axiom_status(constitutional_flexibility_is_desirable, holdable).
narrative_ontology:cs_axiom_grounding('99c42de0-e822-42e2-8072-cc7e9c26d923', constitutional_flexibility_is_desirable, instrumental).
narrative_ontology:cs_reference_frame('99c42de0-e822-42e2-8072-cc7e9c26d923', parliamentary_sovereignty_tradition).
narrative_ontology:cs_drift_state('99c42de0-e822-42e2-8072-cc7e9c26d923', contemporary_rights_charter_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('99c42de0-e822-42e2-8072-cc7e9c26d923', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, legislature).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majority_electorate).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_groups).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, judicial_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the supreme body, the legislature has the final say on constitutional meaning, often through mechanisms like notwithstanding clauses. It benefits from direct implementation of majoritarian will.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, legislature, agenda_setter,
    institutional, generational, constrained, national).

% The will of the majority, expressed through elected representatives, is paramount. This reading ensures that legislative decisions reflect current popular sentiment without significant judicial obstruction.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, majority_electorate, beneficiary,
    organized, biographical, mobile, national).

% The courts provide constitutional advice and review, but their decisions can be overridden by the legislature. This limits their power and makes them a 'payer' in terms of their interpretive authority.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, judicial_branch, payer,
    institutional, generational, identity_locked, national).

% Minority rights and interests are vulnerable to majoritarian legislative action, as judicial protections can be circumvented. They bear the cost of potentially unchecked legislative power.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_groups, payer,
    powerless, generational, trapped, national).

% Analyze the implications of legislative supremacy for constitutional stability, rights protection, and democratic theory. They observe the practical effects of this interpretive framework.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, final arbiter of constitutional meaning, ensuring that legislative action can proceed without perpetual judicial deadlock and reflecting the current democratic mandate.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over the constitution from the judiciary to the legislature, thereby transferring power to implement policy directly from the people's elected representatives.
% ABSENT_VOICES: Advocates for strong judicial review and entrenched minority rights are present in public discourse but are structurally disempowered by this reading's framework, which prioritizes legislative finality.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the balance of power would fundamentally shift. The judiciary would likely assert greater interpretive authority, leading to more frequent judicial invalidation of legislation and a different constitutional equilibrium.
% FOUNDING_PROBLEM: To ensure that the will of the democratically elected representatives is not unduly frustrated by unelected judicial bodies, and to maintain flexibility in constitutional interpretation to adapt to changing societal needs.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and legal historians corroborate that the tension between legislative power and judicial review is a persistent feature of constitutional democracies, and this reading offers a coherent (though contested) resolution to that tension.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).
:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.3) as it primarily extracts interpretive authority from the judiciary and potentially rights from minorities, rather than direct economic rents. Suppression is moderate (0.4) because while judicial decisions can be overridden, the courts still function and advise, and there are political costs to frequent overrides. Theater ratio is low (0.1) as the legislative supremacy is generally a direct and functional aspect of governance, not merely performative. Accessibility collapse is moderate (0.6) as alternatives like strong judicial review are conceptually available but structurally suppressed by this reading. Resistance is moderate (0.3) from judicial advocates and minority groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislature and majority electorate, this is a legitimate and efficient way to govern, ensuring democratic responsiveness. From the perspective of the judiciary and minority groups, it represents a potential for unchecked power and a weakening of constitutional safeguards. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature and the majority electorate are beneficiaries, as this reading empowers them. The judicial branch and minority groups are payers, as their interpretive authority and rights protections are diminished. Constitutional scholars act as observers, analyzing the system's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_of_power_stability,
    'Does this reading of legislative supremacy lead to long-term constitutional stability or does it risk cycles of majoritarian overreach and constitutional crisis?',
    'Comparative historical analysis of jurisdictions operating under similar constitutional frameworks, tracking instances of constitutional crises, rights violations, and legislative overrides.',
    'If it leads to instability, the ''rope'' classification might be challenged, suggesting a more ''tangled rope'' or ''snare'' dynamic where the coordination function is undermined by unchecked power. If stable, it reinforces the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_power_stability, empirical, 'Assesses the long-term stability of a system with legislative supremacy.').

omega_variable(
    minority_rights_protection_efficacy,
    'How effectively are minority rights protected under a legislative sovereignty framework, given the potential for legislative override of judicial decisions?',
    'Empirical study of legislative actions affecting minority groups, judicial interventions, and the use of override clauses, alongside an analysis of the political and social mechanisms that might still protect minorities.',
    'If minority rights are consistently undermined, the ''extractiveness'' and ''suppression'' metrics for minority groups would need to be re-evaluated upwards, potentially shifting the overall classification towards a ''snare'' for those groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_rights_protection_efficacy, empirical, 'Evaluates the practical protection of minority rights under legislative supremacy.').

omega_variable(
    interpretive_authority_location,
    'Is the constitutional text itself inherently ambiguous, allowing for multiple legitimate readings of interpretive authority, or does one reading (e.g., judicial supremacy) have a stronger textual basis?',
    'Deep textual and historical analysis of the constitutional document''s drafting, original intent (if applicable), and subsequent amendments, alongside a conceptual analysis of ''constitutional meaning''.',
    'If the text strongly supports one reading over others, the ''conceptual'' ambiguity of this constraint would lessen, potentially strengthening the claim of one reading as more ''natural'' or ''foundational'' within the constitutional framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_location, conceptual, 'Examines the textual basis for different readings of constitutional interpretive authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__legislative_sovereignty_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__legislative_sovereignty_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__legislative_sovereignty_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cons_be_t10, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(cons_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(cons_be_t30, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement(cons_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(cons_be_t50, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 50, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cons_su_t10, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(cons_su_t20, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(cons_su_t30, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement(cons_su_t40, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(cons_su_t50, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'constitutional_text' kernel, focusing on legislative supremacy. It coexists with judicial_supremacy_reading and popular_sovereignty_reading, each representing a distinct interpretive framework for constitutional authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
