% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary (Balanced Contestation Reading)
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint describes the interpretive boundary of Basic Laws under a
 *   'balanced contestation' reading, where both the legislature and judiciary
 *   hold legitimate but bounded authority. Neither institution is fully
 *   dominant, leading to a dynamic of institutional dialogue and negotiation
 *   over constitutional interpretation. This reading emphasizes the triadic
 *   negotiation between the court, executive, and legislature, with
 *   extractiveness varying by policy domain.
 *
 * KEY AGENTS:
 *   - legislature: Agenda setter (institutional/constrained) — holds ultimate sovereign power, but constrained.
 *   - judiciary: Agenda setter (institutional/constrained) — interprets within its domain, but bounded.
 *   - executive_branch: Payer (institutional/constrained) — bears costs of institutional friction.
 *   - public_trust_in_institutions: Beneficiary (moderate/identity_locked) — benefits from perceived balance.
 *   - individual_rights_advocates: Payer (organized/constrained) — seeks clear rights protection, but faces uncertainty.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.45).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.3).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary (Balanced Contestation Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, 'effb0bbf-569c-4c25-8fa3-47b6ca5b61c7').
narrative_ontology:cs_kernel_codification('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7', formalized).
narrative_ontology:cs_authority_grounding('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7', lineage).
narrative_ontology:cs_interpretation_layer_present('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7').
narrative_ontology:cs_reading_relation('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7', foundational, institutional_dialogue_as_constitutional_norm).
narrative_ontology:cs_axiom_status(institutional_dialogue_as_constitutional_norm, holdable).
narrative_ontology:cs_axiom_grounding('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7', institutional_dialogue_as_constitutional_norm, conventional).
narrative_ontology:cs_axiom('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7', foundational, bounded_authority_for_all_branches).
narrative_ontology:cs_axiom_status(bounded_authority_for_all_branches, holdable).
narrative_ontology:cs_axiom_grounding('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7', bounded_authority_for_all_branches, deontological).
narrative_ontology:cs_reference_frame('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7', dynamic_institutional_equilibrium).
narrative_ontology:cs_drift_state('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7', contemporary_political_polarization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('effb0bbf-569c-4c25-8fa3-47b6ca5b61c7', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, public_trust_in_institutions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, individual_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ultimate sovereign power but is constrained by international obligations and norms of judicial independence. Engages in dialogue with the judiciary, sometimes adjusting legislation in response to judicial signals, sometimes reasserting its authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, legislature, agenda_setter,
    institutional, generational, constrained, national).

% Interprets Basic Laws within its jurisdictional domain, engaging in judicial review. Its authority is legitimate but bounded, leading to a dynamic of contestation and dialogue with the legislature rather than outright supremacy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Often finds its policy initiatives challenged or delayed by the interpretive boundary, requiring negotiation with both the legislature and judiciary. Bears the costs of institutional friction and policy uncertainty.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Benefits from the perception of a balanced system where no single branch is fully dominant, fostering stability and legitimacy. However, prolonged or severe contestation can erode this trust.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, public_trust_in_institutions, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(basic_law_interpretive_boundary__balanced_contestation_reading, public_trust_in_institutions).

% Seek clear and consistent protection of rights, but the contested interpretive boundary can lead to uncertainty and inconsistent enforcement, requiring continuous advocacy and litigation across both legislative and judicial arenas.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, individual_rights_advocates, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of sovereign power by establishing a framework for institutional dialogue and mutual constraint between the legislative and judicial branches, preventing either from unilaterally dominating constitutional interpretation.
% TRANSFER_FUNCTION: Transfers interpretive authority and policy influence between the legislature and judiciary, depending on the specific issue and political context. It also transfers the burden of navigating this contested space to the executive and civil society.
% ABSENT_VOICES: A purely 'popular sovereignty' perspective, which might argue for direct legislative supremacy without judicial or international constraints, is often marginalized in this reading, as are those who seek absolute judicial finality.
% DISAPPEARANCE_RATIONALE: If this interpretive boundary vanished, either the legislature would assert unchecked power, or the judiciary would claim ultimate supremacy, fundamentally altering the balance of power and the nature of governance. The current system relies on this dynamic tension.
% FOUNDING_PROBLEM: The need to establish a stable constitutional order that balances democratic accountability (legislature) with the protection of fundamental rights and rule of law (judiciary), preventing arbitrary power.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, international legal bodies, and civil society organizations consistently attest to the ongoing challenge of balancing these principles, confirming the founding problem remains live. This is corroborated by comparative constitutional studies and ongoing debates in various democracies.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).
:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because the contestation itself creates costs for policy implementation and certainty, particularly for the executive and advocates. Suppression (0.30) is relatively low, as neither institution can fully suppress the other's claims, leading to active resistance and dialogue. Theater ratio (0.10) is low, indicating that the institutional dialogue is largely genuine, not merely performative. The claimed type is Tangled Rope because it involves coordination (balancing powers) and asymmetric extraction (costs of contestation borne by some more than others), requiring active enforcement of the boundaries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislature and judiciary, this constraint is a necessary mechanism for checks and balances, ensuring a robust constitutional order. From the executive and individual rights advocates, it can appear as a source of friction and uncertainty, extracting costs in terms of policy efficiency and consistent rights enforcement. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature and judiciary are beneficiaries in that they both exercise legitimate authority within the system, even if contested. The executive branch and individual rights advocates are payers, as they bear the direct costs of navigating this contested interpretive space. Public trust is a beneficiary, as the system's legitimacy relies on this perceived balance.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the dynamic as a pure Rope (ignoring the extraction from the executive and advocates) or a Snare (ignoring the genuine coordination function of balancing powers). It acknowledges the ongoing, active nature of the contestation, which prevents mandatrophy by forcing continuous re-engagement with the founding problem of balancing powers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_domain_variation_in_extraction,
    'How does the effective extraction of this constraint vary across different policy domains (e.g., security, social welfare, economic regulation)?',
    'Empirical analysis of judicial review outcomes and legislative responses across a range of policy areas, quantifying the costs of contestation and policy uncertainty in each domain.',
    'If extraction is significantly higher in certain domains, it may indicate a ''snare-like'' dynamic within those specific areas, even if the overall constraint is a Tangled Rope. This would refine the understanding of where the ''balance'' is truly located.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_domain_variation_in_extraction, empirical, 'Variation of extraction across policy domains due to contested interpretive boundary.').

omega_variable(
    institutional_dialogue_genuineness,
    'To what extent is the ''institutional dialogue'' a genuine process of mutual adjustment, versus a performative cover for one institution''s de facto dominance?',
    'Longitudinal study of legislative amendments following judicial review, analysis of dissenting opinions, and expert interviews with institutional actors to assess the actual influence of each branch on the other''s constitutional interpretations.',
    'If the dialogue is largely performative, the theater_ratio would be higher, and the constraint might lean towards a Piton (if function atrophied) or a Snare (if one branch consistently extracts from the other under cover of dialogue).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_dialogue_genuineness, empirical, 'Assessing the authenticity of institutional dialogue in constitutional interpretation.').

omega_variable(
    framing_under_determination_of_balance,
    'Is the ''balanced contestation'' framing a descriptive account of the actual power dynamics, or a normative ideal that masks a de facto tilt towards one institution?',
    'Comparative analysis with other constitutional systems explicitly designed for either judicial supremacy or parliamentary sovereignty, to identify structural features that would indicate a ''tilt'' not captured by the ''balanced'' framing. This is a conceptual omega because it questions the framing itself.',
    'If a de facto tilt is identified, the constraint''s classification might shift towards a ''judicial_supremacy_reading'' or ''parliamentary_sovereignty_reading'' as the more accurate structural description, even if the ''balanced'' ideal is still articulated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_of_balance, conceptual, 'Whether ''balanced contestation'' is descriptive or normative, masking a power imbalance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'basic_law_interpretive_boundary' kernel. Its ε value and structural properties differ significantly from the 'judicial_supremacy_reading' and 'parliamentary_sovereignty_reading' siblings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
