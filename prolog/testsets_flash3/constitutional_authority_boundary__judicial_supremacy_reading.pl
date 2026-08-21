% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy over Constitutional Interpretation
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of the
 *   constitutional authority boundary, where courts are the final arbiters of
 *   constitutional questions, capable of invalidating acts of other branches
 *   without remedy. This reading is distinct from 'coordinate construction'
 *   (where all branches interpret) or 'parliamentary primacy' (where the
 *   legislature is supreme). The constraint is claimed as a Tangled Rope
 *   because it provides a coordination function (finality of interpretation)
 *   but also involves significant asymmetric extraction (judicial veto over
 *   elected branches) and requires active enforcement (judicial review).
 *
 * KEY AGENTS:
 *   - judiciary: Primary beneficiary/agenda_setter (institutional/identity_locked)
 *   - legislature: Primary payer (institutional/constrained)
 *   - executive_branch: Payer (institutional/constrained)
 *   - electorate: Payer (organized/constrained)
 *   - legal_profession: Beneficiary (organized/mobile)
 *   - coordinate_construction_advocates: Excluded (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy over Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '9f6b5282-cb8b-493c-85da-29b950455c02').
narrative_ontology:cs_kernel_codification('9f6b5282-cb8b-493c-85da-29b950455c02', fixed_text).
narrative_ontology:cs_authority_grounding('9f6b5282-cb8b-493c-85da-29b950455c02', lineage).
narrative_ontology:cs_interpretation_layer_present('9f6b5282-cb8b-493c-85da-29b950455c02').
narrative_ontology:cs_reading_relation('9f6b5282-cb8b-493c-85da-29b950455c02', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('9f6b5282-cb8b-493c-85da-29b950455c02', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('9f6b5282-cb8b-493c-85da-29b950455c02', foundational, judicial_interpretive_finality).
narrative_ontology:cs_axiom_status(judicial_interpretive_finality, holdable).
narrative_ontology:cs_axiom_grounding('9f6b5282-cb8b-493c-85da-29b950455c02', judicial_interpretive_finality, conventional).
narrative_ontology:cs_axiom('9f6b5282-cb8b-493c-85da-29b950455c02', foundational, constitutional_supremacy_through_judicial_review).
narrative_ontology:cs_axiom_status(constitutional_supremacy_through_judicial_review, holdable).
narrative_ontology:cs_axiom_grounding('9f6b5282-cb8b-493c-85da-29b950455c02', constitutional_supremacy_through_judicial_review, deontological).
narrative_ontology:cs_reference_frame('9f6b5282-cb8b-493c-85da-29b950455c02', marbury_v_madison_doctrine).
narrative_ontology:cs_drift_state('9f6b5282-cb8b-493c-85da-29b950455c02', contemporary_political_polarization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9f6b5282-cb8b-493c-85da-29b950455c02', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional text and invalidates acts of other branches. Benefits from interpretive monopoly and expanded institutional power. Its identity is fused with this role, making exit (relinquishing interpretive supremacy) unthinkable.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Has its legislative acts subject to judicial review and potential invalidation, constraining its policy space. Bears the cost of policy reversals and the need to draft legislation to anticipate judicial interpretation. Exit (asserting co-equal interpretive authority) is politically costly and often fails.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Has its executive orders and actions subject to judicial review and potential invalidation. Bears the cost of policy implementation delays and reversals. Similar to the legislature, direct challenge to judicial supremacy is difficult.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Bears the cost of counter-majoritarian outcomes where democratically enacted laws are overturned by unelected judges. Their only recourse is through the political process (e.g., constitutional amendment), which is a high-friction, long-horizon exit option.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, electorate, payer,
    organized, generational, constrained, national).

% Benefits from the complexity and centrality of judicial interpretation, which creates demand for legal expertise in constitutional litigation and advising. Its influence and status are enhanced by the judiciary's supreme interpretive role.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Argue for a system where all three branches have co-equal interpretive authority, challenging the judiciary's final say. Their arguments are often marginalized in mainstream legal discourse, making their voice absent from the effective decision-making process.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, coordinate_construction_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, authoritative interpretation of the constitutional text, aiming to ensure legal consistency and stability across different governmental acts and over time.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority and policy-veto power from the democratically elected branches (legislature, executive) to the unelected judiciary, along with the associated rents (status, influence, career paths) to the legal profession.
% ABSENT_VOICES: Advocates for coordinate construction or parliamentary supremacy are largely excluded from the institutional mechanisms that reinforce judicial supremacy. They would argue for a more distributed or politically accountable interpretive process.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the legislature and executive would immediately assert their own constitutional interpretations, leading to a period of intense inter-branch conflict and potentially conflicting constitutional regimes until a new interpretive equilibrium emerged. The entire legal and political system would undergo a fundamental reorganization.
% FOUNDING_PROBLEM: To prevent legislative overreach and protect fundamental rights by establishing an independent body to ensure fidelity to the constitutional text.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and legal profession attest the problem is live, citing historical and ongoing threats to rights. The legislature, executive, and electorate (via public opinion) attest that while rights protection is vital, the current arrangement has shifted to judicial policy-making, and the founding problem is now a cover for institutional power. Political scientists and legal historians outside the benefiting parties corroborate the shift in function.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the judiciary's power to overturn democratically enacted laws, imposing significant costs on the legislature and executive. Suppression (0.70) is high because challenging judicial supremacy is institutionally difficult and politically costly, with few effective exit options for the other branches. Theater ratio is low (0.10) because the function of judicial review is genuinely performed, though its scope and finality are contested. The increasing extractiveness and suppression over time reflect the historical expansion of judicial power and the hardening of institutional norms around judicial finality.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and legal profession experience this as a necessary coordination mechanism for constitutional stability and rights protection, justifying their interpretive monopoly. The legislature, executive, and electorate experience it as an extractive constraint that limits democratic self-governance and imposes policy costs. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and legal profession are clear beneficiaries, with the judiciary's identity deeply tied to its supreme interpretive role (identity_locked). The legislature, executive, and electorate are payers, bearing the costs of judicial review and having constrained options to challenge it. Coordinate construction advocates are excluded, their arguments not effectively entering the institutional decision-making process.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting rights, ensuring constitutional fidelity) is still live but its status is contested. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring the coordination function of interpretive finality). The increasing extractiveness suggests a drift towards greater rent-seeking within the coordination structure, rather than a pure atrophy of function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_monopoly_necessity,
    'Is a single, final arbiter of constitutional meaning (the judiciary) structurally necessary for constitutional stability, or could distributed interpretive authority (coordinate construction) achieve similar stability with less extraction?',
    'Comparative analysis of constitutional systems with different interpretive models (e.g., parliamentary supremacy, coordinate construction) on metrics of stability, rights protection, and democratic accountability.',
    'If distributed authority proves viable, the ''coordination'' aspect of judicial supremacy would be re-evaluated as a cover for institutional power, increasing its effective extractiveness and pushing classification towards Snare. If not, the coordination function is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_necessity, conceptual, 'Whether interpretive monopoly is a necessary coordination function or an extractive institutional choice.').

omega_variable(
    counter_majoritarian_legitimacy,
    'To what extent does the electorate genuinely consent to counter-majoritarian judicial review, and what are the limits of that consent?',
    'Public opinion surveys on specific judicial decisions, analysis of electoral responses to judicial activism, and studies of constitutional amendment processes as expressions of popular will.',
    'If consent is low or highly conditional, the ''electorate'' seat''s directionality would shift further towards ''full target'' (d=1.0), increasing effective extraction and highlighting the coercive aspect of the constraint. If consent is robust, the extraction from the electorate is partially internalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_majoritarian_legitimacy, empirical, 'The degree of popular legitimacy for judicial review''s counter-majoritarian effects.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine reading of the constitutional text, or an institutional construction that has become entrenched?',
    'Historical and textual analysis of the constitutional founding, combined with a comparative study of how similar texts are interpreted in other jurisdictions. The core question is whether the ''judicial supremacy'' reading is inherent to the text or a later institutional accretion.',
    'If it is primarily an institutional construction, the ''emerges_naturally'' property would be re-evaluated as false, and the constraint''s classification would be more firmly rooted in human choice and power dynamics, rather than textual inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether judicial supremacy is a textual mandate or an institutional interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(cons_tr_t50, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(cons_be_t50, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(cons_su_t50, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional_authority_boundary' kernel. Its high extractiveness and suppression differentiate it from the 'coordinate_construction_reading' (lower extraction, distributed authority) and 'parliamentary_primacy_reading' (legislative supremacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
