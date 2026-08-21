% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified Sovereignty in Border Control
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'qualified sovereignty' reading of border
 *   control, where states retain authority but must exercise it
 *   proportionately to legitimate state interests and consistently with human
 *   rights obligations. It is a Tangled Rope because it attempts to
 *   coordinate state interests with individual rights, but often results in
 *   asymmetric extraction from vulnerable populations due to the inherent
 *   power imbalance and the active enforcement required to maintain borders.
 *   The constraint creates an adjudication burden on states and places both
 *   excluded migrants and displaced citizens in the victim set.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.65).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.75).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified Sovereignty in Border Control").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, 'b8efd0aa-38c9-4d40-aa63-429a7825381d').
narrative_ontology:cs_kernel_codification('b8efd0aa-38c9-4d40-aa63-429a7825381d', formalized).
narrative_ontology:cs_authority_grounding('b8efd0aa-38c9-4d40-aa63-429a7825381d', lineage).
narrative_ontology:cs_interpretation_layer_present('b8efd0aa-38c9-4d40-aa63-429a7825381d').
narrative_ontology:cs_reading_relation('b8efd0aa-38c9-4d40-aa63-429a7825381d', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('b8efd0aa-38c9-4d40-aa63-429a7825381d', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('b8efd0aa-38c9-4d40-aa63-429a7825381d', foundational, state_sovereignty_is_qualified).
narrative_ontology:cs_axiom_status(state_sovereignty_is_qualified, holdable).
narrative_ontology:cs_axiom_grounding('b8efd0aa-38c9-4d40-aa63-429a7825381d', state_sovereignty_is_qualified, deontological).
narrative_ontology:cs_axiom('b8efd0aa-38c9-4d40-aa63-429a7825381d', foundational, border_control_must_be_proportionate).
narrative_ontology:cs_axiom_status(border_control_must_be_proportionate, holdable).
narrative_ontology:cs_axiom_grounding('b8efd0aa-38c9-4d40-aa63-429a7825381d', border_control_must_be_proportionate, conventional).
narrative_ontology:cs_reference_frame('b8efd0aa-38c9-4d40-aa63-429a7825381d', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('b8efd0aa-38c9-4d40-aa63-429a7825381d', contemporary_migration_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b8efd0aa-38c9-4d40-aa63-429a7825381d', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, states_with_legitimate_interests).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, international_human_rights_regime).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain the authority to control borders for legitimate state interests (e.g., security, public health) but must justify these actions as necessary and proportionate, and consistent with human rights. They bear the burden of adjudication and potential international scrutiny.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, states_with_legitimate_interests, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the recognition and enforcement of human rights obligations within state border policies. It provides the normative framework for evaluating state actions and influences international legal discourse, but lacks direct enforcement power.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, international_human_rights_regime, beneficiary,
    institutional, civilizational, analytical, global).

% Bear the direct costs of exclusion, including loss of opportunity, separation from family, and potential danger. Their movement is restricted, and their claims are subject to state adjudication, often with limited recourse.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, regional).

% May face restrictions on re-entry or movement, even within their own state, if their citizenship status is contested or if border policies are applied broadly. They bear the cost of proving their claims and navigating complex bureaucratic processes.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens, payer,
    moderate, biographical, constrained, national).

% Are particularly vulnerable, often fleeing persecution and seeking protection. Their claims are subject to state discretion and international law, but they frequently face detention, refoulement, and prolonged uncertainty, making exit from their precarious situation extremely difficult.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, asylum_seekers, payer,
    powerless, immediate, identity_locked, regional).

% Monitor state border practices, advocate for the rights of migrants and asylum seekers, and provide assistance. They collect data and challenge state actions, influencing public opinion and international bodies, but do not directly control state policy.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, humanitarian_organizations, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state sovereignty with international human rights norms, providing a framework for states to manage borders while upholding their obligations to individuals, preventing arbitrary or inhumane exclusion.
% TRANSFER_FUNCTION: Transfers the burden of justification and accountability for border control decisions from individuals to states, and potentially transfers resources (e.g., aid, resettlement) to those whose rights are recognized.
% ABSENT_VOICES: Those advocating for open borders or absolute freedom of movement are largely excluded from the state-centric discourse, as are those who prioritize national identity and cultural homogeneity above all else, who would argue for unrestricted state exclusion.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, states would likely revert to more absolute claims of sovereignty, potentially leading to increased arbitrary exclusion and human rights violations at borders. The international human rights regime would lose a key mechanism for accountability, and the situation for migrants and asylum seekers would significantly worsen.
% FOUNDING_PROBLEM: The historical tension between state sovereignty over territory and the emerging recognition of universal human rights, particularly in the context of post-WWII displacement and refugee crises.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and UN bodies consistently attest that this tension remains a live and pressing problem, requiring ongoing legal and political negotiation. State practices and ongoing migration crises corroborate the persistence of this challenge.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant costs borne by individuals whose movement is restricted or denied, even when state actions are deemed 'legitimate.' Suppression (0.75) is high due to the coercive nature of border enforcement and the limited legal recourse for many victims. The theater ratio (0.4) indicates that while states genuinely engage in some legitimate border management, a substantial portion of the justification and enforcement activity serves to maintain control and limit accountability rather than purely uphold human rights. The metrics show a gradual increase in both extractiveness and suppression over time, reflecting the hardening of borders and increasing pressure on migrants.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states, this constraint is a necessary balance, allowing them to protect national interests while adhering to international law. From the perspective of migrants and human rights advocates, it often functions as a mechanism for states to legitimize exclusion, with the 'qualification' of sovereignty being insufficient to prevent significant human rights abuses. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   States are the primary agenda-setters and beneficiaries, as they retain control and define 'legitimate interests.' The international human rights regime benefits from its normative framework being acknowledged, but lacks direct enforcement. Excluded migrants, displaced citizens, and asylum seekers are clear targets, bearing the direct costs of restricted movement and often facing severe consequences. Humanitarian organizations act as observers, advocating for victims but not directly controlling the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (balancing sovereignty with human rights) is still live, but its implementation often drifts towards prioritizing state interests, leading to higher extraction. The classification as Tangled Rope prevents mislabeling it as a pure Rope (which would imply symmetric benefits) or a Snare (which would deny any legitimate coordination function). The ongoing contestation over its application prevents it from becoming a Piton, as there are active parties (humanitarian organizations, international bodies) pushing for its human rights component to be genuinely upheld.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_interest_definition,
    'How are ''legitimate state interests'' defined and by whom? Is the definition sufficiently constrained to prevent arbitrary exclusion?',
    'Analysis of state practice and international jurisprudence: a narrow, consistently applied definition supports the constraint''s coordination function; a broad, self-serving definition indicates a drift towards pure extraction.',
    'If ''legitimate interests'' are too broadly defined, the constraint functions more as a Snare, as states can justify almost any exclusion. If narrowly defined and subject to robust review, it moves closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_interest_definition, conceptual, 'Ambiguity in defining ''legitimate state interests'' in border control.').

omega_variable(
    proportionality_enforcement,
    'To what extent are the principles of necessity and proportionality genuinely enforced and reviewed in state border practices?',
    'Empirical study of judicial review, independent oversight mechanisms, and international monitoring reports on specific border incidents and policies.',
    'Weak enforcement of proportionality means the constraint''s human rights component is largely theatrical, increasing its effective extractiveness and pushing it towards a Snare. Robust enforcement would reduce extraction and strengthen its Rope-like qualities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_enforcement, empirical, 'Effectiveness of proportionality and necessity enforcement in border control.').

omega_variable(
    sovereignty_vs_rights_priority,
    'Is the underlying normative commitment of this reading truly a balance between sovereignty and human rights, or does it implicitly prioritize one over the other?',
    'Analysis of legal and political discourse, and the outcomes of hard cases: if one consistently overrides the other, the claimed balance is a cover for a de facto prioritization.',
    'If sovereignty is implicitly prioritized, the constraint''s coordination function is weaker, and its extractive nature is more pronounced. If human rights are genuinely given equal weight, it functions closer to its claimed type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_rights_priority, conceptual, 'Implicit prioritization of sovereignty or human rights within the ''qualified sovereignty'' framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_normative_status__qualified_sovereignty, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(bord_tr_t1970, border_normative_status__qualified_sovereignty, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__qualified_sovereignty, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(bord_tr_t2010, border_normative_status__qualified_sovereignty, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__qualified_sovereignty, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_normative_status__qualified_sovereignty, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(bord_be_t1970, border_normative_status__qualified_sovereignty, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(bord_be_t1990, border_normative_status__qualified_sovereignty, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(bord_be_t2010, border_normative_status__qualified_sovereignty, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__qualified_sovereignty, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_normative_status__qualified_sovereignty, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(bord_su_t1970, border_normative_status__qualified_sovereignty, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(bord_su_t1990, border_normative_status__qualified_sovereignty, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(bord_su_t2010, border_normative_status__qualified_sovereignty, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__qualified_sovereignty, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
