% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Legitimacy (Inherited Right Reading)
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint describes the monarchical reading of sovereign
 *   legitimacy, where authority is inherited and divinely sanctioned. It is
 *   one reading of the 'sovereign_legitimacy' kernel, distinct from
 *   republican and constitutional-hybrid readings. The system is highly
 *   extractive, transferring power and resources to a hereditary elite, and
 *   relies on significant suppression of alternative legitimacy claims. Its
 *   persistence is maintained through tradition, ritual, and active
 *   enforcement against dissent.
 *
 * KEY AGENTS:
 *   - hereditary_ruling_class: Primary beneficiary and agenda-setter (institutional/identity_locked)
 *   - aristocratic_hierarchy: Secondary beneficiary (powerful/constrained)
 *   - subjects_excluded_from_authority: Primary victim (powerless/trapped)
 *   - proponents_of_popular_sovereignty: Excluded voice (moderate/constrained)
 *   - traditional_institutions: Non-agent beneficiary (institutional/identity_locked)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.85).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.9).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, snare).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Legitimacy (Inherited Right Reading)").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, 'bda24751-c2fa-4963-a1dd-aed56ee6a684').
narrative_ontology:cs_kernel_codification('bda24751-c2fa-4963-a1dd-aed56ee6a684', formalized).
narrative_ontology:cs_authority_grounding('bda24751-c2fa-4963-a1dd-aed56ee6a684', lineage).
narrative_ontology:cs_interpretation_layer_present('bda24751-c2fa-4963-a1dd-aed56ee6a684').
narrative_ontology:cs_reading_relation('bda24751-c2fa-4963-a1dd-aed56ee6a684', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('bda24751-c2fa-4963-a1dd-aed56ee6a684', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('bda24751-c2fa-4963-a1dd-aed56ee6a684', foundational, divine_right_of_kings).
narrative_ontology:cs_axiom_status(divine_right_of_kings, holdable).
narrative_ontology:cs_axiom_grounding('bda24751-c2fa-4963-a1dd-aed56ee6a684', divine_right_of_kings, theological).
narrative_ontology:cs_axiom('bda24751-c2fa-4963-a1dd-aed56ee6a684', foundational, inherited_sovereignty_is_natural_order).
narrative_ontology:cs_axiom_status(inherited_sovereignty_is_natural_order, holdable).
narrative_ontology:cs_axiom_grounding('bda24751-c2fa-4963-a1dd-aed56ee6a684', inherited_sovereignty_is_natural_order, conventional).
narrative_ontology:cs_reference_frame('bda24751-c2fa-4963-a1dd-aed56ee6a684', absolute_monarchical_tradition).
narrative_ontology:cs_drift_state('bda24751-c2fa-4963-a1dd-aed56ee6a684', contemporary_global_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bda24751-c2fa-4963-a1dd-aed56ee6a684', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, subjects_excluded_from_authority).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, proponents_of_popular_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate authority by birthright, enforces succession laws, and benefits directly from the system's stability and the extraction of resources from subjects. Their identity is fused with the system's perpetuation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class, agenda_setter,
    institutional, generational, identity_locked, national).

% Derives power, status, and wealth from their proximity to the sovereign and their role in administering the realm. They are beneficiaries of the system's structure and have a strong interest in its continuity.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, constrained, national).

% Bear the costs of governance without participation in decision-making. Their consent is assumed or compelled, and their options for changing the system are severely limited, often by force or social conditioning.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, subjects_excluded_from_authority, payer,
    powerless, biographical, trapped, national).

% Advocate for alternative legitimacy models where authority derives from the people. They are actively suppressed or marginalized within the monarchical system, facing significant risks for dissent.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, proponents_of_popular_sovereignty, excluded,
    moderate, generational, constrained, national).

% Religious bodies, noble houses, and other long-standing institutions whose power and legitimacy are intertwined with the monarchical system. They are not agents in the same sense as individuals but are structural beneficiaries.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, traditional_institutions, beneficiary,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(sovereign_legitimacy__monarchical_reading, traditional_institutions).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, stable line of succession and a single, unambiguous source of ultimate authority, preventing internal power struggles over leadership and ensuring continuity of governance.
% TRANSFER_FUNCTION: Transfers ultimate decision-making power, wealth, and status from the general populace to a hereditary ruling class and its associated aristocracy, in exchange for perceived stability and order.
% ABSENT_VOICES: Any group advocating for popular sovereignty, democratic participation, or alternative forms of governance are systematically excluded from the discourse, often through censorship, legal prohibitions, or social ostracization. Their arguments for consent-based legitimacy are not heard within the system's official channels.
% DISAPPEARANCE_RATIONALE: If the principle of monarchical legitimacy vanished overnight, the entire political and social order would collapse. Succession would be contested, the basis of law would be questioned, and a power vacuum would emerge, leading to widespread instability and a complete reorganization of governance.
% FOUNDING_PROBLEM: To establish a stable and unquestionable source of authority in a pre-modern state, preventing civil war over succession and providing a divinely sanctioned basis for social order.
% FOUNDING_PROBLEM_CORROBORATION: The hereditary ruling class and traditional institutions assert the problem of order and succession remains live. Proponents of popular sovereignty and historical analysis from outside the benefiting parties argue the problem is largely 'dead' in modern contexts, and the system persists as a mechanism for elite power retention, not genuine problem-solving.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the system concentrates power and wealth in a small, non-accountable elite. Suppression is very high (0.90) as the system actively represses any challenge to its foundational claims, often through force, propaganda, and control of education. Theater ratio is moderate (0.40), reflecting the significant role of ritual, ceremony, and symbolic displays of power in maintaining legitimacy, even as the actual coordination function may be less efficient than alternatives. The slight dip in extractiveness and suppression at t=100 reflects potential external pressures or internal reforms over a long historical period, but the core structure remains.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the hereditary ruling class, this system is a legitimate and necessary framework for order and stability. From the perspective of subjects or proponents of popular sovereignty, it is an oppressive and extractive mechanism. The engine's classification will highlight this divergence by computing a Snare from the victim's seat, while the agenda-setter's seat might compute a different type if their benefits are framed as coordination costs.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary ruling class and aristocratic hierarchy are clear beneficiaries, with their power and identity deeply intertwined with the constraint (low d). Subjects are direct targets, bearing the costs without benefit (high d). Proponents of popular sovereignty are also targets, as their very existence challenges the constraint, leading to high d and active suppression. Traditional institutions, while not agents, are structurally subsidized by the system's perpetuation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (stable succession, divine order) is contested. While it historically solved a problem of succession, modern critiques argue its primary function has shifted to rent extraction and power retention. The high extractiveness and suppression, coupled with the 'contested' status of the founding problem, suggest a Snare-like operation, where the original coordination story serves as cover for ongoing extraction. The system avoids being a Piton because the beneficiaries actively maintain and profit from it, rather than it persisting purely by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_empirical_status,
    'Is the divine sanction grounding this authority empirically verifiable or a matter of faith/tradition?',
    'Theological or philosophical inquiry into the nature of divine authority and its manifestation in political systems. No empirical resolution is possible.',
    'If empirically verifiable (unlikely), it would strengthen the Mountain-like aspects of the constraint. If purely faith-based, it highlights the constructed nature of the legitimacy claim and its reliance on belief systems for persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_sanction_empirical_status, conceptual, 'The epistemic status of divine sanction as a grounding for authority.').

omega_variable(
    succession_stability_vs_extraction,
    'To what extent does the inherited right system genuinely provide unique stability, versus merely entrenching an extractive elite?',
    'Comparative historical analysis of states with different legitimacy models (monarchical vs. republican) regarding internal conflict, economic development, and social equity over long periods.',
    'If unique stability is demonstrated, it would slightly reduce the perceived extractiveness by validating a coordination function. If not, it reinforces the Snare classification by exposing the coordination claim as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_stability_vs_extraction, empirical, 'The actual contribution of inherited right to political stability versus its role in extraction.').

omega_variable(
    identity_lock_internalization,
    'For the hereditary ruling class, is their ''identity_locked'' exit option a genuine internal commitment or a rationalization of structural privilege?',
    'Psychological and sociological studies of elite identity formation, combined with analysis of behavior when faced with genuine threats to the system (e.g., abdication crises, revolutions).',
    'If purely rationalization, their directionality would shift slightly towards ''mobile'' (as they could theoretically abandon the role if costs outweighed benefits), increasing the perceived extractiveness. If genuine, it reinforces their deep structural entanglement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'The nature of identity lock for the ruling elite.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sove_tr_t25, sovereign_legitimacy__monarchical_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(sove_tr_t50, sovereign_legitimacy__monarchical_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(sove_tr_t75, sovereign_legitimacy__monarchical_reading, theater_ratio, 75, 0.45).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__monarchical_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(sove_be_t25, sovereign_legitimacy__monarchical_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement(sove_be_t50, sovereign_legitimacy__monarchical_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement(sove_be_t75, sovereign_legitimacy__monarchical_reading, base_extractiveness, 75, 0.87).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__monarchical_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(sove_su_t25, sovereign_legitimacy__monarchical_reading, suppression_requirement, 25, 0.85).
narrative_ontology:measurement(sove_su_t50, sovereign_legitimacy__monarchical_reading, suppression_requirement, 50, 0.9).
narrative_ontology:measurement(sove_su_t75, sovereign_legitimacy__monarchical_reading, suppression_requirement, 75, 0.92).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__monarchical_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, identity_coordination).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, constitutional_hybrid_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'sovereign_legitimacy' kernel. This monarchical reading emphasizes inherited right and divine sanction, contrasting with republican (popular consent) and constitutional-hybrid (dual-sourced) readings. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
