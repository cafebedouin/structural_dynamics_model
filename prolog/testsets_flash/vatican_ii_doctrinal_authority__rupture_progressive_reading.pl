% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Doctrinal Authority: Rupture-Progressive Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint models the 'rupture-progressive' reading of Vatican II,
 *   which asserts the Council initiated a necessary break from pre-conciliar
 *   rigidity and that the 'spirit of the Council' authorizes ongoing reform
 *   beyond the strict textual limits of its documents. This reading
 *   emphasizes doctrinal development, particularly on issues like religious
 *   freedom (seen as a reversal of earlier condemnations), and views
 *   post-conciliar implementation as the authentic realization of conciliar
 *   intent. It is a contested interpretation within the Catholic Church,
 *   leading to significant internal conflict.
 *
 * KEY AGENTS:
 *   - progressive_theologians: Primary beneficiary (institutional/arbitrage) — gain authority for reform agendas.
 *   - reform_minded_clergy: Primary beneficiary (organized/mobile) — implement changes and gain legitimacy.
 *   - laity_seeking_greater_autonomy: Secondary beneficiary (moderate/constrained) — experience greater participation and agency.
 *   - traditionalist_clergy: Primary victim (organized/identity_locked) — see their understanding of tradition undermined.
 *   - conservative_laity: Primary victim (moderate/identity_locked) — feel alienated by changes and loss of familiar practices.
 *   - pre_conciliar_institutions: Victim (institutional/trapped) — their structures and doctrines are challenged or dismantled.
 *   - magisterium: Agenda setter (institutional/constrained) — officially interprets and enforces the Council, but is itself divided by these readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.65).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.45).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Doctrinal Authority: Rupture-Progressive Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, '161a9953-3aca-44b2-9a47-ea7a1a8cf35c').
narrative_ontology:cs_kernel_codification('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', fixed_text).
narrative_ontology:cs_authority_grounding('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', lineage).
narrative_ontology:cs_interpretation_layer_present('161a9953-3aca-44b2-9a47-ea7a1a8cf35c').
narrative_ontology:cs_reading_relation('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', foundational, doctrinal_development_is_dynamic).
narrative_ontology:cs_axiom_status(doctrinal_development_is_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', doctrinal_development_is_dynamic, deontological).
narrative_ontology:cs_axiom('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', foundational, spirit_of_council_guides_interpretation).
narrative_ontology:cs_axiom_status(spirit_of_council_guides_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', spirit_of_council_guides_interpretation, conventional).
narrative_ontology:cs_reference_frame('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', post_conciliar_renewal).
narrative_ontology:cs_drift_state('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', contemporary_hermeneutical_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('161a9953-3aca-44b2-9a47-ea7a1a8cf35c', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_minded_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, laity_seeking_greater_autonomy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, conservative_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_institutions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a vision for ongoing reform and adaptation within the Church (beneficiaries), but this coordination comes with significant extraction from those committed to pre-conciliar rigidity (victims). The 'spirit of the Council' provides a coordination function by offering a framework for development, but its ambiguity also enables extraction by allowing interpretations that marginalize traditionalist views. Active enforcement is required to implement reforms and suppress traditionalist dissent. Extractiveness is high (0.65) due to the perceived reversal of doctrine and the marginalization of traditional practices. Suppression (0.45) is moderate, as traditionalist resistance is significant but often met with institutional pressure. Theater ratio (0.20) is low, as the reforms are genuinely implemented, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   Progressive theologians and reform-minded clergy experience this reading as a liberating Rope, enabling necessary adaptation and growth. Traditionalist clergy and conservative laity experience it as a Snare, undermining their faith and practices. The Magisterium, as the agenda-setter, attempts to navigate these tensions, often appearing as a Tangled Rope from its own seat, trying to coordinate while managing internal dissent.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive theologians and reform-minded clergy are beneficiaries (d near 0.0) as this reading legitimizes their theological and pastoral agendas. Traditionalist clergy and conservative laity are victims (d near 1.0) as their positions are actively challenged and marginalized. The Magisterium, while the ultimate authority, is internally divided and must actively enforce this reading against significant internal resistance, making its directionality more symmetric (d near 0.5) as it bears the costs of internal conflict while also benefiting from a perceived renewal.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging both the coordination function (providing a framework for adaptation) and the asymmetric extraction (from traditionalists). If it were purely a Rope, it would ignore the victims; if purely a Snare, it would ignore the genuine desire for reform and adaptation. The 'spirit of the Council' is a key mechanism for this dual function, allowing for flexibility but also for interpretations that marginalize dissenting views. The ongoing contestation over its meaning prevents it from becoming a Piton, as its function is still very much live and actively debated/enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint an accurate representation of the ''rupture-progressive'' reading of Vatican II, or does it conflate with other interpretations?',
    'Comparative analysis with primary texts from proponents of this reading (e.g., writings of progressive theologians, official statements from reform-oriented episcopal conferences) to ensure fidelity to their stated positions.',
    'If conflated, the classification may misrepresent the true structural dynamics of this specific reading, potentially overstating or understating its extractiveness or coordination function. A more precise definition would refine the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensures the constraint accurately reflects the rupture-progressive reading of Vatican II.').

omega_variable(
    spirit_of_council_ambiguity,
    'To what extent does the ''spirit of the Council'' genuinely authorize ongoing reform beyond textual limits, versus serving as a rhetorical cover for desired changes?',
    'Historical analysis of post-conciliar reforms: track which reforms explicitly cite textual ambiguities versus those that invoke the ''spirit'' to justify novel interpretations. Examine resistance to such reforms and the arguments used by both sides.',
    'If primarily rhetorical, the constraint''s extractiveness from traditional structures is higher and its coordination function (as a legitimate basis for reform) is lower, pushing it closer to a Snare. If genuinely authorizing, it functions more as a Scaffold for ongoing development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_of_council_ambiguity, empirical, 'Assesses the legitimacy and function of the ''spirit of the Council'' as a basis for reform.').

omega_variable(
    doctrinal_change_naturalness,
    'Is the shift in doctrinal understanding (e.g., religious freedom) a natural evolution of Catholic teaching, or a fundamental reversal of prior, irreformable doctrine?',
    'Theological and historical scholarship examining the continuity of doctrine, specifically focusing on the arguments for and against the possibility of reversal on previously defined teachings. This is a long-standing internal debate within Catholic theology.',
    'If a natural evolution, the constraint''s ''rupture'' aspect is diminished, reducing its perceived extractiveness from traditionalists. If a fundamental reversal, the extractiveness is higher, and the constraint operates more as a Snare for those committed to pre-conciliar teaching.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_change_naturalness, conceptual, 'Examines the theological nature of doctrinal changes introduced by Vatican II.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(vati_be_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(vati_su_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'Vatican II Doctrinal Authority' kernel. Each reading represents a different structural constraint with its own epsilon, beneficiaries, and victims, reflecting the internal hermeneutical conflict within the Catholic Church. This 'rupture-progressive' reading directly influences and is influenced by the 'continuity' and 'rupture-traditionalist' readings, as they are in active theological and institutional contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
