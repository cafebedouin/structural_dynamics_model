% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority (Traditionalist Rupture Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the traditionalist reading of Vatican II,
 *   where the Council's documents and subsequent implementation are seen as a
 *   rupture with Catholic tradition, leading to doctrinal confusion and
 *   liturgical decline. The ambiguities within the conciliar texts are
 *   interpreted as deliberate compromises that enabled heterodox
 *   interpretations and practices, effectively extracting traditional forms
 *   and clarity from the Church. This reading views the post-conciliar period
 *   as a snare, trapping traditional Catholics within a system that
 *   undermines their faith.
 *
 * KEY AGENTS:
 *   - traditional_catholics: Primary target (powerless/identity_locked) — bear extraction of tradition
 *   - modernist_theologians: Primary beneficiary (powerful/mobile) — benefit from doctrinal flexibility
 *   - progressive_clergy: Secondary beneficiary (organized/constrained) — implement changes, gain institutional power
 *   - roman_curia: Agenda setter (institutional/constrained) — administers the post-conciliar Church, often enforcing rupture-aligned policies
 *   - traditional_liturgy: Victim (powerless/trapped) — suppressed and marginalized
 *   - doctrinal_clarity: Victim (powerless/trapped) — eroded by ambiguities and pluralism
 *   - continuity_theologians: Excluded (powerful/constrained) — marginalized in interpretive discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.85).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, snare).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority (Traditionalist Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '47059b11-c440-4d5e-a3cf-179252a45925').
narrative_ontology:cs_kernel_codification('47059b11-c440-4d5e-a3cf-179252a45925', fixed_text).
narrative_ontology:cs_authority_grounding('47059b11-c440-4d5e-a3cf-179252a45925', lineage).
narrative_ontology:cs_interpretation_layer_present('47059b11-c440-4d5e-a3cf-179252a45925').
narrative_ontology:cs_reading_relation('47059b11-c440-4d5e-a3cf-179252a45925', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('47059b11-c440-4d5e-a3cf-179252a45925', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('47059b11-c440-4d5e-a3cf-179252a45925', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('47059b11-c440-4d5e-a3cf-179252a45925', foundational, vatican_ii_contains_doctrinal_errors).
narrative_ontology:cs_axiom_status(vatican_ii_contains_doctrinal_errors, holdable).
narrative_ontology:cs_axiom_grounding('47059b11-c440-4d5e-a3cf-179252a45925', vatican_ii_contains_doctrinal_errors, empirically_contingent).
narrative_ontology:cs_axiom('47059b11-c440-4d5e-a3cf-179252a45925', foundational, traditional_liturgy_is_sacred_and_irreplaceable).
narrative_ontology:cs_axiom_status(traditional_liturgy_is_sacred_and_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('47059b11-c440-4d5e-a3cf-179252a45925', traditional_liturgy_is_sacred_and_irreplaceable, deontological).
narrative_ontology:cs_reference_frame('47059b11-c440-4d5e-a3cf-179252a45925', pre_vatican_ii_magisterial_tradition).
narrative_ontology:cs_drift_state('47059b11-c440-4d5e-a3cf-179252a45925', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('47059b11-c440-4d5e-a3cf-179252a45925', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, modernist_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_catholics).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the perceived loss of traditional doctrine and liturgy is profound and widespread, impacting the core identity of traditional Catholics. Suppression (0.7) is significant, as traditional practices and theological expressions are actively marginalized or forbidden by institutional authority. Theater ratio (0.4) reflects the official narrative of 'continuity' which, from this reading, is seen as a performance to mask the actual rupture and extraction. Resistance is high (0.8) due to ongoing efforts by traditionalists to preserve and restore what they see as authentic Catholic tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of traditional Catholics, the constraint is a snare, actively extracting their spiritual heritage. From the perspective of modernist theologians and progressive clergy, the same constraint is a rope or even a mountain, representing necessary adaptation and organic development. The Roman Curia, while officially promoting continuity, often acts as an agenda-setter for policies that align with the rupture interpretation, creating a complex, internally conflicted position.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernist theologians and progressive clergy are beneficiaries (low d) as the Council's ambiguities enable their theological and pastoral agendas. Traditional Catholics, traditional liturgy, and doctrinal clarity are victims (high d) as they experience the direct loss and suppression. The Roman Curia, while officially neutral, often enforces policies that benefit the progressive interpretation, making it an agenda-setter with a directionality that leans towards enabling the beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the original mandate of Vatican II (renewal) has been subverted, and the constraint now functions as a snare, extracting tradition under the guise of reform. The 'mandatrophy' is not a simple decay but an active perversion of the original intent, where the ambiguity of the texts allows for a continuous reinterpretation that benefits certain factions while harming others. The classification as a snare prevents mislabeling this as a mere 'rope' of coordination or a 'mountain' of inevitable development, highlighting the active extraction and suppression involved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_ambiguity_intent,
    'Were the ambiguities in the Vatican II documents intentional compromises to achieve consensus, or were they genuine attempts at nuanced theological expression?',
    'Historical analysis of conciliar debates, private correspondence of periti (experts), and papal interventions during and immediately after the Council.',
    'If intentional compromises, it strengthens the ''snare'' classification by highlighting a deliberate structural flaw enabling extraction. If genuine nuance, it might soften the ''extractiveness'' slightly, suggesting unintended consequences rather than deliberate design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_ambiguity_intent, empirical, 'The nature of ambiguities in Vatican II documents.').

omega_variable(
    post_conciliar_implementation_fidelity,
    'To what extent does the post-conciliar implementation (liturgical changes, theological trends) accurately reflect the actual texts of Vatican II, versus a ''spirit of the Council'' interpretation?',
    'Comparative textual analysis of conciliar documents against post-conciliar magisterial documents, liturgical reforms, and theological publications.',
    'If implementation largely deviates from the texts, it reinforces the ''rupture'' aspect and the snare classification. If it largely aligns, it would challenge the traditionalist reading''s premise of heterodox implementation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_conciliar_implementation_fidelity, empirical, 'Fidelity of post-conciliar implementation to Vatican II texts.').

omega_variable(
    identity_lock_strength,
    'How strong is the ''identity_locked'' exit option for traditional Catholics? Is it truly impossible for them to leave the Catholic Church, or is it a high-cost but viable option?',
    'Sociological studies of ex-Catholics from traditionalist backgrounds, analysis of schismatic movements, and psychological profiling of identity formation within religious groups.',
    'If identity-lock is weaker, their effective extraction (χ) would be lower, as exit is more viable. If stronger, it reinforces the snare classification by showing deeper entrapment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Strength of identity-lock for traditional Catholics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1962, 0.5).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1975, 0.7).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1962, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'vatican_ii_doctrinal_authority' kernel. Its ε value differs significantly from the 'continuity_reading' (low ε) and 'composite_overdetermination_reading' (variable ε), but shares a high ε with the 'rupture_progressive_reading' (though with different beneficiaries/victims).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
