% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Tisha B'Av Mourning-Practice Reading (D1/D4)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint story captures the mourning_practice_reading of the
 *   catastrophe_memory_function kernel — the view that Tisha B'Av is
 *   fundamentally and exclusively a ritual of commemorative mourning (D1) and
 *   boundary-maintenance (D4). The reading denies that the ritual transmits
 *   survival-competence (D5) or adaptive institutional transformation. The
 *   constraint operates through halakhic obligation (fasting, liturgy,
 *   behavioral restrictions) that synchronizes collective affect across the
 *   global Jewish community. Beneficiaries are the mourning_community
 *   (identity continuity) and ritual_authorities (legitimacy through
 *   transmission). Obligated_participants bear the embodied costs.
 *   Excluded_outsiders are the structural outside that the boundary requires.
 *   The claimed type is rope (pure coordination), but authored metrics show
 *   non-zero extractiveness and suppression, reflecting the lived experience
 *   of obligation as costly.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.35).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.45).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Tisha B'Av Mourning-Practice Reading (D1/D4)").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, '61d69825-e0fb-4d1c-8e49-67d3ca7de834').
narrative_ontology:cs_kernel_codification('61d69825-e0fb-4d1c-8e49-67d3ca7de834', fixed_text).
narrative_ontology:cs_authority_grounding('61d69825-e0fb-4d1c-8e49-67d3ca7de834', lineage).
narrative_ontology:cs_interpretation_layer_present('61d69825-e0fb-4d1c-8e49-67d3ca7de834').
narrative_ontology:cs_reading_relation('61d69825-e0fb-4d1c-8e49-67d3ca7de834', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('61d69825-e0fb-4d1c-8e49-67d3ca7de834', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('61d69825-e0fb-4d1c-8e49-67d3ca7de834', foundational, ritual_essence_is_mourning_boundary).
narrative_ontology:cs_axiom_status(ritual_essence_is_mourning_boundary, holdable).
narrative_ontology:cs_axiom_grounding('61d69825-e0fb-4d1c-8e49-67d3ca7de834', ritual_essence_is_mourning_boundary, theological).
narrative_ontology:cs_axiom('61d69825-e0fb-4d1c-8e49-67d3ca7de834', secondary, survival_competence_not_ritual_function).
narrative_ontology:cs_axiom_status(survival_competence_not_ritual_function, holdable).
narrative_ontology:cs_axiom_grounding('61d69825-e0fb-4d1c-8e49-67d3ca7de834', survival_competence_not_ritual_function, deontological).
narrative_ontology:cs_reference_frame('61d69825-e0fb-4d1c-8e49-67d3ca7de834', catastrophe_memory_as_sacred_mourning).
narrative_ontology:cs_drift_state('61d69825-e0fb-4d1c-8e49-67d3ca7de834', post_enlightenment_secularization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('61d69825-e0fb-4d1c-8e49-67d3ca7de834', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, mourning_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, ritual_authorities).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, obligated_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, ritual_as_boundary_maintenance).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, collective_memory_as_sacred_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Jewish people as a collective entity that maintains cohesion through shared mourning practice. The ritual provides identity continuity across diaspora and centuries. Participation is experienced as sacred privilege rather than burden; exit would mean severing the identity-constitutive tie to collective memory.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, mourning_community, beneficiary,
    organized, generational, identity_locked, global).

% Rabbinic leadership (poskim, communal rabbis) who define halakhic parameters of observance, authorize liturgical texts, and adjudicate boundary cases. They administer the ritual's transmission but are themselves bound by the tradition they transmit. Authority derives from lineage, not innovation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, ritual_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Individual community members for whom the fast, liturgy, and behavioral restrictions (no leather, no bathing, no Torah study) constitute a measurable cost — lost work, physical discomfort, psychological weight. Within the framework, this cost is framed as holy obligation; from outside, it reads as extraction. Exit requires leaving the community or secularizing, both socially costly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, obligated_participants, payer,
    moderate, biographical, constrained, local).

% Non-Jews, apostates, or those excommunicated (cherem) who are structurally outside the boundary the ritual maintains. The ritual's boundary-function depends on their exclusion; they have no voice in its definition but are defined by it. Their absence is not accidental — it is the condition of the ritual's coherence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, excluded_outsiders, excluded,
    powerless, immediate, trapped, local).

% Scholars of religion, ritual theorists, historians who study Tisha B'Av as a case of collective memory preservation. They analyze the constraint from outside its normative frame, tracing its morphologies across periods. Their seat carries no stake in the ritual's enforcement or benefits.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Jewish collective identity across dispersion and catastrophe by synchronizing communal affect around shared loss — the destruction of the Temples becomes the anchor-point for a peoplehood that survives precisely because it mourns together.
% TRANSFER_FUNCTION: Moves affective labor, time, and bodily discipline from individual participants to the collective memory-bank. The fast, the liturgy (Kinnot, Eicha), and the behavioral restrictions are the currency; the return is continued membership in the peoplehood constituted by the mourning.
% ABSENT_VOICES: The excluded_outsiders — those defined out by the boundary — would object to being the constitutive outside. Also absent: early reformers who argued mourning should yield to historical consciousness (e.g., 19th-century Wissenschaft des Judentums), and contemporary Jews who experience the ritual as alienating rather than constitutive. They are absent because the ritual's authority structure does not admit dissent as legitimate participation.
% DISAPPEARANCE_RATIONALE: If Tisha B'Av vanished overnight, the central synchronizing node of Jewish collective memory would dissolve. No other ritual carries the same catastrophe-anchoring function. The peoplehood would not disappear immediately, but its transgenerational transmission mechanism would suffer catastrophic failure — the 'portable homeland' (Heine) would lose its most portable ritual.
% FOUNDING_PROBLEM: After the Roman destruction of the Second Temple (70 CE), the rabbinic leadership faced existential questions: how to maintain a people without a land, a cult without a Temple, a identity without sovereignty. The founding problem was institutional survival through non-territorial, non-sacrificial means.
% FOUNDING_PROBLEM_CORROBORATION: Traditional sources (Talmud Bavli Taanit, Mishnah Taanit 4:6) attest the rabbinic construction of the day as response to catastrophe. Modern historians (e.g., Yosef Hayim Yerushalmi 'Zakhor', David Roskies 'Against the Apocalypse') corroborate the founding problem as historical fact from outside the benefiting tradition. The contestation: traditional voices say the problem remains live (exile continues); secular/historical voices say the founding problem (statelessness) is resolved by Zionism, making the ritual's original function obsolete.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects the real material and affective costs borne by obligated_participants — the fast, work loss, psychological weight — which are non-trivial even when framed as sacred duty. Suppression (0.45) captures the social enforcement: communal pressure, halakhic mandates, and the identity cost of non-observance. Theater_ratio (0.22) is low because the ritual's core — the fast, the Eichah reading, the Kinnot — is genuinely performed with affective sincerity by millions; the performative layer (public displays of piety, competitive stringency) is present but not dominant. Accessibility_collapse (0.52) is moderate: within the tradition, alternative commemorations are halakhically illegitimate; outside it, secular Holocaust memorial days (Yom HaShoah) and Israeli national memory (Yom HaZikaron) provide parallel but distinct frameworks. Resistance (0.18) is low internally (the obligation is widely accepted) but higher at the margins (secular Jews, reform movements).
 *
 * PERSPECTIVAL GAP:
 *   From the ritual_authorities' seat, the constraint is a Mountain — divine ordinance, immutable, emerging naturally from covenant. From obligated_participants' seat, it is a Tangled Rope — genuine coordination (they value the identity) mixed with extraction (the cost is real and enforced). From historical_observers' seat, it is a Scaffold — a brilliant transitional technology for stateless survival that may have outlived its founding condition. The engine computes these divergences from the structural data; the authored claim (rope) represents the reading's own self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Mourning_community and ritual_authorities sit near the beneficiary end (d ~ 0.15-0.25): they receive identity-continuity and legitimating authority respectively. Obligated_participants sit near the target end (d ~ 0.7-0.8): they bear the embodied costs with constrained exit (leaving community = identity rupture). Excluded_outsiders are structurally outside the directionality calculation — they neither benefit nor pay within the constraint; they are the condition of its boundary. Historical_observers are analytical (d = 0.5 by definition). The identity_locked exit of mourning_community is key: their self-concept is fused with the ritual, making exit structurally near-impossible without self-dissolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stateless survival without Temple) is contested: traditional voices say it persists (exile continues); Zionist/secular voices say it is resolved (sovereignty restored). The ritual persists regardless. If the founding problem is dead but the arrangement persists with active enforcement, mandatrophy is unresolved — the constraint continues to extract coordination-costs for a function it no longer serves. The reading's denial of D5 (survival-competence) makes it vulnerable to this charge: by insisting the ritual is ONLY mourning/boundary, it cannot claim adaptive utility as justification for its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the catastrophe_memory_function kernel genuinely exhausted by the mourning_practice_reading (D1/D4 only), or does the ritual''s historical efficacy depend on unacknowledged survival-competence transmission (D5)?',
    'Comparative analysis of communities that maintain Tisha B''Av observance vs. those that abandoned it: correlate observance intensity with measures of communal survival (demographic continuity, institutional resilience, identity transmission) across generations.',
    'If survival-competence transmission is empirically necessary for the ritual''s persistence, the mourning_practice_reading''s claim to purity (D1/D4 only) is falsified — the constraint would be hybrid_transformation_reading in structural reality, regardless of self-understanding. This would shift claimed_type from rope toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether the ritual''s commemorative function is structurally separable from its survival-competence function.').

omega_variable(
    obligation_as_extraction,
    'Do obligated_participants experience the memorial obligation as sacred coordination (beneficial) or as extractive burden (costly without consent)?',
    'Qualitative and quantitative study of participant phenomenology across observance levels (Haredi, Modern Orthodox, Conservative, secular-but-observant), measuring subjective experience of the fast/liturgy/restrictions as meaningful vs. burdensome, correlated with exit intentions.',
    'If a substantial fraction experience it as extraction, the constraint''s effective extraction for the payer seat is higher than the reading''s self-understanding admits, supporting tangled_rope classification. If near-universally experienced as meaningful coordination, rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(obligation_as_extraction, conceptual, 'Whether the memorial obligation is experienced as gift or tax by those who bear it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 60, 0.31).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 80, 0.33).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 60, 0.43).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 80, 0.44).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__mourning_practice_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the catastrophe_memory_function kernel. The mourning_practice_reading (this story) claims pure D1/D4 function. The survival_competence_reading claims pure D5. The hybrid_transformation_reading claims D1/D4+D5. All three are linked via affects_constraints. The epsilon values differ: this reading authors lower extractiveness (commemorative framing); survival_competence_reading would author higher extractiveness (adaptive burden); hybrid_transformation_reading would author intermediate values with higher theater_ratio (dual-function performance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, organized, 0.2).
constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
