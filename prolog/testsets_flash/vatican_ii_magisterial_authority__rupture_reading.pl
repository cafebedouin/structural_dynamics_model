% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority (Rupture Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint models the 'rupture reading' of Vatican II, which asserts
 *   a fundamental break with pre-conciliar Catholic teaching and practice.
 *   The conciliar texts are interpreted as authorizing radical
 *   implementation, superseding prior positions (e.g., 'error has no
 *   rights'), legitimizing liturgical experimentation, and acknowledging
 *   religious freedom (Dignitatis Humanae) as a doctrinal progress that
 *   contradicts prior teaching. This reading is actively enforced by
 *   institutional actors, leading to significant extraction from those who
 *   adhere to a continuity perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.65).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.4).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '1385cc05-b7d9-4e3c-9848-2f7c31e464ba').
narrative_ontology:cs_kernel_codification('1385cc05-b7d9-4e3c-9848-2f7c31e464ba', fixed_text).
narrative_ontology:cs_authority_grounding('1385cc05-b7d9-4e3c-9848-2f7c31e464ba', lineage).
narrative_ontology:cs_interpretation_layer_present('1385cc05-b7d9-4e3c-9848-2f7c31e464ba').
narrative_ontology:cs_reading_relation('1385cc05-b7d9-4e3c-9848-2f7c31e464ba', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('1385cc05-b7d9-4e3c-9848-2f7c31e464ba', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('1385cc05-b7d9-4e3c-9848-2f7c31e464ba', foundational, conciliar_texts_supersede_prior_magisterium).
narrative_ontology:cs_axiom_status(conciliar_texts_supersede_prior_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('1385cc05-b7d9-4e3c-9848-2f7c31e464ba', conciliar_texts_supersede_prior_magisterium, conventional).
narrative_ontology:cs_axiom('1385cc05-b7d9-4e3c-9848-2f7c31e464ba', foundational, religious_freedom_contradicts_prior_teaching_as_progress).
narrative_ontology:cs_axiom_status(religious_freedom_contradicts_prior_teaching_as_progress, holdable).
narrative_ontology:cs_axiom_grounding('1385cc05-b7d9-4e3c-9848-2f7c31e464ba', religious_freedom_contradicts_prior_teaching_as_progress, deontological).
narrative_ontology:cs_reference_frame('1385cc05-b7d9-4e3c-9848-2f7c31e464ba', post_conciliar_aggiornamento).
narrative_ontology:cs_drift_state('1385cc05-b7d9-4e3c-9848-2f7c31e464ba', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1385cc05-b7d9-4e3c-9848-2f7c31e464ba', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, liturgical_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, ecumenical_dialogue_advocates).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, conservative_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the rupture reading as it legitimizes their theological innovations and provides a framework for radical reinterpretation of Catholic doctrine and practice. They actively promote this reading through academic work and pastoral initiatives.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians, beneficiary,
    institutional, generational, mobile, global).

% Find justification for extensive liturgical changes and experimentation in the rupture reading, viewing pre-conciliar forms as superseded. They implement and defend these changes in parishes and dioceses.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, liturgical_reformers, beneficiary,
    organized, biographical, mobile, global).

% Utilize the rupture reading to emphasize common ground with other Christian denominations and world religions, often downplaying or reinterpreting prior exclusive claims of the Catholic Church. This reading facilitates their work.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, ecumenical_dialogue_advocates, beneficiary,
    institutional, generational, mobile, global).

% Experience the rupture reading as a direct challenge to their understanding of Catholic identity and mission. They are often marginalized, disciplined, or excluded for adhering to pre-conciliar practices and doctrines, facing career and social costs.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy, payer,
    moderate, biographical, identity_locked, local).

% Feel alienated by changes in liturgy and doctrine justified by the rupture reading. Their options are limited to seeking out traditionalist communities, leaving the Church, or passively accepting changes they disagree with, often at significant personal cost to their faith life.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, conservative_laity, payer,
    powerless, biographical, constrained, local).

% See their academic work and theological frameworks rendered obsolete or even condemned by the rupture reading. Their careers and intellectual contributions are devalued, and they face pressure to conform to the new interpretive paradigm.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_scholars, payer,
    moderate, generational, constrained, global).

% Administers the implementation of Vatican II, often navigating between different interpretive camps. While officially promoting a 'hermeneutic of reform in continuity,' elements within the Curia may de facto enforce the rupture reading through appointments, disciplinary actions, and official pronouncements, particularly regarding liturgical and ecumenical matters.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, roman_curia, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for the Catholic Church to engage with modernity, fostering ecumenical dialogue and adapting its pastoral approach to contemporary society, thereby coordinating a new relationship between the Church and the world.
% TRANSFER_FUNCTION: Transfers theological authority and interpretive legitimacy from pre-conciliar traditions and scholars to post-conciliar interpretations and progressive theologians, along with associated institutional resources and influence.
% ABSENT_VOICES: The voices of those who believe the rupture reading fundamentally undermines the divine constitution of the Church and its unchanging truths are often marginalized or dismissed as disloyal or schismatic. They are excluded from mainstream theological discourse and institutional power structures.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished, the theological and pastoral landscape of the Catholic Church would fundamentally shift. Many contemporary practices and doctrines would lose their primary justification, leading to a re-evaluation of liturgical forms, ecumenical efforts, and the Church's relationship with secular society. Traditionalist positions would gain significant ground, and the Church's internal dynamics would be profoundly reordered.
% FOUNDING_PROBLEM: The Catholic Church faced a crisis of relevance and engagement with the modern world, perceived as isolated and resistant to necessary reforms, leading to a desire for aggiornamento (updating) and ressourcement (return to sources).
% FOUNDING_PROBLEM_CORROBORATION: The problem of the Church's engagement with modernity is still live, attested by ongoing debates within and outside the Church. However, the specific 'rupture' solution is contested, with traditionalists arguing it created new problems, while progressives maintain it was a necessary and successful adaptation. Historians and sociologists of religion, outside the immediate beneficiaries, corroborate the initial problem of relevance but offer diverse analyses of the rupture reading's efficacy and consequences.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because adherence to the rupture reading often requires individuals and groups to abandon or reinterpret deeply held prior beliefs and practices, incurring significant identity and social costs. Suppression (0.40) is moderate, as traditionalist views are often marginalized or disciplined, but not entirely eliminated. The theater ratio (0.20) is relatively low, as the implementation of the rupture reading is genuinely transformative, not merely performative. Resistance (0.70) is high, reflecting ongoing and vocal opposition from traditionalist factions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this reading is a necessary and liberating adaptation, a 'rope' guiding the Church into modernity. From the perspective of victims, it is a 'snare' that undermines the Church's identity and extracts conformity to novel doctrines. The engine's classification as 'tangled_rope' reflects this hybrid nature: a genuine coordination function (adapting to modernity) coupled with asymmetric extraction from those who resist the rupture.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive theologians, liturgical reformers, and ecumenical advocates are beneficiaries (d near 0.0) as this reading legitimizes their work and provides institutional support. Traditionalist clergy, conservative laity, and pre-conciliar scholars are victims (d near 1.0) as they bear the costs of marginalization, reinterpretation, and loss of influence. The Roman Curia acts as an agenda-setter, often enforcing this reading through institutional mechanisms, even while officially promoting a 'hermeneutic of reform in continuity.'
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_contradiction_status,
    'Is the contradiction between Dignitatis Humanae (religious freedom) and prior teaching (e.g., Syllabus of Errors) a genuine doctrinal rupture or a development of doctrine that can be reconciled?',
    'Further magisterial clarification or a widely accepted theological synthesis that convincingly demonstrates either rupture or continuity without equivocation.',
    'If a genuine rupture, it strengthens the rupture reading''s claim of fundamental discontinuity. If reconcilable development, it weakens the rupture reading and lends support to the continuity reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_contradiction_status, conceptual, 'Ambiguity regarding doctrinal contradiction vs. development.').

omega_variable(
    liturgical_experimentation_legitimacy,
    'To what extent does the rupture reading genuinely authorize radical liturgical experimentation, or is such experimentation an abuse of the conciliar texts?',
    'Official Church pronouncements clarifying the limits of liturgical reform and the proper interpretation of Sacrosanctum Concilium, coupled with empirical observation of adherence to these limits.',
    'If radical experimentation is genuinely authorized, it reinforces the rupture reading''s transformative scope. If it is an abuse, it suggests the rupture reading oversteps the actual conciliar mandate, potentially shifting the constraint towards a ''piton'' of performative change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_experimentation_legitimacy, empirical, 'Scope of liturgical authorization under the rupture reading.').

omega_variable(
    institutional_enforcement_sincerity,
    'Is the Roman Curia''s official ''hermeneutic of reform in continuity'' a sincere attempt at reconciliation, or a rhetorical cover for de facto enforcement of the rupture reading?',
    'Analysis of curial appointments, disciplinary actions, and official documents over time, specifically examining whether actions align with stated continuity or with the practical implications of rupture.',
    'If a sincere attempt, the constraint''s suppression and extractiveness might be lower than perceived by victims, as there is an institutional effort to mitigate rupture. If a cover, the effective suppression and extractiveness are higher, as the official narrative masks coercive implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_sincerity, empirical, 'Sincerity of official hermeneutic vs. de facto enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1965, 0.2).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__rupture_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Vatican II magisterial authority' kernel. It asserts a fundamental rupture with prior teaching, directly influencing and being influenced by the 'continuity reading' and the 'composite overdetermination reading' within the same interpretive domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
