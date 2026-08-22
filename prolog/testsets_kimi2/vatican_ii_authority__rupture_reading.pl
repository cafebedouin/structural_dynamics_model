% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II Authority â Rupture Reading
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   From the rupture reading, the Second Vatican Council (1962â1965)
 *   introduced doctrinal propositions and liturgical norms that cannot be
 *   reconciled with the pre-conciliar magisterium. The post-conciliar
 *   authority structure that enforces these documents as legitimate Catholic
 *   teaching operates as a constraint on traditional Catholic identity: it
 *   suppresses the traditional Latin Mass, marginalizes pre-conciliar
 *   theological categories, and imposes what the reading regards as error.
 *   The modernist faction benefits from the doctrinal flexibility and
 *   expanded interpretive space. The SSPX instantiates this reading in
 *   ecclesial practice, occupying an irregular canonical status precisely
 *   because it rejects the constraint's legitimacy. This is one reading of
 *   the vatican_ii_authority kernel; continuity and composite readings
 *   instantiate different constraints.
 *
 * KEY AGENTS:
 *   - post_conciliar_hierarchy: agenda_setter (institutional/constrained) â enforces conciliar authority
 *   - modernist_faction: beneficiary (organized/constrained) â captures doctrinal flexibility
 *   - traditional_catholics: payer (moderate/identity_locked) â bear liturgical and doctrinal costs
 *   - sspx: excluded (organized/trapped) â holds the rupture reading, excluded from canonical regularity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.78).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.75).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II Authority â Rupture Reading").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '5cf57a88-b495-465a-b643-395cf9dad57e').
narrative_ontology:cs_kernel_codification('5cf57a88-b495-465a-b643-395cf9dad57e', formalized).
narrative_ontology:cs_authority_grounding('5cf57a88-b495-465a-b643-395cf9dad57e', lineage).
narrative_ontology:cs_interpretation_layer_present('5cf57a88-b495-465a-b643-395cf9dad57e').
narrative_ontology:cs_reading_relation('5cf57a88-b495-465a-b643-395cf9dad57e', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('5cf57a88-b495-465a-b643-395cf9dad57e', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('5cf57a88-b495-465a-b643-395cf9dad57e', foundational, conciliar_texts_contradict_irreconcilably).
narrative_ontology:cs_axiom_status(conciliar_texts_contradict_irreconcilably, holdable).
narrative_ontology:cs_axiom_grounding('5cf57a88-b495-465a-b643-395cf9dad57e', conciliar_texts_contradict_irreconcilably, empirically_contingent).
narrative_ontology:cs_axiom('5cf57a88-b495-465a-b643-395cf9dad57e', secondary, strict_indefectibility_precludes_development_of_this_kind).
narrative_ontology:cs_axiom_status(strict_indefectibility_precludes_development_of_this_kind, holdable).
narrative_ontology:cs_axiom_grounding('5cf57a88-b495-465a-b643-395cf9dad57e', strict_indefectibility_precludes_development_of_this_kind, deontological).
narrative_ontology:cs_reference_frame('5cf57a88-b495-465a-b643-395cf9dad57e', indefectible_apostolic_tradition).
narrative_ontology:cs_drift_state('5cf57a88-b495-465a-b643-395cf9dad57e', post_conciliar_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('5cf57a88-b495-465a-b643-395cf9dad57e', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_faction).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the post-Vatican II magisterial authority, enforcing the council's documents as legitimate expressions of Catholic tradition. Controls liturgical norms, doctrinal decrees, and canonical discipline. Cannot repudiate the council without catastrophic loss of institutional credibility, yet faces internal fracture over interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, post_conciliar_hierarchy, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from doctrinal ambiguity and liturgical experimentation authorized under post-conciliar interpretation. Gains theological space for positions that would have been censured under pre-conciliar magisterial norms. Highly networked within universities, religious orders, and diocesan bureaucracies.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, modernist_faction, beneficiary,
    organized, biographical, constrained, global).

% Bear the costs of liturgical restriction, doctrinal confusion, and marginalization of pre-conciliar theological categories. Their identity is fused with the Church's traditional lexicon, liturgy, and catechism; exit to the Novus Ordo or secularism is experienced as apostasy rather than mobility. Subject to restrictions on the traditional Latin Mass and traditional sacramental practice.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholics, payer,
    moderate, biographical, identity_locked, global).

% Holds the rupture reading explicitly: maintains Vatican II introduced doctrinal errors and that the post-conciliar magisterium is gravely defective. Excluded from canonical regularity and official theological conversation despite substantial following and sacramental activity. Would object to the conciliar authority claim if admitted to the conversation; instead exists in irregular juridical limbo.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sspx, excluded,
    organized, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains juridical and sacramental unity across the global Catholic Church under a single magisterial authority, providing a unified doctrinal reference point for a billion-member institution.
% TRANSFER_FUNCTION: Moves doctrinal authority and liturgical legitimacy from traditional theological categories and pre-conciliar norms to progressive theological positions and post-conciliar reinterpretations; moves canonical regularity away from traditionalist communities toward those accepting the council unconditionally.
% ABSENT_VOICES: The SSPX, sedevacantist communities, and the pre-conciliar magisterial tradition itself would object to the council's legitimacy if present in the official conversation. They are excluded by canonical irregularity, historical displacement, or disciplinary suppression. Traditional religious orders with reservations are present but constrained from open dissent.
% DISAPPEARANCE_RATIONALE: If the post-Vatican II authority claim vanished, the SSPX would be canonically regularized, traditional liturgy would proliferate without restriction, progressive theological positions would lose magisterial cover, and the Church would either revert to pre-conciliar norms or schism into distinct communions â the ecclesial landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The mid-20th century Catholic Church faced pastoral and administrative challenges in engaging modernity, ecumenism, and religious liberty while maintaining global unity.
% FOUNDING_PROBLEM_CORROBORATION: The post-conciliar hierarchy and modernist faction attest the problem was live and required conciliar reform. Traditional Catholics and the SSPX attest the problem was manageable within existing doctrine or that the conciliar solution introduced graver problems. Independent historians outside the benefiting parties document significant pre-conciliar institutional vitality, providing mixed corroboration.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint systematically transfers doctrinal authority from traditional norms to progressive reinterpretations, imposing costs on identity-locked traditionalists. Suppression (0.75) is high because persistence depends on canonical penalties, liturgical restrictions, and disciplinary actions against traditional communities. Theater_ratio (0.45) reflects moderate performative maintenance: the hierarchy continues to invoke tradition and continuity while restricting its lived expression. Accessibility_collapse (0.60) captures the partial collapse of pre-conciliar alternatives â they survive only in irregular or marginal forms. Resistance (0.70) is high due to sustained traditionalist non-compliance and the SSPX's continued operation. Temporal measurements show cyclical dynamics: extraction rose after the council, dipped during the Summorum Pontificum period (2007), and rose sharply again under recent restrictions on the traditional Latin Mass.
 *
 * PERSPECTIVAL GAP:
 *   The post-conciliar hierarchy experiences the constraint as necessary institutional unity and pastoral adaptation; traditional Catholics experience it as doctrinal imposition and identity suppression. The engine computes this divergence from the structural data: the hierarchy has constrained exit (cannot repudiate the council without institutional catastrophe) but low extraction due to its agenda-setting role, while traditionalists have identity-locked exit and high extraction. The modernist faction experiences subsidy-level extraction (negative Ï) because the constraint generates the doctrinal space they occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations target the modernist faction (low d, damped or negative Ï). Victim declarations target traditional Catholics (high d, amplified Ï). The hierarchy, as agenda-setter without explicit beneficiary status, reverts to the institutional power-atom fallback â structurally near-symmetric but with constrained exit preventing full mobility. The SSPX, though excluded, is trapped rather than mobile, keeping d elevated. Identity-lock is the key exit modulation for traditional Catholics: their self-concept is fused with the pre-conciliar Church, so structural extraction is amplified by the absence of viable fallback identities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â engaging modernity pastorally â is contested in status. The rupture reading holds the problem was either manageable within existing frameworks or manufactured to justify doctrinal change. This prevents mislabeling the constraint as rope (pure coordination): even if some coordination function exists (global sacramental governance), the asymmetric cost-bearing on identity-locked traditionalists and the contested founding problem push the classification toward tangled_rope. Were the founding problem uncontested and the costs symmetrically borne, the constraint might compute as rope; the structural data reject that reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the rupture reading of the vatican_ii_authority kernel. A continuity reading would reverse the beneficiary_victim structure and lower extractiveness substantially; a composite_overdetermination reading would fragment the constraint into irreducibly multiple sub-constraints. Does the engine''s classification hold across all three readings?',
    'Generate sibling constraint stories for continuity_reading and composite_overdetermination_reading; compare computed per-seat classifications across the kernel family.',
    'If the same structural data produces divergent classifications across readings, the kernel requires decomposition per the epsilon-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Reading identity within the Vatican II authority kernel').

omega_variable(
    rupture_vs_continuity_axiom,
    'Is the rupture reading''s core axiom (conciliar documents contradict irreconcilably with prior magisterial teaching) empirically decidable by historical-theological analysis, or is it a hermeneutical frame that constitutes the evidence?',
    'Systematic comparative analysis of conciliar texts against pre-conciliar magisterial decrees (e.g., Syllabus of Errors, Quanta Cura, Pascendi) by panels including both continuity and rupture scholars; agreement on textual contradiction would support the rupture frame.',
    'If the contradiction is demonstrable textually, the constraint''s extractiveness is grounded in doctrinal defect rather than mere policy preference; if the contradiction is hermeneutically constructed, the constraint may reclassify as identity_coordination or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_continuity_axiom, empirical, 'Whether conciliar-prior contradiction is textual or hermeneutic').

omega_variable(
    modernist_beneficiary_ambiguity,
    'Does the modernist faction constitute a coherent beneficiary group capturing extraction from the constraint, or is the post-conciliar hierarchy the true beneficiary with modernist theologians as incidental beneficiaries?',
    'Trace institutional resource flows: who controls appointments, seminary curricula, and liturgical permissions? If power and extraction accrue to the hierarchy regardless of theological orientation, the beneficiary label shifts.',
    'If the hierarchy is the primary beneficiary, the constraint is a snare (hierarchy extracting from traditionalists); if modernists capture the extraction independently, the constraint is a tangled rope with multiple beneficiary classes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernist_beneficiary_ambiguity, conceptual, 'Whether modernist faction or hierarchy is the primary beneficiary seat').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__rupture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vati_tr_t8, vatican_ii_authority__rupture_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(vati_tr_t15, vatican_ii_authority__rupture_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(vati_tr_t23, vatican_ii_authority__rupture_reading, theater_ratio, 23, 0.5).
narrative_ontology:measurement(vati_tr_t42, vatican_ii_authority__rupture_reading, theater_ratio, 42, 0.35).
narrative_ontology:measurement(vati_tr_t55, vatican_ii_authority__rupture_reading, theater_ratio, 55, 0.58).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__rupture_reading, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vati_be_t8, vatican_ii_authority__rupture_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(vati_be_t15, vatican_ii_authority__rupture_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(vati_be_t23, vatican_ii_authority__rupture_reading, base_extractiveness, 23, 0.62).
narrative_ontology:measurement(vati_be_t42, vatican_ii_authority__rupture_reading, base_extractiveness, 42, 0.48).
narrative_ontology:measurement(vati_be_t55, vatican_ii_authority__rupture_reading, base_extractiveness, 55, 0.7).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__rupture_reading, base_extractiveness, 60, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__rupture_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(vati_su_t8, vatican_ii_authority__rupture_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(vati_su_t15, vatican_ii_authority__rupture_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(vati_su_t23, vatican_ii_authority__rupture_reading, suppression_requirement, 23, 0.65).
narrative_ontology:measurement(vati_su_t42, vatican_ii_authority__rupture_reading, suppression_requirement, 42, 0.38).
narrative_ontology:measurement(vati_su_t55, vatican_ii_authority__rupture_reading, suppression_requirement, 55, 0.75).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__rupture_reading, suppression_requirement, 60, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is the rupture reading of the vatican_ii_authority kernel. The epsilon-invariance principle requires separate stories for continuity_reading and composite_overdetermination_reading because each reading produces a different beneficiary_victim structure and different epsilon values. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
