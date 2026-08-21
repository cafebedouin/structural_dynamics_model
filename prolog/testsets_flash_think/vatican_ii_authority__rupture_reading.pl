% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Vatican II as Doctrinal Rupture
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint is the 'rupture reading' of the Vatican II authority
 *   kernel. It posits that the Second Vatican Council (1962-1965) represents
 *   a substantive break with prior Catholic tradition, introducing doctrinal
 *   errors or irreconcilable contradictions with previous teaching. From this
 *   perspective, the Council led to a crisis in the Church, benefiting
 *   'modernist' factions at the expense of traditional Catholic identity and
 *   doctrinal stability. Sibling readings include 'continuity_reading' and
 *   'composite_overdetermination_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.85).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.78).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, snare).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II as Doctrinal Rupture").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '78f09f83-daff-46cf-928b-a46a411d5cfa').
narrative_ontology:cs_kernel_codification('78f09f83-daff-46cf-928b-a46a411d5cfa', formalized).
narrative_ontology:cs_authority_grounding('78f09f83-daff-46cf-928b-a46a411d5cfa', lineage).
narrative_ontology:cs_interpretation_layer_present('78f09f83-daff-46cf-928b-a46a411d5cfa').
narrative_ontology:cs_reading_relation('78f09f83-daff-46cf-928b-a46a411d5cfa', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('78f09f83-daff-46cf-928b-a46a411d5cfa', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('78f09f83-daff-46cf-928b-a46a411d5cfa', foundational, vatican_ii_contains_doctrinal_errors).
narrative_ontology:cs_axiom_status(vatican_ii_contains_doctrinal_errors, holdable).
narrative_ontology:cs_axiom_grounding('78f09f83-daff-46cf-928b-a46a411d5cfa', vatican_ii_contains_doctrinal_errors, empirically_contingent).
narrative_ontology:cs_axiom('78f09f83-daff-46cf-928b-a46a411d5cfa', foundational, doctrinal_immutability_is_absolute).
narrative_ontology:cs_axiom_status(doctrinal_immutability_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('78f09f83-daff-46cf-928b-a46a411d5cfa', doctrinal_immutability_is_absolute, deontological).
narrative_ontology:cs_reference_frame('78f09f83-daff-46cf-928b-a46a411d5cfa', pre_vatican_ii_tradition).
narrative_ontology:cs_drift_state('78f09f83-daff-46cf-928b-a46a411d5cfa', post_conciliar_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('78f09f83-daff-46cf-928b-a46a411d5cfa', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, liberal_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholics).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, doctrinal_stability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, catholic_laity_mainstream).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, society_of_st_pius_x_sspx).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, catholic_laity_mainstream).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, traditional_catholic_doctrine_of_immutability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority of the Catholic Church, which officially promulgates and enforces the teachings of Vatican II, often presenting them as continuous with tradition. From this reading's perspective, it actively suppresses dissent from traditionalist quarters.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, holy_see_post_vatican_ii, agenda_setter,
    institutional, generational, constrained, global).

% Adherents who believe Vatican II introduced doctrinal errors and contradictions, leading to a crisis of faith and identity. They bear the cost of perceived doctrinal instability and the marginalization of traditional practices. Their identity is deeply tied to pre-conciliar Catholicism, making exit from the Church unthinkable for many.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholics, payer,
    powerless, biographical, identity_locked, global).

% Theological and ecclesiastical groups who actively promote and benefit from the perceived doctrinal shifts and reinterpretations introduced by Vatican II, seeing them as necessary 'aggiornamento' (renewal) for the Church to engage with the modern world.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, modernist_faction, beneficiary,
    organized, biographical, mobile, global).

% Scholars and thinkers who interpret Vatican II in ways that emphasize discontinuity with past teachings, promoting new theological paradigms. They gain influence and academic standing from these interpretations.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, liberal_theologians, beneficiary,
    powerful, biographical, arbitrage, global).

% A traditionalist priestly fraternity that explicitly rejects certain aspects of Vatican II and the post-conciliar reforms. They face canonical penalties and excommunication for their stance, but maintain their identity and mission outside the mainstream Church structure.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, society_of_st_pius_x_sspx, payer,
    organized, generational, constrained, global).

% The majority of Catholic faithful who generally accept Vatican II but may experience confusion or indifference regarding its theological implications. They benefit from perceived openness but also bear the diffuse costs of doctrinal ambiguity and liturgical changes.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, catholic_laity_mainstream, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, catholic_laity_mainstream, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the Council's stated aim of 'aggiornamento' and ecumenism failed to genuinely coordinate, instead introducing disunity and doctrinal confusion. The coordination function is seen as a cover for a rupture.
% TRANSFER_FUNCTION: Transfers doctrinal authority and traditional Catholic identity from the immutable deposit of faith to a 'modernist' interpretation, resulting in a loss of clarity, stability, and traditional practices for many faithful.
% ABSENT_VOICES: The voices of pre-Vatican II popes, theologians, and the 'sensus fidelium' (sense of the faithful) of previous eras, whose teachings are seen as contradicted by the Council. Their historical consensus is excluded from the contemporary interpretive framework.
% DISAPPEARANCE_RATIONALE: If the authority of Vatican II vanished overnight, the entire post-conciliar structure of the Catholic Church, its liturgy, theology, and self-understanding, would be fundamentally challenged. It would necessitate a radical re-evaluation and likely a return to pre-conciliar forms for those who hold this reading, or an explicit schism.
% FOUNDING_PROBLEM: To address the challenges of modernity, foster Christian unity (ecumenism), and renew the Church's mission in the contemporary world.
% FOUNDING_PROBLEM_CORROBORATION: The Holy See (post-Vatican II) attests that the founding problems are still live and the Council's solutions are ongoing. Traditionalist scholars, historians, and theologians (outside the benefiting modernist faction) argue that the Council's solutions either failed, exacerbated existing problems, or created new ones, leading to a crisis in the Church.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because, from this reading's perspective, the Council's changes imposed significant costs on traditional Catholics, including loss of liturgical forms, doctrinal clarity, and a sense of continuity with the past. Suppression is high (0.78) due to the active enforcement by the Holy See of the post-conciliar direction and the marginalization or excommunication of traditionalist dissenters. Theater ratio is high (0.65) because official attempts to reconcile Vatican II with tradition are seen as performative, masking a fundamental discontinuity. Accessibility collapse is high (0.80) as the 'true' path of tradition is perceived as obscured or made inaccessible by the Council's changes. Resistance is high (0.70) due to ongoing opposition from traditionalist groups.
 *
 * PERSPECTIVAL GAP:
 *   The 'rupture reading' fundamentally diverges from the official 'continuity reading' on the nature of Vatican II. From the rupture perspective, the Council is a snare that extracts traditional identity and doctrinal stability, while from the continuity perspective, it is a legitimate development (rope or scaffold). The engine's classification will highlight this divergence based on the authored metrics and structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Holy See (post-Vatican II) acts as the agenda-setter, enforcing the new direction. 'Modernist' factions and liberal theologians are beneficiaries, gaining influence and promoting their interpretations. Traditional Catholics and doctrinal stability are the primary victims, bearing the costs of perceived rupture. Groups like the SSPX are payers who actively resist, facing canonical penalties. Mainstream laity are diffuse payers/beneficiaries, experiencing both perceived openness and doctrinal confusion.
 *
 * MANDATROPHY ANALYSIS:
 *   From this reading's perspective, the original mandate of 'aggiornamento' (renewal) has been subverted. Instead of renewing tradition, the Council is seen as having broken with it, making the persistence of its authority a form of extraction rather than genuine coordination. The constraint is not a piton because there are clear beneficiaries (modernist factions) who actively profit from its operation and enforcement, even if the original mandate is seen as having atrophied or been corrupted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_continuity_vs_rupture,
    'Is Vatican II a genuine rupture with prior Catholic tradition, or an organic development in continuity with it?',
    'Comprehensive theological and historical analysis of Council documents against prior magisterial teaching, focusing on specific points of alleged contradiction. However, ultimate resolution often depends on interpretive frameworks.',
    'If resolved as continuity, the extractiveness and suppression metrics would be significantly lower, and the claimed type would shift towards a rope or scaffold. If resolved as rupture, the current classification as a snare is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_continuity_vs_rupture, conceptual, 'The fundamental interpretive ambiguity regarding Vatican II''s relationship to tradition.').

omega_variable(
    suppression_legitimacy,
    'Is the suppression of traditionalist views by the Holy See necessary for maintaining Church unity and doctrinal coherence, or is it an extractive mechanism to enforce a particular theological agenda?',
    'Analysis of the canonical penalties applied, the theological arguments used to justify them, and the impact on dissenting communities. Examination of whether alternative models of unity could accommodate diverse theological expressions.',
    'If deemed necessary for unity, the suppression would be re-evaluated as a legitimate (though costly) coordination function. If deemed extractive, it reinforces the snare classification and highlights the coercive nature of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_legitimacy, conceptual, 'The justification for suppressing traditionalist dissent.').

omega_variable(
    impact_on_doctrinal_stability,
    'What is the actual, measurable impact of Vatican II on the doctrinal stability and traditional identity of the Catholic Church?',
    'Sociological studies of Catholic belief and practice pre- and post-Council, comparative theological analysis of catechetical materials, and historical examination of theological trends. This is an empirical question with significant interpretive challenges.',
    'Empirical evidence of widespread doctrinal confusion or loss of traditional identity would reinforce the ''victim'' status of traditional Catholics and the high extractiveness. Evidence of enhanced clarity or renewed identity would challenge this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_doctrinal_stability, empirical, 'The empirical consequences of Vatican II on Catholic identity and doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 1962, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_authority__rupture_reading, theater_ratio, 1962, 0.3).
narrative_ontology:measurement(vati_tr_t1972, vatican_ii_authority__rupture_reading, theater_ratio, 1972, 0.45).
narrative_ontology:measurement(vati_tr_t1982, vatican_ii_authority__rupture_reading, theater_ratio, 1982, 0.55).
narrative_ontology:measurement(vati_tr_t1992, vatican_ii_authority__rupture_reading, theater_ratio, 1992, 0.6).
narrative_ontology:measurement(vati_tr_t2002, vatican_ii_authority__rupture_reading, theater_ratio, 2002, 0.63).
narrative_ontology:measurement(vati_tr_t2012, vatican_ii_authority__rupture_reading, theater_ratio, 2012, 0.65).
narrative_ontology:measurement(vati_tr_t2022, vatican_ii_authority__rupture_reading, theater_ratio, 2022, 0.65).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_authority__rupture_reading, base_extractiveness, 1962, 0.6).
narrative_ontology:measurement(vati_be_t1972, vatican_ii_authority__rupture_reading, base_extractiveness, 1972, 0.7).
narrative_ontology:measurement(vati_be_t1982, vatican_ii_authority__rupture_reading, base_extractiveness, 1982, 0.78).
narrative_ontology:measurement(vati_be_t1992, vatican_ii_authority__rupture_reading, base_extractiveness, 1992, 0.82).
narrative_ontology:measurement(vati_be_t2002, vatican_ii_authority__rupture_reading, base_extractiveness, 2002, 0.84).
narrative_ontology:measurement(vati_be_t2012, vatican_ii_authority__rupture_reading, base_extractiveness, 2012, 0.85).
narrative_ontology:measurement(vati_be_t2022, vatican_ii_authority__rupture_reading, base_extractiveness, 2022, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_authority__rupture_reading, suppression_requirement, 1962, 0.55).
narrative_ontology:measurement(vati_su_t1972, vatican_ii_authority__rupture_reading, suppression_requirement, 1972, 0.65).
narrative_ontology:measurement(vati_su_t1982, vatican_ii_authority__rupture_reading, suppression_requirement, 1982, 0.72).
narrative_ontology:measurement(vati_su_t1992, vatican_ii_authority__rupture_reading, suppression_requirement, 1992, 0.75).
narrative_ontology:measurement(vati_su_t2002, vatican_ii_authority__rupture_reading, suppression_requirement, 2002, 0.77).
narrative_ontology:measurement(vati_su_t2012, vatican_ii_authority__rupture_reading, suppression_requirement, 2012, 0.78).
narrative_ontology:measurement(vati_su_t2022, vatican_ii_authority__rupture_reading, suppression_requirement, 2022, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vatican_ii_authority' kernel. This 'rupture reading' posits a substantive break with tradition, contrasting with the 'continuity_reading' and the 'composite_overdetermination_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
