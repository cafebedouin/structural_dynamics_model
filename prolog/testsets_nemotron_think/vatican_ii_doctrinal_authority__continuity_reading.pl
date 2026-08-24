% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II Hermeneutic of Continuity
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The continuity reading of Vatican II (hermeneutic of continuity) is the
 *   magisterium's official interpretive framework, promulgated most
 *   forcefully by Benedict XVI and maintained under Francis. It claims the
 *   Council changed nothing doctrinally — apparent novelties are explications
 *   of implicit prior teaching. Liturgical and pastoral changes are
 *   prudential adaptations. Post-conciliar chaos is blamed on implementation
 *   errors, not conciliar intent. This reading functions as a constraint on
 *   Catholic theological discourse: it authorizes some interpretations and
 *   silences others. The claimed_type is mountain (the reading presents
 *   itself as simply describing the natural structure of tradition). But the
 *   metrics reveal active enforcement, identifiable beneficiaries and
 *   victims, and rising extractiveness over time — the engine will compute
 *   the structural type from these facts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.42).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.58).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, mountain).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II Hermeneutic of Continuity").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).
domain_priors:emerges_naturally(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, '1c335936-e15d-44d8-b6d7-3a5349b20081').
narrative_ontology:cs_kernel_codification('1c335936-e15d-44d8-b6d7-3a5349b20081', formalized).
narrative_ontology:cs_authority_grounding('1c335936-e15d-44d8-b6d7-3a5349b20081', lineage).
narrative_ontology:cs_interpretation_layer_present('1c335936-e15d-44d8-b6d7-3a5349b20081').
narrative_ontology:cs_reading_relation('1c335936-e15d-44d8-b6d7-3a5349b20081', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('1c335936-e15d-44d8-b6d7-3a5349b20081', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('1c335936-e15d-44d8-b6d7-3a5349b20081', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('1c335936-e15d-44d8-b6d7-3a5349b20081', foundational, hermeneutic_of_continuity).
narrative_ontology:cs_axiom_status(hermeneutic_of_continuity, holdable).
narrative_ontology:cs_axiom_grounding('1c335936-e15d-44d8-b6d7-3a5349b20081', hermeneutic_of_continuity, deontological).
narrative_ontology:cs_axiom('1c335936-e15d-44d8-b6d7-3a5349b20081', secondary, implementation_errors_not_conciliar_intent).
narrative_ontology:cs_axiom_status(implementation_errors_not_conciliar_intent, holdable).
narrative_ontology:cs_axiom_grounding('1c335936-e15d-44d8-b6d7-3a5349b20081', implementation_errors_not_conciliar_intent, empirically_contingent).
narrative_ontology:cs_reference_frame('1c335936-e15d-44d8-b6d7-3a5349b20081', pre_conciliar_doctrinal_framework).
narrative_ontology:cs_drift_state('1c335936-e15d-44d8-b6d7-3a5349b20081', post_conciliar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c335936-e15d-44d8-b6d7-3a5349b20081', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, conservative_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, reform_laity).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, hermeneutic_of_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, organic_development_of_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, conciliar_documents_as_explication_not_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively promulgates the hermeneutic of continuity as the only legitimate reading of Vatican II. Controls doctrinal discipline, episcopal appointments, and curial governance. Collects interpretive authority and institutional stability. Exit means abandoning the claim to teach authentically — structurally impossible without institutional suicide.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, magisterium, agenda_setter,
    institutional, generational, arbitrage, universal).

% Gain professional recognition, publishing venues, and institutional positions by advancing the continuity reading. Their theological identity is fused with defending tradition against rupture. Exit would mean professional and existential dislocation — they have become the interpreters of the reading they defend.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_theologians, beneficiary,
    organized, biographical, identity_locked, global).

% Face censorship, silenced teaching positions, denied publication in official channels, and canonical warnings when their work reads Vatican II as rupture or development beyond continuity. The cost is professional marginalization within the Church. Exit options: leave for secular academia (constrained by vocation), submit to discipline (identity cost), or persist in tension.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_theologians, payer,
    moderate, biographical, constrained, global).

% Receive a stable, unchanging faith identity anchored in the continuity reading. Their religious self-understanding depends on the claim that nothing essential changed. Exit would fracture communal and personal identity — they have organized their spiritual lives around this hermeneutic.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, conservative_laity, beneficiary,
    organized, biographical, identity_locked, global).

% Experience the continuity reading as foreclosing pastoral adaptations they seek (married clergy, women's ordination, LGBTQ+ inclusion, liturgical vernacular creativity). Told their desires contradict the Council's true intent. Exit options: leave the Church (high relational cost), submit to teaching (conscience cost), or remain in hopeful dissent (psychic cost).
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, reform_laity, payer,
    moderate, biographical, constrained, global).

% Study the conciliar documents and reception history using historical-critical methods. Many conclude the continuity reading is a post-conciliar construct imposed on texts that show genuine novelty. They neither collect rents nor pay them — they describe what the constraint does to the historical record.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, historians_of_theology, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified hermeneutic for receiving Vatican II that prevents doctrinal fragmentation and schism by anchoring conciliar teaching in the pre-conciliar tradition, giving bishops and theologians a common interpretive framework.
% TRANSFER_FUNCTION: Moves interpretive authority from progressive theologians and historical-critical scholarship to the magisterium; moves the cost of liturgical/pastoral non-adaptation onto reform-minded laity and clergy; moves professional security onto traditionalist theologians who police the continuity boundary.
% ABSENT_VOICES: Progressive theologians silenced by doctrinal discipline; reform laity excluded from synodal processes that operate within the continuity frame; historians whose critical work is marginalized as 'reductionist'; victims of clerical abuse whose structural causes the continuity reading treats as implementation errors rather than conciliar ambiguities.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight, the magisterium would lose its primary legitimating hermeneutic for post-conciliar governance; traditionalist theologians would lose their institutional mandate; conservative laity would lose their identity anchor; progressive theologians and reform laity would gain interpretive space; the Church would face immediate doctrinal fragmentation or a new authoritative reading would be imposed.
% FOUNDING_PROBLEM: How to receive Vatican II's genuine novelties (religious liberty, ecumenism, collegiality, liturgical reform) without admitting rupture with the pre-conciliar doctrinal tradition that claims irreformability.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium (Pope Benedict XVI's 2005 Christmas address, Pope Francis's 2013 interview) attests the founding problem is live. Progressive theologians (Rahner, Schillebeeckx, Congar in their later writings) and historians (O'Malley, Alberigo, Faggioli) attest the problem is a constructed hermeneutic masking genuine rupture. No neutral arbiter exists — the corroboration split mirrors the beneficiary/payer split.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vatican_ii_doctrinal_authority__continuity_reading),
    narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the constraint extracts professional freedom from progressive theologians and pastoral agency from reform laity, but does not extract material resources. Suppression (0.58) is significant: doctrinal discipline (censure, removal, publication bans) actively maintains the reading. Theater ratio (0.31) reflects that the coordination function (unified reception) is real but increasingly performative — the unity it claims to protect is fracturing. Accessibility collapse (0.72) is high: within the magisterial framework, alternatives are nearly unthinkable. Resistance (0.47) is moderate: progressive theology persists in academia and pastoral practice despite discipline. The measurement series shows extractiveness and suppression rising 1965-1995 then stabilizing — the constraint hardened during the John Paul II/Benedict XVI era and has held.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterium's seat, the constraint is a mountain — it simply IS the tradition. From progressive theologians' seat, it is a snare — an enforced reading that extracts their intellectual labor. From reform laity's seat, it is a tangled rope — it coordinates Catholic identity but extracts their pastoral hopes. The engine computes this divergence from the structural data; the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium is the structural beneficiary (d ≈ 0.1): it gains interpretive monopoly and institutional coherence. Traditionalist theologians are beneficiaries (d ≈ 0.2): professional advancement tied to defending the reading. Conservative laity are beneficiaries (d ≈ 0.25): identity stability from the reading. Progressive theologians are targets (d ≈ 0.85): professional survival depends on navigating or resisting the constraint. Reform laity are targets (d ≈ 0.8): pastoral desires foreclosed. Historians are analytical (d = 0.5): they observe without collecting or paying. Exit options differentiate: magisterium has arbitrage (can modify the reading); traditionalists and conservative laity are identity_locked (exit = existential fracture); progressives and reform laity are constrained (exit possible but costly).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (receiving Vatican II without rupture) was live in 1965. By 2025, the continuity reading has become the primary obstacle to addressing the problems Vatican II actually raised (collegiality, religious liberty, liturgical inculturation). The mandate has atrophied: the reading now prevents the conciliar reforms it was built to protect. Yet it persists because the magisterium's legitimacy is fused to it (identity_locked), and no beneficiary has incentive to change it. This is mandatrophy: a coordination structure that has become extractive because its founding problem is contested but its beneficiaries treat it as settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_constructed_hermeneutic,
    'Is the hermeneutic of continuity a genuine organic development discoverable in the conciliar texts, or a post-conciliar construct imposed by magisterial authority to manage reception?',
    'Historical-critical analysis of conciliar debates, voting records, and periti interventions compared to post-conciliar magisterial documents (especially Benedict XVI''s 2005 address). If the continuity claim appears first in magisterial reception documents rather than conciliar texts, it is constructed.',
    'If constructed, the continuity reading is a false summit mountain (FSM candidate) — it claims natural law status but has identifiable beneficiaries (magisterium, traditionalist theologians) and active enforcement. The engine would reclassify via FSM signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_constructed_hermeneutic, conceptual, 'Whether the continuity reading describes a natural doctrinal structure or constructs one through authority.').

omega_variable(
    liturgical_pastoral_extraction_boundary,
    'Does the high extractiveness on liturgical/pastoral practice (the reading''s admitted domain of change) constitute doctrinal extractiveness, given Catholic theology''s claim that lex orandi lex credendi?',
    'Theological analysis of whether suppressing liturgical creativity (e.g., pre-1970 Missal restrictions, vernacular translation control) functionally determines doctrinal boundaries. Empirical study of whether communities using older rites develop distinct doctrinal profiles.',
    'If liturgical control = doctrinal control, the reading''s claimed low doctrinal ε is misleading — the constraint extracts doctrinally through pastoral means. The continuity/mountain claim collapses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liturgical_pastoral_extraction_boundary, conceptual, 'Whether pastoral/liturgical suppression functions as doctrinal extraction under lex orandi lex credendi.').

omega_variable(
    implementation_error_vs_conciliar_ambiguity,
    'Are post-conciliar ''excesses'' genuinely implementation errors, or do they reveal ambiguities in the conciliar texts themselves that the continuity reading must suppress to maintain its claim?',
    'Textual analysis of conciliar documents (especially Gaudium et Spes, Dignitatis Humanae, Lumen Gentium ch.3) for ambiguities that both progressive and traditionalist readings exploit. Correlate with periti testimony on intentional drafting compromises.',
    'If ambiguities are textual, the continuity reading''s suppression of alternative readings is not defending clarity but imposing one resolution of genuine indeterminacy — supporting snare/tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_error_vs_conciliar_ambiguity, empirical, 'Whether post-conciliar conflict stems from implementation failure or conciliar textual indeterminacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_continuity_tr_t1965, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1975, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1985, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1995, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1995, 0.27).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2005, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2015, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2025, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(vatican_ii_continuity_be_t1965, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1965, 0.18).
narrative_ontology:measurement(vatican_ii_continuity_be_t1975, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement(vatican_ii_continuity_be_t1985, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(vatican_ii_continuity_be_t1995, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(vatican_ii_continuity_be_t2005, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(vatican_ii_continuity_be_t2015, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(vatican_ii_continuity_be_t2025, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_continuity_su_t1965, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(vatican_ii_continuity_su_t1975, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1975, 0.42).
narrative_ontology:measurement(vatican_ii_continuity_su_t1985, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(vatican_ii_continuity_su_t1995, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(vatican_ii_continuity_su_t2005, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(vatican_ii_continuity_su_t2015, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(vatican_ii_continuity_su_t2025, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__continuity_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, papal_primacy_interpretation).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, episcopal_collegiality_practice).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, liturgical_reform_authority).

% DUAL FORMULATION NOTE:
% This continuity reading is one of four declared readings of the vatican_ii_doctrinal_authority kernel. It claims mountain status (organic development = natural law) but declares beneficiaries and requires enforcement — FSM candidate. The rupture readings claim the Council changed things (progressive: for good; traditionalist: for ill). The composite reading denies unitary conciliar intent. All four constrain Catholic theological discourse differently; the continuity reading currently holds institutional enforcement power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__continuity_reading, institutional, 0.1).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__continuity_reading, organized, 0.2).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__continuity_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
