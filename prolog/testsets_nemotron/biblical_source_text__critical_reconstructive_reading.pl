% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical Reconstruction of Hypothetical Original Text as Primary
 *   domain: religious/historical/translation_theory
 *
 * SUMMARY:
 *   The critical reconstructive reading treats the hypothetical original text
 *   (Urtext, autograph) as the primary object of recovery. Neither the
 *   received text's structure nor its theological meaning can be privileged
 *   until the textual basis is established through historical-critical
 *   criteria. This reading instantiates the biblical_source_text kernel as a
 *   rope: it solves a genuine coordination problem (shared base text for
 *   scholarship and translation) with minimal coercive overhead —
 *   participants are net beneficiaries, alternatives are not suppressed. The
 *   extraction is low for academic readers (the method serves their work) but
 *   high for confessional communities whose identity is bound to the received
 *   text. The beneficiary set is academic biblical scholarship.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.32).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.18).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical Reconstruction of Hypothetical Original Text as Primary").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "religious/historical/translation_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, 'b52e5cb4-a8c7-4203-bc3c-c68c1418ce94').
narrative_ontology:cs_kernel_codification('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94', fixed_text).
narrative_ontology:cs_authority_grounding('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94', expertise).
narrative_ontology:cs_interpretation_layer_present('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94').
narrative_ontology:cs_reading_relation('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_axiom('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94', foundational, historical_recovery_primary_over_structure_and_meaning).
narrative_ontology:cs_axiom_status(historical_recovery_primary_over_structure_and_meaning, holdable).
narrative_ontology:cs_axiom_grounding('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94', historical_recovery_primary_over_structure_and_meaning, empirically_contingent).
narrative_ontology:cs_axiom('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94', foundational, received_text_is_derivative_witness_not_authoritative_base).
narrative_ontology:cs_axiom_status(received_text_is_derivative_witness_not_authoritative_base, holdable).
narrative_ontology:cs_axiom_grounding('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94', received_text_is_derivative_witness_not_authoritative_base, empirically_contingent).
narrative_ontology:cs_reference_frame('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94', pre_critical_confessional_textual_authority).
narrative_ontology:cs_drift_state('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94', contemporary_critical_consensus, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b52e5cb4-a8c7-4203-bc3c-c68c1418ce94', '').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_communities_holding_received_text).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, translation_committees).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, text_critical_editors).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, translation_committees).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, historical_critical_method).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, textual_criticism_principles).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, source_criticism_foundation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the methodological agenda for critical editions (Nestle-Aland, BHS, UBS), trains the field's practitioners, controls journal gatekeeping and grant structures, and defines what counts as legitimate textual recovery. Benefits from the constraint by having its method authorized as the primary scholarly gateway to the text.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold the received text (Textus Receptus, Masoretic Text, Septuagint tradition, Vulgate) as authoritatively given. The constraint destabilizes their textual basis by treating these as late, derivative witnesses. Exit requires abandoning the textual identity that constitutes the community's self-understanding — liturgy, doctrine, and formation are bound to the received text.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, confessional_communities_holding_received_text, payer,
    organized, generational, identity_locked, global).

% Produce Bible translations (NRSV, ESV, NIV, etc.) using critical editions as base text. Benefit from a standardized, methodologically transparent base. Pay the cost of navigating confessional backlash when critical readings depart from familiar received-text renderings — footnote apparatus becomes a site of contention.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, translation_committees, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, translation_committees, payer).

% Preach and teach from translations that incorporate critical reconstructions. Their congregations often hold the received text as authoritative; the critical apparatus introduces instability they must manage pastorally. They are not consulted in setting critical edition policy.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, pastoral_ministry_practitioners, excluded,
    moderate, biographical, constrained, local).

% Edit the critical apparatuses (NA28, BHS, etc.) that instantiate this constraint. Their professional standing and institutional positions depend on the critical method's authority. They have mobility across editorial projects but their career is constituted by this method.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, text_critical_editors, beneficiary,
    powerful, biographical, mobile, global).

% Studies the history of textual criticism as a discipline. Sees the constraint's genealogical development from Lachmann through Westcott-Hort to modern eclectic editions. Neither collects nor pays.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, analytical_observer_historian, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a methodologically disciplined, evidence-based base text for biblical scholarship and translation, replacing confessional tradition with reconstructive criteria (antiquity, attestation, transcriptional probability). Coordinates thousands of scholars and translators around a shared textual reference point.
% TRANSFER_FUNCTION: Moves epistemic authority from confessional tradition (received text as given) to academic reconstruction (hypothetical original as recovered). The confessional communities pay the destabilization cost; academic scholarship collects the methodological authority.
% ABSENT_VOICES: Liturgical communities using the received text in worship (Byzantine-rite Orthodox, traditionalist Catholic, conservative Protestant) and oral-tradition communities where the received text is memorized and performed. They would object to the privileging of hypothetical reconstruction over the text that constitutes their practice, but they are not seated at the critical edition table.
% DISAPPEARANCE_RATIONALE: If the critical reconstructive constraint vanished overnight, translation committees would lose their shared base text standard, confessional communities would revert to their respective received texts without academic mediation, and the scholarly field would fragment into confessional textual traditions. The coordination function is real and its loss would rearrange the field.
% FOUNDING_PROBLEM: Pre-critical biblical scholarship lacked a shared, evidence-based method for establishing the text. Confessional traditions each held their own received text as authoritative with no common court of appeal. The founding problem was: how to establish a text that transcends confessional boundaries through historical evidence.
% FOUNDING_PROBLEM_CORROBORATION: Metzger and Ehrman's 'The Text of the New Testament' (academic standard) attests the founding problem as live — the reconstructive task is never complete. Confessional scholars (e.g., Robinson-Pierpont Byzantine Priority, TR-only advocates) attest the problem is dead or misconceived — the received text was already established by providential preservation. No neutral arbiter exists.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).
:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) is modest: the constraint extracts destabilization from confessional communities but provides a coordination good (critical edition) that scholars and translators genuinely use. Suppression (0.18) is low: confessional communities retain their received texts liturgically and devotionally; the constraint operates in the scholarly/translation sphere, not by banning received-text use. Theater ratio (0.12) is low: the reconstructive work is functionally real, not performative. Accessibility collapse (0.45) is moderate: alternatives (received-text editions) persist but are marginalized in academic discourse. Resistance (0.55) is moderate: confessional pushback is real but has not displaced the critical edition standard.
 *
 * PERSPECTIVAL GAP:
 *   The confessional payer seat and the academic agenda-setter seat compute differently: from the academy the constraint is a rope (genuine coordination); from the confessional community it reads as a snare (extraction of textual authority). The engine computes this divergence from the structural data — identity_locked exit + organized power + payer role drives high effective extraction for confessional communities; institutional power + agenda_setter role + arbitrage exit drives low effective extraction for academic scholarship.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic biblical scholarship is the agenda-setter and primary beneficiary (d ~ 0.15) — the constraint subsidizes its methodological authority. Text-critical editors are powerful beneficiaries with mobile exit (d ~ 0.2). Translation committees are dual-positioned: they benefit from the coordination but pay confessional backlash costs (d ~ 0.5). Confessional communities are identity-locked payers (d ~ 0.85) — the constraint destabilizes their textual basis and exit requires abandoning constitutive identity. Pastoral practitioners are excluded and constrained (d ~ 0.6). The analytical observer sits at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (shared evidence-based text transcending confession) remains contested — academic scholarship says it's live; confessional traditions say it's dead or misconceived. The constraint has not atrophied into a piton: the critical edition apparatus is actively maintained, new manuscript discoveries (e.g., P137, P.Oxy. LXXXIII) still shift readings, and the coordination function is genuinely used. No concentrated beneficiary captures extraction — the gains are methodological authority distributed across the field. This is not a degraded snare; it is a functioning rope with asymmetric impact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Does the critical reconstructive reading''s premise (historical recovery primary) logically foreclose the formal equivalence reading''s premise (structural fidelity primary) within a single translation framework, or do they coexist as upstream/downstream?',
    'Examine translation prefaces: do committees using critical editions (NA28) describe themselves as bound by structural fidelity to that edition''s Greek, or do they treat the edition as a resource for communicative effectiveness? The formal equivalence reading applies to the critical text as its source; the two premises operate at different levels (text establishment vs. translation strategy).',
    'If forecloses, the kernel has a genuine logical partition. If coexists_with (as authored), the readings are held by different parties at different levels — critical edition choice and translation philosophy are separable decisions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationship between critical reconstruction and formal equivalence as kernel readings.').

omega_variable(
    confessional_identity_fusion_mechanism,
    'Is the confessional community''s identity-locked exit driven by theological conviction (the received text IS the text), sociological formation (liturgy/memory constitutes community), or institutional enforcement (denominational discipline)? The suppression metric (0.18) is low structurally but the exit experience is identity_locked — what mechanism binds?',
    'Comparative study of communities that switched textual bases (e.g., Roman Catholic post-Vatican II adoption of critical editions vs. TR-only Protestants). If identity_locked persists after institutional permission to switch, the mechanism is theological/sociological, not structural.',
    'If theological/sociological, the high effective extraction for confessional communities is not reducible by changing the constraint''s enforcement — the constraint triggers an identity mechanism it does not create. If institutional, the constraint''s suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confessional_identity_fusion_mechanism, empirical, 'Mechanism of identity-locked exit for confessional communities facing critical reconstruction.').

omega_variable(
    critical_edition_necessity_for_coordination,
    'Is a single critical edition (NA28/BHS) structurally necessary for the coordination function, or would multiple competing critical editions serve equally well? The rope classification assumes the constraint solves coordination — but if the coordination good is ''a critical edition'' rather than ''this specific critical edition'', the extraction profile changes.',
    'Counterfactual: if NA29 and a competing edition (e.g., ECM-based) diverged significantly, would translation committees fragment or adopt a consensus? Historical precedent: Westcott-Hort vs. Textus Receptus era saw fragmentation; modern consensus on NA/UBS standard suggests the edition itself coordinates.',
    'If multiple editions would serve, the current standard''s extraction is partly rent on the standard-setting position. If single edition is necessary, the extraction is the price of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_edition_necessity_for_coordination, conceptual, 'Whether the critical edition''s coordination function requires a single standard or tolerates competition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 1750, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1750, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(bibl_tr_t1830, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1830, 0.07).
narrative_ontology:measurement(bibl_tr_t1881, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1881, 0.09).
narrative_ontology:measurement(bibl_tr_t1900, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(bibl_tr_t1965, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1965, 0.11).
narrative_ontology:measurement(bibl_tr_t2025, biblical_source_text__critical_reconstructive_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1750, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1750, 0.15).
narrative_ontology:measurement(bibl_be_t1830, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1830, 0.22).
narrative_ontology:measurement(bibl_be_t1881, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1881, 0.28).
narrative_ontology:measurement(bibl_be_t1900, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(bibl_be_t1965, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1965, 0.31).
narrative_ontology:measurement(bibl_be_t2025, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 2025, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1750, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1750, 0.05).
narrative_ontology:measurement(bibl_su_t1830, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1830, 0.1).
narrative_ontology:measurement(bibl_su_t1881, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1881, 0.14).
narrative_ontology:measurement(bibl_su_t1900, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(bibl_su_t1965, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1965, 0.17).
narrative_ontology:measurement(bibl_su_t2025, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 2025, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, information_standard).
narrative_ontology:boltzmann_floor_override(biblical_source_text__critical_reconstructive_reading, 0.02).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__dynamic_equivalence_reading).

% DUAL FORMULATION NOTE:
% Part of the biblical_source_text constraint family. This reading (critical_reconstructive) establishes the text-critical base; formal_equivalence and dynamic_equivalence are translation-strategy readings that take the critical edition as their upstream input. The ε values differ: this reading has low extractiveness on academics (0.32) but high on confessional communities; the translation readings extract differently (formal equivalence extracts intelligibility cost from readers; dynamic equivalence extracts structural fidelity from source).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__critical_reconstructive_reading, organized, 0.85).
constraint_indexing:directionality_override(biblical_source_text__critical_reconstructive_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
