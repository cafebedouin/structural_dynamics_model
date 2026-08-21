% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority: Composite Overdetermination Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint models Vatican II's magisterial authority as a 'composite
 *   overdetermination' – a reading that views the Conciliar texts not as a
 *   singular, coherent reinterpretation, but as a collection of ambiguous
 *   compromise formulations designed to achieve supermajority votes by
 *   encoding incompatible ecclesiological visions. This structural ambiguity
 *   means that hermeneutical control, rather than textual clarity, becomes
 *   the real locus of authority, leading to persistent implementation
 *   divergence and ongoing interpretive struggles. The 10-12% rejection votes
 *   on key texts are seen as a signal of unresolved theological
 *   incompatibility embedded in the final documents, rather than mere
 *   dissent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.65).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority: Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, '56903a24-0361-4eb3-a1b1-e9a995612c9b').
narrative_ontology:cs_kernel_codification('56903a24-0361-4eb3-a1b1-e9a995612c9b', fixed_text).
narrative_ontology:cs_authority_grounding('56903a24-0361-4eb3-a1b1-e9a995612c9b', lineage).
narrative_ontology:cs_interpretation_layer_present('56903a24-0361-4eb3-a1b1-e9a995612c9b').
narrative_ontology:cs_reading_relation('56903a24-0361-4eb3-a1b1-e9a995612c9b', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('56903a24-0361-4eb3-a1b1-e9a995612c9b', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('56903a24-0361-4eb3-a1b1-e9a995612c9b', foundational, conciliar_texts_encode_incompatible_visions).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_incompatible_visions, holdable).
narrative_ontology:cs_axiom_grounding('56903a24-0361-4eb3-a1b1-e9a995612c9b', conciliar_texts_encode_incompatible_visions, empirically_contingent).
narrative_ontology:cs_axiom('56903a24-0361-4eb3-a1b1-e9a995612c9b', foundational, hermeneutical_control_is_locus_of_authority).
narrative_ontology:cs_axiom_status(hermeneutical_control_is_locus_of_authority, holdable).
narrative_ontology:cs_axiom_grounding('56903a24-0361-4eb3-a1b1-e9a995612c9b', hermeneutical_control_is_locus_of_authority, conventional).
narrative_ontology:cs_reference_frame('56903a24-0361-4eb3-a1b1-e9a995612c9b', conciliar_compromise_formulations).
narrative_ontology:cs_drift_state('56903a24-0361-4eb3-a1b1-e9a995612c9b', contemporary_hermeneutical_struggle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56903a24-0361-4eb3-a1b1-e9a995612c9b', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, theological_moderates).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_factions).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and interprets the Conciliar texts, leveraging their ambiguity to maintain institutional control and manage internal dissent. Benefits from the ability to selectively emphasize aspects of the texts to suit current magisterial priorities, while avoiding explicit repudiation of any faction.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia, agenda_setter,
    institutional, generational, constrained, global).

% Experience the texts as a rupture with tradition, but are bound by their commitment to the Church's authority. They bear the cost of internal contradiction and marginalization, often resorting to 'hermeneutic of continuity' arguments that deny the texts' inherent ambiguities, or forming schismatic groups.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_factions, payer,
    organized, generational, identity_locked, global).

% Experience the texts as an incomplete or compromised rupture, failing to fully implement necessary reforms. They bear the cost of unfulfilled expectations and internal frustration, often pushing for further development that is resisted by the Curia, or facing accusations of dissent.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reformers, payer,
    organized, generational, identity_locked, global).

% Find intellectual space within the texts' ambiguities, allowing for diverse theological exploration without direct confrontation with magisterial authority. They benefit from the texts' capacity to accommodate a range of views, fostering a sense of unity despite underlying tensions.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, theological_moderates, beneficiary,
    moderate, biographical, mobile, global).

% Experience confusion and disunity stemming from the conflicting interpretations of Vatican II. They bear the cost of theological uncertainty and internal strife within the Church, often feeling alienated by debates they cannot resolve.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, laity, payer,
    powerless, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed the Catholic Church to achieve supermajority consensus on conciliar documents by encoding diverse, sometimes incompatible, theological visions through ambiguous compromise formulations, thereby maintaining a semblance of unity and avoiding immediate schism.
% TRANSFER_FUNCTION: Transfers the burden of resolving theological contradictions from the Conciliar fathers to subsequent generations of interpreters and the faithful, while transferring hermeneutical control and institutional stability to the Roman Curia.
% ABSENT_VOICES: Those who explicitly rejected the compromise formulations (e.g., the 10-12% who voted 'non placet' on key documents) were effectively silenced in the final texts, their objections absorbed into the ambiguity rather than resolved. Their voices would highlight the inherent contradictions.
% DISAPPEARANCE_RATIONALE: If the composite nature of Vatican II's texts and the resulting interpretive struggle vanished, the Church would be forced to explicitly choose between continuity and rupture, leading to a fundamental reordering of its theological and institutional landscape, likely resulting in significant schism or a radically different ecclesiology.
% FOUNDING_PROBLEM: The Catholic Church faced a crisis of modernity, needing to update its self-understanding and engagement with the contemporary world while preserving its core doctrines, amidst deep internal divisions between traditionalists and reformers.
% FOUNDING_PROBLEM_CORROBORATION: Historians and theologians across the spectrum attest to the deep divisions preceding and during the Council. The ongoing hermeneutical debates and internal conflicts within the Church corroborate that the problem of reconciling tradition with modernity, and managing internal theological diversity, remains live.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the ongoing cost of managing internal contradictions and the intellectual labor required to reconcile irreconcilable positions, borne by various factions. Suppression (0.70) is high because the institutional authority actively enforces a 'unity' that masks underlying theological incompatibilities, often marginalizing or silencing dissenting interpretations. The theater ratio (0.40) reflects the significant performative effort in maintaining a narrative of continuity and coherence despite the texts' inherent ambiguities. The claimed type is 'tangled_rope' because it genuinely coordinated a supermajority consensus (preventing immediate schism) but did so through a mechanism that extracts ongoing costs from those who must live with the unresolved tensions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Roman Curia, the texts represent a successful act of coordination and magisterial authority, allowing for a 'living tradition.' From the perspective of traditionalists and progressives, the same texts are a source of ongoing extraction, forcing them to either deny their own theological convictions or operate at the margins of institutional legitimacy. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The Roman Curia (agenda_setter) benefits from the texts' ambiguity, allowing it to selectively interpret and maintain institutional control. Traditionalist and progressive factions (payers) bear the costs of internal contradiction and unfulfilled expectations, often identity-locked by their commitment to the Church. Theological moderates (beneficiaries) find intellectual space in the ambiguity. The laity (payer) experiences confusion and disunity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to maintain unity and address modernity is still 'live,' but the 'composite overdetermination' mechanism itself has become a source of extraction. The classification as a Tangled Rope prevents mislabeling it as a pure Snare (ignoring the genuine coordination function of achieving consensus) or a pure Rope (ignoring the asymmetric costs of ambiguity). The ongoing interpretive struggle is not a bug, but a feature of how the constraint operates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_deliberate_ambiguity,
    'To what extent was the ambiguity in the Conciliar texts a deliberate strategy to achieve consensus, versus an unavoidable outcome of complex theological debate?',
    'Historical analysis of conciliar archives, drafting committee minutes, and private correspondence of key participants, focusing on explicit discussions of compromise language and voting strategies.',
    'If largely deliberate, it strengthens the ''tangled_rope'' classification by highlighting the intentionality of the extractive coordination. If largely unavoidable, it might shift the ''extractiveness'' slightly downward, as the costs are less ''designed'' and more ''emergent.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_deliberate_ambiguity, empirical, 'Assessing the intentionality behind the texts'' ambiguities.').

omega_variable(
    hermeneutical_control_as_extraction,
    'Is the Roman Curia''s exercise of ''hermeneutical control'' primarily a legitimate act of magisterial authority, or a mechanism for extracting compliance and suppressing alternative readings?',
    'Comparative study of how different interpretations are treated institutionally (e.g., suppression of theologians, promotion of certain schools of thought), and analysis of the financial and career costs borne by those who challenge the dominant hermeneutic.',
    'If primarily extractive, it would increase the ''extractiveness'' and ''suppression'' metrics, potentially pushing the classification closer to a ''snare'' for certain seats. If primarily legitimate, it would lower these metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_control_as_extraction, conceptual, 'Distinguishing legitimate interpretation from extractive control.').

omega_variable(
    identity_lock_strength_for_factions,
    'How strong is the ''identity_locked'' exit option for traditionalist and progressive factions? What would be the actual cost of schism or leaving the institutional Church for these groups?',
    'Sociological studies of ex-members of such factions, analysis of schismatic movements'' long-term viability, and surveys of current members regarding their perceived costs of exit.',
    'If the identity lock is weaker than perceived, their ''power'' might be slightly higher, and their ''directionality'' less extreme, as their exit options are less constrained. If stronger, it reinforces the current assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_strength_for_factions, empirical, 'Quantifying the binding force of identity on dissenting factions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(vati_tr_t1980, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(vati_be_t1980, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement(vati_su_t1980, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, catechism_of_the_catholic_church_authority).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_infallibility_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Vatican II magisterial authority' kernel, which also includes 'continuity_reading' and 'rupture_reading'. This reading emphasizes the texts' inherent ambiguities and compromise formulations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
