% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Doctrinal Authority (Composite Overdetermination Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint models the 'composite overdetermination' reading of
 *   Vatican II's doctrinal authority, which views the Council not as a
 *   single, monolithic shift but as a convergence of distinct structural
 *   changes (liturgical, ecumenical, ecclesiological, political) packaged as
 *   unified reform. This reading rejects a single epsilon measurement for the
 *   Council's impact, arguing that each component has independent
 *   extractiveness and that the continuity/rupture debate is a category
 *   error, as different components exhibit different degrees of change.
 *   Ambiguities are seen as a structural feature, not a bug, allowing for
 *   adaptive interpretation. This is one reading of the
 *   'vatican_ii_doctrinal_authority' kernel, alongside 'continuity_reading',
 *   'rupture_progressive_reading', and 'rupture_traditionalist_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.45).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.3).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Doctrinal Authority (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '72a0438c-1463-411b-8666-c63ac045cce5').
narrative_ontology:cs_kernel_codification('72a0438c-1463-411b-8666-c63ac045cce5', formalized).
narrative_ontology:cs_authority_grounding('72a0438c-1463-411b-8666-c63ac045cce5', lineage).
narrative_ontology:cs_interpretation_layer_present('72a0438c-1463-411b-8666-c63ac045cce5').
narrative_ontology:cs_reading_relation('72a0438c-1463-411b-8666-c63ac045cce5', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('72a0438c-1463-411b-8666-c63ac045cce5', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('72a0438c-1463-411b-8666-c63ac045cce5', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_axiom('72a0438c-1463-411b-8666-c63ac045cce5', foundational, doctrinal_development_is_multi_faceted).
narrative_ontology:cs_axiom_status(doctrinal_development_is_multi_faceted, holdable).
narrative_ontology:cs_axiom_grounding('72a0438c-1463-411b-8666-c63ac045cce5', doctrinal_development_is_multi_faceted, conventional).
narrative_ontology:cs_axiom('72a0438c-1463-411b-8666-c63ac045cce5', foundational, ambiguity_enables_adaptive_reception).
narrative_ontology:cs_axiom_status(ambiguity_enables_adaptive_reception, holdable).
narrative_ontology:cs_axiom_grounding('72a0438c-1463-411b-8666-c63ac045cce5', ambiguity_enables_adaptive_reception, instrumental).
narrative_ontology:cs_reference_frame('72a0438c-1463-411b-8666-c63ac045cce5', post_conciliar_adaptive_magisterium).
narrative_ontology:cs_drift_state('72a0438c-1463-411b-8666-c63ac045cce5', contemporary_synodal_process, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('72a0438c-1463-411b-8666-c63ac045cce5', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theologians_of_aggiornamento).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_factions).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, laity_seeking_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church, responsible for interpreting and applying Vatican II. Benefits from the flexibility of a composite reading, allowing adaptation to diverse contexts while maintaining a claim to unified reform. Bears the cost of managing internal dissent and hermeneutical disputes.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, magisterium, agenda_setter,
    institutional, generational, constrained, global).

% Theological experts who advocate for the ongoing renewal and adaptation of the Church in light of modern challenges. They benefit from a reading that emphasizes the dynamic and multi-faceted nature of the Council's reforms, providing intellectual space for new theological developments.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theologians_of_aggiornamento, beneficiary,
    organized, biographical, mobile, global).

% Groups within the Church who perceive Vatican II as a departure from immutable tradition. They bear the cost of perceived doctrinal ambiguity and liturgical changes, often feeling marginalized or alienated. Their identity is deeply tied to pre-conciliar forms, making exit difficult.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_factions, payer,
    moderate, generational, identity_locked, global).

% Individual members of the Church who struggle to reconcile the various interpretations and practical implementations of Vatican II. They pay in terms of spiritual confusion or a sense of instability, seeking clear, consistent guidance that the composite reading, by its nature, does not always provide.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, laity_seeking_clarity, payer,
    powerless, biographical, constrained, local).

% Other Christian denominations and religious traditions engaged in dialogue with the Catholic Church. They benefit from the ecumenical and interreligious dimensions of Vatican II, which are distinct components of the Council's composite nature, fostering greater understanding and cooperation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partners, beneficiary,
    organized, generational, mobile, global).

% Scholars who analyze the historical context, development, and reception of Vatican II. They observe the interplay of various forces that shaped the Council and its aftermath, providing critical analysis of the different readings without being directly subject to its authority.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, historical_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for the Catholic Church to engage with the modern world, addressing diverse challenges in liturgy, ecumenism, and ecclesiology through a series of distinct but interconnected reforms.
% TRANSFER_FUNCTION: Transfers interpretive flexibility and adaptive capacity to the Magisterium and progressive theologians, while transferring a sense of disorientation and perceived loss of tradition to traditionalist factions and some laity.
% ABSENT_VOICES: Those who left the Church due to perceived ambiguities or ruptures, or those who never engaged with its authority due to its perceived rigidity, are absent. They would argue for either a clearer break or a more explicit return to pre-conciliar forms.
% DISAPPEARANCE_RATIONALE: If the composite understanding of Vatican II's authority vanished, the Catholic Church would face an immediate crisis of identity and legitimacy. The various factions would lose their common (albeit contested) reference point, leading to potential schism or a radical redefinition of its mission. The global institutional structure would be profoundly destabilized.
% FOUNDING_PROBLEM: The Catholic Church faced increasing alienation from modern society, a perceived rigidity in its structures, and a need for internal renewal and external engagement in the mid-20th century.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium consistently affirms the ongoing relevance of Vatican II for addressing contemporary challenges. Historical theologians and ecumenical partners corroborate the historical context and the continued need for the Council's reforms, even while acknowledging ongoing interpretive debates.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the cost of managing the inherent ambiguities and internal disagreements arising from the composite nature of the reforms, particularly for those seeking clear, unified doctrine. Suppression (0.3) is moderate, as the Magisterium actively enforces a 'hermeneutic of reform in continuity' but allows for diverse theological schools. Theater ratio (0.2) is low, as the reforms are genuinely implemented, though their interpretation is contested. The claimed type is 'tangled_rope' because it genuinely coordinates diverse elements of Church life while simultaneously extracting a cost from those who struggle with its inherent ambiguities and the enforcement of a particular interpretive framework.
 *
 * PERSPECTIVAL GAP:
 *   The Magisterium and theologians of aggiornamento experience this as a flexible, adaptive framework that allows for necessary evolution. Traditionalist factions and laity seeking clarity experience it as a source of confusion and a departure from stable tradition. The engine's per-seat classification will capture this divergence based on their declared power, exit options, and roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and theologians of aggiornamento are beneficiaries, gaining interpretive flexibility and intellectual space. Traditionalist factions and laity seeking clarity are payers, bearing the costs of ambiguity and perceived rupture. Ecumenical partners are beneficiaries of specific components of the composite reform. Historical theologians are observers, analyzing the dynamics without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging the multi-faceted nature of Vatican II. It avoids reducing the Council to a single 'rope' of pure coordination or a 'snare' of pure extraction, recognizing that different components of the reform have distinct functions and impacts. The 'contested' status of the founding problem reflects the ongoing debate about whether the Council's original aims are still being met or if its composite nature has led to new, unforeseen challenges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    component_extractiveness_variance,
    'What is the specific extractiveness of each distinct component of Vatican II (e.g., liturgical reform, religious freedom, ecumenism, ecclesiological shifts)?',
    'Detailed historical and sociological analysis of each component''s implementation and reception, measuring specific costs and benefits for different groups.',
    'If specific components show significantly higher or lower extractiveness, it would refine the overall understanding of the Council''s impact and potentially lead to separate constraint stories for each component, linked by network effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_extractiveness_variance, empirical, 'Disaggregating the overall extractiveness into its constituent parts.').

omega_variable(
    ambiguity_as_feature_or_bug,
    'Is the inherent ambiguity of Vatican II''s composite nature a deliberate structural feature enabling adaptive interpretation, or an unresolved tension that functions as a bug, causing ongoing dissent and confusion?',
    'Analysis of Magisterial statements and theological discourse over time: if ambiguity is consistently framed as a positive for adaptation, it supports ''feature''; if consistently framed as a problem to be resolved, it supports ''bug''.',
    'If ambiguity is a ''feature'', the constraint''s coordination function is higher, and extraction is a necessary cost of flexibility. If a ''bug'', extraction is higher due to unresolved internal conflict, and the coordination function is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_feature_or_bug, conceptual, 'The role of ambiguity in Vatican II''s composite reading.').

omega_variable(
    hermeneutic_of_continuity_enforcement,
    'To what extent does the ''hermeneutic of reform in continuity'' (the official interpretive framework) suppress alternative readings, and how does this enforcement affect the perceived ''composite'' nature?',
    'Content analysis of official documents, disciplinary actions against theologians, and surveys of theological faculties regarding interpretive freedom.',
    'Stronger suppression of alternative readings would increase the constraint''s effective suppression and potentially shift its classification towards a ''snare'' for those whose interpretations are foreclosed. Weaker enforcement would allow for a more genuinely ''composite'' and contested landscape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_of_continuity_enforcement, empirical, 'The impact of official hermeneutics on interpretive diversity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.3).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.2).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'vatican_ii_doctrinal_authority' kernel. It emphasizes the Council as a composite of distinct structural changes, rejecting a monolithic interpretation. Other readings include 'continuity_reading', 'rupture_progressive_reading', and 'rupture_traditionalist_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
