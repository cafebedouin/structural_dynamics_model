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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II as Rupture with Tradition (Traditionalist Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the traditionalist reading of Vatican II,
 *   where the Council's documents and subsequent implementation are seen as a
 *   rupture with Catholic tradition, leading to doctrinal ambiguity,
 *   liturgical decline, and a loss of missionary focus. The constraint's high
 *   extractiveness reflects the perceived cost to traditional Catholics and
 *   the Church's traditional identity. The high suppression reflects the
 *   active marginalization of traditional practices and beliefs within the
 *   post-conciliar Church. The claimed type is 'snare' because the
 *   coordination story (renewal, aggiornamento) is seen as cover for an
 *   extractive process that victimizes traditional elements.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.85).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.75).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, snare).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II as Rupture with Tradition (Traditionalist Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '336d0482-f911-40a0-bf17-e7a7d50e2a0d').
narrative_ontology:cs_kernel_codification('336d0482-f911-40a0-bf17-e7a7d50e2a0d', fixed_text).
narrative_ontology:cs_authority_grounding('336d0482-f911-40a0-bf17-e7a7d50e2a0d', lineage).
narrative_ontology:cs_interpretation_layer_present('336d0482-f911-40a0-bf17-e7a7d50e2a0d').
narrative_ontology:cs_reading_relation('336d0482-f911-40a0-bf17-e7a7d50e2a0d', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('336d0482-f911-40a0-bf17-e7a7d50e2a0d', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('336d0482-f911-40a0-bf17-e7a7d50e2a0d', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('336d0482-f911-40a0-bf17-e7a7d50e2a0d', foundational, vatican_ii_contradicts_prior_magisterium).
narrative_ontology:cs_axiom_status(vatican_ii_contradicts_prior_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('336d0482-f911-40a0-bf17-e7a7d50e2a0d', vatican_ii_contradicts_prior_magisterium, deontological).
narrative_ontology:cs_axiom('336d0482-f911-40a0-bf17-e7a7d50e2a0d', foundational, ambiguity_leads_to_heterodoxy).
narrative_ontology:cs_axiom_status(ambiguity_leads_to_heterodoxy, holdable).
narrative_ontology:cs_axiom_grounding('336d0482-f911-40a0-bf17-e7a7d50e2a0d', ambiguity_leads_to_heterodoxy, empirically_contingent).
narrative_ontology:cs_reference_frame('336d0482-f911-40a0-bf17-e7a7d50e2a0d', pre_conciliar_doctrinal_clarity).
narrative_ontology:cs_drift_state('336d0482-f911-40a0-bf17-e7a7d50e2a0d', contemporary_post_conciliar_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('336d0482-f911-40a0-bf17-e7a7d50e2a0d', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, modernist_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_catholics).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience the post-conciliar Church as a departure from essential tradition, leading to loss of faith, liturgical abuses, and doctrinal confusion. They are identity-locked by their commitment to Catholicism but feel alienated by its current expression.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_catholics, payer,
    powerless, generational, identity_locked, global).

% Benefit from the perceived ambiguities and openness of Vatican II documents, which they interpret as validating their theological innovations and critiques of pre-conciliar teaching. They gain influence and academic freedom.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, modernist_theologians, beneficiary,
    powerful, biographical, mobile, global).

% Implement reforms and pastoral approaches justified by the 'spirit' of Vatican II, often at odds with traditional practices. They gain autonomy and perceived relevance, but are constrained by formal hierarchical structures.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_clergy, beneficiary,
    organized, biographical, constrained, global).

% Administers the Church's doctrinal and liturgical life. From this reading's perspective, the Curia either actively promotes or passively permits the heterodox implementation of Vatican II, failing to uphold tradition. Its power is immense but constrained by internal factions and external pressures.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, roman_curia, agenda_setter,
    institutional, generational, constrained, universal).

% The traditional forms of worship and sacraments, which this reading sees as suppressed or marginalized in favor of novelties. It is a victim of the rupture, losing its central place and being treated as an exception rather than the norm.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy).

% The clear and unambiguous articulation of Catholic doctrine, which this reading believes has been eroded by the ambiguities and compromises within Vatican II documents and their subsequent interpretations. It is a victim of the perceived rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity).

% The fervent desire and effort to convert non-Catholics, which this reading argues has been undermined by the Council's ecumenical and interreligious dialogues, leading to a loss of urgency and distinctiveness. It is a victim of the perceived rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal).

% The bishops who participated in Vatican II. From this reading's perspective, they either naively introduced ambiguities or deliberately compromised, setting the stage for the post-conciliar crisis. They are observed as historical actors whose actions created the constraint.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, conciliar_fathers, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Council aimed to coordinate the Church's response to modernity and foster internal unity, but from this reading, it failed to achieve genuine coordination and instead introduced disunity.
% TRANSFER_FUNCTION: Transfers authority and legitimacy from traditional theological frameworks and liturgical practices to novel interpretations and pastoral approaches, from the perspective of traditionalists.
% ABSENT_VOICES: The voices of pre-conciliar theologians and saints, whose teachings are seen as having been marginalized or contradicted. Also, the voices of future generations of traditional Catholics who would be deprived of a clear, consistent tradition.
% DISAPPEARANCE_RATIONALE: If the 'rupture' interpretation of Vatican II vanished, the entire post-conciliar Church would need to fundamentally re-evaluate its identity, mission, and practices. Traditionalists would no longer feel alienated, and the perceived crisis of faith would be re-framed.
% FOUNDING_PROBLEM: The Council was convened to address the Church's relationship with the modern world, promote Christian unity, and renew Catholic life.
% FOUNDING_PROBLEM_CORROBORATION: The Roman Curia and progressive theologians attest the problems are still live, requiring ongoing adaptation. Traditionalist scholars and lay movements, citing historical and theological arguments, attest that the Council either exacerbated existing problems or created new ones, and that the original problems were not solved but transformed. Independent historians and sociologists offer analyses that support both views, depending on their methodology and focus.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because traditionalists perceive a profound loss of spiritual and doctrinal goods. Suppression is high due to active measures to suppress traditional liturgy and theology, often enforced by hierarchical authority. Theater ratio is moderate, as some efforts to maintain 'continuity' are seen as performative, masking a deeper rupture. The metrics reflect the traditionalist experience of the post-conciliar period as a period of significant loss and coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the traditionalist perspective, the Council's documents and implementation function as a snare, extracting from tradition. From a 'continuity' perspective, the same documents function as a rope or scaffold, guiding organic development. The engine's per-seat classification will highlight this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional Catholics, traditional liturgy, doctrinal clarity, and missionary zeal are the primary targets/victims (high d). Modernist theologians and progressive clergy are beneficiaries (low d), gaining influence and freedom for their interpretations. The Roman Curia, as the agenda-setter, is seen as either complicit or ineffective in preventing the rupture, thus enabling the extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_rupture_vs_misinterpretation,
    'Is the perceived rupture a genuine structural feature of Vatican II''s documents and implementation, or a misinterpretation by traditionalists?',
    'A definitive, universally accepted magisterial interpretation that explicitly forecloses the traditionalist reading, or a historical consensus among theologians that the traditionalist reading is demonstrably false.',
    'If a misinterpretation, the extractiveness and suppression metrics would be re-evaluated downward, potentially reclassifying the constraint as a rope or even a mountain (from the perspective of a different reading). If genuine, the snare classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_rupture_vs_misinterpretation, conceptual, 'Ambiguity regarding the objective nature of the rupture vs. subjective perception.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (hierarchical enforcement, formal bans) or internalized (traditionalists'' self-marginalization, despair)?',
    'Post-exit suppression trajectory: if traditionalist communities thrive and grow after formal suppression is removed, it suggests structural suppression was dominant. If internal divisions and despair persist, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would reinforce the ''snare'' classification by highlighting the depth of the constraint''s hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditional Catholics.').

omega_variable(
    mandatrophy_of_unity,
    'Has the Council''s original mandate to foster unity become a cover for enforcing a particular theological agenda, thus becoming mandatrophic?',
    'Analysis of official statements and actions: if ''unity'' is consistently invoked to suppress dissent or traditional practices, it suggests mandatrophy. If genuine efforts at reconciliation are made, the mandate is still live.',
    'If mandatrophic, the theater_ratio would be re-evaluated upward, and the constraint''s classification would lean more strongly towards snare or piton, as the original purpose has atrophied into a tool for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_unity, empirical, 'Whether the mandate for unity has become mandatrophic.').


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
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1962, 0.6).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1975, 0.75).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1962, 0.5).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Vatican II Doctrinal Authority' kernel. Its high extractiveness and snare classification reflect the traditionalist perspective of rupture, contrasting with the continuity and progressive readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
