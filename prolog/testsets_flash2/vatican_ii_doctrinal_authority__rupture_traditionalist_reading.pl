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
 *   human_readable: Vatican II Doctrinal Authority (Rupture Traditionalist Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint models the traditionalist reading of Vatican II, where
 *   the Council's documents and subsequent implementation are seen as a
 *   rupture with Catholic tradition, leading to doctrinal ambiguity,
 *   liturgical abuses, and a decline in faith. The ambiguities within the
 *   conciliar texts are interpreted as deliberate compromises that enabled
 *   heterodox interpretations and a 'spirit of the Council' that went beyond
 *   the letter. This reading views the post-conciliar period as a period of
 *   significant extraction from traditional Catholic identity and practice.
 *
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
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority (Rupture Traditionalist Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c').
narrative_ontology:cs_kernel_codification('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', fixed_text).
narrative_ontology:cs_authority_grounding('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', lineage).
narrative_ontology:cs_interpretation_layer_present('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c').
narrative_ontology:cs_reading_relation('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', foundational, doctrinal_infallibility_of_prior_magisterium).
narrative_ontology:cs_axiom_status(doctrinal_infallibility_of_prior_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', doctrinal_infallibility_of_prior_magisterium, deontological).
narrative_ontology:cs_axiom('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', foundational, liturgical_tradition_as_sacred_and_immutable).
narrative_ontology:cs_axiom_status(liturgical_tradition_as_sacred_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', liturgical_tradition_as_sacred_and_immutable, theological).
narrative_ontology:cs_reference_frame('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', pre_conciliar_doctrinal_and_liturgical_stability).
narrative_ontology:cs_drift_state('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', contemporary_post_conciliar_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1bcb68e1-6ca7-4f1c-b48f-8f0555f9462c', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, modernist_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_adherents).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_conservatives).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience the loss of traditional liturgical forms and doctrinal clarity as a direct consequence of Vatican II's ambiguities and subsequent implementation. They feel spiritually impoverished and marginalized within the Church, with limited options for recourse beyond internal dissent or schism.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_adherents, payer,
    powerless, biographical, identity_locked, global).

% Perceive a weakening of dogmatic teaching and moral clarity post-Vatican II, attributing it to the Council's flawed texts and their heterodox interpretations. They advocate for a 'hermeneutic of reform in continuity' but feel their efforts are suppressed by the dominant progressive narrative.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_conservatives, payer,
    moderate, generational, constrained, global).

% Benefit from the perceived opening and ambiguities of Vatican II, which they interpret as validating their efforts to adapt Catholic doctrine to modern thought. They gain influence and academic freedom, often promoting interpretations that traditionalists view as heterodox.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, modernist_theologians, beneficiary,
    powerful, generational, mobile, global).

% Actively implement and promote the 'spirit of Vatican II,' often pushing for reforms in liturgy, ecumenism, and social teaching that go beyond the letter of the Council's documents. They leverage their institutional positions to enforce these changes, marginalizing traditionalist resistance.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_clergy, agenda_setter,
    institutional, generational, arbitrage, global).

% As an abstract good, it is seen as a victim of the Council's emphasis on ecumenism and interreligious dialogue, which traditionalists argue diluted the Church's unique salvific mission and reduced evangelization efforts. It is 'excluded' from the post-conciliar agenda.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal).

% The central administrative body of the Catholic Church, responsible for interpreting and implementing conciliar documents. From this reading's perspective, elements within the Curia either actively promote the rupture or fail to adequately suppress heterodox interpretations, thus enabling the perceived decline.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, roman_curia, agenda_setter,
    institutional, civilizational, constrained, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Council aimed to coordinate the Church's engagement with the modern world, fostering internal renewal and promoting Christian unity, while maintaining doctrinal fidelity.
% TRANSFER_FUNCTION: This reading asserts a transfer of doctrinal authority and liturgical practice away from established tradition towards novel, ambiguous, and ultimately heterodox interpretations, from traditionalists to modernists.
% ABSENT_VOICES: The voices of pre-conciliar theological clarity and uncompromised traditional Catholic teaching are seen as absent or suppressed in the post-conciliar discourse, replaced by a dominant progressive narrative.
% DISAPPEARANCE_RATIONALE: If the 'rupture' interpretation of Vatican II vanished, the entire post-conciliar landscape of Catholic theology, liturgy, and institutional practice would be re-evaluated. Traditionalist movements would lose their central grievance, and the internal debates within the Church would fundamentally shift, rearranging the theological and ecclesial world.
% FOUNDING_PROBLEM: The Council was convened to address the Church's relationship with a rapidly changing modern world, seeking aggiornamento (updating) and ressourcement (return to sources) to foster spiritual renewal and promote Christian unity.
% FOUNDING_PROBLEM_CORROBORATION: The official Church and progressive theologians attest the founding problem (modernity's challenges, need for renewal) is still live. Traditionalist critics, however, argue that the Council's approach exacerbated rather than solved these problems, leading to a crisis of faith and identity, a claim corroborated by declining vocations and Mass attendance in many Western countries, which they attribute to the Council's effects.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because this reading perceives a profound loss of traditional forms and clarity, which is seen as a direct cost imposed by the Council's outcome. Suppression (0.70) reflects the perceived marginalization and silencing of traditionalist voices within the Church, often through institutional means. Theater ratio (0.40) indicates that while some official efforts are made to affirm 'continuity,' these are seen as largely performative, masking an underlying rupture. Resistance is high (0.80) due to ongoing traditionalist movements and critiques.
 *
 * PERSPECTIVAL GAP:
 *   From the traditionalist perspective, the Council's outcomes are a snare, extracting from the faithful and from tradition itself. From the progressive perspective (a sibling reading), the same events represent a necessary rope or scaffold, liberating the Church. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernist theologians and progressive clergy are beneficiaries, gaining influence and freedom to pursue their agenda. Traditional liturgy adherents and doctrinal conservatives are victims, experiencing loss and marginalization. The Roman Curia, while officially upholding continuity, is seen by this reading as an agenda-setter that either actively promotes or passively permits the rupture, thus enabling the extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_ambiguity_vs_development,
    'Are the perceived ambiguities in Vatican II documents genuine errors or intentional open-endedness allowing for legitimate doctrinal development?',
    'Future magisterial clarifications or a comprehensive historical-theological analysis that definitively traces the lineage of contested concepts to pre-conciliar teaching or demonstrates their inherent contradiction.',
    'If genuine errors, it strengthens the rupture claim and the constraint''s extractiveness. If legitimate development, it weakens the rupture claim and shifts the constraint towards a more complex, possibly less extractive, classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_ambiguity_vs_development, conceptual, 'Whether textual ambiguities are flaws or features.').

omega_variable(
    implementation_vs_text,
    'To what extent are post-conciliar heterodoxies and liturgical changes a direct consequence of the Council''s texts, versus an independent ''spirit of the Council'' that departed from the texts?',
    'Detailed historical and theological studies comparing the explicit content of conciliar documents with the actual practices and theological trends that emerged, identifying causal links or divergences.',
    'If directly caused by texts, it reinforces the Council''s role as a snare. If largely independent, it shifts the blame to post-conciliar actors and weakens the Council''s direct extractiveness, potentially reclassifying it as a piton (inertial effect) or a tangled rope (misguided coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_vs_text, empirical, 'Causality of post-conciliar changes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of traditionalist voices structural (institutional barriers, formal condemnations) or internalized (social pressure, fear of marginalization)?',
    'Post-exit suppression trajectory: if traditionalist movements gain traction and influence outside formal Church structures, it suggests structural suppression is primary. If internal dissent persists even with reduced external pressure, internalized suppression is more significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — traditionalists carry the suppression with them after exit. If structural, targeted institutional reforms could more effectively alleviate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


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
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'vatican_ii_doctrinal_authority' kernel. This 'rupture_traditionalist_reading' focuses on the Council's perceived break with tradition and its negative consequences, contrasting with the 'continuity_reading' (organic development) and 'rupture_progressive_reading' (necessary break for ongoing reform).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
