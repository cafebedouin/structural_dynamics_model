% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II as Composite Overdetermination of Doctrinal Shifts
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'composite overdetermination'
 *   reading of Vatican II, which posits that the council was not a single
 *   coherent event but a product of multiple, sometimes incompatible,
 *   doctrinal shifts and factional compromises. This inherent ambiguity
 *   prevents a univocal interpretation of either 'continuity' or 'rupture,'
 *   leading to structural conflicts in the post-conciliar Church. The reading
 *   itself acts as a tangled rope, coordinating a more complex understanding
 *   of the council while extracting a cost from institutional authority that
 *   seeks a singular narrative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.65).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II as Composite Overdetermination of Doctrinal Shifts").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'ef5d2ea4-a002-4d4c-8353-567b0115c33c').
narrative_ontology:cs_kernel_codification('ef5d2ea4-a002-4d4c-8353-567b0115c33c', fixed_text).
narrative_ontology:cs_authority_grounding('ef5d2ea4-a002-4d4c-8353-567b0115c33c', lineage).
narrative_ontology:cs_interpretation_layer_present('ef5d2ea4-a002-4d4c-8353-567b0115c33c').
narrative_ontology:cs_reading_relation('ef5d2ea4-a002-4d4c-8353-567b0115c33c', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef5d2ea4-a002-4d4c-8353-567b0115c33c', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('ef5d2ea4-a002-4d4c-8353-567b0115c33c', foundational, doctrinal_overdetermination_is_structural).
narrative_ontology:cs_axiom_status(doctrinal_overdetermination_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('ef5d2ea4-a002-4d4c-8353-567b0115c33c', doctrinal_overdetermination_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('ef5d2ea4-a002-4d4c-8353-567b0115c33c', foundational, factional_compromise_yields_incompatible_rationales).
narrative_ontology:cs_axiom_status(factional_compromise_yields_incompatible_rationales, holdable).
narrative_ontology:cs_axiom_grounding('ef5d2ea4-a002-4d4c-8353-567b0115c33c', factional_compromise_yields_incompatible_rationales, empirically_contingent).
narrative_ontology:cs_reference_frame('ef5d2ea4-a002-4d4c-8353-567b0115c33c', post_conciliar_interpretive_chaos).
narrative_ontology:cs_drift_state('ef5d2ea4-a002-4d4c-8353-567b0115c33c', contemporary_theological_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('ef5d2ea4-a002-4d4c-8353-567b0115c33c', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, theological_scholars_of_complexity).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, critical_historians).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, faithful_seeking_univocal_guidance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars benefit from the recognition of Vatican II's inherent ambiguities and contradictions, as it provides fertile ground for ongoing research, critical analysis, and the development of new theological frameworks. Their careers are advanced by demonstrating the limits of simplistic interpretations.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, theological_scholars_of_complexity, beneficiary,
    analytical, generational, analytical, global).

% Historians who analyze the council's proceedings, compromises, and the diverse theological currents at play find their work validated by this reading. It allows them to highlight the human and political dimensions of doctrinal development, rather than presenting a monolithic narrative.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, critical_historians, beneficiary,
    analytical, generational, analytical, global).

% The teaching authority of the Church (Pope and bishops) is structurally challenged by this reading. It undermines claims of univocal interpretation and consistent doctrinal development, forcing the Magisterium to expend significant effort in reasserting a singular narrative, often through suppressive measures against dissenting interpretations. Their authority is diminished by the perceived lack of clarity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium, payer,
    institutional, civilizational, identity_locked, universal).

% Many adherents seek clear, unambiguous doctrinal guidance from the Church. This reading of Vatican II as inherently contradictory creates confusion, anxiety, and a sense of instability, making it difficult for them to reconcile conflicting teachings or understand the Church's direction. They bear the cognitive and spiritual cost of unresolved ambiguity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, faithful_seeking_univocal_guidance, payer,
    powerless, biographical, identity_locked, global).

% These groups often reject Vatican II entirely or interpret it through a lens of extreme rupture, seeing it as a betrayal of tradition. This reading, by highlighting internal contradictions, might inadvertently validate some of their criticisms, but it also frames the problem as inherent complexity rather than simple error, which they might resist.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_factions, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the understanding of Vatican II as a complex historical and theological event, preventing simplistic narratives of either pure continuity or pure rupture. It allows for a more nuanced engagement with the council's documents and their reception.
% TRANSFER_FUNCTION: It transfers the burden of interpretive coherence from the council's documents themselves to the ongoing theological and historical analysis. It shifts the 'cost' of ambiguity from being a flaw in the council to being a feature of its overdetermined nature, benefiting scholars who thrive on complexity while imposing interpretive strain on institutional authority and the faithful.
% ABSENT_VOICES: Those who insist on a single, divinely inspired, and perfectly coherent interpretation of Vatican II are structurally excluded from this reading's discourse. They would argue that any perceived contradictions are due to flawed human understanding, not inherent to the council itself, and that such a reading undermines faith.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the discourse around Vatican II would likely revert to a binary struggle between 'continuity' and 'rupture' camps, losing the nuanced understanding of internal contradictions and overdetermination. The theological and historical fields would be impoverished, and institutional attempts at univocal interpretation would face less intellectual resistance, potentially leading to a more rigid, less critical engagement with the council's legacy.
% FOUNDING_PROBLEM: The problem this reading addresses is the persistent failure of both 'continuity' and 'rupture' narratives to fully account for the diverse, often conflicting, theological and pastoral impulses present at Vatican II and in its aftermath.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing proliferation of conflicting interpretations, the persistent theological debates, and the historical records of factional struggles during and after the council, as documented by independent historians and theologians (e.g., Giuseppe Alberigo's 'History of Vatican II' project), corroborate that the problem of overdetermination and internal contradiction remains live. This corroboration comes from outside the institutional Magisterium.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the interpretive burden placed on the institutional Magisterium and the faithful, who are forced to navigate inherent contradictions. Suppression (0.70) is high because the institutional Church actively attempts to suppress readings that highlight internal contradictions, often through disciplinary measures or by promoting a singular 'hermeneutic of continuity.' The theater ratio (0.40) reflects the performative efforts by institutional actors to maintain a facade of perfect coherence, despite the evident internal tensions. Resistance (0.75) is high due to ongoing theological debates and the persistence of diverse interpretive schools.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of scholars, this reading is a necessary intellectual tool for understanding a complex historical event. From the perspective of the institutional Magisterium, it is a challenge to authority and a source of disunity, requiring active suppression. The engine's classification will reflect this divergence, showing a more beneficial outcome for analytical observers and a more extractive one for institutional actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Theological scholars and critical historians are beneficiaries (low d) as this reading validates their analytical approach and provides rich material for research. The institutional Magisterium and faithful seeking univocal guidance are victims (high d) as they bear the cost of interpretive ambiguity and the efforts to suppress it. The constraint extracts from those who require or claim a singular, coherent interpretation, while benefiting those who thrive on complexity and critical analysis.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resolvability_of_ambiguity,
    'Are the ambiguities and apparent contradictions within Vatican II documents truly unresolvable, or could a deeper theological synthesis reconcile them?',
    'A future ecumenical council or a definitive papal pronouncement that offers a universally accepted, coherent synthesis, or a sustained period of theological consensus across diverse schools.',
    'If resolvable, the ''composite overdetermination'' reading would be superseded by a more unified understanding, reducing its extractiveness on institutional authority. If truly unresolvable, this reading''s claims of inherent contradiction would be further validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resolvability_of_ambiguity, conceptual, 'Whether Vatican II''s internal ambiguities are fundamentally unresolvable or amenable to future synthesis.').

omega_variable(
    institutional_response_efficacy,
    'How effective are the institutional Magisterium''s efforts to enforce a singular ''hermeneutic of continuity'' in suppressing alternative readings?',
    'Empirical study of theological publications, seminary curricula, and public discourse over time, measuring the prevalence and impact of diverse interpretations despite official pronouncements.',
    'If suppression is highly effective, the constraint''s extractiveness on the Magisterium might appear lower (as dissent is contained), but its suppression on other actors would be higher. If ineffective, the Magisterium''s costs of maintaining a singular narrative would be higher, and the constraint would appear more extractive on them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_response_efficacy, empirical, 'The actual efficacy of institutional suppression of diverse Vatican II interpretations.').

omega_variable(
    theological_pluralism_as_feature,
    'Is the theological pluralism and interpretive struggle resulting from Vatican II''s ambiguities a bug or a feature of post-conciliar Catholicism?',
    'A shift in official Church teaching or a widespread theological consensus that explicitly embraces or rejects the value of interpretive pluralism as a mode of doctrinal development.',
    'If embraced as a feature, the extractiveness on the faithful and Magisterium would decrease, as the ambiguity would be reframed as a positive aspect of ongoing theological inquiry. If definitively rejected as a bug, the pressure for univocal interpretation would intensify, increasing extractiveness and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_pluralism_as_feature, preference, 'Whether interpretive pluralism post-Vatican II is seen as a positive or negative development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.5).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
