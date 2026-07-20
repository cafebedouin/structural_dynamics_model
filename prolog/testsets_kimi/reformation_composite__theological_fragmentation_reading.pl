% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Reformation Confessional Fragmentation (Theological Reading)
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   The Reformation as a theological event: competing soteriological and
 *   ecclesiological commitments generate structurally incompatible
 *   denominations. This constraint story captures the confessional
 *   fragmentation system as a coordination mechanism for religious
 *   communities that simultaneously extracts authority and material support
 *   for denominational leadership. It is ONE READING of the contested kernel
 *   'reformation_composite'; sibling readings (political_realignment_reading,
 *   technological_mediation_reading) instantiate different constraints from
 *   the same historical events per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - confessional_leadership: Primary beneficiary (institutional/constrained) â administers confessional documents and collects authority from boundary maintenance.
 *   - denominational_theologians: Secondary beneficiary (organized/constrained) â produce and refine the theological apparatus legitimating fragmentation.
 *   - lay_dissenters: Primary target (powerless/trapped) â bear costs of exclusion and persecution under confessional enforcement.
 *   - cross_confessional_believers: Secondary target (moderate/identity_locked) â pay psychological and social costs for transgressing denominational boundaries.
 *   - ecumenical_advocates: Excluded voice (moderate/constrained) â marginalized by all confessions for advocating unity over purity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.72).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.71).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Reformation Confessional Fragmentation (Theological Reading)").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '481442ab-a036-4fb9-b819-74a77eca5ce1').
narrative_ontology:cs_kernel_codification('481442ab-a036-4fb9-b819-74a77eca5ce1', fixed_text).
narrative_ontology:cs_authority_grounding('481442ab-a036-4fb9-b819-74a77eca5ce1', lineage).
narrative_ontology:cs_interpretation_layer_present('481442ab-a036-4fb9-b819-74a77eca5ce1').
narrative_ontology:cs_reading_relation('481442ab-a036-4fb9-b819-74a77eca5ce1', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('481442ab-a036-4fb9-b819-74a77eca5ce1', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('481442ab-a036-4fb9-b819-74a77eca5ce1', foundational, scriptural_soteriology_as_primary_causal_engine).
narrative_ontology:cs_axiom_status(scriptural_soteriology_as_primary_causal_engine, holdable).
narrative_ontology:cs_axiom_grounding('481442ab-a036-4fb9-b819-74a77eca5ce1', scriptural_soteriology_as_primary_causal_engine, theological).
narrative_ontology:cs_axiom('481442ab-a036-4fb9-b819-74a77eca5ce1', foundational, confessional_identity_as_ecclesiological_necessity).
narrative_ontology:cs_axiom_status(confessional_identity_as_ecclesiological_necessity, holdable).
narrative_ontology:cs_axiom_grounding('481442ab-a036-4fb9-b819-74a77eca5ce1', confessional_identity_as_ecclesiological_necessity, deontological).
narrative_ontology:cs_reference_frame('481442ab-a036-4fb9-b819-74a77eca5ce1', scriptural_confessionalism).
narrative_ontology:cs_drift_state('481442ab-a036-4fb9-b819-74a77eca5ce1', post_confessionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('481442ab-a036-4fb9-b819-74a77eca5ce1', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_theologians).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, lay_dissenters).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, cross_confessional_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers confessional documents, adjudicates orthodoxy, and enforces denominational boundaries through ecclesiastical discipline and alliance with territorial authorities. Their authority and institutional legitimacy depend directly on the maintenance of structural incompatibility between denominations.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Produce, refine, and teach the theological distinctions that justify separate confessional identities. Their careers, platforms, and scholarly communities are sustained by the ongoing need to elaborate and defend denominational particularity.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_theologians, beneficiary,
    organized, generational, constrained, national).

% Bear the costs of confessional enforcement through social exclusion, heresy proceedings, exile, or execution when their beliefs or practices cross the boundaries set by leadership. They lack theological voice in defining orthodoxy and lack material resources to escape territorial enforcement.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, lay_dissenters, payer,
    powerless, biographical, trapped, local).

% Maintain personal networks and family ties across confessional lines but pay psychological and social costs for each transgression of denominational boundaries. Their identity is fused to the confessional community; exit feels like salvation jeopardy and relational annihilation.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, cross_confessional_believers, payer,
    moderate, biographical, identity_locked, regional).

% Argue for adiaphora, inter-confessional communion, and the priority of unity over purity. They are structurally excluded from official theological discourse within each confession because their position threatens the boundary maintenance that defines denominational identity.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecumenical_advocates, excluded,
    moderate, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__theological_fragmentation_reading, confessional_leadership).
narrative_ontology:fixing_cost_class(reformation_composite__theological_fragmentation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious communities around shared soteriological and ecclesiological commitments, providing liturgical unity, doctrinal clarity, communal boundaries, and salvation assurance for adherents within each confession.
% TRANSFER_FUNCTION: Moves authority, material support, social legitimacy, and the power to define orthodoxy from lay believers and dissenters to confessional leadership and denominational theological institutions, underwritten by the obligation to maintain doctrinal purity against rival communions.
% ABSENT_VOICES: Ecumenical advocates and irenical theologians who argue for unity across confessions are marginalized within every denomination. Anabaptist and radical reformers who reject the magisterial confessional framework altogether are persecuted and structurally excluded from the conversation.
% DISAPPEARANCE_RATIONALE: If the confessional fragmentation system vanished overnight, the enforcement mechanisms that maintain Lutheran, Reformed, and Catholic boundaries would collapse; religious identity would reorganize around territorial nationalism, individual conscience, pre-denominational parish structures, or renewed universal ecclesiology rather than confessional documents.
% FOUNDING_PROBLEM: The perceived doctrinal corruption of the medieval Latin churchâespecially in soteriology and ecclesiologyâand the need to restore apostolic purity and correct salvation theology.
% FOUNDING_PROBLEM_CORROBORATION: Confessional leadership attests the problem remains live, requiring ongoing boundary maintenance. Catholic historians and ecumenical scholars from outside the benefiting parties attest the problem was reframed rather than solved, producing confessional fragmentation in place of the old unity. Independent historiography corroborates the contested status.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because denominational leadership captures substantial authority and resource flows through the maintenance of confessional boundaries that are not strictly necessary for basic Christian communal coordination. Suppression (0.71) is high due to active enforcement via heresy proceedings, state confessionalization, and social exclusion. Theater_ratio (0.52) reflects that by the end of the interval, a significant portion of confessional activity is performative boundary-maintenance rather than genuine doctrinal exploration. Accessibility_collapse (0.68) captures how alternatives (ecumenical unity, non-confessional Christianity) collapse once the confessional system is accepted as normative. Resistance (0.58) reflects persistent Catholic, Anabaptist, and irenical opposition. The measurement series share a single time grid (0â131) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The confessional leadership seat experiences the constraint as necessary coordination to preserve doctrinal purity and communal salvation; the lay dissenter and cross-confessional seats experience it as enforced extraction and identity imprisonment. The engine computes this divergence from the same structural dataâthe theological reading does not adjudicate which seat is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   Confessional leadership and denominational theologians are structural beneficiaries (d near 0.0): they collect authority, career stability, and institutional legitimacy from the fragmentation. Lay dissenters and cross-confessional believers are structural targets (d near 1.0): they pay through exclusion, persecution, and identity-lock. Ecumenical advocates are excluded entirely, bearing directionality near 1.0 but without even the minimal benefits of insider coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the Reformation as pure extraction (snare)âthe confessions genuinely coordinate liturgy, charity, and meaning-making for millionsâwhile also preventing mislabeling it as pure coordination (rope)âthe same structure channels authority and resources asymmetrically to leadership. The founding problem (corrupt medieval church) is contested because the solution (fragmentation) may have reproduced the problem at a smaller scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the theological_fragmentation_reading capture the structural constraint of the Reformation, or is it one of three coexisting historiographical framings (alongside political_realignment_reading and technological_mediation_reading) that select different primary observables?',
    'Cross-reading constraint generation and comparison of per-seat computed types; historical archive triangulation assessing whether doctrinal, political, or technological variables show strongest structural coupling.',
    'If the kernel is genuinely underdetermined, each reading produces a distinct constraint with different epsilon and beneficiary structures; no single reading can claim to be the true constraint without collapsing the kernel''s plural causality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the theological reading is one of multiple valid framings of the same historical kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of confessional dissent structural (territorial exile, state punishment, material deprivation) or internalized (heresy anxiety, identity fusion with denomination, fear of salvation loss)?',
    'Post-exit trajectory analysis: if suppression persists after the subject leaves the territory or ceases public practice, the mechanism is partially internalized.',
    'If internalized, effective suppression and extractiveness are higher than structural measures suggest; the constraint operates through cognitive capture rather than overt coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in confessional communities.').

omega_variable(
    confessional_coordination_extraction_boundary,
    'Is the cost of maintaining confessional boundaries the necessary price of doctrinal coordination, or does it represent extractive overhead captured by denominational leadership?',
    'Comparative analysis of non-confessional religious communities (e.g., early Anabaptist networks, irenical humanist circles) to see whether similar coordination functions were achieved without comparable boundary enforcement and leadership rent extraction.',
    'A finding of viable lower-extraction alternatives would reclassify the constraint toward snare; a finding that confessional rigidity is inseparable from the coordination would support the tangled rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(confessional_coordination_extraction_boundary, conceptual, 'Whether confessional extraction is coordination cost or overhead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 0, 131).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ref_theol_frag_tr_t0, reformation_composite__theological_fragmentation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ref_theol_frag_tr_t20, reformation_composite__theological_fragmentation_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(ref_theol_frag_tr_t40, reformation_composite__theological_fragmentation_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(ref_theol_frag_tr_t60, reformation_composite__theological_fragmentation_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(ref_theol_frag_tr_t80, reformation_composite__theological_fragmentation_reading, theater_ratio, 80, 0.48).
narrative_ontology:measurement(ref_theol_frag_tr_t100, reformation_composite__theological_fragmentation_reading, theater_ratio, 100, 0.5).
narrative_ontology:measurement(ref_theol_frag_tr_t131, reformation_composite__theological_fragmentation_reading, theater_ratio, 131, 0.52).

% Extraction over time
narrative_ontology:measurement(ref_theol_frag_be_t0, reformation_composite__theological_fragmentation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ref_theol_frag_be_t20, reformation_composite__theological_fragmentation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(ref_theol_frag_be_t40, reformation_composite__theological_fragmentation_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(ref_theol_frag_be_t60, reformation_composite__theological_fragmentation_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(ref_theol_frag_be_t80, reformation_composite__theological_fragmentation_reading, base_extractiveness, 80, 0.65).
narrative_ontology:measurement(ref_theol_frag_be_t100, reformation_composite__theological_fragmentation_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(ref_theol_frag_be_t131, reformation_composite__theological_fragmentation_reading, base_extractiveness, 131, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ref_theol_frag_su_t0, reformation_composite__theological_fragmentation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ref_theol_frag_su_t20, reformation_composite__theological_fragmentation_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(ref_theol_frag_su_t40, reformation_composite__theological_fragmentation_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(ref_theol_frag_su_t60, reformation_composite__theological_fragmentation_reading, suppression_requirement, 60, 0.76).
narrative_ontology:measurement(ref_theol_frag_su_t80, reformation_composite__theological_fragmentation_reading, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(ref_theol_frag_su_t100, reformation_composite__theological_fragmentation_reading, suppression_requirement, 100, 0.73).
narrative_ontology:measurement(ref_theol_frag_su_t131, reformation_composite__theological_fragmentation_reading, suppression_requirement, 131, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one member of the reformation_composite constraint family. The kernel (the Protestant Reformation) decomposes into at least three structurally distinct constraints per the epsilon-invariance principle: theological_fragmentation_reading, political_realignment_reading, and technological_mediation_reading. Each has a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
