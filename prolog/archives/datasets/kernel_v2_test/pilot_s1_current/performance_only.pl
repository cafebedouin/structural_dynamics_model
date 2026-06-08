% ============================================================================
% CONSTRAINT STORY: performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_only, []).

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
 *   constraint_id: performance_only
 *   human_readable: Sacrifice Obligation as Performance-Only (No Substitute by Study)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   The obligation to perform animal sacrifice is a foundational law in the
 *   Abrahamic tradition, codified in textual sources (Torah, Mishnah, Talmud)
 *   as a binding perpetual requirement. When the Temple was destroyed in 70
 *   CE, the structural conditions for performance became permanently
 *   impossible: no altar, no priesthood performing sacrifice in the
 *   designated place, no live animal market for sacrificial offerings,
 *   diaspora dispersion, and legal prohibitions on private sacrifice. This
 *   constraint examines one reading of how the obligation persists: the
 *   performance_only reading holds that the obligation remains binding,
 *   unsatisfied, and cannot be substituted by study, prayer, commemoration,
 *   or theoretical engagement. Current obligants are structurally unable to
 *   fulfill an obligation they are required to keep. The performance_only
 *   reading creates a situation where the law is eternally unfulfilled and
 *   unfulfillable — every obligant is in perpetual violation. Study is
 *   permitted but explicitly insufficient. This is a pure extraction
 *   mechanism from the perspective of the obligant (snare) and a coordination
 *   mechanism from the institutional perspective (rope) that benefits from
 *   the obligation's perpetual binding force. The performance_only reading is
 *   one of several possible readings of the same kernel (the obligation's
 *   continuing force); alternative readings interpret the obligation
 *   differently (study_as_performance: study is a legitimate substitute;
 *   messianic_suspension: the obligation is suspended until restoration;
 *   archival_preservation: the obligation is preserved in memory and text
 *   rather than action).
 *
 * KEY AGENTS:
 *   - Current Generation Obligants: Primary victims (powerless/trapped) — structurally unable to perform; guilt mandated without remedy
 *   - Diaspora Observant: Secondary victims (moderate/identity_locked) — identity fused with tradition; cannot exit without abandoning self-concept
 *   - Institutional Priesthood: Primary beneficiary (institutional/arbitrage) — maintains authority through elaboration of the obligation; coordinates community around the institution
 *   - Reform Movement: Organized agents (organized/constrained) — seeking to redefine obligation or substitute study; constrained by traditional authority
 *   - Textual Tradition: Institutional mechanism (institutional/arbitrage) — performs elaboration and interpretation; maintains theater of obligation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional arrangement as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_only, 0.75).
domain_priors:suppression_score(performance_only, 0.68).
domain_priors:theater_ratio(performance_only, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_only, extractiveness, 0.75).
narrative_ontology:constraint_metric(performance_only, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(performance_only, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_only, snare).
narrative_ontology:human_readable(performance_only, "Sacrifice Obligation as Performance-Only (No Substitute by Study)").
narrative_ontology:topic_domain(performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_only, 'd574fd80-1080-45b0-974f-02eb39fe4457').
narrative_ontology:cs_kernel_codification('d574fd80-1080-45b0-974f-02eb39fe4457', formalized).
narrative_ontology:cs_authority_grounding('d574fd80-1080-45b0-974f-02eb39fe4457', extraction).
narrative_ontology:cs_interpretation_layer_present('d574fd80-1080-45b0-974f-02eb39fe4457').
narrative_ontology:cs_reading_relation('d574fd80-1080-45b0-974f-02eb39fe4457', performance_only__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('d574fd80-1080-45b0-974f-02eb39fe4457', performance_only__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('d574fd80-1080-45b0-974f-02eb39fe4457', performance_only__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('d574fd80-1080-45b0-974f-02eb39fe4457', foundational, performance_uniquely_constitutive).
narrative_ontology:cs_axiom_status(performance_uniquely_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('d574fd80-1080-45b0-974f-02eb39fe4457', performance_uniquely_constitutive, deontological).
narrative_ontology:cs_axiom('d574fd80-1080-45b0-974f-02eb39fe4457', foundational, obligation_persists_despite_impossibility).
narrative_ontology:cs_axiom_status(obligation_persists_despite_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('d574fd80-1080-45b0-974f-02eb39fe4457', obligation_persists_despite_impossibility, deontological).
narrative_ontology:cs_reference_frame('d574fd80-1080-45b0-974f-02eb39fe4457', temple_sacrifice_functional).
narrative_ontology:cs_drift_state('d574fd80-1080-45b0-974f-02eb39fe4457', post_temple_destruction, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('d574fd80-1080-45b0-974f-02eb39fe4457', '').
narrative_ontology:cs_kernel_id(performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_only, institutional_priesthood).
narrative_ontology:constraint_victim(performance_only, current_generation_obligants).
narrative_ontology:constraint_victim(performance_only, observant_diaspora).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CURRENT-GENERATION OBLIGANT (SNARE) — Obligated by law but structurally unable to fulfill the obligation (Temple destroyed, no altar, no animal sacrifice possible). Study is declared insufficient as substitution. Guilt is mandated; remedy is impossible. Maximum extraction: the constraint extracts moral obligation without providing a path to satisfaction. Trapped: exit would require abandoning religious identity and ancestral law.
constraint_indexing:constraint_classification(performance_only, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DIASPORA OBSERVANT (SNARE) — Identity fused with observance of law; cannot imagine themselves outside the tradition. Structurally mobile (could leave diaspora, adopt different practice framework) but identity-locked through inherited commitment to the obligation. The constraint extracts perpetual guilt and symbolic performance of impossibility. Study is permitted (perhaps even encouraged) but explicitly NOT a substitute — the obligant remains in violation.
constraint_indexing:constraint_classification(performance_only, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL PRIESTHOOD (ROPE) — Benefits from maintaining the obligation as unsatisfiable. The obligation sustains institutional legitimacy (as keepers of the memory and law), generates textual authority (responsa, interpretation, regulation of substitutionary practices), and channels observant behavior toward institutional forms (prayer, commemoration, study under rabbinic authority). This perspective experiences the constraint as pure coordination: the unsatisfiable obligation coordinates the community around institutional religious practice. The priesthood has exit options (could declare the obligation suspended or satisfied through study) but does not exercise them — this is net-beneficiary positioning.
constraint_indexing:constraint_classification(performance_only, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM MOVEMENT (TANGLED ROPE) — Organized agents seeking to redefine the obligation or substitute alternative practices (study as fulfillment, commemoration as substitute, reinterpretation as satisfaction). Constrained by institutional authority and traditional hermeneutic monopoly. The constraint extracts legitimacy from reform — traditional authority can dismiss reform as violation of law. Reform also coordinates genuine community needs: finding a path forward that honors the obligation while acknowledging structural impossibility. Mixed: both extractive pressure and genuine coordination function present.
constraint_indexing:constraint_classification(performance_only, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: TEXTUAL TRADITION / PERFORMATIVE MAINTENANCE (PITON) — From the civilizational perspective, the obligation persists primarily as performance of continuity. Endless responsa, commentaries, and theoretical frameworks elaborate how one would perform the sacrifice IF circumstances permitted, or how study substitutes for it, or how intention compensates for inability. The functional core (actual sacrifice) has atrophied; what remains is theatrical reconstruction. Theater ratio reflects this: the tradition performs the obligation's binding force while the actual obligation cannot be performed. Maintained through interpretive elaboration, not through functional fulfillment.
constraint_indexing:constraint_classification(performance_only, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing the obligation as immutable law: 'An obligation once given cannot be dissolved; if the condition for performance fails, the obligation persists eternally; this is a law of moral order.' This perspective treats the obligation's binding force as independent of practical possibility — a natural law of religious commitment. However, the structural data reveals this as a false summit: beneficiaries exist (institutional priesthood), suppression is substantial, and theater is present. The 'natural law' reading naturalizes what is actually an institutional arrangement.
constraint_indexing:constraint_classification(performance_only, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_only_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(performance_only, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(performance_only, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(performance_only, TR),
    TR >= 0.70.

:- end_tests(performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. The obligation extracts ongoing guilt and moral obligation without providing a path to satisfaction. The structural impossibility of performance means the obligant cannot reach a state of compliance. This is maximally extractive: the obligant is guaranteed to remain in violation regardless of behavior, and the violation itself is morally significant. Suppression (0.68): Moderate-high. Suppression comes from multiple mechanisms: institutional authority discouraging alternative readings, identity fusion preventing exit, theological framing of the obligation as absolute, and the absence of a legitimate substitute. However, suppression is not total — some communities do adopt alternatives (study, commemoration), and institutional enforcement of the obligation is primarily discursive rather than coercive. Theater ratio (0.55): Moderate. The tradition engages in substantial theatrical activity: elaborate commentaries on how the sacrifice would be performed (if possible), theoretical frameworks for understanding the obligation, performative remembrance. However, the theater is not the primary mechanism of extraction — extraction derives directly from the obligation's binding force and structural impossibility, not from the performance of alternatives. The increasing theater ratio over the interval reflects the tradition's elaboration of interpretive frameworks as the obligation's impossibility becomes more salient.
 *
 * PERSPECTIVAL GAP:
 *   The obligant and the institution experience this constraint entirely differently. The obligant enters the victim set: trapped by an obligation they cannot fulfill, guilty by law, with study explicitly insufficient. Extractiveness is experienced as guilt without remedy. The institution experiences this as coordination: the obligation organizes the community, generates textual authority, channels behavior toward institutional forms. The performance_only reading creates maximum perspectival gap: the same constraint appears as pure snare (powerless/trapped) and pure rope (institutional/arbitrage). The reform movement sees tangled rope: genuine community coordination need (how do we move forward?) mixed with extraction pressure (traditional authority resists alternatives). The piton perspective reveals that much of the tradition's activity is performative maintenance of the obligation's binding force while acknowledging its unfulfillability. The analytical observer risks the false summit: treating the obligation as a natural law of moral order rather than recognizing it as an institutional arrangement that benefits specific actors.
 *
 * DIRECTIONALITY LOGIC:
 *   The obligant's directionality is maximal (d approaching 1.0): trapped exit, no arbitrage option, victim status, powerless position. The effective extraction chi is high and unmodulated — there is no path to beneficiary positioning that reduces experienced extraction. The institutional priesthood's directionality is minimal (d approaching 0.0): arbitrage exit, beneficiary status, institutional power. Their experienced extraction is negative (they are subsidized by the obligation). The reform movement's directionality is moderate (d ≈ 0.5): constrained exit (they face institutional resistance but are organized and have some agency), both beneficiary and victim elements (they coordinate genuine community needs but are pressured by traditional authority). The piton perspective's directionality depends on whether the textual tradition is treated as an agent: if treated as agent, it has arbitrage exit (can abandon the obligation) but chooses to maintain it for institutional reasons, making it a beneficiary with low d. If treated as mechanism rather than agent, directionality does not apply.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL: The performance_only reading creates a mandatrophy state — the obligation's original mandate (enable relationship with the divine through sacrifice) has outlived its condition (the Temple, the priesthood, the system for performing sacrifice). The obligation persists despite losing its functional justification. The mandate was: 'Perform the sacrifice to effect atonement and maintain the covenant.' The condition for fulfilling this mandate no longer exists. Yet the obligation persists not because the mandate is still operative but because the textual tradition treats the obligation as eternally binding independent of conditions. This is a clear case of mandatrophy: a law or obligation whose founding purpose has been superseded or made impossible, yet the law persists. The performance_only reading explicitly refuses to acknowledge the mandatrophy — it insists the obligation remains binding despite the impossible conditions. Other readings (study_as_performance, messianic_suspension) attempt to resolve the mandatrophy by reinterpreting the obligation or suspending it. The performance_only reading deepens the mandatrophy by insisting on the impossibility while maintaining the obligation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_binding_mechanism,
    'Does the binding force of the obligation derive from the action itself (performance-as-constitutive) or from the commander''s will (obedience-as-constitutive)?',
    'Textual analysis of source law; examination of whether suspension/substitute claims are treated as violation or reinterpretation; historical tracking of how communities have handled similar situations where performance became impossible',
    'If action-constitutive: current generation is genuinely in violation; study is relief but not remedy. If command-constitutive: reinterpretation or substitute is logically possible; this reading forecloses study_as_performance but not other readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_binding_mechanism, conceptual, 'Whether obligation is constituted by action or by obedience to command').

omega_variable(
    study_substitution_legitimacy,
    'Can study be a legitimate substitute for performance, or does performance-only doctrine require actual ritual action as irreplaceable?',
    'Hermeneutic analysis: do source texts explicitly permit study as substitute (in which case performance_only is a strict reading), or do they require performance (in which case study_as_performance reading is ungrounded)?',
    'If study can be legitimate substitute: the performance_only reading is contestable; other readings remain live. If study cannot substitute: performance_only reading is the strict law; study_as_performance forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_substitution_legitimacy, conceptual, 'Whether study can substitute for actual sacrifice performance').

omega_variable(
    current_generation_victim_status,
    'Are current obligants victims of an impossible law, or are they bearing a legitimate eternal obligation despite structural impossibility?',
    'Community discourse analysis: how do obligants describe their experience? Language of guilt/relief/restoration? Do alternative readings that reduce victim status (messianic_suspension, archival_preservation) gain traction in communities?',
    'If truly victims: snare classification holds; extractiveness remains high; reform pressure increases. If bearing legitimate eternal obligation: snare classification softens; extractiveness might recalibrate as ''cost of tradition''; community cohesion effect (rope element) could rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(current_generation_victim_status, empirical, 'Whether current obligants experience impossible obligation as victimization').

omega_variable(
    institutional_benefit_from_unsatisfiability,
    'Does the institutional priesthood benefit from the obligation being structurally unsatisfiable, or would they benefit equally from study-as-substitute?',
    'Historical institutional behavior: when reform movements propose study as fulfillment, what is the institutional resistance? Is it doctrinal (performance truly required) or institutional (study-as-substitute would reduce institutional authority)? Do institutions resist other readings with equal force?',
    'If institutional benefit is real: performance_only is partly an extraction mechanism benefiting the institution; beneficiary classification holds. If institutional resistance is purely doctrinal: performance_only may be the logically correct reading, not an institutional preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_benefit_from_unsatisfiability, empirical, 'Whether institutional priesthood benefits from unsatisfiability of obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_only, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement(perf_tr_t500, performance_only, theater_ratio, 500, 0.48).
narrative_ontology:measurement(perf_tr_t1000, performance_only, theater_ratio, 1000, 0.58).
narrative_ontology:measurement(perf_tr_t1500, performance_only, theater_ratio, 1500, 0.55).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_only, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(perf_be_t500, performance_only, base_extractiveness, 500, 0.72).
narrative_ontology:measurement(perf_be_t1000, performance_only, base_extractiveness, 1000, 0.78).
narrative_ontology:measurement(perf_be_t1500, performance_only, base_extractiveness, 1500, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_only, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(perf_su_t500, performance_only, suppression_requirement, 500, 0.68).
narrative_ontology:measurement(perf_su_t1000, performance_only, suppression_requirement, 1000, 0.67).
narrative_ontology:measurement(perf_su_t1500, performance_only, suppression_requirement, 1500, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_only, enforcement_mechanism).
narrative_ontology:affects_constraint(performance_only, study_as_performance).
narrative_ontology:affects_constraint(performance_only, messianic_suspension).
narrative_ontology:affects_constraint(performance_only, archival_preservation).

% DUAL FORMULATION NOTE:
% The sacrifice obligation constraint family decomposes into four distinct readings of the same kernel (sacrifice_obligation_continuity), each with different epsilon values and beneficiary structures. Performance_only treats the obligation as eternally binding and unsatisfiable (epsilon=0.75, snare). Study_as_performance treats study as legitimate fulfillment (lower epsilon, mixed rope/tangled_rope). Messianic_suspension treats the obligation as provisionally suspended (moderate epsilon, scaffold with sunset). Archival_preservation treats the obligation as transformed into textual preservation (moderate-low epsilon, rope). Each reading is a complete constraint story; together they form the obligation's interpretive field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
