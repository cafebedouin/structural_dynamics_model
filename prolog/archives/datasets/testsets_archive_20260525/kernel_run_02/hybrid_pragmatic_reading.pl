% ============================================================================
% CONSTRAINT STORY: hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_pragmatic_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_pragmatic_reading
 *   human_readable: Hybrid Pragmatic Reading: Prophetic Authority as Crisis Management
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The hybrid pragmatic reading interprets the manifesto as a deliberate
 *   institutional strategy deployed by religious leadership to manage
 *   exogenous crisis (state pressure, doctrinal challenge, social transition)
 *   while preserving core theological commitments through strategic scope
 *   ambiguity. Leadership benefits from the manifesto's dual messaging: it
 *   demonstrates institutional responsiveness to external pressure while
 *   maintaining doctrinal flexibility that prevents any faction (hardline or
 *   progressive) from capturing the interpretive apparatus entirely.
 *   Rank-and-file members bear the cost: they inhabit the legitimacy
 *   ambiguity created by scope flexibility, unable to resolve the tension
 *   between competing doctrinal interpretations. Mid-level clergy experience
 *   the constraint as genuine coordination — the manifesto enables them to
 *   navigate dual pressure without institutional fracture — while also
 *   constraining their interpretive freedom. Organized reform and hardline
 *   coalitions vie for control of the manifesto's boundaries, each benefiting
 *   from institutional survival but constrained by the other's presence. The
 *   constraint exhibits tangled-rope characteristics: there is real
 *   coordination function (institutional stability preservation), real
 *   extraction (leadership benefit from ambiguity, member confusion from
 *   legitimacy uncertainty), active enforcement (leadership gatekeeping of
 *   interpretation), and asymmetric structure (leadership gains flexibility,
 *   members lose interpretive certainty).
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — deploys prophetic authority as crisis management tool; preserves both compliance responsiveness and doctrinal flexibility through scope ambiguity
 *   - Rank-and-File Members: Primary victim (powerless/identity_locked) — identity fused with institution; absorb full cost of legitimacy ambiguity without interpretive control
 *   - Mid-Level Clergy: Secondary actor (moderate/constrained) — constrained by career dependency; benefit from legitimacy preservation that enables continued ministry; genuine coordination function
 *   - Progressive Reform Coalition: Organized actor (organized/constrained) — benefit from expanded interpretive scope but constrained by institutional gatekeeping; compete with hardline coalition
 *   - Doctrinal Hardline Coalition: Organized actor (organized/constrained) — constrained by leadership deviation from doctrinal gate-keeping; benefit from institutional legitimacy; compete with progressive coalition
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional strategy as inevitable theological dynamic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_pragmatic_reading, 0.48).
domain_priors:suppression_score(hybrid_pragmatic_reading, 0.52).
domain_priors:theater_ratio(hybrid_pragmatic_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_pragmatic_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hybrid_pragmatic_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(hybrid_pragmatic_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_pragmatic_reading, "Hybrid Pragmatic Reading: Prophetic Authority as Crisis Management").
narrative_ontology:topic_domain(hybrid_pragmatic_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(hybrid_pragmatic_reading, formalized).
narrative_ontology:cs_authority_grounding(hybrid_pragmatic_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(hybrid_pragmatic_reading).
narrative_ontology:cs_kernel_id(hybrid_pragmatic_reading, marriage_commitment_legitimacy).
narrative_ontology:cs_reading_relation(hybrid_pragmatic_reading, exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation(hybrid_pragmatic_reading, endogenous_reinterpretation_reading, influences).
narrative_ontology:cs_axiom(hybrid_pragmatic_reading, foundational, scope_ambiguity_preserves_institutional_survival).
narrative_ontology:cs_axiom_status(scope_ambiguity_preserves_institutional_survival, holdable).
narrative_ontology:cs_axiom_grounding(hybrid_pragmatic_reading, scope_ambiguity_preserves_institutional_survival, instrumental).
narrative_ontology:cs_axiom(hybrid_pragmatic_reading, foundational, prophetic_authority_deployable_as_crisis_management).
narrative_ontology:cs_axiom_status(prophetic_authority_deployable_as_crisis_management, holdable).
narrative_ontology:cs_axiom_grounding(hybrid_pragmatic_reading, prophetic_authority_deployable_as_crisis_management, conventional).
narrative_ontology:cs_reference_frame(hybrid_pragmatic_reading, dual_commitment_doctrine).
narrative_ontology:cs_drift_state(hybrid_pragmatic_reading, exogenous_crisis_moment, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(hybrid_pragmatic_reading, rank_and_file_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RANK-AND-FILE MEMBER (SNARE) — Identity fused with institutional membership; cannot exit without dissolving their sense of self and community. Bears full cost of interpretive uncertainty: the manifesto's scope ambiguity creates legitimacy instability without clear resolution pathway. Experiences maximum extraction through identity lock.
constraint_indexing:constraint_classification(hybrid_pragmatic_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-LEVEL CLERGY (TANGLED ROPE) — Constrained by career dependency on institutional hierarchy; also benefits from legitimacy preservation that allows continued ministry. Genuine coordination function: the manifesto's dual messaging enables them to navigate both institutional loyalty and doctrinal coherence. Significant extraction alongside real benefit — the constraint both enables their role and limits their interpretive freedom.
constraint_indexing:constraint_classification(hybrid_pragmatic_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Beneficiary experiencing this as pure coordination. Leadership uses the manifesto as pragmatic crisis management tool: deploys prophetic authority to address exogenous pressure while scope ambiguity preserves doctrinal flexibility. The constraint solves the coordination problem of managing compliance pressure and internal coherence simultaneously. Net beneficiary — extraction flows toward institutional leadership.
constraint_indexing:constraint_classification(hybrid_pragmatic_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE REFORM COALITION (TANGLED ROPE) — Organized agents seeking doctrinal evolution view the manifesto as partial victory: it opens interpretive space for reform while maintaining institutional stability. Benefits from expanded theological scope but constrained by institutional gatekeeping. Active enforcement required to prevent hardline coalition from recapturing doctrinal boundaries. Mixed coordination and extraction — the reform coalition both benefits and is constrained.
constraint_indexing:constraint_classification(hybrid_pragmatic_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DOCTRINAL HARDLINE COALITION (TANGLED ROPE) — Organized agents committed to doctrinal preservation view the manifesto as threat: scope ambiguity undermines their gatekeeping function. Constrained by institutional authority that adopted the manifesto; also benefit from institutional legitimacy that continues. Active enforcement required to prevent progressive capture. Symmetric mixed structure with the progressive coalition — both benefit from institutional survival, both constrained by competing interpretations.
constraint_indexing:constraint_classification(hybrid_pragmatic_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THEOLOGICAL INEVITABILITY (MOUNTAIN) — From civilizational/universal perspective, the manifesto appears as an inevitable institutional response to exogenous pressure: religious institutions always face the inherent tension between doctrinal coherence and environmental adaptation. Scope ambiguity is an immutable feature of how prophetic authority navigates legitimacy crises. However, the structural data contradicts this mountain classification — the engine's false summit detector will reveal that the 'inevitable theological tension' framing naturalizes what is actually a contingent institutional strategy.
constraint_indexing:constraint_classification(hybrid_pragmatic_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_pragmatic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_pragmatic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_pragmatic_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Leadership benefits materially from scope ambiguity: they gain institutional flexibility, escape doctrinal pressure, and maintain authority to interpret boundaries. Members bear costs: legitimacy uncertainty, interpretive confusion, identity instability. The extractiveness is not maximal (snare-level 0.66+) because the manifesto does deliver genuine institutional stability that benefits all members. The rising trajectory (0.32→0.51 over interval) reflects increasing recognition among members that the ambiguity serves leadership interests more than their own. Suppression (0.52): Moderate-high. Barriers to exit include identity lock (non-negotiable for members), institutional inertia, and theological socialization. But suppression is not total — some members do leave, and organized coalitions maintain interpretive presence. Theater ratio (0.58): Moderate. The manifesto combines genuine crisis response with performative legitimation. Leadership's rhetoric about responding to exogenous pressure is partially sincere (the crisis is real) and partially strategic (the crisis frame enables power consolidation). The rising trajectory (0.42→0.65) reflects increasing recognition that the manifesto's crisis management function declines over time while its legitimacy-maintenance theater increases — the original adaptive function atrophies while the performance persists (piton drift candidate).
 *
 * PERSPECTIVAL GAP:
 *   The hybrid pragmatic reading reveals fundamental disagreement about whether the manifesto is genuinely adaptive (rope: pure coordination response) or strategically extractive (snare: pure power consolidation). Institutional leadership experiences it as rope — solving a real coordination problem. Rank-and-file members experience it as snare — bearing costs of ambiguity with no exit. Organized coalitions experience tangled rope — benefiting from institutional survival but constrained by competing faction. The analytical observer risks the mountain view (inevitable theological necessity) which the structural data contradicts. The perspectival gap is not merely different experiences of the same constraint — it reflects genuine disagreement about the manifesto's constitutive function: Is it crisis response (adaptive) or power consolidation (extractive)?
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership occupies the beneficiary position (arbitrage exit: they can reinterpret, delegate authority, or shift crisis framing without material cost). The derived d-value is low (0.15-0.25 range), producing negative chi — this agent experiences the constraint as enabling, not extracting. Rank-and-file members occupy the victim position (identity_locked exit: they cannot resolve the legitimacy ambiguity without dissolving their identity). The derived d-value is high (0.85-0.95 range), producing maximum chi — this agent experiences high extraction. Mid-level clergy and organized coalitions occupy intermediate positions: constrained exit (institutional career dependency) with mixed beneficiary/victim status (benefit from survival, constrained by others' presence). Their d-values are moderate (0.45-0.65 range), producing moderate chi consistent with tangled rope. The analytical observer (analytical/analytical context) has d≈0.72, producing chi that reveals false-summit risk in the mountain perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_ambiguity_deliberateness,
    'Is the manifesto''s scope ambiguity a deliberate institutional strategy for preserving flexibility, or a genuine theological unresolved tension?',
    'Archival analysis of drafting records, preparatory documents, and leadership correspondence; comparison with institutional response to previous crises; assessment of whether hardline and progressive interpretations were explicitly anticipated vs emerged as surprise disagreements',
    'If deliberate strategy: classification as tangled_rope confirmed — institutional leadership actively manages extraction through ambiguity maintenance. If genuine tension: classification shifts toward scaffold (temporary state) or piton (degraded theology) depending on whether resolution is achievable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_ambiguity_deliberateness, empirical, 'Whether scope ambiguity is institutional strategy or unresolved theological tension').

omega_variable(
    membership_exit_cost_distribution,
    'Are the barriers to exit (identity lock, community dissolution, spiritual loss) equally distributed across member types, or do certain demographics bear disproportionate cost?',
    'Longitudinal membership tracking by demographic; analysis of departure rates correlating with theological commitment level; qualitative interview data on exit costs perceived by different member cohorts',
    'If equally distributed: snare classification applies uniformly to all powerless agents. If distributed unevenly: certain subgroups (e.g., women, minorities, marginalized theologies) experience higher identity lock and should be separately analyzed as victims of extraction concentrated by power asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_exit_cost_distribution, empirical, 'Distribution of exit costs across member demographics').

omega_variable(
    exogenous_crisis_necessity,
    'Did the manifesto represent a genuine exogenous crisis response, or was the crisis framing deployed strategically to justify internal power consolidation?',
    'Comparative institutional analysis: comparison with peer institutions facing similar exogenous pressures (did they respond similarly?); timeline analysis of when crisis was declared vs when manifesto was drafted; structural analysis of who benefited from crisis narrative framing',
    'If genuine crisis: extractiveness derives from legitimate institutional adaptation with some asymmetry. If strategic framing: extractiveness reflects calculated leadership strategy, and the ''pragmatic'' reading becomes explicit justification for power consolidation — classification shifts to higher extractiveness (0.55+) and higher suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exogenous_crisis_necessity, empirical, 'Whether exogenous crisis was genuine or strategically framed').

omega_variable(
    prophetic_authority_erosion,
    'As the manifesto ages and scope ambiguity persists unresolved, does prophetic authority degrade into performative legitimation (piton drift)?',
    'Longitudinal measurement of member engagement with manifesto rhetoric; analysis of whether manifesto references increase in frequency while theological grounding diminishes; assessment of whether leadership invokes manifesto as solution vs as institutional theater',
    'If degradation occurs: the constraint transitions from tangled_rope (active extraction with genuine coordination) to piton (theater-maintained extraction as original function atrophies). Theater ratio tracking enables this detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_authority_erosion, empirical, 'Whether prophetic authority degrades into performative theater over time').

omega_variable(
    reading_specification_underdetermination,
    'Is this ''hybrid pragmatic reading'' a genuine distinct interpretation of the kernel, or a post-hoc rationalization of what is actually institutional opportunism?',
    'Textual analysis comparing the manifesto''s internal arguments to the hybrid pragmatic reading''s logical structure; assessment of whether the reading was articulated contemporaneously or constructed retrospectively; examination of whether institutional actors explicitly adopted this reading as their interpretive framework',
    'If genuine reading: the committer frame is justified. If post-hoc rationalization: the constraint story is reconstructing institutional motives in service of legitimating the authority structure, and should include omega documenting this interpretive framing as itself extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_specification_underdetermination, conceptual, 'Whether hybrid pragmatic reading represents genuine institutional interpretation or post-hoc rationalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_pragmatic_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybr_tr_t0, hybrid_pragmatic_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hybr_tr_t3, hybrid_pragmatic_reading, theater_ratio, 3, 0.5).
narrative_ontology:measurement(hybr_tr_t6, hybrid_pragmatic_reading, theater_ratio, 6, 0.58).
narrative_ontology:measurement(hybr_tr_t9, hybrid_pragmatic_reading, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(hybr_be_t0, hybrid_pragmatic_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hybr_be_t3, hybrid_pragmatic_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(hybr_be_t6, hybrid_pragmatic_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(hybr_be_t9, hybrid_pragmatic_reading, base_extractiveness, 9, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_pragmatic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hybrid_pragmatic_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(hybrid_pragmatic_reading, endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-reading kernel cluster on marriage_commitment_legitimacy. Each reading has its own ε value reflecting different structural interpretations of the manifesto's function: hybrid pragmatic (ε=0.48, extraction-as-strategy), exogenous override (ε=0.65, external pressure imposed), endogenous reinterpretation (ε=0.25, authentic doctrinal evolution). The three readings coexist in the institution as competing interpretive frameworks, not as sequential phases. They are linked via network.affects_constraints to enable contamination and coalition analysis across the kernel cluster.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_pragmatic_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
