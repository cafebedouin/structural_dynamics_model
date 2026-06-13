% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Printing Press as Autonomous Enabling Technology for Reformation
 *   domain: history/technology/religion
 *
 * SUMMARY:
 *   The printing press in the 16th-century Reformation is a touchstone case
 *   in technological-determinism historiography. The reading instantiated
 *   here claims that the press, as an autonomous physical technology, made
 *   vernacular scripture diffusion and Reformation success inevitable.
 *   Alternative readings — strategic_deployment (reformers and printers
 *   deliberately wielded printing as a weapon) and co_constitution
 *   (technology and human agency recursively influenced one another) —
 *   describe the same historical events differently, locate causality
 *   differently, and produce different classifications. This story
 *   instantiates ONLY the technological-determinism reading; it makes no
 *   claim about whether that reading is true. The authored metrics (low
 *   extractiveness, very high accessibility_collapse, near-zero resistance)
 *   describe what this reading asserts: that the press's effects were
 *   autonomous, not extractive, and that once understood, the press made
 *   alternatives to diffusion structurally impossible.
 *
 * KEY AGENTS:
 *   - Printing press technology: the claimed autonomous cause; framed as non-agent entity with no intentionality, merely a mechanism.
 *   - Theological reformers (Luther, Zwingli, Calvin): beneficiaries in this reading, but depicted as passive recipients of the press's effects rather than strategic actors.
 *   - Printing merchants and capital: beneficiaries whose profits from printing are attributed to technological inevitability, not strategic market positioning.
 *   - Ecclesiastical authorities: victims in the sense that their interpretive monopoly eroded, but the erosion is framed as inevitable technological pressure rather than strategic displacement.
 *   - Literate vernacular publics: beneficiaries whose access to texts is attributed to the press's operation, not to deliberate choices by reformers or printers about distribution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.22).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.08).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.22).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Autonomous Enabling Technology for Reformation").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history/technology/religion").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, '37e4c983-1c7e-4faa-9dfc-ae6b157c9c94').
narrative_ontology:cs_kernel_codification('37e4c983-1c7e-4faa-9dfc-ae6b157c9c94', distributed).
narrative_ontology:cs_authority_grounding('37e4c983-1c7e-4faa-9dfc-ae6b157c9c94', distributed).
narrative_ontology:cs_reading_relation('37e4c983-1c7e-4faa-9dfc-ae6b157c9c94', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('37e4c983-1c7e-4faa-9dfc-ae6b157c9c94', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_axiom('37e4c983-1c7e-4faa-9dfc-ae6b157c9c94', foundational, technology_autonomous_causation).
narrative_ontology:cs_axiom_status(technology_autonomous_causation, holdable).
narrative_ontology:cs_axiom_grounding('37e4c983-1c7e-4faa-9dfc-ae6b157c9c94', technology_autonomous_causation, empirically_contingent).
narrative_ontology:cs_axiom('37e4c983-1c7e-4faa-9dfc-ae6b157c9c94', secondary, human_agents_passive_responders).
narrative_ontology:cs_axiom_status(human_agents_passive_responders, holdable).
narrative_ontology:cs_axiom_grounding('37e4c983-1c7e-4faa-9dfc-ae6b157c9c94', human_agents_passive_responders, conventional).
narrative_ontology:cs_reference_frame('37e4c983-1c7e-4faa-9dfc-ae6b157c9c94', technological_autonomous_causation).
narrative_ontology:cs_drift_state('37e4c983-1c7e-4faa-9dfc-ae6b157c9c94', contemporary_historiography, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('37e4c983-1c7e-4faa-9dfc-ae6b157c9c94', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, technology_determinism_historiography).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the technological-determinism reading claims the press's effects were not extractive — it solved a coordination problem (replicating texts cost-effectively) that benefited multiple parties without concentrating gains in one actor's hands. Suppression is near-zero (0.08) because the reading posits no active suppressive mechanism: the press simply made alternatives to vernacular diffusion structurally impossible through cost economics, not coercion. Accessibility_collapse is very high (0.91) because the reading claims that once the press's capability is understood, alternatives are foreclosed: no other mechanism can replicate texts as efficiently. Resistance is near-zero (0.04) because the reading treats resistance as irrational — why resist an autonomous, inevitable technology? The measurement series track the narrative's establishment: extractiveness and theater both rise gradually as the technological-determinism framing becomes historiographically dominant (1460–1550), while the underlying extractiveness claim remains constant.
 *
 * PERSPECTIVAL GAP:
 *   The technological-determinism reading permits no perspectival divergence because it denies agency to all seats except as passive respondents. The press has no perspective (it is not an agent). Reformers, printers, and ecclesiastical authorities all experience the same autonomous technological pressure; their divergent interests (theological, economic, institutional) are secondary effects, not primary causes. The reading explicitly erases the gap between seats that alternative readings (strategic_deployment, co_constitution) would highlight. This erasure is the reading's defining move: by making technology the sole causal force, it removes the question of who benefits from strategic choices.
 *
 * DIRECTIONALITY LOGIC:
 *   The technological-determinism reading struggles with directionality because it denies the structural relationships that generate directionality. In orthodox DR framework, directionality derives from beneficiary/victim declarations and exit options. Here, beneficiary status is attributed to multiple parties, but the reading claims their benefits flow from the technology, not from structured human arrangements. The declared beneficiaries (reformers, printers, literate publics) experience near-zero extraction (ε=0.22) and very high accessibility_collapse (0.91), which should produce d values across the board toward the beneficiary end (d near 0.0). Ecclesiastical authorities are nominally victims but experience no active suppression — the reading claims their loss is passive erosion. The reading's directionality logic is: everyone is a passive responder to autonomous technology; beneficiaries are those whose interests align with the press's effects; victims are those whose interests conflict; no party is an active extractor.
 *
 * MANDATROPHY ANALYSIS:
 *   The technological-determinism reading presents a mandate (the press determines Reformation diffusion) and asserts that mandate remains live and operative (founding_problem_status=contested, not dead). However, the constraint's classification as mountain is contestable precisely because alternative readings locate active human strategic choice at every juncture — the mandate that 'technology determines' is disputed by the strategic_deployment and co_constitution readings. The mandatrophy risk is that the constraint becomes theatrical performance of technological inevitability to justify historical outcomes retroactively. The high theater_ratio (0.12 by 1520, sustained thereafter) and the visible beneficiary structure (reformers who profited from printing, printers who profited from religious controversy, ecclesiastical authorities who lost interpretive monopoly) suggest the technological-determinism framing may mask strategic human action. If the constraint's mandate (technological autonomy) has become functionally dead — replaced by recognition of human strategic choice — while the theater of technological inevitability persists, the constraint is a candidate piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_determinism_historiography,
    'Is the technological-determinism reading an accurate characterization of historical causality, or is it a historiographical narrative that benefits certain interpretive traditions and institutions?',
    'Examine the evidence for explicit strategic choice in reformers'' writings (Luther''s letters about printer selection, timing of publication, distribution networks) and in printers'' economic decisions (choice of which texts to print, investment in particular markets, pricing strategy). If such evidence is abundant and central to understanding Reformation diffusion, the reading is a narrative overlay, not an accurate causal claim.',
    'If the reading is a historiographical narrative serving institutional ends (the history of technology as a discipline, the legitimacy of technology-focused explanations), then the constraint is not a mountain but a tangled_rope or snare — a framework that benefits certain scholars while extracting from alternative interpretations. This would require reclassification and re-authoring of the beneficiary/victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_determinism_historiography, empirical, 'Whether the technological-determinism reading is a causal claim or a historiographical narrative.').

omega_variable(
    false_summit_determinism,
    'Does the mountain classification (emerges_naturally=true) misrepresent the printing press''s role as a natural, inevitable fact when it is actually a contingent human choice?',
    'Examine whether the same historical outcomes (Reformation diffusion at the observed scale and speed) could have occurred with different technology (alternative printing methods, manuscript networks, secret circulation) or without printing at all. If outcomes are contingent on human choices about technology adoption, the constraint is constructed, not natural.',
    'A false summit: the technological-determinism reading declares the press a natural constraint (mountain) to justify historical outcomes as inevitable, thereby obscuring the strategic human choices that made printing dominant. This would trigger FSM detection (false_summit_mountain signature) and reclassification to tangled_rope, with beneficiaries (scholars whose careers depend on technological-determinism framing; reformers and printers who benefit retroactively from framing their choices as inevitable) made visible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_determinism, empirical, 'Whether the printing press is a natural, inevitable fact or a contingent technology whose dominance depended on human strategic choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1440, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1440, press_reformation_causality__technological_determinism, theater_ratio, 1440, 0.0).
narrative_ontology:measurement(pres_tr_t1460, press_reformation_causality__technological_determinism, theater_ratio, 1460, 0.05).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causality__technological_determinism, theater_ratio, 1480, 0.08).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__technological_determinism, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(pres_tr_t1520, press_reformation_causality__technological_determinism, theater_ratio, 1520, 0.12).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__technological_determinism, theater_ratio, 1550, 0.12).

% Extraction over time
narrative_ontology:measurement(pres_be_t1440, press_reformation_causality__technological_determinism, base_extractiveness, 1440, 0.08).
narrative_ontology:measurement(pres_be_t1460, press_reformation_causality__technological_determinism, base_extractiveness, 1460, 0.12).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causality__technological_determinism, base_extractiveness, 1480, 0.16).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__technological_determinism, base_extractiveness, 1500, 0.18).
narrative_ontology:measurement(pres_be_t1520, press_reformation_causality__technological_determinism, base_extractiveness, 1520, 0.2).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__technological_determinism, base_extractiveness, 1550, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causality__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_determinism, global_infrastructure).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__technological_determinism, 0.05).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% The press_reformation_causality kernel decomposes into three structurally distinct constraint stories, each with a different ε value and type classification. The technological_determinism reading (this story) asserts autonomous technology making outcomes inevitable (mountain). The strategic_deployment reading asserts human actors deliberately weaponizing printing for theological and economic gain (tangled_rope or snare, higher ε). The co_constitution reading asserts recursive feedback between technology and human action, with neither autonomous (tangled_rope or rope, intermediate ε). These are not alternative measurements of one constraint — they are different constraints located at the same historical juncture. The ε-invariance principle (DP-001) governs the decomposition: if the measurement changes the observable's definition (technology-autonomous vs. human-strategic vs. co-evolved), you have two constraints, not one. The kernel is the historiographical contest over which constraint is the correct description of reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
