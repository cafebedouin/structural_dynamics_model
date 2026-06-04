% ============================================================================
% CONSTRAINT STORY: seventh_amendment__reexamination_clause_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_seventh_amendment__reexamination_clause_reading, []).

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
 *   constraint_id: seventh_amendment__reexamination_clause_reading
 *   human_readable: Seventh Amendment Reexamination Clause Reading
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   The Seventh Amendment's second sentence—'no fact tried by jury shall be
 *   otherwise re-examined in any Court of the United States, than according
 *   to the rules of the Common law'—instantiates a structural constraint on
 *   appellate authority over jury verdicts. This reading interprets the
 *   clause as a straightforward suppression mechanism: once a jury has
 *   decided facts, appellate courts cannot reexamine those facts except
 *   through the narrow pathways established by common-law doctrine as of
 *   1791. The constraint protects jury verdicts from appellate
 *   second-guessing, stabilizing the jury system as a coordinate equal
 *   authority alongside courts. However, the reading is contested within the
 *   broader 'seventh amendment' kernel. The complexity-exception reading
 *   suggests that modern litigation may be too intricate for lay juries,
 *   creating pressure to narrow the jury right itself. The historical-test
 *   reading anchors jury eligibility to 1791 categories of suit, which
 *   independently gates whether the reexamination clause applies at all. This
 *   reading—the reexamination-clause reading—focuses on the suppression
 *   mechanism itself: what counts as 'reexamination' and what 'common-law
 *   rules' permit.
 *
 * KEY AGENTS:
 *   - Trial Jury and Verdict Finality: Primary beneficiary (institutional/arbitrage) — gains constitutional armor against appellate second-guessing; enables jury function to persist
 *   - Appellate Reviewing Court: Secondary beneficiary and constrained actor (institutional/constrained) — receives appellate docket reduction and stable jury verdicts, but loses factual reach
 *   - Losing Litigant with Factual Grievance: Primary victim (moderate/constrained) — locked into jury verdict on facts; suppressed from appellate fact-reexamination
 *   - Appellate Reach as Institutional Capacity: Abstracted victim (analytical/analytical) — appellate courts' general capacity to police trial error through fact-reexamination is suppressed by the Amendment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(seventh_amendment__reexamination_clause_reading, 0.38).
domain_priors:suppression_score(seventh_amendment__reexamination_clause_reading, 0.52).
domain_priors:theater_ratio(seventh_amendment__reexamination_clause_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(seventh_amendment__reexamination_clause_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(seventh_amendment__reexamination_clause_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(seventh_amendment__reexamination_clause_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(seventh_amendment__reexamination_clause_reading, tangled_rope).
narrative_ontology:human_readable(seventh_amendment__reexamination_clause_reading, "Seventh Amendment Reexamination Clause Reading").
narrative_ontology:topic_domain(seventh_amendment__reexamination_clause_reading, "legal/constitutional_doctrine").

domain_priors:requires_active_enforcement(seventh_amendment__reexamination_clause_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(seventh_amendment__reexamination_clause_reading, 'aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997').
narrative_ontology:cs_kernel_codification('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997', fixed_text).
narrative_ontology:cs_authority_grounding('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997', lineage).
narrative_ontology:cs_interpretation_layer_present('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997').
narrative_ontology:cs_reading_relation('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997', seventh_amendment__complexity_exception_question, influences).
narrative_ontology:cs_reading_relation('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997', seventh_amendment__historical_test_reading, coexists_with).
narrative_ontology:cs_axiom('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997', foundational, jury_verdict_finality_inviolable).
narrative_ontology:cs_axiom_status(jury_verdict_finality_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997', jury_verdict_finality_inviolable, conventional).
narrative_ontology:cs_axiom('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997', foundational, common_law_rules_bind_reexamination_pathways).
narrative_ontology:cs_axiom_status(common_law_rules_bind_reexamination_pathways, holdable).
narrative_ontology:cs_axiom_grounding('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997', common_law_rules_bind_reexamination_pathways, conventional).
narrative_ontology:cs_reference_frame('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997', jury_factfinding_supremacy_1791).
narrative_ontology:cs_drift_state('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997', contemporary_appellate_review_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aec9c9d3-a5d4-4ca2-b93b-4218ee9f8997', '').
narrative_ontology:cs_kernel_id(seventh_amendment__reexamination_clause_reading, seventh_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(seventh_amendment__reexamination_clause_reading, verdict_finality).
narrative_ontology:constraint_beneficiary(seventh_amendment__reexamination_clause_reading, trial_juries).
narrative_ontology:constraint_victim(seventh_amendment__reexamination_clause_reading, reviewing_court_appellate_reach).
narrative_ontology:constraint_victim(seventh_amendment__reexamination_clause_reading, litigants_with_fact_disputes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REVIEWING COURT (SNARE) — Appellate judges cannot reexamine facts even when error appears clear. The verdict's factual findings are locked in place; appellate reach is suppressed by the Amendment's text. The court bears the cost of this constraint: limited remedial power when facts seem wrongly found. No exit from this suppression except through extraordinary writs or constitutional amendment.
constraint_indexing:constraint_classification(seventh_amendment__reexamination_clause_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRIAL JURY / VERDICT FINALITY (ROPE) — The jury's verdict receives constitutional armor against appellate second-guessing. This is pure coordination: the constraint protects the jury function from being subordinated to appellate review. Juries benefit from verdict finality; the coordination function is genuine — appellate deference enables the jury system to function as designed. This is the constraint as beneficiary sees it.
constraint_indexing:constraint_classification(seventh_amendment__reexamination_clause_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: LOSING LITIGANT WITH FACTUAL GRIEVANCE (TANGLED ROPE) — The losing party is locked into the trial verdict on facts; they cannot pursue appellate fact-reexamination except through narrow common-law pathways (manifest error on the face of the record). The constraint serves the coordination function of protecting jury verdicts but simultaneously extracts from litigants by suppressing their appellate remedies. Mixed extraction and coordination.
constraint_indexing:constraint_classification(seventh_amendment__reexamination_clause_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: APPELLATE JUDICIARY (TANGLED ROPE) — Appellate courts are bound by the Amendment's constraint but also benefit from it: verdict finality reduces appellate docket pressure and creates a stable, predictable appellate function (review law, not facts). The constraint serves coordination (stable appellate review) while extracting from appellate reach. Appellate judges experience mixed constraint and benefit.
constraint_indexing:constraint_classification(seventh_amendment__reexamination_clause_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / HISTORICAL COMMON-LAW (ROPE) — From the analytical civilizational view, this constraint is pure coordination: it replicates common-law rules as they stood in 1791, when juries decided facts and appellate review was absent or minimal. The constraint coordinates the modern system to a historical baseline. No extraction is expected because the beneficiary (jury system) and the coordinated mechanism are aligned.
constraint_indexing:constraint_classification(seventh_amendment__reexamination_clause_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(seventh_amendment__reexamination_clause_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(seventh_amendment__reexamination_clause_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(seventh_amendment__reexamination_clause_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(seventh_amendment__reexamination_clause_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from losing litigants and reviewing courts by suppressing appellate fact-reexamination. However, the extraction is not severe because common-law doctrines (manifest error, clear error, abuse of discretion) provide alternative appellate remedies. The 'except by common law rules' language creates an outlet for appellate reach, preventing total suppression. The measurement trajectory shows extractiveness rising from 0.22 (early 19th century, when appellate review was sparse) to 0.38 (modern era, where appellate review mechanisms have proliferated). Suppression (0.52): Moderate-high. The Amendment directly suppresses appellate fact-reexamination. However, the suppression is not total: reviewing courts can use narrow common-law pathways. The measurement trajectory shows suppression rising from 0.45 to 0.52 as appellate capacity has grown and the constraint's suppressive force against that capacity has become more pronounced. Theater ratio (0.48): Low-moderate. This reading's performative content is relatively low because the reexamination clause text is explicit and the enforcement mechanism (verdicts lock facts) is straightforward. There is some theater in the 'common-law rules' exception—courts sometimes stretch what counts as 'common law' to permit appellate reach—but the core constraint is functionally enforced.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (jury system, verdict finality) sees this constraint as pure coordination (Rope): it protects the jury from being subordinated to appellate review, enabling the jury to function. The reviewing court sees it as mixed coordination and extraction (Tangled Rope): the constraint stabilizes the jury verdict (coordination benefit) but suppresses appellate reach (extraction cost). The losing litigant sees it as pure extraction (Snare): they are locked into an unfavorable verdict with no appellate remedy for facts. The analytical observer sees it as historical coordination (Rope): the constraint replicates common-law rules as they stood in 1791, coordinating the modern system to a historical baseline. However, the analytical observer also risks a false summit (seeing the constraint as a natural law of appellate jurisdiction) when the structural data reveals it as a contingent constitutional choice about which authority—jury or appellate court—decides facts.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality varies by the agent's structural relationship to verdict finality and appellate reach. The jury (beneficiary) has arbitrage exit—it can appeal to appellate protection to maintain its authority, and appellate deference creates a safe niche. The reviewing court (beneficiary-constrained) has constrained exit—it benefits from verdict finality reducing docket pressure but loses factual authority. The losing litigant (victim) is trapped—they cannot reexamine facts at appellate level and have no exit except through extraordinary writs. Each agent's d value (derived from beneficiary/victim + exit options) feeds into the sigmoid f(d) to produce the experienced extractiveness chi. The beneficiary's low d produces negative chi (coordination experienced as benefit); the victim's high d produces high chi (extraction experienced as suppression).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_exception_latency,
    'Does the Seventh Amendment permit a complexity exception—cases too intricate for lay jurors—and if so, has this exception been recognized as valid constitutional law or remains it an argued-but-unblessed practice?',
    'Historical analysis of complexity exception doctrine: (a) has Supreme Court endorsed it as constitutional; (b) what evidence exists of its quiet practice through summary judgment; (c) does it functionally override the reexamination clause for complex cases',
    'If exception is recognized: extractiveness rises (suppression of jury fact-finding via complexity gate); the reexamination clause is functionally narrowed. If exception is foreclosed: reexamination clause stands as written with full suppression of appellate fact-reexamination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(complexity_exception_latency, empirical, 'Recognition status and functional practice of complexity exception to jury right').

omega_variable(
    common_law_rules_scope_ambiguity,
    'What counts as ''common-law rules'' under which facts may be reexamined? Does this phrase limit appellate review to 1791-era rules of fact reexamination, or does it permit evolution of common-law doctrines post-1791?',
    'Doctrinal analysis: compare scope of fact-reexamination practice under Restatement (Second) of Judgments vs. what was permitted in 1791; identify cases where modern developments (abuse-of-discretion review, manifest error standards) are claimed as evolutionary common-law rules',
    'If ''common-law rules'' means fixed-as-of-1791: reexamination clause becomes more restrictive (higher suppression). If evolutionary interpretation permitted: reexamination clause becomes more flexible (lower suppression, higher extractiveness of appellate reach).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_law_rules_scope_ambiguity, conceptual, 'Temporal scope of ''common-law rules'' permitting fact reexamination').

omega_variable(
    historical_test_interaction,
    'Does the historical test reading (measuring modern civil claims against 1791 suit-categories to determine jury eligibility) functionally constrain the reexamination clause reading, or do these operate independently?',
    'Doctrinal mapping: identify cases where historical test gates jury eligibility and where reexamination clause gates appellate review; determine if a claim excluded from jury trial via historical test is then subject to appellate fact-review (escaping reexamination suppression)',
    'If readings interact: reexamination clause applies only to jury-tried facts; historical test determines whether facts were jury-tried; combined effect is more restrictive than reexamination clause alone. If independent: reexamination clause applies to all jury verdicts regardless of historical test outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_test_interaction, empirical, 'Interaction between historical test for jury eligibility and reexamination clause suppression').

omega_variable(
    appellate_reach_degradation,
    'Has appellate capacity to police trial errors (through abuse-of-discretion review, manifest error, clear error standards) actually declined relative to pre-reexamination-clause enforcement practices, or do these doctrines merely shift the mechanism without reducing appellate reach?',
    'Empirical analysis: reversal rates on factual grounds; range of appellate remedies available (new trial, remand, settlement pressure); comparison to hypothetical appellate capacity if reexamination clause did not suppress direct fact-reexamination',
    'If reach has declined: suppression value (0.52) is accurate and extractiveness reflects real appellate limitation. If reach has not declined (shifted mechanisms achieve same oversight): suppression and extractiveness may be overstated; constraint may be more rope-like (pure coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_reach_degradation, empirical, 'Whether appellate remedial capacity has actually declined under reexamination suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(seventh_amendment__reexamination_clause_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(seventh_reexam_be_t0, seventh_amendment__reexamination_clause_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(seventh_reexam_be_t40, seventh_amendment__reexamination_clause_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(seventh_reexam_be_t80, seventh_amendment__reexamination_clause_reading, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seventh_reexam_su_t0, seventh_amendment__reexamination_clause_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(seventh_reexam_su_t40, seventh_amendment__reexamination_clause_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(seventh_reexam_su_t80, seventh_amendment__reexamination_clause_reading, suppression_requirement, 80, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(seventh_amendment__reexamination_clause_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(seventh_amendment__reexamination_clause_reading, seventh_amendment__historical_test_reading).
narrative_ontology:affects_constraint(seventh_amendment__reexamination_clause_reading, seventh_amendment__complexity_exception_question).

% DUAL FORMULATION NOTE:
% The reexamination-clause reading is one structural constraint within the broader seventh-amendment kernel contest. Its sibling readings (historical test, complexity exception) operate on different structural variables: historical test gates jury eligibility; complexity exception creates exceptions to jury reach; reexamination clause suppresses appellate fact-reexamination. Each constraint has its own ε, beneficiary/victim structure, and classification. They are linked because they operate on the same doctrinal text (Seventh Amendment) and their combined effect determines when jury verdicts lock facts and when appellate review can reach facts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(seventh_amendment__reexamination_clause_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
