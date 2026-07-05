% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__limited_responsibility_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations Under the Capacity-to-Pay (Limited Responsibility) Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   Between 1919 and 1932, German negotiators, financial experts, and
 *   successive Reparations Commission committees (Dawes 1924, Young 1929)
 *   progressively substituted a capacity-to-pay framework for the original
 *   London Schedule's damage-assessment basis. Article 231 (the 'war guilt
 *   clause') was reframed in German and increasingly in Anglo-American
 *   financial circles as a legal peg establishing jurisdiction for claims,
 *   not a moral finding — a distinction that allowed schedules to be revised
 *   downward without appearing to repudiate treaty obligations outright. The
 *   practical effect was a real transfer of negotiating power to German
 *   industrial and financial elites, at the direct cost of French
 *   reconstruction financing, Belgian claimants, and ultimately Allied
 *   taxpayers who absorbed the resulting war-debt shortfall.
 *
 * KEY AGENTS:
 *   - german_negotiating_delegation: sets and defends the capacity ceiling at each renegotiation (institutional/constrained)
 *   - german_industrial_elites: primary beneficiaries of downward-revised schedules (organized/mobile)
 *   - french_reconstruction_authorities: bear the direct shortfall in reconstruction funding (institutional/constrained)
 *   - belgian_occupied_territories: powerless claimants with no seat at the table (powerless/trapped)
 *   - allied_war_bond_holders: absorb the war-debt shortfall indirectly (powerless/trapped)
 *   - reparations_commission: adjudicates competing capacity claims (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.42).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.38).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations Under the Capacity-to-Pay (Limited Responsibility) Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '853e4029-c3b8-4713-ba27-6cc52b64fc6d').
narrative_ontology:cs_kernel_codification('853e4029-c3b8-4713-ba27-6cc52b64fc6d', fixed_text).
narrative_ontology:cs_authority_grounding('853e4029-c3b8-4713-ba27-6cc52b64fc6d', distributed).
narrative_ontology:cs_reading_relation('853e4029-c3b8-4713-ba27-6cc52b64fc6d', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('853e4029-c3b8-4713-ba27-6cc52b64fc6d', versailles_reparations_clauses__repudiation_reading, influences).
narrative_ontology:cs_axiom('853e4029-c3b8-4713-ba27-6cc52b64fc6d', foundational, article_231_is_jurisdictional_peg_not_moral_finding).
narrative_ontology:cs_axiom_status(article_231_is_jurisdictional_peg_not_moral_finding, holdable).
narrative_ontology:cs_axiom_grounding('853e4029-c3b8-4713-ba27-6cc52b64fc6d', article_231_is_jurisdictional_peg_not_moral_finding, conventional).
narrative_ontology:cs_axiom('853e4029-c3b8-4713-ba27-6cc52b64fc6d', foundational, reparations_bounded_by_demonstrated_fiscal_capacity).
narrative_ontology:cs_axiom_status(reparations_bounded_by_demonstrated_fiscal_capacity, holdable).
narrative_ontology:cs_axiom_grounding('853e4029-c3b8-4713-ba27-6cc52b64fc6d', reparations_bounded_by_demonstrated_fiscal_capacity, instrumental).
narrative_ontology:cs_reference_frame('853e4029-c3b8-4713-ba27-6cc52b64fc6d', london_schedule_1921_baseline).
narrative_ontology:cs_drift_state('853e4029-c3b8-4713-ba27-6cc52b64fc6d', young_plan_settlement_1929, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('853e4029-c3b8-4713-ba27-6cc52b64fc6d', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_finance_ministry).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_negotiating_delegation).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, french_reconstruction_authorities).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, belgian_occupied_territories).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_war_bond_holders).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, capacity_to_pay_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, article_231_as_legal_formality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advances the capacity-to-pay framing at every renegotiation point (London Schedule, Dawes, Young), arguing Article 231 is a jurisdictional peg for claims rather than a moral verdict, and that any schedule exceeding German fiscal and export capacity is self-defeating and will not be honored regardless of what is signed. Uses default risk and currency collapse as leverage to push the ceiling downward.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_negotiating_delegation, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit directly from schedules revised toward capacity: lower reparations-linked taxation, capital retained for reindustrialization, currency stabilized around domestic production needs rather than transfer obligations. Their productive capacity is the very metric the reading uses to bound payments, giving them structural leverage to shape what 'capacity' is measured as.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites, beneficiary,
    organized, generational, mobile, national).

% Administers the schedule internally, reports capacity figures to the Reparations Commission, and benefits from any downward revision by preserving fiscal room for domestic priorities. Its exit is constrained by continued Allied occupation leverage (Ruhr) but it gains negotiating room each time capacity arguments succeed.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_finance_ministry, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, german_finance_ministry, agenda_setter).

% Depend on reparations transfers to rebuild devastated northern departments; every downward revision under the capacity framing directly reduces funds available for reconstruction, forcing France to borrow or tax domestically to cover the gap. Exit is constrained — they cannot unilaterally enforce a higher schedule without renewed occupation, which carries its own diplomatic costs.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, french_reconstruction_authorities, payer,
    institutional, biographical, constrained, national).

% War-damaged populations awaiting compensation for destroyed infrastructure and requisitioned property; capacity-bounded schedules mean the compensation that does arrive is smaller and slower. They have no seat at the renegotiation table and no exit from the outcome.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, belgian_occupied_territories, payer,
    powerless, biographical, trapped, national).

% Allied governments financed the war partly on the expectation that German reparations would offset war debt to the United States; capacity-bounded reparations schedules leave that debt burden falling instead on Allied taxpayers and bondholders, who have no direct voice in the reparations negotiations at all.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_war_bond_holders, payer,
    powerless, biographical, trapped, national).

% Adjudicates competing capacity estimates, commissions economic studies (Dawes, Young committees), and sets revised schedules. Formally neutral but structurally dependent on both German cooperation for data and Allied political tolerance for revision — its findings become the arena where the limited-responsibility reading is either ratified or rejected.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, reparations_commission, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, reparations_commission, agenda_setter).

% Extend loans (Dawes Plan) that effectively fund German reparations payments in a circular flow; their interests in continued lending profitability shape which capacity estimates get treated as credible, but they are not formal parties to the treaty negotiations and their structural influence is exercised informally through capital markets rather than a seat at the table.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, us_creditor_banks, excluded,
    powerful, biographical, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__limited_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for setting reparations schedules that Germany can plausibly sustain without currency collapse or sovereign default, avoiding a repeat cycle of missed payments, sanctions, and re-occupation that serves no party's long-run interest.
% TRANSFER_FUNCTION: Bounds the transfer of wealth from Germany to Allied reconstruction funds and war-debt service by tying the schedule to German fiscal/export capacity rather than to assessed war damage or total war cost — the effect is to shift the burden of the shortfall from Germany onto French reconstruction budgets, Belgian claimants, and Allied taxpayers.
% ABSENT_VOICES: Belgian and northern French civilian claimants whose property damage is the nominal basis for reparations have no seat in the capacity negotiations; U.S. creditor banks whose lending shapes what capacity estimates are treated as credible are also absent from the formal treaty apparatus despite substantial informal influence.
% DISAPPEARANCE_RATIONALE: If the capacity-to-pay ceiling were removed and reparations reverted to an uncapped liability basis (the punitive_liability_reading), German fiscal policy, currency stability, and industrial reinvestment would be reorganized around debt service; conversely, French and Belgian reconstruction financing would receive a materially larger and steadier transfer. The schedule is not incidental — real budgets on both sides are built around its ceiling.
% FOUNDING_PROBLEM: The founding problem this reading was built to solve: reconciling Allied demands for compensation with the observed reality that Germany's actual fiscal and export capacity could not sustain the London Schedule's original figures without currency collapse, which would make reparations self-defeating for everyone including the creditors.
% FOUNDING_PROBLEM_CORROBORATION: Independent economists on the Dawes and Young committees (including American and British financial experts not directly party to either German or French claims) corroborated that the original schedules exceeded sustainable capacity — this is the strongest outside corroboration for the reading. However, French and Belgian reconstruction authorities, entirely outside the German beneficiary set, dispute that capacity was measured honestly rather than as a negotiating construct German elites had incentive to understate.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).
:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 — moderate, not low — because the capacity framing is a genuine constraint on German fiscal policy (schedules were real and enforced by threat of Ruhr-style occupation) but the framing itself operates to shift real costs onto French and Belgian claimants who have no comparable capacity-based ceiling protecting their reconstruction budgets. Suppression (0.38) reflects that the mechanism does rely on occupation threat and commission adjudication rather than pure voluntary agreement, but is lower than a pure extraction constraint because Germany's counter-leverage (default risk, currency collapse) is a genuine structural check, not manufactured. Theater ratio spikes to 0.35 around 1923 (hyperinflation/Ruhr crisis) reflecting a period where capacity arguments became more performative — used to justify non-payment during the crisis — before settling back toward 0.28 under Dawes/Young's more technocratic capacity assessment.
 *
 * PERSPECTIVAL GAP:
 *   From the German negotiating delegation's seat, this is coordination: a rational adjustment of an unsustainable original schedule that serves everyone's long-run interest, including creditors who would otherwise receive nothing from a collapsed German economy. From the French reconstruction authorities' and Belgian claimants' seats, the same capacity framework is experienced as an enforced transfer of the reconstruction shortfall onto occupied and war-damaged populations who never consented to have their compensation bounded by their debtor's convenience. The engine should compute these as different seat-level classifications from the same structural data — this is exactly the tangled_rope signature: real coordination function (avoiding a second unpayable-debt crisis) layered over asymmetric extraction (French/Belgian claimants absorbing the difference).
 *
 * DIRECTIONALITY LOGIC:
 *   German industrial elites and the finance ministry sit near the beneficiary end of directionality: they gain fiscal room and negotiating leverage directly from every downward revision, and their exit options (mobile capital, constrained but improving negotiating position) reduce effective extraction toward them further. French reconstruction authorities and Belgian occupied territories sit near the target end: trapped or constrained exit, no capacity-ceiling protection of their own, and direct fiscal exposure to any revision. Allied war bond holders are diffuse targets — individually powerless, geographically dispersed, bearing an indirect rather than direct cost, which the engine should register as high suppression-exposure despite low visibility of the mechanism to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unsustainable schedules risking currency collapse and repeated payment crises) was substantially real in 1919-1924 and is corroborated by independent Dawes/Young committee economists — this is not a manufactured crisis. But by 1929-1932 the founding problem's acute form (imminent German fiscal collapse) had been addressed by Dawes-era stabilization, while the capacity-bounded framework continued to be invoked to justify further downward revision (Young Plan, then de facto suspension), suggesting the mandate persisted past the acute phase of its original justification. The founding_problem_status is authored as 'contested' rather than 'dead' because German negotiators continued to attest genuine fiscal fragility through the Depression years (1930-32), a claim with real empirical support given the German banking crisis of 1931 — this is a case where the mandate's obsolescence is genuinely disputed on the merits, not simply asserted by beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_measurement_construction,
    'Was ''German economic capacity'' as measured by the Dawes and Young committees an objective empirical assessment, or a construct substantially shaped by German negotiators'' incentive to understate capacity and Allied creditors'' (especially U.S. bank) incentive to keep German solvency high enough to service Dawes loans?',
    'Comparative analysis of committee capacity estimates against independent contemporaneous German trade, tax revenue, and industrial output data not supplied by German negotiators; examination of U.S. bank correspondence around Dawes Plan lending decisions.',
    'If capacity was substantially constructed rather than discovered, the limited_responsibility_reading''s ε should be revised upward (more extractive toward Allied/occupied claimants than the capacity framing admits); if genuinely empirical, the reading''s coordination function is stronger than the extraction reading credits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_measurement_construction, empirical, 'Whether the capacity ceiling was objectively measured or negotiated-construct.').

omega_variable(
    article_231_formality_vs_moral_content,
    'Is Article 231''s ''war guilt clause'' language genuinely severable into a pure jurisdictional formality (as this reading holds) or does its drafting history and contemporaneous Allied usage show it was intended and understood as a moral-liability finding, making the limited_responsibility_reading''s core premise a retrospective reinterpretation rather than the treaty''s original sense?',
    'Textual and drafting-history analysis of the treaty negotiations (Paris 1919) comparing French/British/American drafting intent against the German legal argument developed after signature; comparison to contemporaneous Allied public statements.',
    'If Article 231 was originally intended as moral liability, this reading''s foundational axiom is a constructed legal fiction adopted for negotiating advantage rather than a discovered legal truth, which would strengthen the punitive_liability_reading''s claim that the limited_responsibility framing is itself an extraction device for German elites rather than a neutral legal clarification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_231_formality_vs_moral_content, conceptual, 'Whether Article 231''s formal-jurisdictional reading is a genuine legal distinction or retrospective reframing.').

omega_variable(
    counterfactual_schedule_sustainability,
    'Would the original London Schedule (uncapped by capacity) have actually produced German default and currency collapse as claimed, or was default risk itself partly a negotiating position rather than an economic certainty?',
    'Counterfactual economic modeling using comparable interwar sovereign debt cases and German fiscal capacity data independent of negotiation-context statements.',
    'If default was a near-certainty, the capacity-bounded reading''s coordination claim is strongly vindicated; if default risk was overstated as leverage, more of the measured extraction (toward French/Belgian claimants) should be attributed to strategic behavior rather than genuine necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_schedule_sustainability, empirical, 'Whether German default under the original schedule was a genuine economic near-certainty or overstated leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1921, 0.2).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1923, 0.35).
narrative_ontology:measurement(vers_tr_t1924, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1924, 0.3).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1929, 0.28).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1932, 0.28).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1919, 0.58).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1921, 0.55).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1923, 0.61).
narrative_ontology:measurement(vers_be_t1924, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1924, 0.5).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1929, 0.4).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1932, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1919, 0.5).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1921, 0.55).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1923, 0.7).
narrative_ontology:measurement(vers_su_t1924, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1924, 0.45).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1929, 0.32).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1932, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__limited_responsibility_reading, 0.12).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, dawes_plan_loan_structure).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, young_plan_settlement).

% DUAL FORMULATION NOTE:
% This story is one of three siblings sharing the versailles_reparations_clauses kernel. punitive_liability_reading claims Germany bears near-unlimited moral/financial responsibility grounded in Article 231 as a moral finding — its victim set is German taxpayers and its ε should register substantially higher extraction toward Germany. repudiation_reading claims the treaty is void ab initio for duress — its beneficiary set is German sovereignty claims broadly and its victim set is the entire Allied reparations apparatus, with likely snare or scaffold classification depending on how the duress claim is structured. This story (limited_responsibility_reading) occupies the middle ground: a genuine partial constraint on Allied maximalism that nonetheless operates to shift real costs onto French/Belgian claimants and Allied taxpayers. All three should be read as distinct constraints, never averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
