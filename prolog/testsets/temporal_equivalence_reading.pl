% ============================================================================
% CONSTRAINT STORY: temporal_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_equivalence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: temporal_equivalence_reading
 *   human_readable: Temporal Equivalence Reading of AI Displacement (Industrial Revolution Analogy)
 *   domain: political_economy/labor_economics/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the 'temporal equivalence' reading of the
 *   technological_displacement_axiom kernel: the claim that current AI-driven
 *   labor displacement is structurally identical to the Industrial Revolution
 *   — painful in the short run, but resolved by universal long-run job
 *   creation given sufficient time. Under this reading there is no structural
 *   break and no permanent victim class; the appropriate policy stance is
 *   patience and market adjustment rather than large-scale intervention. This
 *   is NOT a claim about whether the analogy is true — it is a claim about
 *   the structure this specific reading commits to: an empty victim set over
 *   'the relevant horizon,' displacement treated as temporary friction, and
 *   market clearing treated as the default outcome absent intervention. The
 *   sibling readings (clock_incompatibility_reading, which argues AI
 *   diffusion is too fast relative to human/institutional adaptation
 *   timescales for the analogy to hold, and skills_mismatch_reading, which
 *   argues the displaced and the newly-employed are structurally different
 *   populations such that 'job creation' does not reach 'job replacement')
 *   are separate constraints with their own ε and stakeholder structures, not
 *   alternate measurements of this one.
 *
 * KEY AGENTS:
 *   - ai_capital_owners: primary beneficiary (institutional/arbitrage) — captures productivity gains while the framing forestalls redistribution pressure
 *   - technology_deployment_firms: secondary beneficiary (powerful/mobile) — lower political cost of restructuring
 *   - policy_status_quo_advocates: agenda-setter (institutional/analytical) — administers the framing that governs policy response
 *   - displaced_workers_transition_cohort: bears the adjustment cost now, structurally denied victim status by this reading's own terms
 *   - economic_historians: analytical observer — supplies partially corroborating, partially disconfirming evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_equivalence_reading, 0.18).
domain_priors:suppression_score(temporal_equivalence_reading, 0.12).
domain_priors:theater_ratio(temporal_equivalence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_equivalence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(temporal_equivalence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temporal_equivalence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temporal_equivalence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(temporal_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_equivalence_reading, rope).
narrative_ontology:human_readable(temporal_equivalence_reading, "Temporal Equivalence Reading of AI Displacement (Industrial Revolution Analogy)").
narrative_ontology:topic_domain(temporal_equivalence_reading, "political_economy/labor_economics/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temporal_equivalence_reading, 'edcae2ec-69e6-43ea-a66e-22ec6b814f5a').
narrative_ontology:cs_kernel_codification('edcae2ec-69e6-43ea-a66e-22ec6b814f5a', distributed).
narrative_ontology:cs_authority_grounding('edcae2ec-69e6-43ea-a66e-22ec6b814f5a', distributed).
narrative_ontology:cs_reading_relation('edcae2ec-69e6-43ea-a66e-22ec6b814f5a', technological_displacement_axiom__clock_incompatibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('edcae2ec-69e6-43ea-a66e-22ec6b814f5a', technological_displacement_axiom__skills_mismatch_reading, coexists_with).
narrative_ontology:cs_axiom('edcae2ec-69e6-43ea-a66e-22ec6b814f5a', foundational, displacement_horizon_is_temporary_by_default).
narrative_ontology:cs_axiom_status(displacement_horizon_is_temporary_by_default, holdable).
narrative_ontology:cs_axiom_grounding('edcae2ec-69e6-43ea-a66e-22ec6b814f5a', displacement_horizon_is_temporary_by_default, empirically_contingent).
narrative_ontology:cs_axiom('edcae2ec-69e6-43ea-a66e-22ec6b814f5a', foundational, aggregate_labor_market_clearing_implies_no_permanent_victim_class).
narrative_ontology:cs_axiom_status(aggregate_labor_market_clearing_implies_no_permanent_victim_class, holdable).
narrative_ontology:cs_axiom_grounding('edcae2ec-69e6-43ea-a66e-22ec6b814f5a', aggregate_labor_market_clearing_implies_no_permanent_victim_class, empirically_contingent).
narrative_ontology:cs_reference_frame('edcae2ec-69e6-43ea-a66e-22ec6b814f5a', industrial_revolution_absorption_precedent).
narrative_ontology:cs_drift_state('edcae2ec-69e6-43ea-a66e-22ec6b814f5a', contemporary_ai_diffusion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('edcae2ec-69e6-43ea-a66e-22ec6b814f5a', '').
narrative_ontology:cs_kernel_id(temporal_equivalence_reading, technological_displacement_axiom).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_equivalence_reading, ai_capital_owners).
narrative_ontology:constraint_beneficiary(temporal_equivalence_reading, technology_deployment_firms).
narrative_ontology:constraint_beneficiary(temporal_equivalence_reading, policy_status_quo_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temporal_equivalence_reading, displaced_workers_transition_cohort).
narrative_ontology:constraint_vindicates(temporal_equivalence_reading, long_run_labor_market_clearing_doctrine).
narrative_ontology:constraint_vindicates(temporal_equivalence_reading, creative_destruction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own the models, compute, and deployment infrastructure driving displacement. The temporal-equivalence reading lets them frame current disruption as a historically familiar, self-correcting phase, which reduces pressure for redistribution, retraining mandates, or transition funding while they capture productivity gains now.
narrative_ontology:constraint_stakeholder(temporal_equivalence_reading, ai_capital_owners, beneficiary,
    institutional, generational, arbitrage, global).

% Deploy AI systems to automate roles across sectors. The 'this happened before, it works out' framing lowers the political and reputational cost of layoffs and restructuring, since displacement reads as a natural, temporary phase rather than a policy choice with distributional consequences.
narrative_ontology:constraint_stakeholder(temporal_equivalence_reading, technology_deployment_firms, beneficiary,
    powerful, biographical, mobile, global).

% Legislators, central bankers, and economic advisors who cite the Industrial Revolution analogy to justify minimal intervention — no large retraining programs, no wage insurance, no accelerated safety net expansion. They administer the framing that governs policy response and could change it, but the analogy licenses inaction.
narrative_ontology:constraint_stakeholder(temporal_equivalence_reading, policy_status_quo_advocates, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(temporal_equivalence_reading, policy_status_quo_advocates, beneficiary).

% Workers currently losing jobs or wage share to AI automation. Under this reading, they are not victims of a structural break but participants in a temporary adjustment; they bear the actual costs of unemployment, retraining, and geographic relocation now, on a promise that a labor market they cannot verify will absorb them eventually. Whether they are a 'victim' at all is precisely what this reading denies — its ε and structure assume no permanent victim set exists over the relevant horizon.
narrative_ontology:constraint_stakeholder(temporal_equivalence_reading, displaced_workers_transition_cohort, payer,
    moderate, biographical, constrained, national).

% Study whether the Industrial Revolution's multi-generational, geographically uneven adjustment (decades of falling real wages in parts of England before recovery) is structurally comparable to a compressed, general-purpose technology diffusing across every sector simultaneously. They supply the evidence this reading depends on but does not fully corroborate.
narrative_ontology:constraint_stakeholder(temporal_equivalence_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% Labor organizations and displaced-worker coalitions argue the analogy is being used to forestall transition support. They are cited in media coverage but rarely seated in the policy venues where the temporal-equivalence framing is adopted as the operating assumption.
narrative_ontology:constraint_stakeholder(temporal_equivalence_reading, displaced_worker_advocacy_groups, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temporal_equivalence_reading, diffuse).
narrative_ontology:fixing_cost_class(temporal_equivalence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading coordinates policy expectation-setting: it gives governments, firms, and markets a shared historical template for how much transition support to provide and for how long, avoiding ad hoc, uncoordinated responses to each wave of displacement.
% TRANSFER_FUNCTION: Moves the burden of proof and the burden of adjustment cost onto currently displaced workers and away from capital owners and deploying firms — the claim that disruption is 'temporary' licenses withholding transition spending now against a promised future equilibrium that cannot be verified in advance.
% ABSENT_VOICES: Displaced-worker advocacy groups and regional economies undergoing concentrated job loss are rarely present when the analogy is invoked in policy or boardroom settings; they would point to multi-decade regional decline following past technology shocks (e.g. deindustrialized regions that never recovered) as counter-evidence the analogy elides.
% DISAPPEARANCE_RATIONALE: If this reading vanished, policy discourse would lose its default justification for minimal intervention — some analysts argue urgent transition-support legislation would follow (world_rearranges); others argue the underlying political economy of low intervention would persist under a different justification (world_unchanged). The parties dispute which.
% FOUNDING_PROBLEM: Early debates about automation and AI needed a framework to distinguish transitional disruption from permanent structural unemployment, to avoid both premature panic-driven overregulation and complacent underinvestment in worker transition support.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream macroeconomists and industry-funded think tanks attest the framing remains sound, citing aggregate employment recovery after past technology shocks. Independent labor economists and economic historians outside the beneficiary set (academic labor economics literature on regional trade-shock persistence, e.g. studies of Chinese import competition's decade-plus-long local labor market effects) corroborate only a partial and contested version — long-run aggregate recovery is documented, but regional and cohort-level permanent losses are also documented, which this reading's structure excludes by declaring the victim set empty.
narrative_ontology:disappearance_verdict(temporal_equivalence_reading, contested).
narrative_ontology:founding_problem_status(temporal_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temporal_equivalence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(temporal_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(temporal_equivalence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_equivalence_reading_tests).
:- end_tests(temporal_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because THIS READING's structure specifically denies a permanent extraction relationship exists — it claims displacement is temporary friction, not a transfer. Suppression is low (0.12) because no active coercive machinery enforces the reading; it operates through persuasion and default policy inertia, not coercion. Theater ratio is moderate and rising (0.25→0.42) because as displacement continues without the promised absorption materializing on the promised timescale, more of the reading's public defense becomes reassurance rhetoric ('this happened before') relative to substantive transition support — a genuine Goodhart-style drift where the analogy is invoked more as disruption persists, not less. Accessibility collapse is moderate (0.35): alternative framings (skills mismatch, pace incompatibility) remain visible and contested, they have not been suppressed, which is itself informative — this reading does not need to eliminate rivals because it operates through selection of which framing officials adopt, not through foreclosing others. Resistance is moderate-high (0.55) because labor economists, affected regions, and advocacy groups actively contest the analogy's applicability.
 *
 * DIRECTIONALITY LOGIC:
 *   AI capital owners and deploying firms sit near full-beneficiary (d low): the reading's adoption reduces the probability of costly intervention, and they hold institutional power with arbitrage-grade exit (capital and firms can relocate operations or restructure faster than policy can respond). Policy status-quo advocates administer the framing itself. The displaced-worker cohort is structurally positioned as bearing real costs now against a deferred, unverifiable promise — but this reading's own structure declares them non-victims over 'the relevant horizon,' which is exactly the contested move: the reading assumes away the question the sibling readings are built to ask. This is authored deliberately as the ε-defining feature of THIS reading, not fudged to make the type land somewhere convenient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing transitional disruption from permanent structural unemployment to calibrate policy response correctly — remains genuinely contestable rather than obviously dead or obviously live, which is why founding_problem_status is authored as 'contested' rather than resolved in either direction. The risk this story flags is not that the analogy is false but that its INVOCATION has begun to outrun its EVIDENTIARY SUPPORT: rising theater_ratio alongside flat extractiveness suggests the reading is increasingly deployed as reassurance rhetoric to forestall transition-support policy, independent of whether the underlying empirical claim (long-run job creation) is holding at the pace originally implied. Classifying this as a rope (genuine coordination function: shared expectation-setting) rather than snare avoids mislabeling a contestable historical analogy as pure extraction — but the rising theater ratio is the signal that would, if it continued, push toward a piton or tangled_rope reclassification as the coordination function atrophies relative to its defensive/legitimating function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relevant_horizon_underspecification,
    'What is ''the relevant horizon'' over which long-run job creation is claimed to be universal — and is it long enough to include multiple human working lifetimes, which would make the temporary/permanent distinction meaningless for any individual worker even if true in aggregate?',
    'Specify a bounded time horizon (e.g., 10, 20, 40 years) ex ante and test aggregate employment and displaced-cohort re-employment rates against it, rather than allowing the horizon to expand indefinitely whenever near-term data disconfirms the claim.',
    'If the horizon is allowed to be unboundedly long, the claim becomes unfalsifiable and the reading functions as permanent cover for withholding transition support rather than a genuine empirical prediction — this would push the constraint from rope toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relevant_horizon_underspecification, conceptual, 'Whether ''long-run'' is a testable claim or an unfalsifiable deferral.').

omega_variable(
    aggregate_vs_cohort_victim_status,
    'Does aggregate long-run labor market recovery (which this reading''s structure asserts) actually preclude a permanent victim class at the cohort or regional level, or can both be true simultaneously — aggregate recovery AND specific cohorts permanently displaced?',
    'Longitudinal tracking of specific displaced worker cohorts (not aggregate employment statistics) across the full claimed horizon, disaggregated by age, region, and sector at time of displacement, compared against the general population''s employment and wage trajectories.',
    'If cohort-level permanent losses coexist with aggregate recovery (as documented in trade-shock literature), this reading''s empty victim-set claim is falsified even while its top-line historical analogy holds — this is the exact structural question the sibling skills_mismatch_reading is built to answer, and it directly bears on whether victims should be declared here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_vs_cohort_victim_status, empirical, 'Whether aggregate recovery is compatible with permanent cohort-level exclusion.').

omega_variable(
    committer_framing_selection,
    'Why was the temporal-equivalence framing selected as this story''s reading rather than treating the Industrial Revolution analogy as inherently ambiguous across all three readings simultaneously?',
    'This selection follows the ε-invariance principle: because ''the analogy is structurally identical to the Industrial Revolution'' produces a clearly low-ε, empty-victim-set structure under one framing and a clearly high-ε, populated-victim-set structure under the pace-mismatch or skills-mismatch framings, these are not one constraint measured differently — they are three constraints. The signal guiding this file''s selection was the explicit kernel/reading assignment in the generation manifest, not an independent structural judgment made here.',
    'Readers must not average this story''s low ε against the siblings'' higher ε to produce a ''true'' composite score for ''the AI-Industrial-Revolution analogy'' — doing so would violate DP-001 and reintroduce the exact conflation the kernel/reading split exists to prevent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_selection, conceptual, 'Documents why this reading was decomposed from its siblings rather than measured as one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_equivalence_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temporal_equivalence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(temp_tr_t4, temporal_equivalence_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(temp_tr_t8, temporal_equivalence_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(temp_tr_t12, temporal_equivalence_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(temp_tr_t16, temporal_equivalence_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(temp_tr_t20, temporal_equivalence_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temporal_equivalence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(temp_be_t4, temporal_equivalence_reading, base_extractiveness, 4, 0.13).
narrative_ontology:measurement(temp_be_t8, temporal_equivalence_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(temp_be_t12, temporal_equivalence_reading, base_extractiveness, 12, 0.18).
narrative_ontology:measurement(temp_be_t16, temporal_equivalence_reading, base_extractiveness, 16, 0.19).
narrative_ontology:measurement(temp_be_t20, temporal_equivalence_reading, base_extractiveness, 20, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temporal_equivalence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_equivalence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(temporal_equivalence_reading, 0.12).
narrative_ontology:affects_constraint(temporal_equivalence_reading, clock_incompatibility_reading).
narrative_ontology:affects_constraint(temporal_equivalence_reading, skills_mismatch_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language claim 'the AI transition is like the Industrial Revolution.' temporal_equivalence_reading asserts an empty victim set and low ε (0.18) grounded in the historical analogy holding at the pace and population level. clock_incompatibility_reading and skills_mismatch_reading are separate files asserting non-empty victim sets on different structural grounds (diffusion speed vs. population mismatch) and should be authored with correspondingly higher ε. Do not average across the three; they are linked via affects_constraints because adoption of this reading structurally suppresses the political salience of the other two (a shared policy discourse can typically sustain only one dominant framing at a time), not because they measure the same ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
