% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total War Reachability Boundary (Contingent Technology Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'total_war_reachability_boundary'—the contingent-reachability reading. It
 *   asserts that total-war reachability is fundamentally
 *   technology-dependent: nuclear deterrence currently suppresses
 *   reachability, but future technologies (hypersonics, autonomous systems,
 *   space weapons, AI-enabled targeting) could restore escape routes. This
 *   reading justifies continued military modernization and strategic hedging
 *   as rational responses to a temporarily-suppressed but reversal-prone
 *   boundary. The constraint is authored as a PITON (atrophied coordination
 *   function persisting through institutional inertia) because the original
 *   rationale—explaining why deterrence held after nuclear weapons made total
 *   war devastating—has been solved for 80 years, yet the reading persists
 *   and now primarily justifies arms perpetuation rather than clarity. The
 *   theater_ratio rises from 0.25 to 0.61 over the interval, indicating
 *   increasing performance of contingency narratives relative to functional
 *   strategic stabilization. Simultaneously, civilian populations remain
 *   trapped in deterrence frameworks justified by claims about technology
 *   futures that may be false.
 *
 * KEY AGENTS:
 *   - Destabilizing technology investors (beneficiary): states and military-industrial complexes developing hypersonics, AI systems, space weapons
 *   - Nuclear modernization advocates (agenda setter + beneficiary): strategic establishments maintaining the contingent-reachability doctrine
 *   - Civilian populations under deterrence (payer): held in latent risk exposure justified by contingency narratives
 *   - Non-nuclear states (payer): strategically subordinated by others' technology choices
 *   - Arms control skeptics (beneficiary): benefit from blocking disarmament by claiming contingency prevents settlement
 *   - Deterrence theory consumers (observer): analysts who adjudicate competing reachability readings
 *   - Alternative deterrence frameworks (excluded): disarmament advocates, existential-risk researchers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.68).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.72).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, piton).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total War Reachability Boundary (Contingent Technology Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, 'fe6dd789-fe04-4116-bdd8-9f71a4f1d8df').
narrative_ontology:cs_kernel_codification('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df', distributed).
narrative_ontology:cs_authority_grounding('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df', extraction).
narrative_ontology:cs_interpretation_layer_present('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df').
narrative_ontology:cs_reading_relation('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df', total_war_reachability_boundary__dropping_reading, influences).
narrative_ontology:cs_axiom('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df', foundational, reachability_technology_contingent).
narrative_ontology:cs_axiom_status(reachability_technology_contingent, holdable).
narrative_ontology:cs_axiom_grounding('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df', reachability_technology_contingent, empirically_contingent).
narrative_ontology:cs_axiom('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df', foundational, deterrence_stability_requires_capability_parity).
narrative_ontology:cs_axiom_status(deterrence_stability_requires_capability_parity, holdable).
narrative_ontology:cs_axiom_grounding('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df', deterrence_stability_requires_capability_parity, instrumental).
narrative_ontology:cs_reference_frame('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df', technology_dependent_closure_framework).
narrative_ontology:cs_drift_state('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df', contemporary_post_cold_war_technological_acceleration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe6dd789-fe04-4116-bdd8-9f71a4f1d8df', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_investors).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, nuclear_modernization_advocates).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_under_deterrence).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, arms_control_skeptics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States and military-industrial complexes that invest in technologies (hypersonics, AI-guided systems, space weapons, autonomous platforms) designed to restore escapes from nuclear deterrence gridlock. They benefit from the constraint's narrative: that current reachability contraction is temporary and reversible, justifying continued weapons development and strategic hedging. Their exit would require abandoning long-term modernization programs and accepting permanent strategic disadvantage.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_investors, beneficiary,
    institutional, generational, constrained, global).

% Strategic planners, defense establishments, and arms-control skeptics who set and defend the operational framing that total war reachability is merely suppressed by current tech equilibrium, not eliminated. They maintain this reading through doctrine, exercises, procurement justifications, and threat assessments. They benefit from the constraint by avoiding the political costs of accepting permanent mutual vulnerability (which the contraction reading would require).
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nuclear_modernization_advocates, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, nuclear_modernization_advocates, beneficiary).

% Populations in nuclear-armed states and their allies, held in a state of latent exposure. They depend on deterrence holding, but that dependency is justified to them via the narrative that reachability is temporary and tech-contingent—which justifies continued weapons development and elevated strategic tension. Their exit is impossible; they bear the risk of failure if deterrence breaks and the restored reachability materializes.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_under_deterrence, payer,
    powerless, biographical, trapped, global).

% States without nuclear weapons face a constraint imposed by others' technology choices. They pay through strategic subordination, vulnerability to nuclear-armed neighbors, and the costs of alignment with nuclear guarantors. The contingent-reachability reading justifies their continued dependency ('deterrence is temporary, you must stay aligned') without offering escape.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states, payer,
    moderate, biographical, constrained, global).

% Strategic communities that benefit from the contingent-reachability framing because it delegitimizes permanent arms-control settlements. They argue that any agreement freezing current tech would be unstable and violated, so only continued deterrence through capability parity works. They collect influence from this reading by blocking disarmament initiatives and justifying high military spending.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, arms_control_skeptics, beneficiary,
    institutional, generational, arbitrage, global).

% Academic, policy, and military analysts who interpret and adjudicate deterrence doctrine. They evaluate competing readings of reachability (this one, contraction, dropping) and their implications for strategy. They do not directly benefit or pay but their interpretive framing influences which reading dominates policy.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, deterrence_theory_consumers, observer,
    organized, biographical, analytical, global).

% Communities and states advocating non-nuclear deterrence, existential risk reduction, or mutual disarmament are structurally excluded from the decision-making that sustains the contingent-reachability reading. They would argue for permanent reachability-closure and technology abandonment; their exclusion is maintained by the reading's institutional entrenchment.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, alternative_deterrence_frameworks, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_investors).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contingent_reachability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework (contingent reachability, technology-dependent closure) for coordinating military modernization programs across states: if all parties believe reachability could be restored by future technology, then capability parity justifies continued investment, avoiding arms-race instability from mutual defection to disarmament. The coordination story is: 'We all hedge against the same uncertain future, therefore none of us can afford to pause modernization unilaterally.'
% TRANSFER_FUNCTION: Transfers agency and strategic options from civilian populations to technology-investing states and military establishments. Also transfers resources: defense budgets remain high (justified by contingency narratives), and technological development is prioritized over disarmament or alternative security frameworks. In terms of institutional power: the reading transfers interpretive authority from disarmament advocates to military strategists.
% ABSENT_VOICES: Disarmament advocates, existential-risk researchers, indigenous communities threatened by weapons testing, and non-military security experts are structurally excluded from strategic doctrine-setting. They would argue that reachability closure is permanent (contraction reading) or that deterrence is stable and should be accepted rather than hedged (dropping reading). Their exclusion is maintained by classification of research, suppression of disarmament discourse, and institutional entrenchment of military analysis.
% DISAPPEARANCE_RATIONALE: If the contingent-reachability reading vanished and were replaced by either the contraction or dropping reading, strategic doctrine would shift fundamentally: modernization rationales would evaporate, disarmament politics would open, technology development priorities would reverse, and military budgets would face pressure. The distribution of power among states (advantage accrues to those with modernization capabilities) would reorganize around new assumptions about technological stability.
% FOUNDING_PROBLEM: After nuclear weapons created mutual vulnerability, strategists needed to explain why deterrence held and what could break it. The contingent-reachability reading answered: deterrence holds because current technology prevents war-winning strategies, but future technology might restore them—creating rational grounds for continued capability parity and hedging.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested dead by independent security analysts, disarmament researchers, and historical deterrence studies: deterrence has held robustly for 80 years across multiple technological transitions (ICBMs, MIRVs, strategic defense proposals), suggesting closure is structural rather than technology-contingent. Military strategists and modernization advocates attest the problem remains live, citing ongoing technological development by adversaries; however, this attestation is from the beneficiary set (those justifying modernization) and is not corroborated by independent analysis. The reading persists not because the founding problem is live but because the narrative serves institutional interests.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint shifts risk and agency burdens from technology-investing states to dependent populations, justified by contested claims about technology futures. Suppression is higher (0.72) because maintaining the contingent-reachability reading requires classification of alternative research, suppression of disarmament discourse, and institutional entrenchment of modernization programs. Theater_ratio is substantial (0.61) because the constraint's institutional presence persists—exercises, doctrine, threat assessments—but the functional payoff (explaining deterrence success) was achieved decades ago; what remains is theatrical justification for arms races. Accessibility_collapse is moderate (0.48) because alternatives to nuclear deterrence exist (disarmament, non-nuclear deterrence, existential-risk strategies) but are institutionally suppressed; for those trapped in the deterrence framework, the collapse is higher. Resistance is moderate (0.55) because disarmament movements exist but face institutional and military-strategic opposition. The coercion_grid shows that suppression and stakes_inflation intensify across all levels from 1945 to 2026, while resistance declines—indicating the constraint's enforcement machinery has strengthened while opposition has been marginalized. This pattern is consistent with piton dynamics: an atrophied function maintained through increasing institutional weight.
 *
 * PERSPECTIVAL GAP:
 *   The modernization-advocate seat should compute differently from the civilian-population seat: from the modernization advocate's position, the constraint is a necessary coordination on rational behavior (deterrence requires capability parity, which requires hedge against future technology escape). From the civilian position, the constraint is enforced extraction justified by false contingency claims. The engine computes this divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (d ≈ 0.1–0.3): modernization advocates, technology investors, arms-control skeptics. They set the agenda, collect resources and influence, and enjoy high exit mobility (they could shift priorities but choose not to). Targets (d ≈ 0.75–1.0): civilian populations (powerless, trapped, bearing concentrated risk) and non-nuclear states (moderate power but structurally subordinated to technology-investing states' choices). Symmetric actors: deterrence-theory consumers (observers with analytical exit). The directionality derivation follows from power, time_horizon, and exit_options: powerless agents with trapped exit and no control over the constraint are targets; institutional agents with mobile exit and agenda-setting authority are beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is authored as resolved mandatrophy: the founding problem (explaining why nuclear deterrence holds despite mutual vulnerability) was solved by the early 1960s—deterrence has been robust for 60+ years across changing technology, so the original rationale (deterrence fragility due to technology contingency) has been empirically falsified. Yet the constraint persists: modernization continues, contingency narratives are maintained, and the reading justifies itself through a self-fulfilling loop ('we modernize because technology might escape, and if our modernization programs succeed, that proves escape is possible'). The theater_ratio rising to 0.61 indicates the constraint is now primarily maintained through institutional performance (threat assessments, exercises, doctrinal updates) rather than through genuine strategic problem-solving. This is the diagnostic signature of a piton: an atrophied function (explaining deterrence) whose original mandate is dead, but whose institutional apparatus persists because it distributes benefits to powerful actors (modernization advocates, military establishments) and because reversing it would impose costs on those actors (budget cuts, loss of influence, admission of strategic redundancy). The constraint is not a Tangled Rope (coordination + extraction) because the extraction is not justified by actual coordination gains—it is purely justified by the false contingency narrative. It is not a Scaffold because there is no exit condition or sunset clause; modernization is indefinite. It is a Piton because the function is atrophied, the performance is theatrical, and the persistence is institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_reversibility_contingency,
    'Is total-war reachability genuinely technology-contingent and reversible, or has nuclear mutual vulnerability created a permanent structural closure that no plausible technology can escape?',
    'Retrospective analysis after any technology maturation (hypersonics, AI-enabled systems, space-based defenses) that asymptotically approaches its claimed capability: if the technology fails to restore escape, or if the cost of deployment proves prohibitive relative to countermeasures, the contingency claim weakens. Alternatively, formal mathematical proofs of reachability impossibility under mutual vulnerability would resolve the empirical question.',
    'If reachability is genuinely reversible, the contingent-reachability reading is correct and modernization is rational. If permanent closure is proven, the reading becomes a cover story for arms perpetuation, and should be reclassified as snare (extraction masked by false contingency) or piton (maintained by institutional inertia despite atrophied function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reachability_reversibility_contingency, empirical, 'Whether technological change can truly restore total-war reachability or whether closure is structural.').

omega_variable(
    piton_vs_scaffold_distinction,
    'Is the constraint a piton (atrophied capability persisting theatrically) or a scaffold (temporary arrangement with a functional exit path)?',
    'Institutional commitment test: if modernization programs contain sunset clauses or explicit milestones at which the reading would be revisited (e.g., ''if technology X is not achieved by year Y, pivot to disarmament''), it is scaffold; if programs persist indefinitely with only rhetorical contingency, it is piton. Also: measurement of theater_ratio trajectory—if it continues rising, performance is outpacing function and the constraint is piton; if it stabilizes or drops, function remains active and the reading may be salvageable as scaffold.',
    'Piton diagnosis justifies policy reversal (the reading should be abandoned as institutional theater); scaffold diagnosis justifies patience (the arrangement has legitimate exit conditions and function). The empirical basis for choosing between them is institutional commitment structure, not rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_scaffold_distinction, empirical, 'Whether the contingent-reachability framing is temporary-with-exit or theater-of-atrophy.').

omega_variable(
    alternative_kernel_readings_committer_ambiguity,
    'Which reading of the total-war-reachability-boundary kernel is epistemically and institutionally justified: this contingent-reachability reading, the contraction reading (permanent closure), or the dropping reading (accepted permanent risk)?',
    'Cross-framework adjudication: compare the three readings'' assumptions about physics (can technology escape mutual vulnerability?), institutional track record (how stable has deterrence been relative to reading predictions?), and policy outcomes (which reading''s justified actions have produced better security outcomes?). The resolution is not empirical alone but depends on how security is defined and what counts as success.',
    'If contraction reading is vindicated, the contingent-reachability reading is false and justifies continued arms races on false premises. If dropping reading is vindicated, reachability is constant and the reading''s contingency framing is irrelevant. If contingent-reachability is vindicated, the other readings misunderstand the strategic landscape. The three readings cannot all be correct simultaneously.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_kernel_readings_committer_ambiguity, conceptual, 'Kernel committer ambiguity: which reading of total-war reachability is the correct framing of the strategic problem.').

omega_variable(
    suppression_internalization_dynamics,
    'Is the measured suppression (0.72) structural (enforced by military hierarchy, weapons treaties, technological barriers) or internalized (strategic elites believe the contingent-reachability narrative and suppress dissent from within)?',
    'Defection test: if strategic planners encounter evidence that reachability closure is permanent and still suppress public discussion of disarmament, suppression is partly internalized. If suppression relies primarily on institutional hierarchy and classification rules, it is structural. Post-exit trajectory: if strategists exit from the doctrine and continue to believe contingency narratives, suppression is internalized; if they reverse their views, it was structural.',
    'If suppression is internalized, the constraint persists through elite cognitive capture and would require re-education or institutional turnover to reverse. If structural, policy change by institutional fiat could disrupt it faster. The composition of suppression mechanisms affects the cost of constraint reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_dynamics, empirical, 'Whether suppression of alternative reachability readings is structural or internalized among strategic elites.').

omega_variable(
    kernel_reading_sibling_foreclosure,
    'Do the axioms of this reading (technology-contingent reachability; deterrence is rationally dependent on capability parity) logically foreclose the contraction reading, or do the readings coexist as different parties'' simultaneous commitments?',
    'Logical analysis: if the contraction reading denies technological reversibility (reachability is permanently closed), and this reading asserts it, a single party holding both is incoherent. But multiple parties can hold both simultaneously if they disagree on physics/strategy. The question is whether any SINGLE party could coherently hold both readings, or whether they are forced apart by logic.',
    'If they foreclose each other, the engine should compute a foreclosure edge and the kernel contains a genuine logical contradiction. If they coexist, the kernel is a contested framing where different institutional actors rationally choose different readings, and the constraint-family analysis should treat them as coexisting alternatives, not as logical opposites.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_foreclosure, conceptual, 'Whether this reading''s core axioms foreclose the contraction reading or coexist with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1962, 0.38).
narrative_ontology:measurement(tota_tr_t1980, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1980, 0.52).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2000, 0.58).
narrative_ontology:measurement(tota_tr_t2015, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2015, 0.61).
narrative_ontology:measurement(tota_tr_t2026, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2026, 0.61).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1962, 0.58).
narrative_ontology:measurement(tota_be_t1980, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(tota_be_t2015, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(tota_be_t2026, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1945, 0.48).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1962, 0.68).
narrative_ontology:measurement(tota_su_t1980, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement(tota_su_t2015, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(tota_su_t2026, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2026, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=2026
narrative_ontology:measurement(tota_grid_01, total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse(class), 1945, 0.28).
narrative_ontology:measurement(tota_grid_02, total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse(class), 2026, 0.4).
narrative_ontology:measurement(tota_grid_03, total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse(individual), 1945, 0.15).
narrative_ontology:measurement(tota_grid_04, total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse(individual), 2026, 0.32).
narrative_ontology:measurement(tota_grid_05, total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse(organizational), 1945, 0.42).
narrative_ontology:measurement(tota_grid_06, total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse(organizational), 2026, 0.55).
narrative_ontology:measurement(tota_grid_07, total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse(structural), 1945, 0.35).
narrative_ontology:measurement(tota_grid_08, total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse(structural), 2026, 0.48).
narrative_ontology:measurement(tota_grid_09, total_war_reachability_boundary__contingent_reachability_reading, resistance(class), 1945, 0.72).
narrative_ontology:measurement(tota_grid_10, total_war_reachability_boundary__contingent_reachability_reading, resistance(class), 2026, 0.55).
narrative_ontology:measurement(tota_grid_11, total_war_reachability_boundary__contingent_reachability_reading, resistance(individual), 1945, 0.85).
narrative_ontology:measurement(tota_grid_12, total_war_reachability_boundary__contingent_reachability_reading, resistance(individual), 2026, 0.62).
narrative_ontology:measurement(tota_grid_13, total_war_reachability_boundary__contingent_reachability_reading, resistance(organizational), 1945, 0.58).
narrative_ontology:measurement(tota_grid_14, total_war_reachability_boundary__contingent_reachability_reading, resistance(organizational), 2026, 0.42).
narrative_ontology:measurement(tota_grid_15, total_war_reachability_boundary__contingent_reachability_reading, resistance(structural), 1945, 0.65).
narrative_ontology:measurement(tota_grid_16, total_war_reachability_boundary__contingent_reachability_reading, resistance(structural), 2026, 0.48).
narrative_ontology:measurement(tota_grid_17, total_war_reachability_boundary__contingent_reachability_reading, stakes_inflation(class), 1945, 0.38).
narrative_ontology:measurement(tota_grid_18, total_war_reachability_boundary__contingent_reachability_reading, stakes_inflation(class), 2026, 0.58).
narrative_ontology:measurement(tota_grid_19, total_war_reachability_boundary__contingent_reachability_reading, stakes_inflation(individual), 1945, 0.25).
narrative_ontology:measurement(tota_grid_20, total_war_reachability_boundary__contingent_reachability_reading, stakes_inflation(individual), 2026, 0.52).
narrative_ontology:measurement(tota_grid_21, total_war_reachability_boundary__contingent_reachability_reading, stakes_inflation(organizational), 1945, 0.52).
narrative_ontology:measurement(tota_grid_22, total_war_reachability_boundary__contingent_reachability_reading, stakes_inflation(organizational), 2026, 0.68).
narrative_ontology:measurement(tota_grid_23, total_war_reachability_boundary__contingent_reachability_reading, stakes_inflation(structural), 1945, 0.48).
narrative_ontology:measurement(tota_grid_24, total_war_reachability_boundary__contingent_reachability_reading, stakes_inflation(structural), 2026, 0.62).
narrative_ontology:measurement(tota_grid_25, total_war_reachability_boundary__contingent_reachability_reading, suppression(class), 1945, 0.32).
narrative_ontology:measurement(tota_grid_26, total_war_reachability_boundary__contingent_reachability_reading, suppression(class), 2026, 0.62).
narrative_ontology:measurement(tota_grid_27, total_war_reachability_boundary__contingent_reachability_reading, suppression(individual), 1945, 0.18).
narrative_ontology:measurement(tota_grid_28, total_war_reachability_boundary__contingent_reachability_reading, suppression(individual), 2026, 0.55).
narrative_ontology:measurement(tota_grid_29, total_war_reachability_boundary__contingent_reachability_reading, suppression(organizational), 1945, 0.48).
narrative_ontology:measurement(tota_grid_30, total_war_reachability_boundary__contingent_reachability_reading, suppression(organizational), 2026, 0.75).
narrative_ontology:measurement(tota_grid_31, total_war_reachability_boundary__contingent_reachability_reading, suppression(structural), 1945, 0.42).
narrative_ontology:measurement(tota_grid_32, total_war_reachability_boundary__contingent_reachability_reading, suppression(structural), 2026, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contingent_reachability_reading, 0.18).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel decomposes into three constraint stories, each instantiating a different reading. The three readings differ in their assertions about whether reachability is technologically reversible (contingent vs. contraction), whether its present level is temporary (contingent/contraction) or stable (dropping), and what this implies for strategy. This contingent-reachability reading asserts technology-dependent reversibility and justifies continued hedging; the contraction reading asserts permanent closure and justifies disarmament; the dropping reading asserts permanent presence and justifies accepted-risk management. Each reading has its own beneficiaries, its own suppression machinery, and its own theater dynamics. The network edges link all three as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, institutional, 0.15).
constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
