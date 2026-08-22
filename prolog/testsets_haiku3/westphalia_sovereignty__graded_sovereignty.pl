% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: Intervention Legitimacy by Capacity Deficit
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   The graded sovereignty reading instantiates a framework where territorial
 *   authority is calibrated to measurable state capacity. This reading
 *   contests the postcolonial absolute non-intervention norm by authorizing
 *   intervention in states deemed capacity-deficient by international
 *   evaluators. Unlike conditional responsibility (which keys intervention to
 *   mass atrocities—an external criterion of state failure), graded
 *   sovereignty keys it to institutional metrics: governance indices,
 *   rule-of-law scores, bureaucratic coherence, security-force capacity. This
 *   creates a de facto hierarchical state system where intervention
 *   legitimacy is a function of technical assessment. The reading vindicates
 *   the proposition that sovereignty must be earned through institutional
 *   performance, not merely claimed through territorial control.
 *   Beneficiaries are the capacity-evaluating authorities (who set metrics
 *   and earn legitimacy-granting power) and intervening coalitions (who gain
 *   justification for external governance). Victims are weak states (subject
 *   to conditional authority and external oversight) and their populations
 *   (subject to external governance decisions made without local voice). The
 *   constraint is actively enforced through development conditionality,
 *   peacekeeping operations, and the institutional architecture of
 *   international finance and governance standards.
 *
 * KEY AGENTS:
 *   - Capacity evaluating authorities (UN, World Bank, governance-index bodies): set metrics, render verdicts, enable intervention.
 *   - Intervention coalition states (Western democracies, regional powers): conduct interventions justified by capacity deficits, reshape governance, extract concessions.
 *   - Weak states subject to oversight (fragile, post-conflict, low-income states): lose autonomy, face conditionality, bear cost of external governance.
 *   - Subordinated populations (subject populations in intervened states): nominally protected, de facto governed by external agents, lose voice.
 *   - Development finance institutions (World Bank, IMF, regional banks): operationalize metrics through conditionality, embed the frame into structural adjustment.
 *   - Capacity-metric skeptics (Global South states, critical scholars, regional powers): excluded from metric-setting forums, contest the frame as encoding Western preferences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.72).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: Intervention Legitimacy by Capacity Deficit").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, 'a290c30c-e528-46a3-bb6b-a860b27a7d79').
narrative_ontology:cs_kernel_codification('a290c30c-e528-46a3-bb6b-a860b27a7d79', formalized).
narrative_ontology:cs_authority_grounding('a290c30c-e528-46a3-bb6b-a860b27a7d79', extraction).
narrative_ontology:cs_interpretation_layer_present('a290c30c-e528-46a3-bb6b-a860b27a7d79').
narrative_ontology:cs_reading_relation('a290c30c-e528-46a3-bb6b-a860b27a7d79', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('a290c30c-e528-46a3-bb6b-a860b27a7d79', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_axiom('a290c30c-e528-46a3-bb6b-a860b27a7d79', foundational, sovereignty_earned_through_institutional_performance).
narrative_ontology:cs_axiom_status(sovereignty_earned_through_institutional_performance, holdable).
narrative_ontology:cs_axiom_grounding('a290c30c-e528-46a3-bb6b-a860b27a7d79', sovereignty_earned_through_institutional_performance, empirically_contingent).
narrative_ontology:cs_axiom('a290c30c-e528-46a3-bb6b-a860b27a7d79', secondary, capacity_metrics_objective_and_neutral).
narrative_ontology:cs_axiom_status(capacity_metrics_objective_and_neutral, overridden).
narrative_ontology:cs_axiom_grounding('a290c30c-e528-46a3-bb6b-a860b27a7d79', capacity_metrics_objective_and_neutral, empirically_contingent).
narrative_ontology:cs_reference_frame('a290c30c-e528-46a3-bb6b-a860b27a7d79', capacity_indexed_sovereignty).
narrative_ontology:cs_drift_state('a290c30c-e528-46a3-bb6b-a860b27a7d79', post_afghanistan_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a290c30c-e528-46a3-bb6b-a860b27a7d79', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluating_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, intervention_coalition_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states_subject_to_oversight).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, subordinated_populations_under_trusteeship).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, subordinated_populations_under_trusteeship).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, local_elites_in_capacity_deficient_states).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, development_finance_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, local_elites_in_capacity_deficient_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International institutions (UN bodies, World Bank, IMF, OECD), Western state governments, and governance-assessment organizations (Transparency International, Mo Ibrahim Foundation, World Justice Project) that author capacity metrics, conduct assessments, and render verdicts on state capacity. They set criteria (rule of law, institutional autonomy, security-force discipline), conduct evaluations, and declare which states are capacity-deficient and thus subject to intervention legitimacy. They gain authority to authorize or criticize interventions, shape conditionality terms on development finance, and influence international reputation hierarchies.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluating_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Militarily and diplomatically capable states (primarily NATO members, regional powers like Australia, Turkey, and Gulf states) that conduct interventions and reconstruction operations in weak states. They justify interventions by pointing to capacity deficits diagnosed by evaluating authorities. They extract strategic positioning, resource concessions, client relationships with local elites, military base access, and moral authority (framed as humanitarian rather than strategic). They bear some costs (military casualties, intervention expenses) but shift most burden to weak states and their populations.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, intervention_coalition_states, beneficiary,
    powerful, generational, arbitrage, global).

% States assessed as capacity-deficient (fragile, post-conflict, low-governance-index states including Somalia, Democratic Republic of Congo, South Sudan, Afghanistan) that bear the primary costs of the graded sovereignty frame. They face capacity assessments that declare their governance inadequate, making them subject to conditionality on international finance, technical oversight by external experts, and vulnerability to intervention. They lose autonomy over domestic policy priorities (required institutional reforms often contradict local interests), must adopt external experts' governance models, and see their sovereignty delegitimated as 'capacity-deficient' rather than merely contested.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states_subject_to_oversight, payer,
    moderate, generational, trapped, national).

% Civilian populations in intervened states who are nominally the beneficiaries of intervention (protection from violence, reconstruction assistance, access to resources) but are de facto governed without voice by international administrators, peacekeepers, and external experts. In East Timor, Kosovo, and post-2001 Afghanistan, external authorities held executive power over domestic law, budget allocation, and security decisions. Populations could not vote out external authorities, appeal decisions, or organize alternative governance. They simultaneously benefit from relative security and suffer from external governance decisions made without their participation or consent.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, subordinated_populations_under_trusteeship, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, subordinated_populations_under_trusteeship, beneficiary).

% Existing power-holders (military officers, business elites, traditional authorities, political parties) in weak states who navigate cooptation: they gain international recognition and access to reconstruction resources if they align with external authorities, but lose real power over domestic decisions. They become interlocutors between external authorities and populations, buffering external agents from local political resistance while implementing external priorities. They can extract rents from reconstruction funding and development projects but cannot chart independent policy. Their survival depends on being the external authority's preferred partner.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, local_elites_in_capacity_deficient_states, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, local_elites_in_capacity_deficient_states, payer).

% States (Non-Aligned Movement members, China, Russia, regional powers), scholars (postcolonial theorists, critical international relations scholars), and advocates (Global South development organizations, state sovereignty advocates) who argue that capacity metrics encode Western institutional preferences and that graded sovereignty is a neocolonial framework disguising power politics as technical assessment. They contend that capacity metrics penalize alternative institutional designs (Islamic law, customary governance, decentralized authority) and advantage Western-style bureaucratic institutions. They are systematically excluded from metric-setting forums, international governance bodies, and intervention-authorization processes. Their voice appears only as dissent in UN General Assembly votes (non-binding) while capacity assessments and intervention authorizations are made in UN Security Council (binding) and development finance institutions (enforcement power).
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_metric_skeptics, excluded,
    moderate, biographical, constrained, global).

% World Bank, IMF, regional development banks (Asian Development Bank, African Development Bank, Inter-American Development Bank) that operationalize capacity metrics through structural adjustment programs and conditionality. They make loans and grants contingent on governance reforms: privatization, institutional strengthening, civil service professionalization, rule-of-law improvements defined by external experts. They collect authority over domestic economic and institutional policy in weak states. They extract fees, interest, and policy leverage while distributing the adjustment burden to weak-state populations (reduced public services, labor-market deregulation, loss of locally-controlled resources).
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, development_finance_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, development_finance_institutions, beneficiary).

% International law scholars, human rights organizations, some UN bodies (Human Rights Council, Independent fact-finding missions) that analyze the constraint's operation, measure whether capacity metrics are applied consistently, document outcomes of capacity-deficit-justified interventions, and publish critical accounts of whether graded sovereignty functions as claimed. They lack enforcement power but can produce evidence, shape academic discourse, and influence non-binding UN resolutions and soft law.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, neutral_observer_international_law_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, capacity_evaluating_authorities).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a decision rule for when external authorities are permitted to override the norm of non-interference: if a state's institutional capacity is assessed as deficient (measured by governance indices, institutional autonomy, security-force discipline, rule-of-law function), external intervention and governance transfer are legitimated as assistance rather than conquest. Solves the coordination problem of preventing either pure non-intervention (leaving humanitarian crises and state collapse ungoverned) or unilateral intervention (enabling great-power occupation under guise of humanitarian concern). The capacity-metric framework provides a supposedly objective assessment that constrains discretionary intervention.
% TRANSFER_FUNCTION: Transfers authority and control over domestic governance from weak states to external evaluators and intervening powers. Moves domestic policy priorities (institutional design, economic policy, security arrangements) from local to external actors. Extracts strategic positioning, resource concessions, institutional reforms favoring investor interests, and client relationships with local elites. Moves the revenue from reconstruction funding largely back to intervening states' contractors and external experts rather than to local populations.
% ABSENT_VOICES: Populations in weak states who would contest the capacity verdicts that authorize external governance (they do not vote on metrics or intervention authorization); scholars and policymakers from Global South and critical traditions who argue capacity metrics encode Western preferences (excluded from metric-setting forums); regional powers who would prefer non-interventionist norms (marginalized in Western-dominated UN bodies); authoritarian states who oppose intervention-legitimacy claims generally (excluded from Security Council decision-making); alternative institutional traditions (Islamic law, customary governance, decentralized authority) that would challenge the measurement frame (not represented in governance-index design).
% DISAPPEARANCE_RATIONALE: If graded sovereignty vanished and territorial sovereignty were treated as categorical (either states have it or they don't, not on a spectrum), the international system would lose its primary legitimation framework for external governance. Humanitarian crises in weak states would persist without authorized external response; regional powers would fill governance vacuums; great powers would fall back on naked strategic positioning rather than reformist narrative; development finance would lose its conditionality mechanism; institutional reconstruction in post-conflict states would fall to regional actors or remain ungoverned. The norm structure undergirding the entire system of development conditionality and external institutional tutelage would require reconstruction.
% FOUNDING_PROBLEM: State collapse, institutional failure, and the inability of weak states to protect populations from violence created humanitarian crises and regional instability. Somalia's state collapse, Sierra Leone's civil war, Kosovo's ethnic cleansing, and Afghanistan's Taliban rule demonstrated that some states could not provide basic security or governance. The international system needed a framework that justified intervention without abandoning the principle of sovereignty. Absolute non-intervention left failed states ungoverned; unconditional intervention legitimacy enabled colonialism. The graded sovereignty frame promised a middle path: capacity deficits trigger intervention and external assistance, improving state capacity toward the point where external withdrawal became possible.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian agencies and development institutions attest that state failure and institutional weakness create genuine crises requiring external response, documented in UN reports on Somalia, Sierra Leone, and Afghanistan. However, Global South states, international relations scholars (Stephen Krasner, Thandeka Gumede, Fantu Cheru), and post-intervention assessments (Afghanistan Institute, International Crisis Group) document that capacity-deficit-justified interventions have not reliably produced institutional improvements, that interventions have often entrenched dependency on external management rather than building capacity, and that populations report lower agency under external governance than under weak but locally-accountable states. The problem (state failure, humanitarian need) is empirically live; the solution (external governance to build capacity) is heavily contested by those subject to it and by evidence-based assessment of intervention outcomes.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.48) at the constraint's inception because the humanitarian crisis framing and the promise of reconstruction-assistance coordination are genuinely coordinating. As the constraint matures, extractiveness rises (0.68 at interval end) because the capacity-metric framework becomes institutionalized, external agents consolidate authority over domestic policy, and the initial humanitarian justification is layered over with resource extraction and strategic positioning. Suppression is consistently high (0.55→0.72) because weak states cannot exit the assessment without facing isolation, and their populations cannot voice objections without risking their state's capacity rating (voice becomes a liability). Theater ratio rises (0.38→0.58) because governance reforms increasingly become performative—external agents and local elites enact formal institutional changes to satisfy metric requirements while real power and resource control remain externalized. The measurement series tracks a transformation from genuine coordination (humanitarian response to state failure) to institutionalized extraction (graded sovereignty becomes a tool for subordinating weak states and populations). All metrics share the same time grid; each point reports a measurement or basis. The cyclical tension between humanitarian legitimacy and strategic extraction is visible in the theater-ratio trajectory—as extraction becomes more obvious, more performative activity is required to sustain the humanitarian frame.
 *
 * PERSPECTIVAL GAP:
 *   From the capacity-evaluating authorities' and intervening states' seats, the constraint is genuine coordination: they are managing collective action problems (humanitarian crisis, regional instability, institutional failure) and providing development assistance to weak states. They author and believe they operate transparent, meritocratic capacity metrics. From the weak states' seats, the constraint operates as enforced hierarchical subordination: their sovereignty is graded as deficient (a status no state voluntarily accepts), their domestic policy is subject to external veto, and their populations lose voice in governance. The evaluating authorities see coordination and remedy; the weak states see extraction and subordination. The engine computes this seat divergence from the structural data: the beneficiary seats (agenda-setters, interventionists) show low directionality (d near 0—they benefit); the victim seats (weak states, subordinated populations) show high directionality (d near 1—they are targets). The commentary explains why the same constraint produces opposite perceptions across seats: institutional power asymmetry, unilateral metric-setting, and the conditionality machinery create genuine structural divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Capacity-evaluating authorities have high institutional power, arbitrage-grade exit options (they can adopt alternative frameworks if capacity metrics fail), and global scope. They derive d near 0.1 (full beneficiary)—they set the rules, earn legitimacy-granting authority, and collect strategic influence. Intervening coalition states have institutional power, arbitrage exit (they can decline to intervene), and global scope; they derive d near 0.15 (beneficiary)—they gain intervention legitimacy and strategic position. Weak states have moderate power (enough to matter regionally but not globally), trapped or severely constrained exit (refusal to submit to capacity metrics means exclusion from international finance and development), and national scope. They derive d near 0.85 (full target)—they bear the costs of external governance, external veto over domestic policy, and loss of strategic autonomy. Subordinated populations have powerless status, identity-locked exit (they cannot physically leave; their identity is constituted in the state being governed), and local scope. They derive d near 0.95 (deepest target)—they lose voice in decisions about their own governance and are governed by external agents they did not choose. Local elites sit near symmetric (d ≈ 0.45) because they benefit from external validation and resources while losing real authority—they are coopted targets, neither fully extracted from nor fully benefiting. No directionality overrides are needed; the structural data derives accurate d values without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits textbook mandatrophy dynamics: the founding problem (state failure, humanitarian crisis) is real at interval start, but the remedy (external capacity-building under graded sovereignty) has not delivered the promised outcomes. Interventions justified by capacity deficits in East Timor, Kosovo, Sierra Leone, and Afghanistan have not reliably produced the institutional improvements their capacity metrics predicted. Populations subject to external governance report lower agency and voice than under pre-intervention domestic governance, even when material welfare improves. The capacity metrics themselves have become ritualized—states adopt metric-visible reforms (formal institutions, legal codes) without functional change, and the evaluating authorities accept metric compliance as evidence of capacity improvement. The constraint is increasingly maintained not by its coordination function (which has largely atrophied) but by the institutional interests of the evaluators and interveners: the metrics justify their authority; the interventions justify the metrics; the conditionality sustains the framework even as evidence of effectiveness declines. Theater ratio rising to 0.58 is the diagnostic signal: more than half the enforcement activity is now performative rather than functional. The mandate—to build state capacity and enable populations to govern themselves—has been outlived by the institutional machinery maintaining the framework. The verdict is not that the constraint should be abandoned (state failure remains real), but that its operation has transformed from coordination to extraction, and the founding problem's persistent reality has become a justification for perpetuating the extraction rather than solving the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metric_validity,
    'Do the capacity metrics (governance indices, rule-of-law scores, institutional assessments) actually measure a state''s ability to protect populations and provide services, or do they measure cultural and institutional conformity to Western models?',
    'Longitudinal analysis comparing capacity metric scores with actual outcomes (security, welfare, institutional effectiveness, population welfare) across interventions; assessment of whether states with high metric scores but non-Western institutional designs perform comparably to Western-modeled states.',
    'If metrics measure actual capacity, the intervention frame is justified; if they measure conformity to Western models, the constraint is pure cultural extraction disguised as technical assessment, reclassifying from tangled rope (legitimate coordination + extraction) to snare (pure extraction with coordination cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metric_validity, empirical, 'Whether capacity metrics measure functional capacity or institutional conformity to Western models.').

omega_variable(
    intervention_outcome_accountability,
    'Have interventions justified by capacity deficits actually improved state capacity, institutional effectiveness, and population welfare at intervention-end or post-withdrawal?',
    'Comparative analysis of pre- and post-intervention metrics and outcomes across multiple cases (East Timor, Kosovo, Sierra Leone, Afghanistan, Libya); assessment of whether improvements persist after intervening forces withdraw or if reversion occurs.',
    'If interventions improve outcomes, the founding problem diagnosis is validated and the constraint''s extraction can be framed as the cost of remedy; if outcomes regress post-withdrawal or fail to improve, the mandatrophy reading dominates—the constraint persists to serve the interests of evaluators and interveners rather than to solve the stated problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_outcome_accountability, empirical, 'Whether interventions justified by capacity deficits achieve their stated outcomes.').

omega_variable(
    metric_bias_across_regions,
    'Are capacity metrics applied consistently across regions and power levels, or do states aligned with metric-setting authorities receive more favorable ratings than geopolitically opposed states with similar objective capacity?',
    'Audit of capacity metric assignments across all states, controlling for objective institutional metrics (security, bureaucratic coherence, rule-of-law functioning); statistical analysis of regional and geopolitical bias in metric ratings.',
    'If metrics are consistently applied, the constraint''s extraction is bounded by technical assessment; if metrics show systematic bias favoring allied states, the constraint is operating as a tool of geopolitical subordination, reclassifying toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_bias_across_regions, empirical, 'Whether capacity metrics are applied with consistent standards across all states or exhibit geopolitical bias.').

omega_variable(
    readings_kernel_foreclosure,
    'Can absolute_non_intervention and graded_sovereignty coexist within a single framework, or does grading sovereignty into a scalar metric necessarily foreclose categorical territorial inviolability?',
    'Conceptual analysis of the core premises: if sovereignty is either inviolable or capacity-indexed, can it be both? Can a framework hold that territorial authority is categorical in principle but scalar in practice?',
    'If the readings foreclose each other, the kernel contest is genuine logical opposition, not mere disagreement; the committer choice is not neutral but commits to a foundational view of statehood. If they can coexist (categorical principle, scalar application), the constraint sits at a lower-level policy choice and the kernel contest is less structurally deep.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readings_kernel_foreclosure, conceptual, 'Whether absolute_non_intervention and graded_sovereignty logically foreclose each other or can coexist in a single framework.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of weak-state capacity-metric skepticism maintained by structural exclusion from metric-setting forums and international institutions, or have weak-state governments internalized the capacity-deficit framing and now discipline their own populations rather than externally-applied discipline?',
    'Post-exit suppression trajectory: if weak states withdraw from conditionality regimes and suppression persists in the form of self-imposed metric conformity pressures, suppression is partially internalized; if suppression dissolves, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than measured—weak states and their populations carry the suppression with them even if external pressure relaxes. If structural, removing external pressure would allow suppression to decay, suggesting the constraint could be destabilized by changing institutional relationships.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of capacity-metric skepticism is structurally imposed or internalized by weak states.').

omega_variable(
    kernel_reading_provenance,
    'Is the graded_sovereignty reading indigenous to international law and political theory traditions, or was it synthesized by Western institutional actors seeking to legitimate intervention?',
    'Historical and textual analysis tracing the graded sovereignty reading''s appearance in international law, development economics, and political theory; attribution of authorship and institutional provenance.',
    'If graded sovereignty is a native tradition, the reading competes on equal footing with alternatives; if it was synthesized by institutional actors with stakes in intervention, the reading is a constructed-for-benefit framework and the constraint operates with less legitimacy than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_provenance, conceptual, 'Whether the graded_sovereignty reading is a native international law tradition or a constructed framework serving institutional interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(west_tr_t0, observed).
narrative_ontology:measurement(west_tr_t8, westphalia_sovereignty__graded_sovereignty, theater_ratio, 8, 0.44).
narrative_ontology:measurement_basis(west_tr_t8, observed).
narrative_ontology:measurement(west_tr_t16, westphalia_sovereignty__graded_sovereignty, theater_ratio, 16, 0.5).
narrative_ontology:measurement_basis(west_tr_t16, observed).
narrative_ontology:measurement(west_tr_t24, westphalia_sovereignty__graded_sovereignty, theater_ratio, 24, 0.55).
narrative_ontology:measurement_basis(west_tr_t24, observed).
narrative_ontology:measurement(west_tr_t32, westphalia_sovereignty__graded_sovereignty, theater_ratio, 32, 0.57).
narrative_ontology:measurement_basis(west_tr_t32, observed).
narrative_ontology:measurement(west_tr_t40, westphalia_sovereignty__graded_sovereignty, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(west_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(west_be_t0, observed).
narrative_ontology:measurement(west_be_t8, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(west_be_t8, observed).
narrative_ontology:measurement(west_be_t16, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(west_be_t16, observed).
narrative_ontology:measurement(west_be_t24, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(west_be_t24, observed).
narrative_ontology:measurement(west_be_t32, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(west_be_t32, observed).
narrative_ontology:measurement(west_be_t40, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(west_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(west_su_t0, observed).
narrative_ontology:measurement(west_su_t8, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(west_su_t8, observed).
narrative_ontology:measurement(west_su_t16, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(west_su_t16, observed).
narrative_ontology:measurement(west_su_t24, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(west_su_t24, observed).
narrative_ontology:measurement(west_su_t32, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(west_su_t32, observed).
narrative_ontology:measurement(west_su_t40, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(west_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__graded_sovereignty, 0.18).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, humanitarian_intervention_legitimacy).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, development_conditionality).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, state_fragility_assessment_framework).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, international_trusteeship_systems).

% DUAL FORMULATION NOTE:
% The graded_sovereignty reading decomposes from the westphalia_sovereignty kernel alongside absolute_non_intervention and conditional_responsibility. Each reading instantiates a different constraint with distinct ε values, beneficiary/victim structures, and types. Graded sovereignty is substantially extractive (ε ≈ 0.68) because it converts humanitarian coordination into institutionalized subordination of weak states; absolute_non_intervention is low-extraction (ε ≈ 0.15) because it preserves categorical sovereignty without hierarchical tiering; conditional_responsibility is moderate-extraction (ε ≈ 0.55) because it permits intervention for specific atrocities but does not institutionalize general capacity evaluation. The three readings are linked by network.affects_constraints: conditional_responsibility influences graded_sovereignty (atrocity-response establishes intervention precedent that enables capacity-based justifications), and both influence humanitarian_intervention_legitimacy (which aggregates the various readings into a system-level norm).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
