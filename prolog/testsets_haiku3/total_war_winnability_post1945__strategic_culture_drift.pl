% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War Winnability Post-1945: Strategic Culture Drift Reading
 *   domain: international_relations/strategic_studies/commitment_systems
 *
 * SUMMARY:
 *   The strategic culture reading of total war winnability post-1945 treats
 *   total war as a capacity that remains physically and logically reachable
 *   but has been removed from elite discourse through institutional drift,
 *   intellectual fashion, and the consolidation of limited-war frameworks by
 *   defense intellectuals. The kernel is contested: other readings claim
 *   total war is normatively illegitimate (via law) or structurally
 *   impossible (via nuclear weapons). This reading asserts the core
 *   constraint is ideational — the capacity persists, but the intellectual
 *   infrastructure that would justify its contemplation has atrophied and is
 *   now maintained theatrically by institutions invested in the limited-war
 *   paradigm. The constraint extracts legitimacy and institutional authority
 *   FROM military planners and strategic flexibility AND TO limited-war
 *   intellectuals and status-quo powers. The theater ratio rises sharply over
 *   time, indicating that maintaining the taboo requires increasing
 *   performative effort as the founding problem (nuclear deterrence) becomes
 *   less salient and the constraint's true function (protecting a particular
 *   strategic narrative) becomes more visible.
 *
 * KEY AGENTS:
 *   - limited_war_strategic_intellectuals — institutional beneficiaries, control the discourse boundaries, maintain the taboo performatively
 *   - military_operational_planners — organizational payers, identity-locked into the limited-war framework, strategically constrained
 *   - political_leadership — institutional actors, partly beneficiaries (legitimacy) and partly payers (reduced optionality)
 *   - revisionist_state_actors — excluded from the framework-setting conversation; would benefit from total-war capacity but are systematically delegitimized if they propose it
 *   - historical_precedent_custodians — institutional beneficiaries, maintain narrative coherence via selective historical interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.38).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.62).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.38).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War Winnability Post-1945: Strategic Culture Drift Reading").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies/commitment_systems").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__strategic_culture_drift).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, 'f17930d4-796f-4dc9-8a12-21290206a325').
narrative_ontology:cs_kernel_codification('f17930d4-796f-4dc9-8a12-21290206a325', distributed).
narrative_ontology:cs_authority_grounding('f17930d4-796f-4dc9-8a12-21290206a325', extraction).
narrative_ontology:cs_interpretation_layer_present('f17930d4-796f-4dc9-8a12-21290206a325').
narrative_ontology:cs_reading_relation('f17930d4-796f-4dc9-8a12-21290206a325', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('f17930d4-796f-4dc9-8a12-21290206a325', total_war_winnability_post1945__structural_contraction_reading, influences).
narrative_ontology:cs_axiom('f17930d4-796f-4dc9-8a12-21290206a325', foundational, strategic_culture_evolves_through_learning).
narrative_ontology:cs_axiom_status(strategic_culture_evolves_through_learning, holdable).
narrative_ontology:cs_axiom_grounding('f17930d4-796f-4dc9-8a12-21290206a325', strategic_culture_evolves_through_learning, conventional).
narrative_ontology:cs_axiom('f17930d4-796f-4dc9-8a12-21290206a325', foundational, institutional_discourse_governs_state_optionality).
narrative_ontology:cs_axiom_status(institutional_discourse_governs_state_optionality, holdable).
narrative_ontology:cs_axiom_grounding('f17930d4-796f-4dc9-8a12-21290206a325', institutional_discourse_governs_state_optionality, instrumental).
narrative_ontology:cs_reference_frame('f17930d4-796f-4dc9-8a12-21290206a325', total_war_as_rational_option_post1945).
narrative_ontology:cs_drift_state('f17930d4-796f-4dc9-8a12-21290206a325', contemporary_strategic_discourse_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f17930d4-796f-4dc9-8a12-21290206a325', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_strategic_intellectuals).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, strategic_planners_requiring_total_war_option).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, political_leadership).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, historical_precedent_custodians).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, military_operational_planners).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, political_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defense think tanks, military academies, and strategic studies programs have built careers and institutional legitimacy on limited-war doctrine. They define acceptable strategic discourse, teach the next generation that total war is unthinkable, and control which questions are asked. They accumulate intellectual authority and prestige from the constraint's maintenance.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, limited_war_strategic_intellectuals, agenda_setter,
    institutional, generational, arbitrage, global).

% Staff officers and war planners operate within narrow discourse constraints. They understand total war remains technically reachable but cannot publicly propose it without career consequences and institutional exile. Their strategic toolkit has been narrowed; certain scenarios are removed from planning discourse not by impossibility but by institutional taboo.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, military_operational_planners, payer,
    organized, biographical, identity_locked, national).

% Political leaders inherit the limited-war framing as the only legitimate option. They are constrained to seek limited objectives with limited means, even in existential scenarios where total commitment might be strategically rational. They benefit from the predictability and legitimacy of bounded conflict but bear the cost of removed strategic optionality.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, political_leadership, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__strategic_culture_drift, political_leadership, beneficiary).

% States that might benefit from total-war capacity or its credible threat are excluded from serious discourse about it. They can observe Western self-restraint, but any proposal for total mobilization is immediately delegitimized. Their voice is shut out of the framework-setting conversation by the same machinery that maintains the taboo.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, revisionist_state_actors, excluded,
    powerful, generational, trapped, global).

% Institutions and scholars invested in the narrative that total war has been transcended through learning and cultural evolution. They maintain intellectual coherence by controlling which historical lessons are cited and which are forgotten. The piton persists theatrically through their constant reaffirmation that humanity has moved beyond total war.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, historical_precedent_custodians, beneficiary,
    institutional, generational, arbitrage, global).

% Military doctrine institutions preserve and update operational guidance. They implement the constraint's directives and are the institutional memory keepers. They can detect when the constraint requires increasing performative effort to maintain.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, operational_doctrine_keepers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__strategic_culture_drift, limited_war_strategic_intellectuals).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__strategic_culture_drift, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates expectations about escalation ceilings: by treating total war as tabooed, great powers can assume conflict will remain bounded, reducing uncertainty in crisis and tempering arms-race spirals.
% TRANSFER_FUNCTION: Transfers strategic legitimacy and intellectual authority FROM military planners and revisionist actors (whose total-war options are delegitimized) TO limited-war intellectuals and status-quo powers (who consolidate discourse control and narrative authority).
% ABSENT_VOICES: Strategic planners operating in classified settings who understand total war's technical reachability but cannot speak against the taboo without career consequences. Revisionist state actors who would benefit from total-war capacity as a credible option but are systematically excluded from legitimate strategic discourse. Military historians and strategists who notice the constraint's atrophy but are marginalized as 'dangerously anachronistic'. Operational officers who recognize the constraint as theatrical but enforce it theatrically themselves.
% DISAPPEARANCE_RATIONALE: If the taboo disappeared and total war re-entered elite discourse, some arrangements would shift: deterrence calculations would include the possibility of total mobilization; military planning would expand its scenarios; the intellectual prestige of limited-war frameworks would erode. But whether the world would substantially rearrange depends on whether the underlying capacity still exists — if it has atrophied through institutional forgetting, lifting the discourse taboo would not restore capacity rapidly. Alternatively, if the constraint is already hollow and military planners think in total-war terms in classified rooms, its public disappearance would only formalize a hidden reality.
% FOUNDING_PROBLEM: Post-1945 strategists faced the problem of rational warfare under nuclear deterrence: how to fight without triggering mutual annihilation. Total war became strategically irrational under this constraint, so the founding problem was to build strategic culture around bounded conflict and limited objectives.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Cold War strategy (Bernard Brodie, Thomas Schelling, Henry Kissinger, Paul Nitze) attest the founding problem was live and urgent post-1945. Military doctrine papers from the era (declassified) confirm the driving problem was how to maintain military capacity and deterrence under nuclear constraints. The corroboration is strong for the historical fact. What is CONTESTED: whether the founding problem remains live. Defense strategists argue nuclear weapons persist and therefore limited-war discipline remains essential. Military planners and some strategic theorists argue the problem is solved (nuclear arsenals are stabilized; deterrence is institutionalized) and the constraint now persists from institutional habit rather than necessity. No external, disinterested party attests the problem is still live in its original form.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, contested).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).
:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.38 at interval end) because the constraint's main function is narrative closure rather than direct resource capture — the beneficiaries gain intellectual authority and institutional prestige, not rents in the monetary sense. The suppression is substantial (0.62) because maintaining the taboo requires actively excluding alternative strategic discourses and delegitimizing actors who propose total war options. Theater ratio rises from 0.38 to 0.71 over the 80-year interval: early in the Cold War, the limited-war framework had genuine functional utility (nuclear deterrence made total war irrational), so enforcement was partly structural. By the end of the interval, the founding problem is partially resolved (nuclear weapons are stabilized; the existential crisis has cooled), yet the constraint persists and requires increasing institutional theater to maintain — reaffirmations that we have transcended total war, historical narratives emphasizing the progress of strategic culture, institutional gatekeeping against heterodox voices. Accessibility collapse is moderate (0.45 at interval end) because total war remains logically and technically reachable — it is not physically impossible, only discoursively removed. Resistance is substantial (0.58) because military strategists and some intellectuals continue to notice the constraint's atrophy and resist its most performative iterations. The coercion grid shows organizational-level suppression rising (the military bureaucracy intensifies gatekeeping) while individual-level resistance persists and even grows slightly — military officers at individual levels understand the constraint is theatrical but cannot speak against it without career consequences.
 *
 * PERSPECTIVAL GAP:
 *   The constraint appears to the limited-war intellectuals as beneficial learning and necessary restraint; to military planners as ideological constraint; to political leadership as an offer of legitimacy in exchange for strategic bottlenecking; to revisionist powers as delegitimization machinery keeping them out of the frame. The three authorized seats (intellectuals, planners, political leaders) will compute different types when the engine applies the directionality derivation — that divergence is the constraint's signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for limited-war intellectuals: they benefit from the constraint (high institutional prestige, control of discourse, vindication of their intellectual project), so d should be near 0.0 (full beneficiary end). They have arbitrage exit (they could theoretically shift to a different strategic narrative, though incentives against this are strong). Directionality for military planners: they bear the cost of the constraint (strategic optionality removed, identity fusion with limited-war doctrine), so d should approach 1.0 (full target end). Their exit is identity-locked — they are fused to the limited-war framework through career and professional identity, making exit deeply costly. Directionality for political leadership: symmetric to near-beneficiary (they get legitimacy for restraint, but lose strategic optionality in crises), so d should be near 0.3-0.4. The constraint's scope is global (affects all great-power conflict conceptualization), which the engine will amplify effective extraction moderately. No directionality overrides are needed — the structural derivation should capture the seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was live post-1945 (nuclear deterrence genuinely made total war irrational) and is now dead (nuclear weapons remain but deterrence is stabilized, and total war's irrationality is no longer actively contested — it is accepted as settled fact). The constraint persists not because the founding problem requires it but because institutions have become invested in its perpetuation. The theater ratio rising sharply (0.38 → 0.71) indicates increasing performative effort. The suppression requirement remaining stable while extractiveness plateaus suggests the constraint is held through active gatekeeping, not through structural inevitability. This is the Piton signature: atrophied primary function, persistence via institutional inertia and theatrical reaffirmation, no concentrated beneficiary wealthy enough to maintain it alone (limited-war intellectuals benefit, but diffusely, through prestige and authority rather than rents), cost to fix exceeding cost to maintain for whoever could fix it (military leadership could theoretically re-open total-war discourse, but the reputational cost of being seen as warmongering exceeds any strategic benefit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_forgetting_vs_sustained_capacity,
    'Has total war capacity actually atrophied through institutional forgetting, or do military planning institutions maintain it in classified settings while keeping it out of public discourse?',
    'Declassification of military doctrine papers, war-planning simulations, and strategic assessments over the 80-year interval. Evidence of active total-war planning in closed settings would indicate the constraint is purely discursive; absence of such planning would confirm institutional forgetting.',
    'If capacity is maintained in secret, the constraint is a thin layer of rhetoric over a stable underlying capacity — the piton is shallow. If capacity has genuinely atrophied, the constraint is deeper — the cognitive apparatus for total-war thinking has been lost, making reconstitution difficult even if discourse rules loosened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_forgetting_vs_sustained_capacity, empirical, 'Whether the constraint is discursive-only or reflects genuine atrophy of planning capacity').

omega_variable(
    performance_vs_real_enforcement,
    'To what extent is the rising theater ratio (0.38 → 0.71) driven by conscious institutional performance to maintain a narrative, versus by the natural evolution of academic and strategic discourse?',
    'Qualitative analysis of strategic literature, doctrine changes, and institutional emphasis across the interval. Evidence of deliberate gatekeeping (rejection of heterodox papers, career consequences for total-war proposals, historical reframing) would support conscious performance; absent such evidence would support natural discourse drift.',
    'If performance is conscious and deliberate, the constraint is a snare masquerading as natural evolution — it involves active suppression of alternatives. If it is natural discourse drift, the constraint is a weaker piton — no concentrated actor is maintaining it, just institutional inertia and fashion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(performance_vs_real_enforcement, conceptual, 'Whether rising theater is evidence of conscious performance or natural discourse evolution').

omega_variable(
    kernel_reading_contingency,
    'Is the strategic culture reading the correct framing of the kernel, or does one of the sibling readings (normative or structural) better explain why total war remains absent from elite discourse?',
    'Examine the empirical record: (1) if total war is normatively illegitimate (normative reading), we should see principled objections based on law and humanity; (2) if total war is structurally impossible (structural reading), we should see technical and military arguments about nuclear deterrence and escalation dynamics; (3) if strategic culture drift is correct, we should see institutional and discursive reasons (fashion, institutional authority, career incentives). Each produces a different discourse signature.',
    'If a sibling reading is more accurate, the constraint''s classification and beneficiary structure would shift. A structural reading would make this a mountain (total war is impossible, not chosen). A normative reading would make this a tangled rope (normative framework + institutional extraction). Strategic culture drift (this reading) makes it a piton (atrophied capacity + theatrical maintenance).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether this reading or a sibling reading better explains the kernel''s constraint').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.62) structural (institutional gatekeeping, career consequences, journal rejection) or internalized (military officers have genuinely absorbed the belief that total war is unthinkable)?',
    'Qualitative interviews with military strategists and classified-setting planners: post-constraint removal, how much of the suppression persists? Structural suppression disappears when gatekeeping ends; internalized suppression persists after the constraint is removed because the officer carries it with them.',
    'If suppression is mostly structural, the constraint''s effective force is the gatekeeping machinery; if it is internalized, the constraint is deeper — it has rewritten what strategists find cognitively available as options. Internalized suppression suggests the piton is more entrenched than institutional inertia alone would indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression is structural gatekeeping or internalized belief').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0, 0.38).
narrative_ontology:measurement(tota_tr_t10, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 10, 0.45).
narrative_ontology:measurement(tota_tr_t20, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 20, 0.52).
narrative_ontology:measurement(tota_tr_t40, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 40, 0.64).
narrative_ontology:measurement(tota_tr_t60, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 60, 0.69).
narrative_ontology:measurement(tota_tr_t80, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 80, 0.71).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(tota_be_t10, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(tota_be_t20, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(tota_be_t40, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(tota_be_t60, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(tota_be_t80, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(tota_su_t10, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(tota_su_t20, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(tota_su_t40, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(tota_su_t60, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(tota_su_t80, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 80, 0.62).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=80
narrative_ontology:measurement(tota_grid_01, total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse(class), 0, 0.35).
narrative_ontology:measurement(tota_grid_02, total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse(class), 80, 0.3).
narrative_ontology:measurement(tota_grid_03, total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse(individual), 0, 0.28).
narrative_ontology:measurement(tota_grid_04, total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse(individual), 80, 0.22).
narrative_ontology:measurement(tota_grid_05, total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse(organizational), 0, 0.48).
narrative_ontology:measurement(tota_grid_06, total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse(organizational), 80, 0.42).
narrative_ontology:measurement(tota_grid_07, total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse(structural), 0, 0.52).
narrative_ontology:measurement(tota_grid_08, total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse(structural), 80, 0.48).
narrative_ontology:measurement(tota_grid_09, total_war_winnability_post1945__strategic_culture_drift, resistance(class), 0, 0.48).
narrative_ontology:measurement(tota_grid_10, total_war_winnability_post1945__strategic_culture_drift, resistance(class), 80, 0.44).
narrative_ontology:measurement(tota_grid_11, total_war_winnability_post1945__strategic_culture_drift, resistance(individual), 0, 0.38).
narrative_ontology:measurement(tota_grid_12, total_war_winnability_post1945__strategic_culture_drift, resistance(individual), 80, 0.35).
narrative_ontology:measurement(tota_grid_13, total_war_winnability_post1945__strategic_culture_drift, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(tota_grid_14, total_war_winnability_post1945__strategic_culture_drift, resistance(organizational), 80, 0.58).
narrative_ontology:measurement(tota_grid_15, total_war_winnability_post1945__strategic_culture_drift, resistance(structural), 0, 0.68).
narrative_ontology:measurement(tota_grid_16, total_war_winnability_post1945__strategic_culture_drift, resistance(structural), 80, 0.65).
narrative_ontology:measurement(tota_grid_17, total_war_winnability_post1945__strategic_culture_drift, stakes_inflation(class), 0, 0.42).
narrative_ontology:measurement(tota_grid_18, total_war_winnability_post1945__strategic_culture_drift, stakes_inflation(class), 80, 0.38).
narrative_ontology:measurement(tota_grid_19, total_war_winnability_post1945__strategic_culture_drift, stakes_inflation(individual), 0, 0.32).
narrative_ontology:measurement(tota_grid_20, total_war_winnability_post1945__strategic_culture_drift, stakes_inflation(individual), 80, 0.28).
narrative_ontology:measurement(tota_grid_21, total_war_winnability_post1945__strategic_culture_drift, stakes_inflation(organizational), 0, 0.68).
narrative_ontology:measurement(tota_grid_22, total_war_winnability_post1945__strategic_culture_drift, stakes_inflation(organizational), 80, 0.72).
narrative_ontology:measurement(tota_grid_23, total_war_winnability_post1945__strategic_culture_drift, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(tota_grid_24, total_war_winnability_post1945__strategic_culture_drift, stakes_inflation(structural), 80, 0.62).
narrative_ontology:measurement(tota_grid_25, total_war_winnability_post1945__strategic_culture_drift, suppression(class), 0, 0.48).
narrative_ontology:measurement(tota_grid_26, total_war_winnability_post1945__strategic_culture_drift, suppression(class), 80, 0.5).
narrative_ontology:measurement(tota_grid_27, total_war_winnability_post1945__strategic_culture_drift, suppression(individual), 0, 0.35).
narrative_ontology:measurement(tota_grid_28, total_war_winnability_post1945__strategic_culture_drift, suppression(individual), 80, 0.36).
narrative_ontology:measurement(tota_grid_29, total_war_winnability_post1945__strategic_culture_drift, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(tota_grid_30, total_war_winnability_post1945__strategic_culture_drift, suppression(organizational), 80, 0.66).
narrative_ontology:measurement(tota_grid_31, total_war_winnability_post1945__strategic_culture_drift, suppression(structural), 0, 0.58).
narrative_ontology:measurement(tota_grid_32, total_war_winnability_post1945__strategic_culture_drift, suppression(structural), 80, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__strategic_culture_drift, 0.12).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__structural_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'total_war_winnability_post1945' kernel. The kernel contest involves three readings: strategic_culture_drift (this story — ideational shift in strategic culture), normative_reading_drop (legal/normative prohibition), and structural_contraction_reading (nuclear weapons make it physically impossible). The three readings share the kernel but decompose into structurally distinct constraints with different ε values, beneficiary/victim structures, and classifications. Sibling stories are linked via network.affects_constraints; each story instantiates one reading as a clean constraint (Rule 1 compliance). The divergence in readings is routed to omega variables in each story documenting the kernel contest (Rule 2). This story claims strategic culture drift (piton); the structural reading would be mountain (total war is impossible); the normative reading would be tangled rope (normative framework + institutional extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__strategic_culture_drift, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
