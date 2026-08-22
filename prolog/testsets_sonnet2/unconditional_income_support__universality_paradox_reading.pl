% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Universal Basic Income as Cross-Ideological Ambiguity Vehicle
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This story treats the 'UBI debate' not as a single contested policy but
 *   as a kernel with three structurally distinct readings, each of which is
 *   its own constraint. This reading — the universality paradox — treats the
 *   political ambiguity itself as the structural object under analysis:
 *   cross-ideological appeal is not incidental marketing but a load-bearing
 *   coordination mechanism that lets an otherwise-unpassable reform proceed,
 *   while simultaneously functioning as an extraction vector against
 *   targeted-program recipients and against the public's capacity for
 *   coherent evaluation. Two sibling readings exist as separate constraints:
 *   the freedom_floor_reading treats the same policy as an autonomy-enabling
 *   exit from labor coercion (near-mountain/rope framing from a rights
 *   perspective), and the dependency_trap_reading treats it as an
 *   incentive-distorting upward-redistributing subsidy (snare-leaning framing
 *   from a work-incentive perspective). Those two readings hold incompatible
 *   normative premises about what the policy IS; this reading is agnostic
 *   about which is 'true' and instead measures the cost of that very
 *   incompatibility being papered over by shared vocabulary. Epsilon here is
 *   authored LOW, consistent with the taxing-back equivalence literature
 *   showing that fiscal/distributional outcomes converge across nominally
 *   different implementations (universal demogrant + clawback tax ≈ negative
 *   income tax) — the extraction this reading measures is not primarily
 *   fiscal, it is epistemic/political: the erosion of the public's ability to
 *   evaluate what it is actually voting for, and the risk transfer onto
 *   existing categorical-program recipients.
 *
 * KEY AGENTS:
 *   - political_entrepreneurs: coalition-builders across left/right/libertarian factions who exploit ambiguity to pass otherwise-blocked reform
 *   - policy_designers: technocrats who select among fiscally-equivalent implementation paths for rhetorical convenience
 *   - targeted_program_recipients: current categorical-aid recipients whose specific benefits may be cut under 'universal' branding
 *   - ideological_clarity: the non-agent casualty — coherent public evaluation of the actual policy design
 *   - welfare_economists: analytical observers documenting taxing-back fiscal equivalence across nominally distinct designs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.28).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.34).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Universal Basic Income as Cross-Ideological Ambiguity Vehicle").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '9c066bcb-e325-4160-b77f-62669f641cfd').
narrative_ontology:cs_kernel_codification('9c066bcb-e325-4160-b77f-62669f641cfd', distributed).
narrative_ontology:cs_authority_grounding('9c066bcb-e325-4160-b77f-62669f641cfd', distributed).
narrative_ontology:cs_reading_relation('9c066bcb-e325-4160-b77f-62669f641cfd', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c066bcb-e325-4160-b77f-62669f641cfd', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('9c066bcb-e325-4160-b77f-62669f641cfd', foundational, framing_ambiguity_is_load_bearing_not_incidental).
narrative_ontology:cs_axiom_status(framing_ambiguity_is_load_bearing_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('9c066bcb-e325-4160-b77f-62669f641cfd', framing_ambiguity_is_load_bearing_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('9c066bcb-e325-4160-b77f-62669f641cfd', foundational, distributional_equivalence_renders_ideological_framing_orthogonal_to_fiscal_substance).
narrative_ontology:cs_axiom_status(distributional_equivalence_renders_ideological_framing_orthogonal_to_fiscal_substance, holdable).
narrative_ontology:cs_axiom_grounding('9c066bcb-e325-4160-b77f-62669f641cfd', distributional_equivalence_renders_ideological_framing_orthogonal_to_fiscal_substance, empirically_contingent).
narrative_ontology:cs_reference_frame('9c066bcb-e325-4160-b77f-62669f641cfd', categorical_welfare_state_baseline).
narrative_ontology:cs_drift_state('9c066bcb-e325-4160-b77f-62669f641cfd', contemporary_ubi_pilot_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9c066bcb-e325-4160-b77f-62669f641cfd', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(unconditional_income_support__universality_paradox_reading, taxing_back_fiscal_equivalence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Left, right, and libertarian coalition-builders each invoke 'universal basic income' to recruit supporters who believe fundamentally different things about it — one side hears 'dignity floor,' another hears 'welfare state replacement,' another hears 'labor discipline reform.' The ambiguity itself is the asset: it lets a single slogan build a coalition that would fracture immediately if forced to specify a clawback schedule, funding mechanism, or interaction with existing means-tested programs.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary,
    organized, biographical, arbitrage, national).

% Treasury and welfare-agency technocrats can implement a negative income tax, a universal demogrant with high marginal tax clawback, or a targeted guarantee dressed in universal language — and because taxing-back research shows these converge to similar net fiscal transfers for most of the income distribution, designers can select whichever framing survives the current legislature while delivering nearly the same distributional outcome. This gives them rhetorical cover to claim continuity with whichever ideological promise was used to pass the bill.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, policy_designers, agenda_setter).

% Current recipients of disability, housing, and categorical assistance are told a 'universal' payment will replace their patchwork of targeted supports. Because the universal language obscures that many designs pay them LESS than their current combined targeted benefits (universality being funded partly by consolidating and capping what specific-need programs previously provided), they bear the risk that the popular 'give everyone the same amount' framing is used to justify a net cut to their case specifically, while the aggregate program is politically defended as generous.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, national).

% Coherent public evaluation of the policy — what it actually redistributes, to whom, funded how — becomes structurally impossible because the same vehicle is marketed simultaneously as a libertarian minimal-state reform, a social-democratic dignity guarantee, and a labor-market flexibility tool. No single electorate ever votes on the actual fiscal design; they vote on whichever narrative reached them, and the enacted mechanism can diverge from all three.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, ideological_clarity).

% Fund whichever version passes through general taxation or consolidated welfare spending. Because campaign framing emphasized universality's simplicity and dignity rather than its actual net redistributive math, taxpayers evaluate the policy's cost against the wrong mental model — they were sold administrative simplicity, but taxing-back designs frequently retain or exceed the complexity of the means-tested systems they replaced.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, general_taxpayers, payer,
    moderate, biographical, constrained, national).

% Study the taxing-back literature showing that a universal demogrant with a flat clawback tax is fiscally and distributionally near-equivalent to a negative income tax with a phase-out — meaning the 'universal vs. targeted' debate that dominates public discourse is largely orthogonal to the actual distributional question, which is the marginal tax rate structure. Their findings rarely penetrate the political framing that sustains coalition-building.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, welfare_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ambiguous universal framing genuinely does solve a coalition-formation problem: no single ideological faction can pass an income-support reform alone, so a vehicle broad enough to be read as compatible with several incompatible worldviews is often the only politically viable path to any income floor being enacted at all.
% TRANSFER_FUNCTION: Moves political capital and coalition-building leverage to whichever entrepreneurs and designers control the framing, while moving fiscal risk onto existing targeted-program recipients (whose specific benefits may be capped or folded into the 'universal' calculation) and onto the public's capacity to evaluate what it actually voted for.
% ABSENT_VOICES: The specific caseworkers and beneficiaries of existing categorical programs (disability, housing, child support) are rarely centered in the universal-vs-targeted debate, which is conducted mostly between ideological framers; their concrete stake in the clawback design is treated as a technical detail rather than the central distributional question it actually is.
% DISAPPEARANCE_RATIONALE: If the ambiguous universal framing disappeared and every UBI proposal had to be evaluated purely on its actual clawback schedule and net distributional math, most current cross-ideological coalitions supporting it would fracture, since the coalition depends on each faction believing the design vindicates its own priors. Debate would shift entirely to marginal tax rate structure, which is a narrower and less politically saleable fight.
% FOUNDING_PROBLEM: Incremental, categorical welfare states left large gaps and high administrative overhead; income-support reform needed a framing that could unite otherwise-opposed political factions long enough to pass any coherent floor at all.
% FOUNDING_PROBLEM_CORROBORATION: Welfare economists studying taxing-back equivalence attest that the underlying distributional problem is real and could be solved by several structurally similar mechanisms, but attest separately that the political ambiguity used to build coalitions is not required by the economics — it is a political-entrepreneurship artifact. Advocacy organizations on both the libertarian and social-democratic sides, who benefit from the ambiguous framing, are not neutral corroborators of whether the ambiguity itself is still functionally necessary.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28, rising modestly) because the taxing-back equivalence result means the arrangement's actual fiscal transfers are not dramatically different from what a transparent, ideologically-unambiguous negative income tax would produce — most of the 'damage' this reading measures is not redistributive extraction but epistemic and political-process extraction (the public voting on a narrative rather than a mechanism, and specific vulnerable recipients bearing risk that the aggregate framing obscures). Theater ratio is authored comparatively high and rising (0.35 to 0.62) because an increasing share of the political activity around the policy is performative alignment-signaling (each faction publicly claiming the policy vindicates its priors) rather than substantive design debate — this is the Goodhart-drift signature of a policy vehicle whose primary function has shifted from 'solve the income floor problem' toward 'sustain the coalition that keeps it alive.' Suppression (0.34) is moderate: no one is coerced into supporting the framing, but the ambiguity is actively maintained by entrepreneurs who resist demands for design specificity, which functions as a soft suppression of the clarifying question.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers sit near the beneficiary end: they extract coalition-building leverage and rhetorical flexibility directly from the ambiguity, and their exit options are strong (they can pivot framing or move to other issues without personal cost). Targeted program recipients sit near the target end: trapped exit options, immediate time horizon, and they bear concrete risk (benefit clawback disguised as universal generosity) from a debate they do not control. Ideological clarity is modeled as a non-agent payer — nothing benefits from its erosion directly, but it is the structural casualty of the coordination function, which is why it is flagged agent: false and excluded from directionality math proper.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uniting incompatible factions to pass any income floor) is genuinely live in some polities and genuinely dead in others where a coherent floor could now pass on its economic merits alone — hence 'contested' status. The classification as tangled_rope rather than snare matters here: there IS a real coordination function (political ambiguity does let currently-blocked reforms proceed), which prevents mislabeling this as pure extraction; but there is also a real, asymmetric cost (targeted recipients bear risk, public loses evaluative capacity) that prevents mislabeling it as pure rope. Calling it a rope would erase the risk transferred to targeted-program recipients; calling it a snare would erase the genuine coalition-passage function the ambiguity performs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_necessity_vs_artifact,
    'Is the cross-ideological ambiguity a necessary condition for passing any income-support reform in a polarized polity, or is it an artifact exploited by entrepreneurs beyond what passage actually requires?',
    'Comparative case study of jurisdictions that enacted income-support reform with high design transparency (explicit clawback schedules debated publicly) versus those that relied on ambiguous universal framing — compare passage rates and subsequent political stability of the enacted program.',
    'If ambiguity is structurally necessary for passage, the coordination function is stronger than authored and the classification leans more rope-like; if ambiguity persists well past the passage moment and is actively re-manufactured by entrepreneurs for ongoing coalition maintenance, the extraction component is understated and the classification leans more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_necessity_vs_artifact, conceptual, 'Whether the political ambiguity is a load-bearing coordination necessity or an exploitable political artifact.').

omega_variable(
    sibling_reading_incompatibility_scope,
    'Do the freedom_floor_reading and dependency_trap_reading actually describe the same underlying fiscal mechanism differently, or do their supporters'' preferred implementations diverge enough in practice that the taxing-back equivalence result does not hold across the full range of enacted designs?',
    'Track enacted UBI/NIT pilot and program designs across jurisdictions and code each by actual marginal tax rate structure and eligibility rules, then test whether the taxing-back equivalence prediction holds empirically or whether real-world political compromise produces genuinely different distributional outcomes across the reading-aligned implementation paths.',
    'If empirical divergence is large, epsilon for this reading should be revised upward (the incompatibility is not merely rhetorical, it produces materially different outcomes) and the ''fiscal outcomes converge'' premise of this reading weakens; if divergence stays small, this reading''s low-epsilon, ambiguity-as-extraction framing is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_incompatibility_scope, empirical, 'Whether taxing-back fiscal equivalence holds across the actual range of enacted designs associated with the competing readings, or breaks down under real political compromise.').

omega_variable(
    targeted_recipient_capture_mechanism,
    'Is the risk to targeted-program recipients (benefit consolidation/capping under universal branding) an inherent feature of universalist reform design, or a contingent choice made by specific fiscally-constrained designers that could be structurally avoided with adequate funding commitments?',
    'Compare designs that fund universal payments additively (on top of existing targeted programs) against designs that fund them by consolidation/replacement, and assess whether the additive designs are systematically underrepresented in enacted legislation for identifiable political-economy reasons (e.g., cost visibility, competing budget claims).',
    'If additive designs are structurally disfavored regardless of design intent, the victim status of targeted_program_recipients is a durable structural feature of this reading rather than a contingent implementation flaw, strengthening the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(targeted_recipient_capture_mechanism, empirical, 'Whether risk to existing targeted-program recipients is structurally inherent to universalist reform or a contingent, avoidable design choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__universality_paradox_reading, theater_ratio, 4, 0.42).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__universality_paradox_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__universality_paradox_reading, theater_ratio, 12, 0.53).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__universality_paradox_reading, theater_ratio, 16, 0.57).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__universality_paradox_reading, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__universality_paradox_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__universality_paradox_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__universality_paradox_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__universality_paradox_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(unco_be_t24, unconditional_income_support__universality_paradox_reading, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unconditional_income_support__universality_paradox_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__universality_paradox_reading, 0.12).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is the third member of the unconditional_income_support kernel family. freedom_floor_reading and dependency_trap_reading each author a specific, internally-coherent normative claim about the policy (autonomy-enabling vs. incentive-distorting) with correspondingly different epsilon values reflecting their own lights. This reading (universality_paradox_reading) is agnostic between those two normative claims and instead treats their cross-ideological co-existence under one policy label as the structural object, authoring a low epsilon consistent with taxing-back fiscal equivalence research while measuring political-process and evaluative-capacity extraction rather than distributional extraction. All three share the kernel_id unconditional_income_support and are linked bidirectionally via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
