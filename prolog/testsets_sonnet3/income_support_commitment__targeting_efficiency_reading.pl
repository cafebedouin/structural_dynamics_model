% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Targeting-Efficiency Reading of the Income Support Commitment
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the targeting-efficiency reading of the
 *   income_support_commitment kernel: the claim that income support should be
 *   concentrated on demonstrated need rather than distributed universally.
 *   The reading is structurally distinct from the dependency_trap_reading
 *   (which centers work-disincentive effects of unconditional support) and
 *   the freedom_floor_reading (which centers autonomy and dignity effects of
 *   unconditional support) — those are separate constraint stories with their
 *   own ε and stakeholder sets, not alternative measurements of this one.
 *   Here, ε is authored high because the reading's own internal logic,
 *   followed to its typical policy conclusion (fund any move toward
 *   universalism by consolidating existing targeted programs within a fixed
 *   fiscal envelope), extracts from the very population it claims to serve:
 *   current recipients lose a documented net amount ($31,100 to $19,100 in
 *   the canonical Queens-parent illustration) when the targeting apparatus is
 *   dismantled in favor of flat universal distribution funded from the same
 *   pool. The poor are simultaneously the reading's stated beneficiary class
 *   and its actual victim class under its own preferred reform trajectory —
 *   that double role is the structural signature this reading is built to
 *   name.
 *
 * KEY AGENTS:
 *   - current_targeted_program_recipients: primary nominal beneficiary and primary actual victim under the reading's preferred reform (powerless/trapped) — receives targeted package now, loses net value if package is flattened
 *   - means_testing_administrators: agenda_setter (institutional/arbitrage) — designs and enforces demonstrated-need verification, institutionally invested in targeting's persistence
 *   - fiscal_conservative_policymakers: beneficiary (powerful/arbitrage) — uses targeting-efficiency framing to justify lower aggregate transfer budgets
 *   - benefits_cliff_workers: payer (powerless/trapped) — bears the marginal-tax-rate wall that targeting's stacked-program structure produces
 *   - welfare_policy_analysts: analytical observer — models and documents the beneficiary/victim overlap this reading depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.72).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.58).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Targeting-Efficiency Reading of the Income Support Commitment").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, 'bf1cf20f-418a-4b69-91cd-6afbb1b2face').
narrative_ontology:cs_kernel_codification('bf1cf20f-418a-4b69-91cd-6afbb1b2face', distributed).
narrative_ontology:cs_authority_grounding('bf1cf20f-418a-4b69-91cd-6afbb1b2face', distributed).
narrative_ontology:cs_reading_relation('bf1cf20f-418a-4b69-91cd-6afbb1b2face', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf1cf20f-418a-4b69-91cd-6afbb1b2face', income_support_commitment__dependency_trap_reading, influences).
narrative_ontology:cs_axiom('bf1cf20f-418a-4b69-91cd-6afbb1b2face', foundational, allocation_should_track_demonstrated_need).
narrative_ontology:cs_axiom_status(allocation_should_track_demonstrated_need, holdable).
narrative_ontology:cs_axiom_grounding('bf1cf20f-418a-4b69-91cd-6afbb1b2face', allocation_should_track_demonstrated_need, instrumental).
narrative_ontology:cs_axiom('bf1cf20f-418a-4b69-91cd-6afbb1b2face', secondary, universal_distribution_dilutes_scarce_transfer_capacity).
narrative_ontology:cs_axiom_status(universal_distribution_dilutes_scarce_transfer_capacity, holdable).
narrative_ontology:cs_axiom_grounding('bf1cf20f-418a-4b69-91cd-6afbb1b2face', universal_distribution_dilutes_scarce_transfer_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('bf1cf20f-418a-4b69-91cd-6afbb1b2face', means_tested_categorical_welfare_state).
narrative_ontology:cs_drift_state('bf1cf20f-418a-4b69-91cd-6afbb1b2face', contemporary_benefits_cliff_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bf1cf20f-418a-4b69-91cd-6afbb1b2face', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, means_testing_administrators).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, fiscal_conservative_policymakers).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, categorically_ineligible_low_income_households).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, benefits_cliff_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A household stacking SNAP, housing assistance, Medicaid, TANF, and EITC currently receives a combined package (e.g. $31,100 for a Queens single parent) calibrated to documented categorical need. Under this reading's preferred targeting-efficiency reform, any move toward a flatter universal payment funded by consolidating those programs would replace the stacked package with a smaller flat sum (e.g. $19,100) — the same household is simultaneously the reading's cited justification (targeting serves the needy) and the population that loses most if targeting is abandoned in favor of universalism. They cannot easily leave the benefits system without losing categorical eligibility entirely.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients, payer).

% Design and enforce the eligibility verification, asset tests, and categorical rules that determine who receives what. Their institutional relevance, budget lines, and professional expertise are built around administering complexity; a shift to unconditional universal payment would eliminate much of their function. They set the terms of the 'demonstrated need' standard and can tighten or loosen it.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, means_testing_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Cite targeting efficiency to argue that scarce fiscal resources should be concentrated on the poorest rather than spread thinly across all income levels, which keeps the aggregate income-support budget lower than a universal alternative would require. Benefit politically from a narrative of fiscal discipline and from a program structure that is easier to cut incrementally by tightening eligibility than a universal payment would be.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, fiscal_conservative_policymakers, beneficiary,
    powerful, generational, arbitrage, national).

% Households just above categorical thresholds, or whose need doesn't fit the documented categories (irregular gig income, undocumented status, non-custodial caregivers) receive nothing despite comparable material hardship to enrolled households. Targeting's demonstrated-need standard structurally excludes them; they bear the cost of a system organized around proving categorical eligibility rather than income level alone.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, categorically_ineligible_low_income_households, payer,
    powerless, biographical, trapped, national).

% Workers whose earnings, if increased even modestly, would trigger loss of multiple stacked benefits simultaneously (the 'benefits cliff'), producing effective marginal tax rates over 80-100%. Targeting's need-verification structure creates this cliff; they experience it as a wall against earning more, and their exit option (working more) is punished by the same targeting apparatus that is defended as efficient.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, benefits_cliff_workers, payer,
    powerless, immediate, trapped, national).

% Argue that unconditional universal payment would eliminate benefits cliffs, administrative gatekeeping, and stigma, and would reach the categorically-excluded poor. Are structurally absent from the targeting-efficiency policy conversation, which treats universalism's cost (spreading a fixed budget across all income levels) as self-evidently wasteful without engaging the coverage-gap argument on its own terms.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, universal_basic_income_advocates, excluded,
    organized, generational, constrained, national).

% Model the distributional effects of targeted versus universal transfers, including the specific finding that funding a UBI by consolidating targeted programs (rather than through new revenue) produces net losses for currently-enrolled poor households. Their modeling is the primary source for the 'poor as both beneficiary and victim' framing this reading depends on.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, welfare_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, means_testing_administrators).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrating a fixed pool of transfer dollars on households that pass a demonstrated-need test solves the problem of finite fiscal resources meeting unlimited potential claims — it directs the largest per-household transfer to those with the most documented need rather than diluting the same budget across every income level.
% TRANSFER_FUNCTION: Moves tax revenue to categorically-verified low-income households through multiple stacked programs; simultaneously withholds transfer from anyone who cannot document categorical need, and structurally would withdraw resources from currently-enrolled households if the same fiscal envelope were redistributed as a flatter universal payment.
% ABSENT_VOICES: Universal basic income advocates and the categorically-ineligible poor (irregular income, non-standard household structures) are largely absent from the targeting-efficiency debate as it is conventionally staged; the debate is framed as targeted-vs-universal spending levels rather than as a question about who administrative categories exclude.
% DISAPPEARANCE_RATIONALE: If the targeting commitment vanished and were replaced by unconditional universal distribution funded from the same consolidated budget, currently-enrolled recipients would see their package shrink substantially (the $31,100-to-$19,100 delta), means-testing administrators would lose their institutional function, and benefits-cliff dynamics would disappear — a genuine rearrangement in multiple directions simultaneously, which is exactly why this kernel produces three incompatible readings rather than one.
% FOUNDING_PROBLEM: Twentieth-century welfare states faced the problem of finite public budgets and wanted transfers to reach those in the deepest material need rather than being spread indiscriminately across all income levels, including the affluent.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal conservative policymakers and means-testing administrators attest the founding problem (efficient allocation of scarce transfer dollars) remains live. Independent welfare-economics research (outside both the administering agencies and the advocacy groups on either side) documents that the administrative machinery built to solve this problem now generates benefits-cliff disincentives and coverage gaps that arguably create as much hardship as they prevent — external corroboration is mixed, not unanimous.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.72 by interval end because the targeting-efficiency reading, when it translates into policy (consolidating targeted programs to fund a flatter universal replacement within the same budget envelope), produces a documented net loss for the population it is offered to protect. Suppression (0.58) reflects the demonstrated-need verification apparatus itself — asset tests, categorical eligibility rules, recertification burdens — which actively filters claimants rather than passively distributing. Theater ratio is moderate-low (0.31): means-testing does perform real allocative work, but a rising share of its apparatus (recertification frequency, documentation burden) has drifted toward administrative self-justification rather than improved targeting accuracy, which the rising trajectory captures.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (means-testing administrators) this reads as prudent stewardship of scarce resources toward those who need them most. From the payer seat (current recipients under the reading's own preferred reform trajectory, and benefits-cliff workers under the status quo) the same demonstrated-need architecture reads as an extraction mechanism that produces cliffs, gaps, and — in the specific UBI-funded-by-consolidation scenario — a net loss for its own nominal beneficiaries. The engine should compute these as structurally different seat classifications from the same authored data.
 *
 * DIRECTIONALITY LOGIC:
 *   Current targeted-program recipients carry a dual role deliberately: nominal beneficiary of the present targeted system (low d against the CURRENT arrangement) but the primary target/victim of this reading's own preferred reform trajectory (high d against the STRUCTURAL SHIFT the reading advocates). Means-testing administrators and fiscal conservative policymakers are the reading's actual structural beneficiaries — administrators via institutional persistence, policymakers via lower aggregate fiscal exposure and a defensible-sounding efficiency narrative. Benefits-cliff workers and categorically-ineligible households are victims of the targeting apparatus's own design, independent of any reform question — their cost is inherent to demonstrated-need verification, not contingent on the UBI-replacement scenario.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (finite budgets should reach the neediest first) remains partially live — genuine scarcity constraints exist — but the specific administrative machinery built to solve it (categorical verification, asset testing, program stacking) has accumulated extraction and gap-creation effects that arguably now exceed its original coordination benefit. Classifying this as snare rather than rope or tangled_rope reflects the authored judgment that, within THIS reading's own preferred policy trajectory, the coordination story (target the needy) is used to justify an outcome (fund universalism from the targeted pool) that extracts from the targeted population itself — the coordination function and the extraction are not merely coupled but the latter runs through the former's own logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_and_disagreement_locus,
    'Is the disagreement among the three income_support_commitment readings located in empirical predictions (does unconditional support cause dependency, does targeting cause cliffs) or in prior normative commitments (is demonstrated need or unconditional dignity the correct allocation principle)?',
    'Cross-national natural experiments comparing targeted and universal transfer systems on labor supply, poverty depth, and administrative cost could resolve the empirical sub-claims; the normative sub-claim (which allocation principle is correct) is not resolvable by data and would remain a live disagreement across readings regardless.',
    'If the disagreement is purely empirical, convergent evidence could in principle collapse the three readings toward one policy consensus. If it is substantially normative, the three readings will persist as genuinely different constraints indefinitely, each with its own ε, exactly as modeled here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_and_disagreement_locus, conceptual, 'Whether the kernel''s sibling readings disagree empirically or normatively, and whether that disagreement is resolvable.').

omega_variable(
    sibling_reading_structural_delta,
    'How would the beneficiary/victim structure and ε change under the freedom_floor_reading and dependency_trap_reading, given they are authored as separate constraint stories?',
    'Author both sibling stories independently and compare: the freedom_floor_reading is expected to show low ε (autonomy gain, minimal identifiable victims) and the dependency_trap_reading is expected to locate victims among long-term unconditional recipients rather than current targeted-program recipients.',
    'Confirms the ε-invariance principle holds across the kernel: each reading is a genuinely distinct constraint rather than a different observable of one constraint, since their ε values and victim populations differ substantially.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents the expected structural divergence across the three sibling readings of this kernel.').

omega_variable(
    administrative_capture_vs_genuine_scarcity,
    'Is the demonstrated-need verification apparatus''s persistence driven by genuine fiscal scarcity that requires targeting, or has the apparatus itself become an entrenched administrative interest that would resist replacement even if fiscal capacity for universalism existed?',
    'Compare jurisdictions with expanded fiscal capacity (e.g. resource-windfall states) to see whether targeting apparatus is dismantled or retained when scarcity constraints loosen.',
    'If apparatus persists despite loosened scarcity, this supports reclassifying the administrative layer itself toward piton/snare independent of the targeting-efficiency argument''s merits; if apparatus is dismantled when scarcity loosens, this supports the reading''s own claim that targeting is a scarcity-driven necessity rather than institutional self-preservation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(administrative_capture_vs_genuine_scarcity, empirical, 'Whether targeting apparatus reflects genuine scarcity constraint or entrenched administrative interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__targeting_efficiency_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__targeting_efficiency_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(inco_tr_t24, income_support_commitment__targeting_efficiency_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(inco_tr_t32, income_support_commitment__targeting_efficiency_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__targeting_efficiency_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(inco_be_t24, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(inco_be_t32, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(inco_su_t8, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(inco_su_t16, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(inco_su_t24, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(inco_su_t32, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the income_support_commitment kernel, each authored as a separate story per the ε-invariance principle. targeting_efficiency_reading (this story) authors high ε because its own preferred policy trajectory extracts from current targeted recipients. freedom_floor_reading is expected to author low ε (autonomy/dignity gains, minimal victims). dependency_trap_reading is expected to author moderate-to-high ε with a different victim population (long-term unconditional recipients experiencing labor-market erosion). All three should be linked bidirectionally via affects_constraints, and none should attempt to average or hedge ε across the readings — that would violate DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__targeting_efficiency_reading, powerless, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
