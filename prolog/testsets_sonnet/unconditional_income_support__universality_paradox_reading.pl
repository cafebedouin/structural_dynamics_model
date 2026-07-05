% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This story isolates one of three structurally distinct claims
 *   colloquially bundled under 'universal basic income.' The
 *   freedom_floor_reading treats UBI as an autonomy-enabling coordination
 *   mechanism with genuine low-extraction coordination function. The
 *   dependency_trap_reading treats it as an incentive-distorting
 *   upward-redistributive subsidy. This reading —
 *   universality_paradox_reading — is different in kind from both: it does
 *   not claim the underlying transfer mechanism is beneficial or harmful in
 *   itself, but that the LABEL's cross-ideological ambiguity is itself a
 *   constraint with its own structure, coordinating incompatible coalitions
 *   into a shared vehicle while masking the fact that the taxing-back
 *   mechanism design (which the label's ambiguity permits to remain
 *   unspecified until late in the legislative process) converges most
 *   implementations toward outcomes indistinguishable from targeted
 *   means-testing. The genuine coordination function (building a large enough
 *   coalition to pass anything at all) is real; the asymmetric extraction
 *   (targeted-program recipients bearing benefit-consolidation risk they
 *   never consented to under the 'universal' framing, and the public losing
 *   the capacity for coherent ideological evaluation) rides on the same
 *   ambiguity that enables the coordination. Low ε reflects that
 *   fiscal/distributional outcomes across implementations are, per
 *   taxing-back equivalence research, similar in net-incidence terms — the
 *   harm here is not primarily fiscal magnitude but political-epistemic:
 *   coalition capture and evaluative degradation.
 *
 * KEY AGENTS:
 *   - political_entrepreneurs: Primary beneficiary (organized/arbitrage) — exploits ambiguity to build cross-ideological coalitions
 *   - policy_designers: Primary beneficiary (institutional/arbitrage) — uses taxing-back equivalence for rhetorical flexibility in program design
 *   - targeted_program_recipients: Primary target (powerless/trapped) — bears consolidation/cut risk under universal relabeling
 *   - ideological_clarity: Diffuse victim (analytical, non-agent) — public evaluative capacity degraded by sustained ambiguity
 *   - welfare_economists: Analytical observer (analytical/global) — documents the taxing-back equivalence from outside the beneficiary set
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
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Universal Basic Income as Cross-Ideological Ambiguity Vehicle").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '4ee53b20-bdf7-48d4-ba6c-c1206f2f099b').
narrative_ontology:cs_kernel_codification('4ee53b20-bdf7-48d4-ba6c-c1206f2f099b', distributed).
narrative_ontology:cs_authority_grounding('4ee53b20-bdf7-48d4-ba6c-c1206f2f099b', distributed).
narrative_ontology:cs_reading_relation('4ee53b20-bdf7-48d4-ba6c-c1206f2f099b', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ee53b20-bdf7-48d4-ba6c-c1206f2f099b', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('4ee53b20-bdf7-48d4-ba6c-c1206f2f099b', foundational, ambiguity_is_load_bearing_coordination_resource).
narrative_ontology:cs_axiom_status(ambiguity_is_load_bearing_coordination_resource, holdable).
narrative_ontology:cs_axiom_grounding('4ee53b20-bdf7-48d4-ba6c-c1206f2f099b', ambiguity_is_load_bearing_coordination_resource, empirically_contingent).
narrative_ontology:cs_axiom('4ee53b20-bdf7-48d4-ba6c-c1206f2f099b', secondary, fiscal_convergence_renders_normative_dispute_secondary).
narrative_ontology:cs_axiom_status(fiscal_convergence_renders_normative_dispute_secondary, holdable).
narrative_ontology:cs_axiom_grounding('4ee53b20-bdf7-48d4-ba6c-c1206f2f099b', fiscal_convergence_renders_normative_dispute_secondary, empirically_contingent).
narrative_ontology:cs_reference_frame('4ee53b20-bdf7-48d4-ba6c-c1206f2f099b', pre_specification_coalition_bargaining).
narrative_ontology:cs_drift_state('4ee53b20-bdf7-48d4-ba6c-c1206f2f099b', contemporary_ubi_pilot_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4ee53b20-bdf7-48d4-ba6c-c1206f2f099b', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, fiscal_conservatives).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, labor_advocates).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, fiscal_conservatives).
narrative_ontology:constraint_vindicates(unconditional_income_support__universality_paradox_reading, taxing_back_equivalence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Left and right coalition-builders each invoke 'unconditional income support' to recruit allies who would never agree on an actual bill if forced to specify eligibility, financing, and clawback design. The label's ambiguity lets a libertarian free-market advocate and a labor-solidarity organizer both claim the same slogan while building toward incompatible policies. They gain coalition breadth precisely because the term stays underspecified; specifying it would cost them votes on one flank or the other.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary,
    organized, biographical, arbitrage, national).

% Treasury and welfare-agency technocrats know that a nominally universal payment plus a tax clawback produces near-identical net transfers to a means-tested program with a phase-out — the 'taxing-back' equivalence is well established in public finance. This lets them design programs that can be marketed as universal (politically popular, administratively simple to defend) while functioning as targeted transfers in net-incidence terms. The rhetorical flexibility is a design resource they actively exploit when building coalitions for passage.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, policy_designers, agenda_setter).

% Current means-tested benefit recipients (disability support, targeted housing assistance, categorical welfare) are told that a 'universal' program will replace and simplify their patchwork of benefits. In practice, universalization is frequently used as the political cover for consolidating and cutting benefit levels below what the targeted programs previously provided, since 'everyone gets something' polls better than 'the poor get more.' They have no say in which implementation path is chosen and cannot exit the system that determines their benefit level.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, biographical, trapped, national).

% The public capacity to evaluate whether a given UBI proposal advances autonomy, punishes idleness, or simply relabels existing transfers is degraded by the ambiguity — voters and even legislators frequently cannot tell which of three incompatible normative projects (freedom floor, work-incentive fix, fiscal retrenchment) a specific bill actually implements until it is enacted and its clawback schedule is visible.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, ideological_clarity).

% Support 'universal' framing as a Trojan horse for consolidating and shrinking the welfare state under a popular banner, then negotiate clawback rates that produce net transfers below current targeted-program levels. They benefit from the ambiguity while it is being sold, then bear political cost if the retrenchment becomes visible before the coalition locks in.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, fiscal_conservatives, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, fiscal_conservatives, payer).

% Support the same 'universal' framing believing it delivers an unconditional floor that removes labor-market coercion. They are structurally excluded from the design decisions that actually determine clawback rates and net incidence, since those are negotiated in technical drafting sessions after the coalition-building rhetoric has already committed the label.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, labor_advocates, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, labor_advocates, excluded).

% Public finance researchers who have established the taxing-back equivalence between nominally universal and means-tested designs with equivalent net-incidence schedules. They can show the fiscal outcomes converge but have limited power to affect which political narrative the ambiguity serves in any given legislative cycle.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, welfare_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ambiguous 'unconditional income support' label genuinely coordinates otherwise-incompatible political factions into a single legislative coalition large enough to pass something — without the label's flexibility, no single specification of UBI commands majority support.
% TRANSFER_FUNCTION: Coalition-building capacity and rhetorical flexibility move from a hypothetical fully-specified policy debate to political entrepreneurs and policy designers, who convert the label's ambiguity into legislative leverage; net fiscal incidence — via the clawback design chosen after coalition-building — can move value away from prior targeted-program recipients toward general revenue savings.
% ABSENT_VOICES: Targeted program recipients whose current benefits would be consolidated or reduced under a 'universal' relabeling are not present in the technical drafting sessions where clawback schedules are actually set; the coalition-building conversation happens in the abstract 'universal vs targeted' register that never surfaces the concrete transfer schedule until enactment.
% DISAPPEARANCE_RATIONALE: If the ambiguous framing vanished and policy debates were forced to specify concrete eligibility, financing, and clawback design up front, the cross-ideological coalitions that currently form around 'UBI' would fracture into their constituent factions — freedom-floor advocates, work-incentive reformers, and fiscal retrenchers would have to fight separate, smaller battles, and far fewer bills bearing the UBI label would pass at all.
% FOUNDING_PROBLEM: The term was popularized to solve a genuine coordination problem: building a large enough coalition to move any unconditional cash transfer through a legislature dominated by factions with incompatible reasons for wanting it (or opposing it).
% FOUNDING_PROBLEM_CORROBORATION: Independent public-finance economists (outside both the political-entrepreneur and policy-designer beneficiary groups) corroborate via published taxing-back equivalence research that the coalition problem is real and that the fiscal outcomes of 'universal' and 'targeted' designs converge once clawback is modeled — but they also document, as an outside empirical finding rather than a beneficiary claim, that the ambiguity is exploited to relabel benefit cuts as universalization.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.28 at interval end) because the underlying fiscal transfer, per taxing-back equivalence research, does not differ dramatically in net-incidence terms across the competing implementation paths — the harm is not that money moves in an unusually extractive pattern, it is that the political process extracts coalition-building value and evaluative clarity from parties who believed they were buying a specific, coherent policy. Theater ratio is authored HIGH and rising (0.35 to 0.62) because an increasing share of the public discourse around 'UBI' is performative advocacy detached from the concrete clawback-schedule specifics that actually determine who benefits — the label does more rhetorical work over time as more factions adopt it, while less of the debate addresses the underlying design question. Suppression (0.34) reflects that alternatives (forcing early specification of clawback design) are not actively blocked so much as strategically avoided by all coalition-building parties, since specification would fracture the coalition before it reaches a vote.
 *
 * PERSPECTIVAL GAP:
 *   From the political_entrepreneur and policy_designer seats, the ambiguity is a coalition-building tool they actively wield — closer to a rope from where they sit. From the targeted_program_recipient seat, the same structure looks like an enforced bait-and-switch: universal framing sold as expansion, delivered as consolidation. The engine computes these divergent per-seat readings from the declared power/exit structure; the claim/metric independence here is deliberate — claimed_type is tangled_rope precisely because both the coordination reading and the extraction reading are simultaneously true from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers sit near the beneficiary end: they extract coalition-building leverage and design flexibility from the ambiguity itself, and the ambiguity is a resource they actively cultivate rather than passively enjoy. Targeted program recipients sit near the target end: trapped in existing benefit structures, they have no voice in the technical drafting sessions where clawback schedules — the actual determinant of their welfare — are set, yet the 'universal' framing that led to their program's potential consolidation was built and sold without their structural participation. Fiscal conservatives and labor advocates both derive genuine short-term benefit from the coalition the ambiguity enables, which is why they are declared with secondary payer/excluded roles rather than pure victim status — they are willing co-participants whose own coalition eventually forces a specification neither fully controls.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — building a legislative coalition large enough to pass an unconditional-transfer bill across factions with genuinely incompatible reasons for wanting one — remains live; this is not a dead-mandate case. What resolves the tangled_rope classification rather than a pure snare reading is that the coordination function is real and ongoing, not merely historical cover: absent some ambiguity-tolerant vehicle, virtually no UBI-adjacent legislation would pass at all, and welfare_economists corroborate from outside the beneficiary set that specification-forcing tends to fracture coalitions before votes. The extraction (evaluative degradation, benefit-consolidation risk to targeted recipients) is asymmetric and rides on the same structure that enables the coordination, which is exactly the tangled_rope signature rather than either a pure rope (no victims) or pure snare (no genuine coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coalition_necessity_vs_deliberate_obfuscation,
    'Is the label''s ambiguity a necessary feature of building any large-enough coalition for unconditional transfer policy, or is it deliberately maintained by political entrepreneurs and policy designers specifically because a clarified debate would fracture their coalition and reveal the retrenchment agenda?',
    'Comparative case study of jurisdictions where UBI-style proposals were forced to specify clawback design early in the legislative process (e.g. through mandatory fiscal-note requirements) versus those where ambiguity persisted through passage: compare coalition survival rates and final net-incidence outcomes.',
    'If ambiguity is structurally necessary for any coordination to occur at all, this reading''s tangled_rope classification understates the genuine coordination value; if ambiguity is a deliberately cultivated tool for masking retrenchment, the classification may understate the extraction and lean closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_necessity_vs_deliberate_obfuscation, empirical, 'Whether the ambiguity is a coordination necessity or a deliberately exploited cover.').

omega_variable(
    taxing_back_equivalence_scope_limits,
    'Does the taxing-back equivalence between universal and targeted designs hold uniformly, or does it break down in specific implementation contexts (e.g. administrative-burden differences, stigma effects, or take-up rate differences) that would make the fiscal-outcome-convergence claim (and hence the low ε authored here) too strong?',
    'Meta-analysis of implemented pilot programs and enacted universal-versus-targeted transfer schemes, comparing actual (not modeled) net incidence including administrative costs and take-up differentials.',
    'If equivalence breaks down materially in practice, ε for this reading should be revised upward, since the ambiguity would then be masking a real fiscal-outcome difference, not merely a rhetorical one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taxing_back_equivalence_scope_limits, empirical, 'Robustness of the equivalence claim underlying this reading''s low-ε authoring.').

omega_variable(
    kernel_reading_boundary_stability,
    'Is the universality_paradox_reading genuinely a distinct structural claim from the freedom_floor and dependency_trap readings, or is it better understood as a meta-observation about how those two readings interact politically rather than a third co-equal reading of the same kernel?',
    'Track whether legislative outcomes attributable to this reading''s dynamics (coalition capture, benefit consolidation under universal framing) can be fully explained by combinations of the other two readings'' mechanisms, or whether they require an independent causal factor (the ambiguity itself) not reducible to either.',
    'If reducible, this reading should be merged or reframed as a network relationship between the other two rather than a standalone constraint; if independent, the three-way decomposition is the correct ε-invariant treatment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_stability, conceptual, 'Whether this reading is a genuinely independent kernel reading or a derived meta-pattern over the other two.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__universality_paradox_reading, theater_ratio, 4, 0.41).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__universality_paradox_reading, theater_ratio, 8, 0.47).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__universality_paradox_reading, theater_ratio, 12, 0.52).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__universality_paradox_reading, theater_ratio, 16, 0.56).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.59).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__universality_paradox_reading, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__universality_paradox_reading, base_extractiveness, 4, 0.17).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__universality_paradox_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__universality_paradox_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__universality_paradox_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.26).
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
% This constraint is the third member of the unconditional_income_support kernel family (3 stories). freedom_floor_reading claims a low-ε rope structure (genuine autonomy-enabling coordination, minimal victim structure). dependency_trap_reading claims a higher-ε snare-adjacent or tangled_rope structure (incentive distortion, upward redistribution to non-needy). This reading (universality_paradox_reading) is orthogonal to both normative evaluations: it claims low ε (fiscal outcomes converge across designs per taxing-back research) but tangled_rope type (the political vehicle itself entangles a genuine coordination function — coalition-building — with an asymmetric extraction — evaluative degradation and benefit-consolidation risk to targeted recipients). All three stories share the same underlying label but instantiate structurally distinct constraints per the ε-invariance principle; none averages or hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
