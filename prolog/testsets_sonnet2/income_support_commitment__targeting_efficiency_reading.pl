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
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This story instantiates the targeting-efficiency reading of the
 *   income_support_commitment kernel: income support should be concentrated
 *   on demonstrated categorical need rather than distributed universally. The
 *   reading's own arithmetic is the source of its high measured extraction —
 *   the standing arrangement it is ABOUT is the current targeted-program
 *   stack, and this reading treats a UBI-style conversion (funded by
 *   cannibalizing that stack) as extracting roughly $12,000 annually from the
 *   household it is nominally meant to help (a Queens parent moving from
 *   ~$31,100 to ~$19,100). The poor are simultaneously the beneficiaries this
 *   reading credits with the current concentration and, under a
 *   targeting-to-universal conversion, the victims of the redistribution it
 *   authors. This is deliberately NOT the freedom_floor_reading (which
 *   authors the opposite valence — UBI as autonomy-enabling) or the
 *   dependency_trap_reading (which focuses on work-disincentive rather than
 *   allocative comparison). Each reading is a separate constraint with its
 *   own ε, authored independently per the ε-invariance principle; they are
 *   linked only through the shared kernel, not through a shared metric.
 *
 * KEY AGENTS:
 *   - targeted_program_recipients_current_regime: primary beneficiary under the current targeted architecture (powerless/trapped) — collects the concentrated stack
 *   - targeted_program_recipients_under_ubi_conversion: same population modeled as victim under this reading's UBI-conversion counterfactual — loses ~$12,000
 *   - categorically_ineligible_working_poor: excluded by design from targeted programs; this reading does not treat their inclusion as a design failure
 *   - means_testing_administrative_apparatus: agenda-setter that designs and defends the categorical architecture and administers the verification burden
 *   - deficit_conscious_fiscal_policymakers: institutional beneficiary of a budget-capping targeting design
 *   - ubi_advocacy_coalition: excluded voice disputing the reading's revenue-neutral comparison assumption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.71).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.58).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Targeting-Efficiency Reading of the Income Support Commitment").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, '103ab10a-2259-46e6-826e-468a02fef8b3').
narrative_ontology:cs_kernel_codification('103ab10a-2259-46e6-826e-468a02fef8b3', distributed).
narrative_ontology:cs_authority_grounding('103ab10a-2259-46e6-826e-468a02fef8b3', distributed).
narrative_ontology:cs_reading_relation('103ab10a-2259-46e6-826e-468a02fef8b3', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('103ab10a-2259-46e6-826e-468a02fef8b3', income_support_commitment__dependency_trap_reading, influences).
narrative_ontology:cs_axiom('103ab10a-2259-46e6-826e-468a02fef8b3', foundational, fixed_transfer_budget_premise).
narrative_ontology:cs_axiom_status(fixed_transfer_budget_premise, holdable).
narrative_ontology:cs_axiom_grounding('103ab10a-2259-46e6-826e-468a02fef8b3', fixed_transfer_budget_premise, empirically_contingent).
narrative_ontology:cs_axiom('103ab10a-2259-46e6-826e-468a02fef8b3', secondary, categorical_documentation_is_legitimate_allocation_mechanism).
narrative_ontology:cs_axiom_status(categorical_documentation_is_legitimate_allocation_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('103ab10a-2259-46e6-826e-468a02fef8b3', categorical_documentation_is_legitimate_allocation_mechanism, conventional).
narrative_ontology:cs_reference_frame('103ab10a-2259-46e6-826e-468a02fef8b3', fixed_budget_allocative_efficiency).
narrative_ontology:cs_drift_state('103ab10a-2259-46e6-826e-468a02fef8b3', post_universal_basic_income_pilot_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('103ab10a-2259-46e6-826e-468a02fef8b3', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients_current_regime).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, means_testing_administrative_apparatus).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, deficit_conscious_fiscal_policymakers).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients_under_ubi_conversion).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, categorically_ineligible_working_poor).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, administratively_burdened_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients_current_regime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A household (e.g. a Queens parent) that qualifies for a stack of categorical programs — TANF, SNAP, housing assistance, EITC, Medicaid — totaling roughly $31,100 annually because each program targets a specific demonstrated need (children, low income, housing insecurity, medical need). This stack is larger than any single flat transfer would be, because targeting lets the state concentrate resources on documented deprivation rather than spreading a fixed pool across everyone. They bear the transaction cost of proving need through repeated documentation, recertification, and program-specific eligibility rules, but the concentrated dollar amount currently exceeds what a universal, non-targeted replacement would deliver to the same household.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients_current_regime, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients_current_regime, payer).

% The same demographic profile as the current-regime recipients, but modeled under a UBI-style conversion that funds a universal flat payment by eliminating or consolidating the targeted programs. Under this reading's own arithmetic the same household drops from roughly $31,100 to $19,100 — a roughly $12,000 loss — because the flat payment spread across the entire population is worth less per targeted-need household than the sum of programs it replaced. They cannot opt back into the eliminated categorical programs once consolidated; the loss is structural, not a choice they made.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients_under_ubi_conversion, payer,
    powerless, biographical, trapped, national).

% Low-wage workers just above categorical thresholds (income, household composition, disability status) who receive little or nothing from the current targeted system despite genuine need, because targeting is calibrated to specific documented categories rather than income level alone. They would gain under a universal payment but this reading treats their exclusion as the correct operation of demonstrated-need concentration, not as a flaw — their inclusion is not the story's declared design goal.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, categorically_ineligible_working_poor, payer,
    powerless, biographical, constrained, national).

% Applicants across all targeted programs who must repeatedly document income, household composition, work hours, medical status, and residency to maintain eligibility. Administrative churn causes eligible people to lose benefits through paperwork failure, not need resolution. This population absorbs the verification cost that concentration on demonstrated need requires.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, administratively_burdened_applicants, payer,
    powerless, immediate, trapped, national).

% The eligibility-verification bureaucracy (caseworkers, program offices, contracted verification vendors) that administers, designs, and defends the targeting architecture. Its institutional survival and budget depend on demonstrated-need concentration remaining the organizing principle; a universal flat payment would eliminate most of its function. It sets the documentation and recertification rules that recipients and applicants must satisfy.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, means_testing_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Legislators and budget officials who favor targeting because it caps aggregate outlay by excluding the non-needy, holding the program's fiscal footprint down relative to a universal payment covering the entire population at any comparable per-recipient level. They benefit politically and fiscally from being able to defend spending as need-concentrated rather than universal.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, deficit_conscious_fiscal_policymakers, beneficiary,
    institutional, generational, analytical, national).

% Advocates for unconditional universal payment who argue targeting's administrative burden and categorical exclusions are themselves a form of extraction from the poor, and that the $31,100-vs-$19,100 comparison this reading relies on assumes a specific, low-revenue-neutral UBI design rather than a fully funded one. They are not treated as a party to this reading's own arithmetic — their design assumptions are excluded from the comparison this reading authors.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, ubi_advocacy_coalition, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, means_testing_administrative_apparatus).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrating income support on documented categorical need allows a fixed pool of public funds to deliver larger per-household transfers to the most deprived households than an equal-sized pool spread universally across the whole population — this is a real allocative-efficiency coordination problem when the funding pool is treated as fixed.
% TRANSFER_FUNCTION: Moves general tax revenue to households that can document specific categorical need (children, disability, housing insecurity, low income) rather than to the population at large; the administrative apparatus that verifies eligibility captures a share of the pool as operating cost.
% ABSENT_VOICES: The UBI advocacy coalition disputes the reading's core comparison — they argue the $31,100-vs-$19,100 framing assumes a revenue-neutral swap rather than a fully funded universal payment, and that administrative churn under targeting already extracts from eligible non-applicants who fail to navigate paperwork. They are not represented in this reading's own stakeholder arithmetic.
% DISAPPEARANCE_RATIONALE: If targeting were abandoned overnight in favor of an unfunded universal conversion (the scenario this reading models), the documented household drops roughly $12,000 in annual support, the means-testing apparatus loses its institutional function, and fiscal policymakers lose the mechanism that currently caps aggregate outlay to the demonstrably needy — the transfer system reorganizes around a flatter, lower-concentration structure.
% FOUNDING_PROBLEM: Public assistance historically faced a fixed-budget allocation problem: with finite tax revenue, undifferentiated distribution to the whole population delivers less to those in acute need than concentrating the same revenue on documented deprivation. Categorical targeting was built to solve that allocation problem.
% FOUNDING_PROBLEM_CORROBORATION: The means-testing apparatus and deficit-conscious policymakers attest the founding problem remains live — budgets are still finite and need is still unevenly distributed. Independent poverty researchers and the excluded UBI coalition attest that the founding problem has been partially superseded by administrative burden itself becoming a source of unmet need (eligible non-take-up), a finding corroborated by GAO and academic take-up-rate studies conducted outside both the administering agencies and the advocacy groups with a stake in either outcome.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (ε=0.71) is authored high because this reading's central empirical claim — that a UBI-style conversion funded by consolidating targeted programs would cost the modeled household roughly $12,000 annually — describes a direct transfer of resources away from the demonstrably needy toward the general population, with the poor bearing that transfer's cost under the reading's own accounting. Suppression (0.58) reflects the administrative apparatus that must actively verify and re-verify categorical need to sustain the targeting architecture and defend it against consolidation pressure; this is moderate rather than extreme because the targeting regime, while burdensome, is not a coercive extraction mechanism in itself — it is the counterfactual conversion this reading evaluates that produces the loss. Theater ratio (0.42) captures the growing share of administrative activity devoted to eligibility verification theater (recertification cycles, documentation churn) relative to the substantive transfer function, rising over the measured interval as programs layered additional verification requirements. Accessibility collapse is moderate (0.40) — alternatives to targeting (universal payment designs, negative income tax, guaranteed income pilots) remain visible and actively debated, unlike a genuine natural-law constraint. Resistance (0.62) is substantial: UBI advocates, poverty researchers, and some fiscal conservatives all actively contest the targeting architecture from different directions.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (the means-testing apparatus), this arrangement is coordination — efficient allocation of scarce public funds toward documented need. From the payer seat modeled under UBI conversion, the identical arrangement (understood as 'targeting persisting against consolidation pressure') looks like extraction — the poor lose ground precisely because the system that is supposed to serve them is structured to resist redistribution to the broader population, and when redistribution happens anyway it is funded by cannibalizing their existing support rather than by new revenue. The engine computing these as different seat-level types from the same structural data is expected and is not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Current-regime recipients are declared both beneficiaries (of the concentrated stack) and payers (of the administrative burden required to obtain it) — a genuinely dual-positioned seat, hence the secondary_role. Under the UBI-conversion counterfactual this reading models, the same demographic becomes purely a payer relative to their current position, having their concentrated benefit diluted. The administrative apparatus and fiscal policymakers are structural beneficiaries with institutional exit options (arbitrage/analytical) — they set the rules and are not bound by them the way recipients are. The categorically-ineligible working poor are payers by exclusion rather than by transfer — the targeting design's cost to them is opportunity cost, not extraction of something they held.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — finite public funds allocated more efficiently to acute need than to universal distribution — remains genuinely live in a world of constrained budgets, which argues against pure mandatrophy. But the administrative apparatus's own institutional survival now depends on targeting's persistence independent of whether targeting remains the most efficient allocation mechanism, and take-up-rate research (corroborated outside both the apparatus and the UBI coalition) documents that administrative burden itself now generates unmet need. This is not full mandatrophy (the founding problem has not fully died) but partial goal displacement: the mechanism increasingly defends its own continuation rather than being re-evaluated against alternative designs that might serve the same allocative goal with less administrative extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revenue_neutrality_assumption,
    'Does the $31,100-to-$19,100 loss comparison assume a revenue-neutral UBI conversion (consolidating existing programs into a flat payment at the same total budget), or does it compare against an alternative UBI design funded by new revenue that would not require cannibalizing targeted programs?',
    'Explicit budget-scoring of the specific UBI proposal being compared against: does it hold total transfer spending constant while flattening distribution, or does it raise new revenue?',
    'If the comparison assumes revenue neutrality, the reading''s snare classification depends entirely on that funding assumption — a fully-funded UBI design (new revenue, not cannibalized targeting) could deliver the same per-household benefit without the modeled loss, which would substantially undercut this reading''s ε and its claim that universalization is inherently extractive from the poor.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revenue_neutrality_assumption, empirical, 'Whether the reading''s central loss figure depends on an unstated revenue-neutrality assumption.').

omega_variable(
    administrative_cost_offset,
    'How much of the $31,100 in current targeted benefits is offset by administrative compliance costs (time, foregone work hours, professional assistance for paperwork) borne by the recipient household but not counted in the benefit figure?',
    'Time-use studies and administrative-burden research measuring hours spent on recertification, documentation gathering, and appeals across the stacked programs, converted to an implicit cost figure.',
    'If administrative compliance costs are substantial, the true net value of targeted benefits is lower than $31,100, narrowing (or even reversing) the gap this reading uses to characterize universal conversion as extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_cost_offset, empirical, 'Whether administrative burden erodes the nominal targeted-benefit advantage this reading relies on.').

omega_variable(
    targeting_as_natural_versus_constructed_efficiency,
    'Is concentrating support on demonstrated need a natural implication of budget scarcity (a mountain-like allocative constraint), or is it a constructed institutional choice that itself creates the scarcity framing it claims to solve (by capping aggregate transfer spending below what a universal design would deliver)?',
    'Comparative cross-national analysis of countries with universal versus targeted transfer systems, controlling for total transfer spending as a share of GDP, to determine whether targeting is a response to fixed budgets or a cause of constrained budgets.',
    'If targeting itself suppresses the political will to expand aggregate transfer spending (by making transfers look like charity to the needy rather than a universal entitlement), the reading''s framing of a ''fixed pool'' is partly self-fulfilling, which would weaken the coordination-function claim underlying this reading''s efficiency argument.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(targeting_as_natural_versus_constructed_efficiency, conceptual, 'Whether the fixed-budget premise underlying targeting-efficiency is exogenous or partly constructed by targeting itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__targeting_efficiency_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__targeting_efficiency_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(inco_tr_t24, income_support_commitment__targeting_efficiency_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(inco_tr_t32, income_support_commitment__targeting_efficiency_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__targeting_efficiency_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(inco_be_t24, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(inco_be_t32, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(inco_su_t8, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(inco_su_t16, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(inco_su_t24, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(inco_su_t32, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'universal vs. targeted income support' debate into structurally distinct constraints per the ε-invariance principle. targeting_efficiency_reading (this story) authors high ε because it evaluates a specific counterfactual (targeted-to-UBI conversion funded by program cannibalization) as extraction from the poor. freedom_floor_reading authors low ε because it evaluates unconditional support's autonomy-enabling function under a different (typically fully-funded) design assumption. dependency_trap_reading authors its own distinct ε focused on behavioral work-disincentive effects rather than the allocative comparison this story relies on. The three do not share a metric; they share only the kernel (the underlying policy commitment the three parties dispute) and are linked here for contamination-propagation analysis, not for metric averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
