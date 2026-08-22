% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This story instantiates the freedom-floor reading of the unconditional
 *   income support kernel: the arrangement is read as a coordination
 *   mechanism that removes coercion from labor and household relationships by
 *   decoupling survival from any single employer, partner, or caseworker's
 *   discretion. Under this reading, no victim class is claimed — the cost to
 *   taxpayers is treated as the price of a genuine Pareto-improving
 *   coordination gain, not as extraction from an identifiable payer group.
 *   This is a strict rope reading: pilot evidence (Alaska Permanent Fund
 *   Dividend, Kenya GiveDirectly trials) is cited to support the claim that
 *   labor supply effects are minimal, so the feared extraction-via-idleness
 *   channel does not materialize at scale. This story does NOT represent the
 *   dependency-trap reading (which claims the same policy instrument produces
 *   incentive-distorting extraction) or the universality-paradox reading
 *   (which treats the cross-ideological coalition itself as structurally
 *   ambiguous) — those are separate constraints, linked here via
 *   network.affects_constraints, each with its own ε and stakeholder
 *   structure.
 *
 * KEY AGENTS:
 *   - precarious_workers: primary beneficiary (powerless/constrained) — gains real exit leverage against bad jobs
 *   - domestic_abuse_survivors: primary beneficiary (powerless/trapped) — gains an income stream not gatekept by an abuser or caseworker
 *   - general_taxpayers: payer (organized/constrained) — funds the floor, not framed as a victim under this reading
 *   - program_administrators: agenda_setter (institutional/analytical) — administers a deliberately thin, low-discretion mechanism
 *   - welfare_policy_researchers: analytical observer — supplies the empirical basis (Alaska, Kenya) for the minimal-labor-supply-effect claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.22).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.08).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '72985fc0-2254-4e29-a217-0bf1d3b266e5').
narrative_ontology:cs_kernel_codification('72985fc0-2254-4e29-a217-0bf1d3b266e5', distributed).
narrative_ontology:cs_authority_grounding('72985fc0-2254-4e29-a217-0bf1d3b266e5', distributed).
narrative_ontology:cs_reading_relation('72985fc0-2254-4e29-a217-0bf1d3b266e5', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('72985fc0-2254-4e29-a217-0bf1d3b266e5', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('72985fc0-2254-4e29-a217-0bf1d3b266e5', foundational, survival_must_not_depend_on_coercive_choice).
narrative_ontology:cs_axiom_status(survival_must_not_depend_on_coercive_choice, holdable).
narrative_ontology:cs_axiom_grounding('72985fc0-2254-4e29-a217-0bf1d3b266e5', survival_must_not_depend_on_coercive_choice, deontological).
narrative_ontology:cs_axiom('72985fc0-2254-4e29-a217-0bf1d3b266e5', foundational, empirical_labor_supply_effects_are_minimal).
narrative_ontology:cs_axiom_status(empirical_labor_supply_effects_are_minimal, holdable).
narrative_ontology:cs_axiom_grounding('72985fc0-2254-4e29-a217-0bf1d3b266e5', empirical_labor_supply_effects_are_minimal, empirically_contingent).
narrative_ontology:cs_reference_frame('72985fc0-2254-4e29-a217-0bf1d3b266e5', conditional_welfare_baseline).
narrative_ontology:cs_drift_state('72985fc0-2254-4e29-a217-0bf1d3b266e5', post_pilot_evidence_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('72985fc0-2254-4e29-a217-0bf1d3b266e5', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists_and_creators).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, domestic_abuse_survivors).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, low_wage_workers_with_exit_leverage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, autonomy_enhancing_welfare_hypothesis).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, labor_supply_effects_are_minimal_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently accept unsafe conditions, wage theft, or abusive supervisors because refusing work means losing income entirely. The floor gives them a real, if modest, ability to say no to a specific job without starving, shifting their bargaining position without requiring them to stop working.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, biographical, constrained, national).

% Perform childcare or eldercare that markets do not price, currently unpaid and often financially dependent on a partner or family member. The floor provides an independent income stream that does not require monetizing care work through a formal employer.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, generational, trapped, national).

% Produce culturally valuable work with thin or nonexistent market demand in the short run. The floor lets them sustain production during the years before (if ever) their work finds an audience, without requiring a patron or day job that consumes their working hours.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists_and_creators, beneficiary,
    powerless, biographical, mobile, national).

% Remain in abusive households substantially because leaving means immediate destitution, especially where they have no independent work history or the abuser controls household finances. An unconditional floor is income they cannot be cut off from by a partner controlling a joint account or a caseworker's discretion.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, domestic_abuse_survivors, beneficiary,
    powerless, biographical, trapped, national).

% Work jobs with high turnover and low bargaining power. With a floor, they can decline the worst jobs, negotiate marginally better terms, or take time between jobs to search rather than accepting the first offer out of necessity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, low_wage_workers_with_exit_leverage, beneficiary,
    moderate, biographical, mobile, national).

% Fund the floor through general taxation. Under this reading the cost is treated as the price of a genuine coordination gain (a functioning low-coercion labor market and a social floor against shocks), not as extraction from a victim class — this reading claims no victims.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, general_taxpayers, payer,
    organized, generational, constrained, national).

% Design and run the transfer mechanism. Because eligibility is unconditional, their administrative footprint is deliberately thin compared to means-tested welfare bureaucracies — the coordination function does not depend on their continuous discretionary judgment.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, program_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Currently benefit from a labor supply willing to accept low wages and poor conditions under necessity. Under this reading they are not named victims, but their objection — that a floor could raise their labor costs or reduce compliant labor supply — is not centered in the freedom-floor framing, which treats reduced coercive leverage over workers as the point, not a cost to be weighed against employer interests.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, low_wage_employers, excluded,
    powerful, biographical, mobile, national).

% Study labor supply response, stigma reduction, and shock resilience using pilot data (Alaska Permanent Fund Dividend, Kenya GiveDirectly trials, Finland and Ontario pilots). Their empirical readings inform, but do not settle, the contest between this reading and its siblings.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, welfare_policy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(unconditional_income_support__freedom_floor_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of labor-market participation requiring coercive necessity: without a floor, workers must accept whatever terms are offered regardless of safety, dignity, or fit, because refusal means destitution. An unconditional floor decouples survival from any specific employment relationship, enabling voluntary rather than coerced sector participation and letting people exit acutely bad arrangements (abusive relationships, exploitative jobs) without administrative gatekeeping.
% TRANSFER_FUNCTION: Moves general tax revenue to every resident unconditionally; the redistributive component is small and universal rather than targeted, since the same nominal transfer flows to all recipients regardless of need, financed progressively through the tax system.
% ABSENT_VOICES: Low-wage employers who rely on a labor supply with limited exit options are not centered as a party whose interests are weighed against the floor's autonomy gains — their objection is structurally present in the labor market but excluded from this reading's own framing of the arrangement as a Pareto improvement.
% DISAPPEARANCE_RATIONALE: If the floor disappeared, precarious workers would lose their strongest available exit leverage against bad jobs, caregivers and abuse survivors would lose independent income streams, and artists without market demand would face immediate pressure to abandon uncommercial work — the coordination gain (voluntary rather than coerced labor participation) would collapse back into the pre-floor baseline.
% FOUNDING_PROBLEM: Labor markets and household arrangements routinely place people (precarious workers, caregivers, abuse survivors) in positions where the cost of exit is destitution, making formal 'choice' to remain coercive in substance. The floor was proposed to sever survival from any single employer, partner, or bureaucratic gatekeeper.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying the Alaska Permanent Fund Dividend and the Kenya GiveDirectly randomized trials — parties with no stake in the floor's continuation — report minimal labor supply reduction and measurable gains in reported autonomy and reduced tolerance for poor working conditions, corroborating that the coercion-reduction function is operative rather than a rhetorical gloss on a program that has drifted into something else.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.22) because this reading's own evidentiary basis (Alaska, Kenya pilots) supports small labor supply effects and no identifiable victim class — the transfer is read as a coordination cost, not a rent. Suppression is low (0.08) because the mechanism is unconditional and non-discretionary: there is no caseworker gatekeeping, no means test, no behavioral conditionality to enforce compliance against. Theater ratio is low (0.10) because the coordination function (income independent of employer/partner/caseworker) is the actual mechanism, not a performative gloss over something else. Accessibility collapse is low (0.15): recipients retain full freedom to work, not work, or exit relationships — the floor expands rather than collapses alternatives. Resistance is moderate (0.35): employers relying on a captive low-wage labor supply, and taxpayers skeptical of universal transfers, mount real political resistance, but it is resistance to the policy's adoption, not evidence of victims within its operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (precarious workers, caregivers, artists, abuse survivors) sit near the full-beneficiary end of directionality: the floor subsidizes their autonomy directly and its removal would concretely worsen their exit options. General taxpayers sit as payers but are NOT read as targets of extraction under this reading — the story authors no victims, consistent with the Pareto-improvement claim; their d should sit closer to symmetric than to full-target, reflecting a genuine coordination cost rather than an extractive transfer. Low-wage employers are excluded from the beneficiary/victim structure entirely — their reduced leverage over labor supply is a structural effect of the floor but not modeled as victimhood under this reading's own framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coercive necessity substituting for genuine choice in labor and household exit) is read as live, not dead — precarious labor markets and domestic financial control persist as documented phenomena. This forecloses a piton or dependency-trap reading of the SAME mechanism at this seat: the coordination function has not atrophied into theater, because the mechanism's structure (unconditional, non-discretionary, universal) is the same structure that continuously performs the coercion-removal function, not a vestige of one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_effect_generalizability,
    'Do the minimal labor-supply-reduction findings from the Alaska Permanent Fund Dividend and Kenya GiveDirectly trials generalize to a full-scale, permanent, high-income-country unconditional transfer, or are they artifacts of partial/temporary implementation and different labor market structures?',
    'Long-run panel data from a permanent, universal, high-benefit-level pilot (e.g., a full national rollout) tracking labor force participation over a full business cycle.',
    'If the minimal-effect finding fails to generalize, this reading''s central empirical claim (ε stays moderate rather than rising) is undermined, and the dependency-trap reading''s predictions gain support instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_effect_generalizability, empirical, 'Whether pilot-scale minimal labor-supply-effect findings generalize to permanent full-scale implementation.').

omega_variable(
    no_victim_claim_contestability,
    'Is the claim that this reading has no victims (a strict Pareto improvement) sustainable once low-wage employers'' reduced access to a captive labor supply, and taxpayers bearing the funding cost, are weighed as genuine costs rather than excluded framing artifacts?',
    'Distributional incidence analysis of the tax structure funding the floor, combined with labor market studies of employer adjustment costs in sectors reliant on low-bargaining-power workers.',
    'If employers or specific taxpayer subgroups bear concentrated, non-diffuse costs, this reading''s rope classification is contestable and the constraint would need re-evaluation toward tangled_rope with those groups named as victims — a structurally different constraint from the one authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(no_victim_claim_contestability, conceptual, 'Whether the strict no-victims framing survives scrutiny of employer and taxpayer incidence.').

omega_variable(
    kernel_reading_selection_pressure,
    'Which reading of the unconditional income support kernel (freedom_floor, dependency_trap, universality_paradox) becomes operative in actual policy design is itself contested — is the selection driven by empirical resolution of the omegas above, or by prior ideological commitment that treats the same pilot data as confirming whichever reading was already held?',
    'Track policy adoption language and justification across jurisdictions implementing similar transfers; compare stated rationale to the reading it structurally resembles.',
    'If reading selection is prior-driven rather than evidence-driven, no amount of pilot data resolves the kernel contest, and the three sibling constraints will persist as parallel, non-converging framings indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether the choice among sibling readings of the kernel is evidence-resolvable or prior-determined.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(unco_tr_t0, observed).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__freedom_floor_reading, theater_ratio, 4, 0.09).
narrative_ontology:measurement_basis(unco_tr_t4, observed).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__freedom_floor_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement_basis(unco_tr_t8, observed).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__freedom_floor_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement_basis(unco_tr_t12, projected).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__freedom_floor_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement_basis(unco_tr_t16, projected).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(unco_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(unco_be_t0, observed).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__freedom_floor_reading, base_extractiveness, 4, 0.21).
narrative_ontology:measurement_basis(unco_be_t4, observed).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__freedom_floor_reading, base_extractiveness, 8, 0.21).
narrative_ontology:measurement_basis(unco_be_t8, observed).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__freedom_floor_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement_basis(unco_be_t12, projected).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__freedom_floor_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement_basis(unco_be_t16, projected).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(unco_be_t20, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unconditional_income_support__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the unconditional_income_support kernel, decomposed per the ε-invariance principle because the readings assign structurally different ε values, beneficiary/victim sets, and types to what colloquially is called 'UBI' or 'basic income.' freedom_floor_reading (this story, rope, ε≈0.22, no victims) coexists in public and academic discourse with dependency_trap_reading (extraction framing, victims likely named as targeted-aid recipients displaced or as fiscally burdened future generations) and universality_paradox_reading (a reading about the coalition's structural incoherence rather than the policy's direct effects). All three link to each other bidirectionally to preserve the kernel-family structure for contamination and drift analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
