% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor (Autonomy and Exit-Capacity Reading)
 *   domain: political economy / social policy / welfare state
 *
 * SUMMARY:
 *   This story instantiates the freedom_floor reading of the
 *   income_support_commitment kernel as a single epsilon-invariant
 *   constraint: an unconditional, universal periodic income payment — funded
 *   from the general tax base, paid to every resident regardless of work
 *   status, household means, or family composition — assessed by this
 *   reading's own lights as an arrangement whose function is to guarantee a
 *   survivable floor beneath every member. The arrangement solves a
 *   coordination problem private markets cannot: subsistence insurance fails
 *   under adverse selection, and exit capacity — the credible ability to
 *   refuse a job, a contract, or a relationship — is a good no individual can
 *   provision alone. Its operation concentrates enablement on those whose
 *   options are otherwise bound by care duties, precarity, violence, or
 *   unfunded risk, and it disciplines wage-setting at the bottom of the labor
 *   market by making refusal survivable. The epsilon referent is the standing
 *   unconditional-support arrangement as instituted, never some alternative
 *   arrangement this reading would prefer. Per the claim/metric independence
 *   rule, the claimed type and the metrics are authored independently: the
 *   claim states what this reading holds structurally true; the metrics state
 *   what is descriptively true of the arrangement's operation. The kernel
 *   contest is recorded in commentary.kernel_context and the omega variables,
 *   not inside the constraint.
 *
 * KEY AGENTS:
 *   - caregivers: Primary beneficiary (moderate power / constrained exit) — unpaid care work made survivable by an income that does not depend on paid hours or household means
 *   - precarious_workers: Primary beneficiary (moderate / constrained) — subsistence between engagements converts refusal of underpriced or unsafe work into a real option
 *   - abuse_survivors: Primary beneficiary (powerless / constrained) — an individually-held payment converts an unaffordable exit from violence or exploitation into a hard but feasible one
 *   - artists_entrepreneurs: Primary beneficiary (moderate / mobile) — the floor floats unfunded development periods; failure lands on a floor rather than on debt
 *   - net_taxpayer_households: Net payer with secondary beneficiary position (moderate / constrained) — pays more in than received back, holds the same floor as insurance
 *   - employers: Payer (organized / mobile) — wage-setting at the bottom tightens against a survivable outside option; offsets via pay, policy pressure, relocation, or automation
 *   - legislature_finance_ministry: Agenda setter (institutional / constrained) — sets level, tax base, and unconditionality; amendment procedurally simple, politically expensive
 *   - categorical_program_administrators: Excluded (institutional / identity_locked) — the means-test bureaucracy that universalization would displace, outside the universalist coalition
 *   - welfare_state_researchers: Analytical observer (analytical / analytical) — designs and evaluates pilots, advises every seat, collects no flow
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.2).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support as Freedom Floor (Autonomy and Exit-Capacity Reading)").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political economy / social policy / welfare state").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '3e07830a-099e-477e-a4c8-a84c5e11cbcb').
narrative_ontology:cs_kernel_codification('3e07830a-099e-477e-a4c8-a84c5e11cbcb', formalized).
narrative_ontology:cs_authority_grounding('3e07830a-099e-477e-a4c8-a84c5e11cbcb', expertise).
narrative_ontology:cs_interpretation_layer_present('3e07830a-099e-477e-a4c8-a84c5e11cbcb').
narrative_ontology:cs_reading_relation('3e07830a-099e-477e-a4c8-a84c5e11cbcb', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e07830a-099e-477e-a4c8-a84c5e11cbcb', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('3e07830a-099e-477e-a4c8-a84c5e11cbcb', foundational, unconditional_floor_precondition_of_autonomy).
narrative_ontology:cs_axiom_status(unconditional_floor_precondition_of_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('3e07830a-099e-477e-a4c8-a84c5e11cbcb', unconditional_floor_precondition_of_autonomy, deontological).
narrative_ontology:cs_axiom('3e07830a-099e-477e-a4c8-a84c5e11cbcb', foundational, exit_capacity_disciplines_wage_power).
narrative_ontology:cs_axiom_status(exit_capacity_disciplines_wage_power, holdable).
narrative_ontology:cs_axiom_grounding('3e07830a-099e-477e-a4c8-a84c5e11cbcb', exit_capacity_disciplines_wage_power, empirically_contingent).
narrative_ontology:cs_reference_frame('3e07830a-099e-477e-a4c8-a84c5e11cbcb', universal_citizenship_income_floor).
narrative_ontology:cs_drift_state('3e07830a-099e-477e-a4c8-a84c5e11cbcb', contemporary_pilot_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3e07830a-099e-477e-a4c8-a84c5e11cbcb', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, net_taxpayer_households).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, net_taxpayer_households).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, employers).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, exit_capacity_monopsony_correction).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, universality_take_up_completeness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates the payment level, the tax base that funds it, and the statute's unconditionality; the finance ministry runs the payment machinery and the revenue side and sets the rate through the annual budget process. Its room to amend is bounded by the organized defense of the payment among recipients and by fiscal-market reactions to the debt-funded share; repeal or restructuring is procedurally simple but politically expensive.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, legislature_finance_ministry, agenda_setter,
    institutional, generational, constrained, national).

% Provide unpaid care to children, elders, or disabled family members, which limits paid hours and makes them financially dependent on a partner or on fragmented conditional benefits. The unconditional payment arrives regardless of marital status, work hours, or household means, so reducing paid work to care — or leaving a household that constrains them — no longer requires destitution first.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, national).

% Move between short contracts, gig platforms, and spells of self-employment with volatile income. The payment covers subsistence between engagements, which lets them decline underpriced or unsafe work and wait for better terms; their position at the point of hire improves because walking away is survivable.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, constrained, national).

% Need money of their own to leave a violent partner or an exploitative employer. Means-tested aid requires proving destitution and often routes through the abuser's household income; the unconditional payment is theirs individually, paid regardless of household composition, which converts an exit that was previously unaffordable into a hard but feasible one.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, constrained, local).

% Carry long unpaid development periods — rehearsal, research, prototyping — that markets do not fund upfront. The payment floats subsistence through those periods, so creative and business risk-taking no longer requires family money or accumulated savings; failure lands on a floor rather than on debt.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Pay more in taxes than they receive back in the payment, netting against the universal transfer they also collect. They fund the scheme through income and consumption taxes; their exit is limited, since tax residence follows them, though capital at the top is more mobile. They hold the same floor as everyone else — the payment is theirs too if income falls — which is the insurance value they carry alongside the net cost.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, net_taxpayer_households, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__freedom_floor_reading, net_taxpayer_households, beneficiary).

% Face a workforce that can refuse low offers and quit without destitution. Wage-setting at the bottom of the labor market tightens: vacancies in unpleasant or underpaid work must be priced to compete with a survivable outside option. Some respond by raising pay and improving conditions; others press for offsetting policy through associations, or shift work abroad or toward automation, which they can do more easily than workers can exit the tax base.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers, payer,
    organized, generational, mobile, national).

% Staff the existing means-tested benefit agencies — caseworkers, eligibility assessors, compliance auditors — whose professional function is assessing need and policing conditionality. Universalization would shrink their caseloads and displace their expertise; they are not part of the universalist coalition that drafts the legislation and rarely hold seats in its design process.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, categorical_program_administrators, excluded,
    institutional, biographical, identity_locked, national).

% Academic and statistical analysts of cash-transfer programs: they design and evaluate pilots, publish employment and wellbeing findings, and advise ministries. They neither fund nor receive the payment and hold no vote over its level; their findings feed every seat's arguments.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_state_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools subsistence risk that private markets cannot insure (adverse selection and moral hazard make private income-floor insurance unworkable) and produces exit capacity — the credible outside option — as a shared good no individual worker, caregiver, or household can provision alone. Solves the funding-level and tax-base collective-action problem once, centrally, instead of leaving each household to face subsistence risk alone.
% TRANSFER_FUNCTION: Moves purchasing power from the general tax base — netting against the universal payment, progressively from higher-income households — to every resident as an unconditional periodic payment; and moves bargaining power from employers to workers by making refusal of any particular job or relationship survivable.
% ABSENT_VOICES: Net-contributor households are present in fiscal politics but under-organized relative to recipient coalitions at funding-renewal moments; administrators of existing means-tested programs — whose caseloads and budgets universalization would displace — sit structurally outside the universalist coalition that authors the arrangement; future taxpayers bearing the debt-funded share have no seat at all. They would object on rate, displacement, and intergenerational-fairness grounds respectively.
% DISAPPEARANCE_RATIONALE: If the floor vanished overnight, household bargaining would revert to dependence on continuous employment or on partners; abuse survivors and caregivers would lose the outside option that makes refusal survivable; wage-setting at the bottom would revert toward monopsony terms as exit capacity collapsed; and the care economy plus creative and entrepreneurial risk-taking would contract to what private savings can float. Labor-market terms, household structure, and the care economy are arranged around the floor's existence.
% FOUNDING_PROBLEM: Securing subsistence for all in economies where income depends on selling labor or on someone else's willingness to transfer — without making receipt conditional on employment (work-discipline) or on demonstrated destitution (means-test stigma, take-up failure, cliff effects). The founding problem is how to guarantee a floor people can actually stand on.
% FOUNDING_PROBLEM_CORROBORATION: Material-deprivation and food-insecurity statistics from national statistical offices, and the labor-economics literature documenting monopsony power and involuntary precarity, attest that the founding problem is live — from seats outside the beneficiary coalition. No neutral party attests that this reading's arrangement solves the problem: the liveness of the problem is corroborated; the sufficiency of the unconditional floor as the solution is the reading's own claim, contested by the sibling readings.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.20): the transfer is transparent and universal, the funding claim is the declared coordination cost rather than concealed rent, and the residual epsilon reflects the compulsory tax claim on net contributors — real, but the price of the good rather than extraction from a trapped class. Suppression is low (0.12) and is authored as a raw, unscaled structural property: the arrangement adds an option rather than removing one, blocks no exit (its function IS exit capacity), and compels nothing beyond ordinary taxation; no suppression_requirement series is authored because the enforcement picture is static — payment machinery, not coercive enforcement — so the story-level scalar carries it. Theater is 0.18 at interval end: where implemented (resource-funded dividends, universal child allowances, pandemic-era emergency transfers) the function is direct money movement with little performance, but the pilot-and-advocacy layer has grown partly performative — repeated piloting and evaluation as a substitute for national rollout — driving the post-2016 rise; the 2020 dip reflects emergency implementation at scale temporarily reversing the pilot-era theater share. Accessibility collapse is low (0.25): means-testing, workfare, categorical programs, and private saving remain fully live alternatives — nothing about this arrangement forecloses them. Resistance is moderate (0.40): sustained fiscal-political contest from net-contributor coalitions and employer interests, conducted through ordinary democratic channels the arrangement does not need to crush. Claimed type: rope — a genuine collective-action problem (uninsurable subsistence risk, unprovisionable exit capacity) solved with minimal coercive overhead, participants net beneficiaries on the insurance-value logic of universality, alternatives unsuppressed. The measurement series share one grid (1982, 1997, 2008, 2010, 2016, 2020, 2026) with both tracked metrics authored at every point. Receipt surface: gain_flow is authored as 'diffuse' — affirmatively checked: the transfer is spread across the universal recipient base by design, and while the four beneficiary seats receive concentrated net gains, no single named seat captures the arrangement's aggregate gains. fixing_cost is authored 'prohibitive': for the legislature that could restructure or repeal, removal carries organized recipient defense, immediate hardship, and fiscal-market reaction costs exceeding any benefit of removal, and the residual completion gap (scaling pilots to a national floor) is likewise expensive relative to what any single seat bears — the engine should read the diffuse+prohibitive cell alongside the low theater and the live founding function, which point away from piton dynamics.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same arrangement. From the caregiver, survivor, and precarious-worker seats the arrangement is pure enablement: it arrives unconditionally and converts unaffordable exits into feasible ones — directionality near the beneficiary end. From the employer seat the same structure operates as a binding limit on wage-setting: the cost is real, transparent, and is the mechanism's declared output rather than a concealed harm, which holds the employer seat above symmetric but well short of full-target. The net-taxpayer seat sits nearest symmetric of the paying seats: net cost damped by the universal receipt its members also collect and by the insurance value of holding the same floor. The categorical-program-administrator seat experiences the arrangement existentially — universalization displaces the function their professional identity is built on, and their identity-locked exit makes their resistance structural rather than negotiable. The researcher seat sees the full structure and collects no flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (caregivers, precarious workers, abuse survivors, artists/entrepreneurs) derive low d for those seats — the payment subsidizes them directly. The payer seats (net-taxpayer households, employers) derive elevated d, damped by structure rather than by override: net taxpayers collect the same universal payment and hold its insurance value; employers' cost is the arrangement's transparent, declared output, and their exit (relocation, automation) is mobile. No victims are declared because no seat's exit is suppressed, no seat is trapped into paying, and no harm class is concealed — this reading holds every cost as a declared coordination cost, which is what keeps epsilon low and the rope classification available. Suppression is authored raw and unscaled; only extractiveness is scaled — by directionality and by national scope, whose verification difficulty (opacity of funding incidence) is the modest amplifier flagged in the funding_incidence_extraction_boundary omega. No directionality overrides are authored: the beneficiary/payer declarations plus exit options already produce the correct d structure, and the override mechanism is keyed by power atom, which would misapply across seats sharing a power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Against pure-extraction mislabeling: the arrangement compels taxpayers, and a reading that saw only the compulsory transfer would call it extractive; the coordination function — pooling uninsurable subsistence risk and producing exit capacity as a shared good — is primary, real, and transparent, which is the rope signature. Against permanent-coordination complacency: two live degradation paths exist — a regressive funding turn (consumption-tax funding, clawback phase-outs) would create victims inside the scheme and migrate it toward tangled_rope, and full pilot-layer substitution for implementation would drift the advocacy layer toward piton dynamics; both are tracked by omegas and the theater series. Mandatrophy status: the founding problem (subsistence insecurity without dignity or exit) remains live, the mandate has not outlived its function, and no sunset clause applies — this is steady-state coordination, not a transitional scaffold, so mandatrophy_resolved is not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    income_support_kernel_reading_delta,
    'This constraint is one reading of the income_support_commitment kernel (freedom_floor_reading) — what would the sibling readings change structurally?',
    'Compare the compiled sibling stories: dependency_trap_reading would re-author high epsilon with recipients bearing capability-atrophy costs and net taxpayers as the harmed class; targeting_efficiency_reading would re-author the beneficiary set to demonstrated-need recipients only and re-author universality itself as the allocative defect.',
    'The low-extraction rope classification holds only within this reading; the same kernel instantiates different constraints with different epsilon, different victim sets, and different types under the siblings. Cross-reading comparison is kernel-level analysis, not a defect in this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(income_support_kernel_reading_delta, conceptual, 'Committer structure: which kernel this is, which reading this file instantiates, and what each sibling would re-author.').

omega_variable(
    universality_valuation_ambiguity,
    'Is the unconditional, universal character of the support constitutive of the floor (this reading: no stigma, no take-up failure, no cliff effects) or the core allocative defect (the targeting reading''s location of disagreement)?',
    'Not resolvable by implementation data alone — it turns on whether stigma, take-up failure, and cliff-effect harms are counted as real costs. The disagreement is located in the valuation of universality itself, not in any measurable output.',
    'If universality is re-valued as waste, the beneficiary set contracts to demonstrated-need recipients, epsilon rises, and the classification migrates toward the targeting reading''s constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_valuation_ambiguity, conceptual, 'Where the kernel disagreement is located: the valuation of unconditionality/universality.').

omega_variable(
    labor_market_effect_sign,
    'Does unconditional support expand effective exit capacity and discipline wage-setting at the bottom of the labor market (this reading''s claim), or erode work attachment and skill accumulation over time (the dependency reading''s claim about the same mechanism)?',
    'Long-run panel evidence from permanent implementations (resource-funded dividends, universal child allowances) on employment, wage growth, and job mobility, beyond the short horizons of time-limited pilots.',
    'If atrophy dominates, this reading''s epsilon rises (recipients pay in capability), a victim class appears, and the classification drifts from rope toward tangled_rope or snare; if enablement dominates, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_effect_sign, empirical, 'The empirical sign of the labor-market effect on which this reading and the dependency reading directly conflict.').

omega_variable(
    funding_incidence_extraction_boundary,
    'Is the tax funding of the floor a pure coordination cost, or does the enacted funding mix (consumption-tax shares, benefit clawback phase-outs) impose net costs on some recipient deciles, creating an extraction boundary inside the scheme?',
    'Distributional incidence analysis of the enacted funding package set against the universal payment, by income decile, for each implementing jurisdiction.',
    'Regressive net incidence for some deciles would raise epsilon, introduce victims, and push the classification toward tangled_rope; progressive net incidence supports the rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_incidence_extraction_boundary, empirical, 'Whether the funding side stays a coordination cost or generates a concealed victim class.').

omega_variable(
    pilot_layer_theater_drift,
    'Is the growing pilot-and-advocacy layer substituting for permanent implementation (theater) or feeding eventual statutory adoption (function)?',
    'Track the conversion rate of pilot programs into permanent statutory floors, and whether advocacy and evaluation output grows faster than the covered population.',
    'If the pilot layer becomes a permanent substitute, theater_ratio continues rising and the advocacy layer drifts toward piton dynamics even while implemented schemes remain rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pilot_layer_theater_drift, empirical, 'Whether the post-2016 theater rise in the measurement series is transitional or a settling-in of performative substitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(freedom_floor_tr_t1982, income_support_commitment__freedom_floor_reading, theater_ratio, 1982, 0.06).
narrative_ontology:measurement(freedom_floor_tr_t1997, income_support_commitment__freedom_floor_reading, theater_ratio, 1997, 0.07).
narrative_ontology:measurement(freedom_floor_tr_t2008, income_support_commitment__freedom_floor_reading, theater_ratio, 2008, 0.08).
narrative_ontology:measurement(freedom_floor_tr_t2010, income_support_commitment__freedom_floor_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(freedom_floor_tr_t2016, income_support_commitment__freedom_floor_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(freedom_floor_tr_t2020, income_support_commitment__freedom_floor_reading, theater_ratio, 2020, 0.11).
narrative_ontology:measurement(freedom_floor_tr_t2026, income_support_commitment__freedom_floor_reading, theater_ratio, 2026, 0.18).

% Extraction over time
narrative_ontology:measurement(freedom_floor_be_t1982, income_support_commitment__freedom_floor_reading, base_extractiveness, 1982, 0.1).
narrative_ontology:measurement(freedom_floor_be_t1997, income_support_commitment__freedom_floor_reading, base_extractiveness, 1997, 0.12).
narrative_ontology:measurement(freedom_floor_be_t2008, income_support_commitment__freedom_floor_reading, base_extractiveness, 2008, 0.13).
narrative_ontology:measurement(freedom_floor_be_t2010, income_support_commitment__freedom_floor_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(freedom_floor_be_t2016, income_support_commitment__freedom_floor_reading, base_extractiveness, 2016, 0.16).
narrative_ontology:measurement(freedom_floor_be_t2020, income_support_commitment__freedom_floor_reading, base_extractiveness, 2020, 0.19).
narrative_ontology:measurement(freedom_floor_be_t2026, income_support_commitment__freedom_floor_reading, base_extractiveness, 2026, 0.2).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_commitment__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__targeting_efficiency_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, minimum_wage_regulation).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, means_tested_assistance_programs).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'unconditional income support' decomposes, per the epsilon-invariance principle, into three kernel readings with different epsilon and different beneficiary/victim structures: freedom_floor_reading (this file — low epsilon, no victims, enablement), dependency_trap_reading (high epsilon, recipients bearing capability costs, taxpayers as harmed class), and targeting_efficiency_reading (universality itself re-authored as the defect; beneficiary set restricted to demonstrated need). Family edges run between the readings via affects_constraints and via cs_structure.reading_relations; the targeting reading sits downstream of both, since its institutional base (means-tested programs) is what universalization displaces.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
