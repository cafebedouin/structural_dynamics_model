% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Decommodification Floor (Freedom-Floor Reading)
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   An unconditional income floor pays every resident a regular grant with no
 *   work requirement, means test, or behavioral condition, funded by
 *   progressive taxation. This story instantiates the freedom_floor reading
 *   of the contested kernel income_support_conditionality: on this reading
 *   the arrangement's defining operation is decommodification — it severs
 *   survival from acceptance of offered labor terms, creating positive
 *   freedom to refuse coercive work. The epsilon referent is the standing
 *   unconditional-support arrangement itself, assessed by this reading's own
 *   lights: a real but modest fiscal transfer from net contributors, weighed
 *   against a large liberation effect for those whose labor was previously
 *   priced by desperation. KEY AGENTS (by structural relationship): -
 *   low_wage_workers: primary beneficiary (moderate/constrained) — gains
 *   survivable refusal - unpaid_caregivers: beneficiary
 *   (powerless/identity_locked) — first arrangement recognizing unpaid care -
 *   unemployed_job_seekers: beneficiary (powerless/trapped) — search without
 *   panic - employers_dependent_on_desperate_labor: target of the
 *   constraint's effect (institutional/arbitrage) — loses the desperation
 *   lever, collects nothing - net_taxpayer_households: funder, near-symmetric
 *   (organized/mobile) - national_legislature_treasury: agenda setter
 *   (institutional/constrained) — authors grant level and creeping conditions
 *   - undocumented_residents: excluded voice (powerless/trapped) -
 *   labor_economists: analytical observer. Sibling readings of the same
 *   kernel are separate constraints in separate files; they are not averaged
 *   into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.22).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Decommodification Floor (Freedom-Floor Reading)").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "economic/political/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '3442b1ae-c40a-4e17-8d81-534bff5d9b29').
narrative_ontology:cs_kernel_codification('3442b1ae-c40a-4e17-8d81-534bff5d9b29', formalized).
narrative_ontology:cs_authority_grounding('3442b1ae-c40a-4e17-8d81-534bff5d9b29', distributed).
narrative_ontology:cs_reading_relation('3442b1ae-c40a-4e17-8d81-534bff5d9b29', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('3442b1ae-c40a-4e17-8d81-534bff5d9b29', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('3442b1ae-c40a-4e17-8d81-534bff5d9b29', foundational, material_independence_constitutes_positive_freedom).
narrative_ontology:cs_axiom_status(material_independence_constitutes_positive_freedom, holdable).
narrative_ontology:cs_axiom_grounding('3442b1ae-c40a-4e17-8d81-534bff5d9b29', material_independence_constitutes_positive_freedom, deontological).
narrative_ontology:cs_axiom('3442b1ae-c40a-4e17-8d81-534bff5d9b29', secondary, decommodified_reservation_wages_discipline_monopsony_employers).
narrative_ontology:cs_axiom_status(decommodified_reservation_wages_discipline_monopsony_employers, holdable).
narrative_ontology:cs_axiom_grounding('3442b1ae-c40a-4e17-8d81-534bff5d9b29', decommodified_reservation_wages_discipline_monopsony_employers, empirically_contingent).
narrative_ontology:cs_reference_frame('3442b1ae-c40a-4e17-8d81-534bff5d9b29', unconditional_decommodified_floor).
narrative_ontology:cs_drift_state('3442b1ae-c40a-4e17-8d81-534bff5d9b29', contemporary_administrative_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3442b1ae-c40a-4e17-8d81-534bff5d9b29', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, unemployed_job_seekers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers_dependent_on_desperate_labor).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, net_taxpayer_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, net_taxpayer_households).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, decommodification_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive an unconditional cash floor regardless of employment status. Refusing a hazardous, degrading, or underpaid offer now costs forgone upside rather than survival. They also pay consumption and payroll taxes on their other income, and they cannot leave the tax base short of emigration. Their reservation wage now includes the grant, which changes what they can credibly walk away from.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__freedom_floor_reading, low_wage_workers, payer).

% Search for work without the panic of imminent destitution, holding out for positions that match their skills instead of taking the first offer. They face stigma and activation requirements administered on top of the grant, and they have no realistic exit from the jurisdiction that administers it.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, unemployed_job_seekers, beneficiary,
    powerless, immediate, trapped, national).

% Perform care work outside the labor market and receive the floor without any employment condition attached, the first arrangement in which their work registers as economically supported rather than invisible. Their caregiving role is fused with who they are; they are not positioned to relocate or restructure their lives around the grant, and their claim on it rests on residence and personhood, not productivity.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, biographical, identity_locked, national).

% Operate in sectors built on take-it-or-leave-it offers to workers whose alternative to acceptance was hardship. The floor converts worker refusal from ruin into inconvenience, so they must now offer terms employees can voluntarily accept. They respond by automating, relocating, subcontracting, restructuring roles, and funding political opposition to the floor's expansion. They collect nothing from the arrangement; what they lose is the desperation lever itself.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers_dependent_on_desperate_labor, payer,
    institutional, generational, arbitrage, global).

% Pay progressive taxes that exceed the universal grant they also receive, making them net funders of the floor. Their exit is partial but real: the highly skilled among them can migrate to lower-tax jurisdictions, and capital can be shifted abroad. They bear the fiscal cost while sharing in the arrangement's stability benefits — lower crisis spending, functioning communities where they employ people and sell goods.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, net_taxpayer_households, payer,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__freedom_floor_reading, net_taxpayer_households, beneficiary).

% Sets the grant level, the tax schedule that funds it, and — decisively for this reading — whatever administrative conditions get attached to disbursement. Adjusts under fiscal pressure and electoral cycles. The seat where conditionality creep originates: work-search requirements, sanction regimes, and eligibility carve-outs are all authored here while the program's title continues to say unconditional.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, national_legislature_treasury, agenda_setter,
    institutional, generational, constrained, national).

% Work the same bottom-tier labor market the floor reshapes but are barred from the grant by status. As citizen workers gain refusal power, employers' remaining desperation leverage concentrates on precisely the workers the floor excludes. They would object that the arrangement reforms coercion for some while intensifying vulnerability for others, but they are structurally unable to press the objection.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, undocumented_residents, excluded,
    powerless, immediate, trapped, national).

% Measure reservation-wage shifts, labor supply elasticities, quit rates, and wage incidence around unconditional transfers. Their findings are the evidentiary terrain on which the contest over what this arrangement fundamentally is gets fought, though they collect no rent from any resolution of it.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, labor_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_conditionality__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem among workers: individually, each must accept whatever terms are offered because unilateral refusal means destitution; collectively, workers would prefer a world in which refusal is survivable, but no worker can create that world alone. The unconditional floor coordinates on a shared exit option — it makes refusal survivable for everyone simultaneously, converting individual desperation into credible collective bargaining position.
% TRANSFER_FUNCTION: Moves purchasing power from net contributor households (via progressive taxation of income, capital, and consumption) to every resident equally as an unconditional grant; second-order, it moves bargaining power from employers to workers without any contractual transfer between them.
% ABSENT_VOICES: Undocumented residents are excluded from the grant while absorbing the redirected residue of employer leverage; they are not in the room where eligibility is drawn. Future generations are absent where funding leans on debt. Workers in trading-partner jurisdictions who receive relocated production are absent from the national conversation entirely.
% DISAPPEARANCE_RATIONALE: If the floor vanished overnight, reservation wages would collapse back toward subsistence, employers would regain the desperation lever the floor removed, quits in coercive jobs would stop, and the low-wage labor market would re-coerce within months. Caregivers and job seekers would lose their only unconditional claim on resources. The arrangements of every seated party depend on the floor's existence or its absence — nothing stays put.
% FOUNDING_PROBLEM: Industrial labor markets priced labor at subsistence because the worker's alternative to accepting any offered terms was starvation, and early poor relief was made conditional, stigmatizing, and deliberately harsh ('less eligibility') precisely to preserve that compulsion. The unconditional floor was built to sever survival from acceptance of whatever terms are on the table.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the contemporary monopsony literature documents wage-setting below competitive levels in concentrated labor markets; employer organizations' own testimony against the floor — that it raises quit rates and wage demands — attests that the desperation lever is real and currently load-bearing; and the historical Poor Law record independently documents the survival-contingent acceptance problem the floor targets. No corroborating source is limited to the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-to-moderate (0.22): the arrangement does move real resources from net contributors, and some portion of the grant plausibly leaks into suppressed wages at the margin, but on this reading the dominant flow is liberation rather than extraction. Suppression is very low (0.12): the floor blocks no alternative and forecloses no exit — the residual suppression is administrative conditionality layered on disbursement, which is exactly the trajectory the suppression_requirement series tracks (0.08 to 0.28 over the interval: sanctions, work-search requirements, and eligibility carve-outs accumulating in the administrative layer while the statutory label stays unconditional). Theater is low (0.18): delivery is real and continuous; the performative share is pilot-program announcement cycles and universality rhetoric that coexists with quietly re-attached conditions. Accessibility collapse is low (0.20): understanding the floor collapses no alternatives — it enlarges the option set, which is characteristic of benign coordination rather than of constructs that must close exits to survive. Resistance is high (0.70): intense, sustained political resistance from employer interests, fiscal conservatives, and work-norm traditionalists — resistance here tracks threatened rents and norm violation, not extraction borne by victims, which is why high resistance coheres with low extraction in this profile. All three tracked series run on one shared six-point grid; the trajectory is monotone drift, not cyclical, so no intermittent-reinforcement reading applies. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine's directionality and scope computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the low_wage_worker and caregiver seats the arrangement is nearly pure coordination: a shared exit option each worker could never build alone — rope-like, with directionality near the beneficiary pole. From the net_taxpayer_household seat the same structure is a genuine transfer with real cost, partially offset by the universal grant they also receive — near-symmetric directionality, plausibly computing mild tangled_rope coloring. From the employer seat the arrangement is experienced as pure loss with zero collection — maximal directionality despite collecting nothing — because what the floor removes is an instrument they were using, not a good they were receiving. The agenda-setter seat sees administration, not liberation or theft. The engine derives these divergences from the declared beneficiary/victim structure and exit options; the authored rope claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (low_wage_workers, unpaid_caregivers, unemployed_job_seekers) drive directionality toward the subsidized pole, strongest where exit is most blocked (caregivers, job seekers) because trapped beneficiaries sit furthest from any offsetting cost. Victim declarations split asymmetrically: employers_dependent_on_desperate_labor bear the constraint's operative cost (lost leverage) with arbitrage-grade exit that dampens but does not erase their target position, while net_taxpayer_households carry the fiscal cost but also receive the universal grant, pulling them toward symmetry. The excluded undocumented_residents sit near the target pole without being declared victims — the floor's costs reach them through labor-market redirection, not through the arrangement's own transfer mechanics, which is precisely what makes their exclusion an absent-voice finding rather than a structural victim declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — survival-contingent acceptance of labor terms — remains live per corroboration from outside the benefiting parties (monopsony findings, employer opposition testimony, Poor Law history), so the mandate has not outlived its function and mandatrophy is not resolved. The arrangement carries no sunset clause and is not transitional: on this reading it is steady-state coordination, not a scaffold awaiting dissolution. The classification discipline matters here in a specific way: the dependency_trap move is to recode a liberation structure as a capture structure by relabeling its beneficiaries as its victims. Holding the structural data (who receives, who pays, who lost what) separate from the narrative frame is what prevents that relabeling from passing as analysis — and symmetrically prevents the freedom-floor framing from being waved past the real fiscal cost the net-taxpayer seat bears.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the standing unconditional-support arrangement better described by this freedom_floor reading (decommodification and liberating refusal power) or by the sibling readings — dependency_trap (incentive corrosion harming recipients) or wage_subsidy (employer capture via wage suppression)?',
    'Natural experiments from unconditional transfers at scale — Alaska Permanent Fund dividends, Finnish basic-income trial, Stockton SEED, GiveDirectly cohorts — measuring quit rates, reservation wages, labor supply, skill acquisition, and wage incidence over multi-year horizons.',
    'If dependency_trap evidence dominates, the supported themselves enter the victim set and the computed type shifts toward snare; if wage_subsidy evidence dominates, employers re-enter as beneficiaries of the transfer and the type shifts toward tangled_rope or snare with reversed polarity; if this reading''s evidence holds, the rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Which reading of the income_support_conditionality kernel the structural evidence supports.').

omega_variable(
    employer_lever_moral_status,
    'Is the employer''s lost desperation lever a genuine cost borne by a victim, or the removal of an advantage no one was entitled to — in which case the arrangement has no employer victims at all?',
    'Conceptual analysis separating baseline entitlements from exercised power: if the pre-floor wage bargain was set under duress rather than agreement, the lever''s loss is not a harm to the employer; if portions of the bargain reflected legitimate productivity terms now disrupted, a residual genuine cost exists.',
    'If the lever''s loss is not victimhood, the victim set reduces to net taxpayers alone and the arrangement computes closer to pure rope; if it is victimhood, the arrangement carries a structural loser and the tangled_rope question stays live despite the reading''s liberating verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_lever_moral_status, conceptual, 'Whether losing coercive labor-market leverage counts as bearing a cost.').

omega_variable(
    fiscal_incidence_and_wage_capture,
    'Who actually bears the net cost after behavioral response — do worker wage gains offset taxpayer burdens, and what fraction of the grant capitalizes into lower pre-transfer wages (the wage_subsidy mechanism operating inside this reading''s referent)?',
    'Incidence studies comparing wage trajectories in covered versus uncovered sectors around floor introduction, combined with tax-side distributional analysis of the funding schedule.',
    'Substantial wage capture would mean part of the measured liberation is illusory — workers'' total compensation unchanged with the grant substituting for wages — raising effective extraction on the worker seat and pushing the computed type away from rope; negligible capture supports the reading as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_incidence_and_wage_capture, empirical, 'Whether the grant''s benefits survive wage-market offset or leak to employers.').

omega_variable(
    conditionality_creep_risk,
    'Will the arrangement remain substantively unconditional, or will administrative conditionality — sanctions, work-search mandates, eligibility carve-outs — accumulate until the compulsion the floor removed is reinstated through the back door?',
    'Longitudinal audit of disbursement conditions against the statutory text: track the count and severity of attached conditions, sanction rates, and the gap between the program''s unconditional label and its administrative practice.',
    'Full conditionality creep would invert the reading: the arrangement would become a conditional work-enforcement system wearing an unconditional name, shifting the computed type toward snare and transferring the freedom_floor claim to a program that no longer exists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditionality_creep_risk, empirical, 'Whether unconditionality survives administrative accretion over the interval.').

omega_variable(
    state_dependence_vs_market_dependence,
    'Does decommodification constitute positive freedom, or does it merely substitute dependence on the state and its administrators for dependence on employers — relocating the coercion rather than removing it?',
    'Compare the structure of the two dependencies: whether the grant''s continuity is protected by rules insulated from discretionary revocation (rule-bound dependence) or subject to administrative and political discretion (personalized dependence), and whether recipients experience and act on the difference.',
    'If the new dependence is discretionary, the freedom_floor claim weakens and the arrangement looks closer to a managed population than a liberated one, lowering the beneficiary seats'' computed standing; if rule-bound, the substitution argument fails and the rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_dependence_vs_market_dependence, conceptual, 'Whether the floor replaces market dependence with an equivalent or weaker form of dependence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t6, income_support_conditionality__freedom_floor_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement_basis(inco_tr_t6, observed).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__freedom_floor_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement_basis(inco_tr_t12, observed).
narrative_ontology:measurement(inco_tr_t18, income_support_conditionality__freedom_floor_reading, theater_ratio, 18, 0.19).
narrative_ontology:measurement_basis(inco_tr_t18, observed).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__freedom_floor_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(inco_tr_t24, observed).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__freedom_floor_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(inco_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t6, income_support_conditionality__freedom_floor_reading, base_extractiveness, 6, 0.17).
narrative_ontology:measurement_basis(inco_be_t6, observed).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__freedom_floor_reading, base_extractiveness, 12, 0.19).
narrative_ontology:measurement_basis(inco_be_t12, observed).
narrative_ontology:measurement(inco_be_t18, income_support_conditionality__freedom_floor_reading, base_extractiveness, 18, 0.2).
narrative_ontology:measurement_basis(inco_be_t18, observed).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__freedom_floor_reading, base_extractiveness, 24, 0.21).
narrative_ontology:measurement_basis(inco_be_t24, observed).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__freedom_floor_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement_basis(inco_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t6, income_support_conditionality__freedom_floor_reading, suppression_requirement, 6, 0.11).
narrative_ontology:measurement_basis(inco_su_t6, observed).
narrative_ontology:measurement(inco_su_t12, income_support_conditionality__freedom_floor_reading, suppression_requirement, 12, 0.15).
narrative_ontology:measurement_basis(inco_su_t12, observed).
narrative_ontology:measurement(inco_su_t18, income_support_conditionality__freedom_floor_reading, suppression_requirement, 18, 0.19).
narrative_ontology:measurement_basis(inco_su_t18, observed).
narrative_ontology:measurement(inco_su_t24, income_support_conditionality__freedom_floor_reading, suppression_requirement, 24, 0.24).
narrative_ontology:measurement_basis(inco_su_t24, observed).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__freedom_floor_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement_basis(inco_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'unconditional income support' decomposes into three structurally distinct claims about the same standing arrangement, per the epsilon-invariance principle. This file instantiates freedom_floor_reading (liberation reading: beneficiaries are workers and caregivers, victims are lever-dependent employers and net taxpayers, low epsilon, rope-type claim). dependency_trap_reading instantiates the incentive-corrosion claim (recipients as victims, high epsilon, snare-family claim). wage_subsidy_reading instantiates the employer-capture claim (workers as victims of wage offset, employers as beneficiaries, high epsilon). The three stories share the kernel income_support_conditionality and are linked pairwise through affects_constraints; each carries its own epsilon, stakeholder surface, and claimed type, and none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
