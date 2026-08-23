% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap (Dependency Trap Reading)
 *   domain: political economy/social policy/welfare state theory
 *
 * SUMMARY:
 *   A full-scale unconditional income support arrangement — an equal cash
 *   payment to every resident, financed by broad taxation and, in the leading
 *   blueprints, by folding existing targeted programs into the grant —
 *   assessed from the seat that reads it as an incentive-distorting subsidy.
 *   On this reading the arrangement pays the non-needy alongside the needy,
 *   thins the benefit stack that bottom-quintile households actually live on,
 *   and imposes a net fiscal drawdown on the order of 1.4 trillion annually
 *   after offsets, while pilot evidence on labor-supply effects remains
 *   contested in magnitude. The epsilon authored here refers to this standing
 *   arrangement as this reading assesses it — the arrangement under contest,
 *   never the arrangement this reading would prefer — and the claimed type
 *   and the metrics are authored independently: the snare claim follows from
 *   the victim structure (working poor and net taxpayers positioned against
 *   non-needy recipients and the advocacy coalition), while the metric values
 *   describe the arrangement's observed and projected operation. This file is
 *   one reading of a contested kernel; the committer structure is recorded in
 *   the omega variables and kernel_context, and the sibling readings live in
 *   their own constraint files linked through the network section.
 *
 * KEY AGENTS:
 *   - working_poor: primary target (powerless/trapped) — bears the deepest losses as stacked targeted supports convert into a shallow flat grant
 *   - net_taxpayers: primary target (moderate/constrained) — bears the roughly 1.4 trillion annualized net cost after offsets
 *   - middle_and_upper_income_recipients: primary beneficiary (organized/mobile) — receives the grant decoupled from need; net gainer under flat-dividend designs
 *   - ubi_advocacy_movement: secondary beneficiary (organized/identity_locked) — collects political capital, funding, and coalition space from universality
 *   - targeted_program_bureaucracies: institutional loser (institutional/identity_locked) — appropriations, staffing, and purpose dissolve under replacement designs
 *   - pilot_governments_and_foundations: agenda setter (institutional/mobile) — controls pilot parameters and whether replacement financing enters the blueprint
 *   - fiscal_scoring_agencies: analytical observer (institutional/analytical) — attests costs and incidence from outside the advocacy coalition
 *   - deep_need_advocates: excluded voice (moderate/trapped) — represents populations whose needs exceed any flat grant; absent from the design rooms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.75).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.6).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Dependency Trap (Dependency Trap Reading)").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political economy/social policy/welfare state theory").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, 'ba6727cf-0fb8-4bad-9e21-02985eae82a0').
narrative_ontology:cs_kernel_codification('ba6727cf-0fb8-4bad-9e21-02985eae82a0', distributed).
narrative_ontology:cs_authority_grounding('ba6727cf-0fb8-4bad-9e21-02985eae82a0', distributed).
narrative_ontology:cs_reading_relation('ba6727cf-0fb8-4bad-9e21-02985eae82a0', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba6727cf-0fb8-4bad-9e21-02985eae82a0', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('ba6727cf-0fb8-4bad-9e21-02985eae82a0', foundational, unconditional_income_erodes_work_incentives).
narrative_ontology:cs_axiom_status(unconditional_income_erodes_work_incentives, holdable).
narrative_ontology:cs_axiom_grounding('ba6727cf-0fb8-4bad-9e21-02985eae82a0', unconditional_income_erodes_work_incentives, empirically_contingent).
narrative_ontology:cs_axiom('ba6727cf-0fb8-4bad-9e21-02985eae82a0', secondary, program_replacement_shifts_resources_from_deep_need_to_broad_coverage).
narrative_ontology:cs_axiom_status(program_replacement_shifts_resources_from_deep_need_to_broad_coverage, holdable).
narrative_ontology:cs_axiom_grounding('ba6727cf-0fb8-4bad-9e21-02985eae82a0', program_replacement_shifts_resources_from_deep_need_to_broad_coverage, empirically_contingent).
narrative_ontology:cs_reference_frame('ba6727cf-0fb8-4bad-9e21-02985eae82a0', incentive_distorting_universal_subsidy).
narrative_ontology:cs_drift_state('ba6727cf-0fb8-4bad-9e21-02985eae82a0', post_large_scale_pilot_evidence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ba6727cf-0fb8-4bad-9e21-02985eae82a0', '2026-06-12T12:00:00Z').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_and_upper_income_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocacy_movement).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, net_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, targeted_program_bureaucracies).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, universality_ratchet_thesis).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, equal_per_capita_entitlement_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the same per-person cash grant as everyone else, regardless of need or employment. Under flat-dividend designs financed by broad taxes, their household net position is positive — the grant exceeds their marginal tax contribution. They carry no dependency on the payment and face no barrier to opposing it, but as the median-voter bloc they are the constituency that makes universality durable.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_and_upper_income_recipients, beneficiary,
    organized, biographical, mobile, national).

% A cross-ideological coalition of technologists, libertarian thinkers, progressive organizers, and pilot-funding foundations. Universality is the movement's organizing claim: it supplies fundraising appeals, media relevance, and cross-partisan coalition space. Movement credibility is fused with adoption — a retreat to targeted programs would strand careers, networks, and intellectual capital built around the universal frame.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocacy_movement, beneficiary,
    organized, generational, identity_locked, global).

% Households near the bottom of the earnings distribution who stack several targeted supports — food assistance, housing vouchers, utility aid, transit subsidies — whose combined value exceeds any politically feasible flat grant. Replacement-financed designs convert that stacked depth into a single shallow payment. They cannot decline the swap, their budgets reprice immediately, and paid work remains the margin on which household survival turns.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    powerless, immediate, trapped, national).

% Households and firms whose total tax contribution exceeds what they receive back. They bear the roughly 1.4 trillion annualized net cost of full-scale designs after offsets. The tax obligation is compulsory, relocation or fiscal arbitrage is unrealistic for most, and their opposition is diffuse — expressed through elections and taxpayer organizations rather than through any exit.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, net_taxpayers, payer,
    moderate, biographical, constrained, national).

% Federal and state agencies, county offices, and caseworker workforces that administer means-tested programs. Their appropriations, staffing, and institutional purpose are bound up with targeted delivery. Replacement designs convert their program envelopes into flat disbursements, dissolving the administrative layer; the institutions campaign for their own continuation while their specialized expertise loses its object.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, targeted_program_bureaucracies, payer,
    institutional, generational, identity_locked, national).

% State and municipal governments, research foundations, and philanthropic funders that design and run guaranteed-income pilots and draft national blueprint proposals. They choose grant size, duration, sample, and — decisively — whether replacement financing appears in the design. They publish the results that anchor the national argument and can withdraw from the field at will.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, pilot_governments_and_foundations, agenda_setter,
    institutional, biographical, mobile, regional).

% Treasury staff, congressional scoring offices, and official cost estimators. They produce the gross and net cost tables and distributional incidence analyses that determine which designs are legislatively discussable. They hold no stake in adoption, and their assessments bind the debate from outside the advocacy coalition.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, fiscal_scoring_agencies, observer,
    institutional, generational, analytical, national).

% Organizers and service providers for disabled people, elders on fixed incomes, and the chronically poor — populations whose needs exceed any flat grant and who depend on benefit depth rather than breadth. They argue for preserved targeted programs but hold no seat in the pilot-design rooms or the cross-ideological universality coalitions; the people they serve cannot exit their need.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, deep_need_advocates, excluded,
    moderate, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__dependency_trap_reading, middle_and_upper_income_recipients).
narrative_ontology:fixing_cost_class(unconditional_income_support__dependency_trap_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Delivers a uniform, unconditional cash stream to every resident through a single administrative channel, solving the exclusion-error, stigma, and administrative-burden problems of means-tested delivery and providing automatic countercyclical payment capacity.
% TRANSFER_FUNCTION: Moves money from the broad tax base to every resident in equal per-capita amounts; under replacement-financed designs it additionally moves the budgetary envelope of targeted programs — whose value concentrates on households with deep needs — into the flat grant, transferring benefit depth from the poorest recipients to the broad recipient pool.
% ABSENT_VOICES: Advocates for deep-need populations (disability communities, chronic-poverty caseworkers) would object that no feasible flat grant matches stacked targeted support; targeted-program administrators would object to institutional dissolution; fiscal conservatives outside the advocacy coalition would object to the net cost. They sit outside the pilot-design rooms and the cross-ideological universality coalitions, where the agenda is set by movements whose credibility rides on adoption.
% DISAPPEARANCE_RATIONALE: Pilot infrastructures, advocacy organizations, and fiscal planning built around universal payments would dissolve or repurpose overnight; legislative agendas organized around universal-check designs would rearrange toward targeted reform; and recipient expectations formed by recurring payments (pandemic-era rounds, pilot stipends) would collapse, taking the cross-ideological coalition's coordinating claim with them.
% FOUNDING_PROBLEM: Provide an income floor decoupled from work status and employment record, delivered without the stigma, exclusion errors, and administrative burden of means-testing — originally framed in the negative-income-tax era as a replacement for bureaucratic paternalism, later reframed around automation-driven displacement and pandemic income shocks.
% FOUNDING_PROBLEM_CORROBORATION: Independent corroboration exists for the administrative-burden core: benefits take-up studies and access research document exclusion errors in targeted systems, and randomized pilot evaluations (Kela/Finland, OpenResearch) attest delivery feasibility from outside the advocacy set. The automation-displacement framing, by contrast, is attested almost exclusively by the advocacy movement and its technology-sector funders — no external body corroborates that specific variant, which is itself signal.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.75) because the referent arrangement combines two flows: a broad tax drawdown of roughly 1.4 trillion net after offsets, and conversion of targeted benefit depth — worth more than any feasible flat grant to bottom-quintile households — into a universal shallow payment. Suppression (0.60) is authored as a raw structural property and is deliberately NOT reconciled with extractiveness: only extractiveness is scaled by directionality and scope downstream, while suppression measures the machinery holding alternatives shut — compulsory taxation with no individual exit, plus the crowding-out of targeted programs as live policy options once universality coalitions form. Theater (0.40) reflects the growing share of activity that performs inclusion and simplicity while the binding design choice (replacement financing) proceeds with little scrutiny; pilots increasingly function as demonstration rather than measurement. Accessibility collapse is moderate (0.45): targeted aid remains a live alternative in the policy space, and the arrangement does not close it off until enacted — the collapse is prospective, not accomplished. Resistance is high (0.65): fiscal conservatives contest cost, program defenders contest replacement, and labor-market advocates contest the incentive effects; no camp is quiescent. The measurement series run on one shared ten-point grid — every tracked metric authored at every point — so no end-state value leaks backward into earlier rows. Dynamics are episodic rather than smoothly cyclical: the arrangement surges when displacement anxiety and fiscal windfalls align (the late-2010s advocacy wave, pandemic payment rounds) and recedes during consolidation eras (the post-welfare-reform decades), with base_properties values taken at the interval end (t55, the present large-pilot and blueprint era). Coalition note: the principal targets are individually weak — the working poor vote at low rates and taxpayers are diffuse — but a working-poor and deep-need-advocate coalition demanding preserved benefit depth is the structurally available counterweight, and its absence from the design rooms is itself part of how the arrangement operates.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the payer seats (working_poor, net_taxpayers) the arrangement presents as a forced trade — benefit depth exchanged for breadth, taxes levied without exit — and computes extraction-dominant. From the beneficiary seats (middle_and_upper_income_recipients, ubi_advocacy_movement) the same structure presents as a simple, dignified, universally accessible payment and computes coordination-dominant. The agenda-setter seat experiences it as open design space: replacement financing is a line item, not a fate. Fiscal scorers see only cost tables and incidence columns. The engine derives these divergences from the declared roles, power levels, and exit options; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive low directionalities: middle_and_upper_income_recipients (mobile exit, net gainer under flat-dividend designs) sits near the full-beneficiary end; ubi_advocacy_movement collects political capital rather than transfer dollars, so its derived directionality is low but not zero — its credibility is hostage to pilot outcomes, which tempers pure subsidy. The victim declarations drive high directionalities: working_poor (trapped, deepest per-dollar loss) sits nearest the full-target end; net_taxpayers (constrained, compulsory taxation) is high; targeted_program_bureaucracies bear institutional dissolution and derive high-moderate. No directionality overrides are used: the derivation from declared roles, power, and exit options reproduces the reading's incidence account without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare claim hangs entirely on the victim structure — replacement financing that converts targeted depth into universal breadth — not on universality as such. That separation prevents mislabeling in both directions: an additive, progressively financed unconditional payment would compute as ordinary redistribution (rope-adjacent), while labeling today's blueprint a pure coordination device would erase the working-poor loss the incidence tables show. The founding problem (income security without stigma or exclusion error) is contested rather than dead — poverty and administrative burden persist — so no mandatrophy declaration is authored; the arrangement advances on advocacy momentum and design lock-in, not on the husk of a finished mission.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This file instantiates only the dependency_trap_reading of the unconditional_income_support kernel; the freedom_floor_reading and universality_paradox_reading siblings would instantiate structurally different constraints with different beneficiary/victim sets and different epsilon — which structure does the corpus settle on?',
    'Generate and compare the sibling stories: the freedom floor reading should declare constrained workers as beneficiaries with low epsilon over the same standing arrangement; the universality paradox reading should center the fiscal-convergence outcome as its contested core. Cross-read the three epsilon values and victim sets as three seats on one kernel.',
    'If the sibling structures dominate the corpus verdict, this file''s snare classification stands as one seat''s contribution rather than the kernel''s classification. The disagreement between readings is located in two specific structural elements: the sign and magnitude of the labor-supply response, and the incidence accounting of program replacement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame routing: one reading of a three-reading kernel; siblings are separate constraint files, not averaged into this one.').

omega_variable(
    labor_supply_response_magnitude,
    'Is the employment response to unconditional income large enough to ground the dependency claim at national scale — the AEI meta-analysis reports roughly -3.2 percent employment in large pilots, but individual experiments range from null to modest?',
    'Pooled reanalysis of completed pilots (Finland/Kela, OpenResearch, SEED) with harmonized labor-supply endpoints, supplemented by quasi-experimental evidence from universal-payment episodes such as pandemic stimulus rounds.',
    'Near-null pooled effects dissolve the reading''s empirical foundation and pull the kernel toward the sibling readings; confirmed large effects harden the snare verdict and the victim weighting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_response_magnitude, empirical, 'Magnitude of the labor-supply distortion on which the reading''s foundational axiom rests.').

omega_variable(
    program_replacement_counterfactual,
    'Do the leading proposals actually replace targeted programs (making the working poor net losers), or is unconditional support additive on top of existing aid?',
    'Legislative costing of the major replacement-financed blueprints versus additive designs; compare the stacked value of targeted benefits for bottom-quintile households against any politically feasible flat grant.',
    'If additive and progressively financed, the victim set shrinks toward net taxpayers alone and the computed classification shifts away from pure extraction; if replacement financing is load-bearing for fiscal feasibility, the working-poor victim structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(program_replacement_counterfactual, empirical, 'Whether the victim structure depends on replacement financing being intrinsic to the design.').

omega_variable(
    net_incidence_progressivity,
    'Does the flat grant plus its financing actually redistribute upward to the non-needy, or do clawback and progressive-funding designs make net incidence progressive?',
    'Distributional scoring of specific designs (universal dividend with broad-based consumption tax versus negative-income-tax clawback schedules) by official fiscal scoring bodies using household microdata.',
    'Progressive net incidence removes the middle-and-upper-income beneficiary seat and undermines the upward-redistribution half of the reading; regressive incidence confirms it and stabilizes the snare profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_incidence_progressivity, empirical, 'Net incidence of the arrangement determines whether the non-needy are true net beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__dependency_trap_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__dependency_trap_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(unco_tr_t25, unconditional_income_support__dependency_trap_reading, theater_ratio, 25, 0.23).
narrative_ontology:measurement(unco_tr_t30, unconditional_income_support__dependency_trap_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(unco_tr_t35, unconditional_income_support__dependency_trap_reading, theater_ratio, 35, 0.14).
narrative_ontology:measurement(unco_tr_t40, unconditional_income_support__dependency_trap_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(unco_tr_t45, unconditional_income_support__dependency_trap_reading, theater_ratio, 45, 0.29).
narrative_ontology:measurement(unco_tr_t50, unconditional_income_support__dependency_trap_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(unco_tr_t55, unconditional_income_support__dependency_trap_reading, theater_ratio, 55, 0.4).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__dependency_trap_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__dependency_trap_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(unco_be_t25, unconditional_income_support__dependency_trap_reading, base_extractiveness, 25, 0.41).
narrative_ontology:measurement(unco_be_t30, unconditional_income_support__dependency_trap_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(unco_be_t35, unconditional_income_support__dependency_trap_reading, base_extractiveness, 35, 0.33).
narrative_ontology:measurement(unco_be_t40, unconditional_income_support__dependency_trap_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(unco_be_t45, unconditional_income_support__dependency_trap_reading, base_extractiveness, 45, 0.54).
narrative_ontology:measurement(unco_be_t50, unconditional_income_support__dependency_trap_reading, base_extractiveness, 50, 0.69).
narrative_ontology:measurement(unco_be_t55, unconditional_income_support__dependency_trap_reading, base_extractiveness, 55, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__dependency_trap_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__dependency_trap_reading, suppression_requirement, 20, 0.24).
narrative_ontology:measurement(unco_su_t25, unconditional_income_support__dependency_trap_reading, suppression_requirement, 25, 0.31).
narrative_ontology:measurement(unco_su_t30, unconditional_income_support__dependency_trap_reading, suppression_requirement, 30, 0.23).
narrative_ontology:measurement(unco_su_t35, unconditional_income_support__dependency_trap_reading, suppression_requirement, 35, 0.2).
narrative_ontology:measurement(unco_su_t40, unconditional_income_support__dependency_trap_reading, suppression_requirement, 40, 0.27).
narrative_ontology:measurement(unco_su_t45, unconditional_income_support__dependency_trap_reading, suppression_requirement, 45, 0.39).
narrative_ontology:measurement(unco_su_t50, unconditional_income_support__dependency_trap_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement(unco_su_t55, unconditional_income_support__dependency_trap_reading, suppression_requirement, 55, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'universal basic income' decomposes into three structurally distinct claims per the epsilon-invariance principle, forming a constraint family: this dependency_trap_reading (high epsilon over the standing arrangement as the critical seat assesses it; victims = working poor and net taxpayers; snare), freedom_floor_reading (low epsilon; beneficiaries = constrained workers; the arrangement as liberation), and universality_paradox_reading (meta-analytical; contested epsilon centered on implementation-path convergence). The freedom floor reading is the upstream advocacy claim whose pilots the dependency trap reading cites against; neither forecloses the other — they coexist as live positions held by different factions. Each file links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
