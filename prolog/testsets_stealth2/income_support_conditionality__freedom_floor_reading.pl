% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Floor as Decommodification of Labor Power
 *   domain: political economy/social policy/labor economics
 *
 * SUMMARY:
 *   A national unconditional income floor pays every resident a
 *   subsistence-level amount from general taxation, with no work requirement
 *   and no means test beyond residency. Its operative effect runs through the
 *   labor market's fallback structure: because subsistence no longer depends
 *   on accepting any offered job, workers can refuse degrading terms, leave
 *   abusive employers, interrupt employment for care or training, and wait
 *   out bad offers; employers consequently face upward wage drift at the
 *   entry level and have lost the disciplinary lever that destitution once
 *   provided. The scheme is administered by a dedicated agency under a
 *   legislature that retains ordinary-legislation power over its level and
 *   eligibility perimeter, and its unconditionality is maintained against
 *   continuous administrative accretion of compliance machinery. KEY AGENTS
 *   (by structural relationship): - low_wage_workers: primary beneficiary
 *   (moderate/constrained) — hold the refusal option the floor backs -
 *   unpaid_caregivers_and_trainees: beneficiary (moderate/constrained) —
 *   finance non-market time with the floor - labor_unions: secondary
 *   beneficiary (organized/constrained) — members' improved fallback
 *   strengthens bargaining - employers_of_low_wage_labor: primary payer
 *   (institutional/arbitrage) — bear wage drift and lost workplace discipline
 *   - general_taxpayers: payer with insurance stake (moderate/constrained) —
 *   fund the floor they may fall onto - support_administration: agenda_setter
 *   (institutional/constrained) — runs payments and the compliance perimeter
 *   - legislative_authority: agenda_setter (institutional/mobile) — holds
 *   repeal power - undocumented_informal_workers: excluded
 *   (powerless/trapped) — subject to the old fallback, barred from the floor
 *   - labor_economists: analytical observer (analytical/analytical) — measure
 *   the mechanism
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.44).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.29).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Floor as Decommodification of Labor Power").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political economy/social policy/labor economics").

domain_priors:requires_active_enforcement(income_support_conditionality__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, 'b29bb37f-da7f-4ccf-962e-0b19ead028f4').
narrative_ontology:cs_kernel_codification('b29bb37f-da7f-4ccf-962e-0b19ead028f4', formalized).
narrative_ontology:cs_authority_grounding('b29bb37f-da7f-4ccf-962e-0b19ead028f4', practice).
narrative_ontology:cs_interpretation_layer_present('b29bb37f-da7f-4ccf-962e-0b19ead028f4').
narrative_ontology:cs_reading_relation('b29bb37f-da7f-4ccf-962e-0b19ead028f4', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('b29bb37f-da7f-4ccf-962e-0b19ead028f4', income_support_conditionality__wage_subsidy_reading, forecloses).
narrative_ontology:cs_axiom('b29bb37f-da7f-4ccf-962e-0b19ead028f4', foundational, positive_freedom_requires_material_independence).
narrative_ontology:cs_axiom_status(positive_freedom_requires_material_independence, holdable).
narrative_ontology:cs_axiom_grounding('b29bb37f-da7f-4ccf-962e-0b19ead028f4', positive_freedom_requires_material_independence, deontological).
narrative_ontology:cs_axiom('b29bb37f-da7f-4ccf-962e-0b19ead028f4', foundational, destitution_backed_employment_terms_are_coercive).
narrative_ontology:cs_axiom_status(destitution_backed_employment_terms_are_coercive, holdable).
narrative_ontology:cs_axiom_grounding('b29bb37f-da7f-4ccf-962e-0b19ead028f4', destitution_backed_employment_terms_are_coercive, deontological).
narrative_ontology:cs_reference_frame('b29bb37f-da7f-4ccf-962e-0b19ead028f4', universal_decommodification_floor).
narrative_ontology:cs_drift_state('b29bb37f-da7f-4ccf-962e-0b19ead028f4', contemporary_administrative_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b29bb37f-da7f-4ccf-962e-0b19ead028f4', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, unpaid_caregivers_and_trainees).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, labor_unions).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers_of_low_wage_labor).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, general_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, reservation_wage_mechanism).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, decommodification_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work in retail, care, hospitality, and warehouse jobs where pay and conditions are set against each worker's need to accept whatever is offered. The monthly payment arrives regardless of employment status, so turning down a degrading shift or leaving an abusive supervisor no longer means missing rent. Most live close to the payment amount; few have savings that could substitute for it.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    moderate, biographical, constrained, national).

% Raise children, care for elderly relatives, or study for credentials while outside or at the edge of the labor market. The payment covers basic costs during periods when market work is impossible or would cost more than it pays. Before the floor existed, these periods were financed by debt or by piecing together whatever work fit around caregiving hours.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, unpaid_caregivers_and_trainees, beneficiary,
    moderate, biographical, constrained, national).

% Represent members in sectors where the floor has raised the cost of replacing a striking or refusing worker. Strike funds stretch further when members' household basics are secure, and organizers pitch the floor as background security that members hold whether or not the union wins a particular dispute.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, labor_unions, beneficiary,
    organized, generational, constrained, national).

% Run businesses staffed heavily from the low-wage labor pool. Entry-level wages have drifted upward since the floor arrived, turnover among dissatisfied staff has risen, and the implicit threat of destitution no longer secures attendance or acceptance of inconvenient schedules. Some firms respond with automation and scheduling software; others lobby for repeal or for converting the payment into work-conditioned credits.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers_of_low_wage_labor, payer,
    institutional, generational, arbitrage, national).

% Fund the payment through income and consumption taxes. Households above the floor pay in more than they receive in most years, but any of them can fall onto the floor through job loss, illness, or divorce, and most carry the payment as insurance against that fall as much as they experience it as a bill.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, general_taxpayers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__freedom_floor_reading, general_taxpayers, beneficiary).

% Runs the payment system: verifies residency and identity, disburses monthly amounts, and operates the compliance machinery that has grown up around the scheme — fraud investigation, address checks, and the sanction schedule applied when recipients miss administrative appointments. Career officials defend the scheme's universality while expanding the paperwork that surrounds it.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, support_administration, agenda_setter,
    institutional, generational, constrained, national).

% Sets the payment level, the tax rates that fund it, and the eligibility perimeter, and holds the ordinary-legislation power to narrow, condition, or abolish the scheme. Each budget cycle brings proposals from the fiscal-conservative wing to convert the payment into earned credits; each dies against the electoral weight of households on or near the floor.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, legislative_authority, agenda_setter,
    institutional, immediate, mobile, national).

% Work in agriculture, construction, domestic service, and informal trade under the same subsistence pressures the floor was built to relieve, but legal status bars them from claiming it. They experience the post-floor labor market from beneath it: wages at the very bottom have risen somewhat as covered workers gained refusal power, but their own terms remain set by deportation risk.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, undocumented_informal_workers, excluded,
    powerless, immediate, trapped, national).

% Track reservation wages, quit rates, participation, and price pass-through around the floor's introduction and subsequent adjustments. Their natural-experiment studies supply the numbers every other seat argues with, and their professional disagreements track the same underlying mechanism emphasized from different directions.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, labor_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:fixing_cost_class(income_support_conditionality__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every resident a survivable income unconditionally, which converts refusal of a job offer from an individual gamble with destitution into a collectively backed position: each worker's no is credible because no one's subsistence depends on saying yes.
% TRANSFER_FUNCTION: Moves tax revenue from the general tax base to households at or below subsistence, and moves bargaining leverage from employers to workers by detaching subsistence from job acceptance.
% ABSENT_VOICES: Undocumented and informal-sector workers are subject to the same labor-market pressures the floor relieves but are barred from it by legal status; future taxpayers bear the fiscal consequences without a seat; recipients most exposed to sanction design have thin representation in the administrative consultations that shape the compliance perimeter.
% DISAPPEARANCE_RATIONALE: Bottom-quartile wages would fall back toward subsistence tolerance within a few contract cycles, quits and refusals would collapse, caregiving and training spells would be financed by debt or abandoned, and the scheduling and attendance practices the floor priced out would return; the fiscal apparatus would shrink, but the labor market's previous disciplinary equilibrium would reassert itself.
% FOUNDING_PROBLEM: Market economies set wages against each worker's fallback, and the fallback for most of history was destitution; individually rational acceptance of whatever terms were offered produced collectively degraded terms, because no worker could credibly refuse alone.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: labor-history scholarship documents subsistence-tolerance wage setting before public income support existed; employer-side testimony and lobbying concede that guaranteed incomes raise reservation wages, attesting the mechanism while disputing its worth; and the scheme's harshest critics build their case on the same behavioral fact this reading celebrates — that recipients can now say no.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.44: the fiscal transfer is large and real, employer-side adjustment costs are real, and administrative overhead grows; the freedom-floor frame counts much of the employer-side change as intended correction rather than extraction, which is why the figure sits at moderate rather than high. Suppression is 0.29: compulsory taxation and a growing sanction-and-compliance perimeter coerce, but the floor closes no alternative arrangement — conditional designs remain legislatively available and the floor adds options rather than removing them. Theater is 0.22: delivery is overwhelmingly functional, with performative growth concentrated in compliance rituals (appointment-keeping, address verification) that reproduce the appearance of conditionality inside an unconditional scheme. Accessibility collapse is 0.28: understanding the floor does not foreclose rival policy architectures. Resistance is 0.58: sustained employer and fiscal-conservative opposition, recurring conversion proposals, and framing conflicts meet the scheme every budget cycle. The three measurement series share one seven-point grid (t=0..24) so no metric's row is backfilled from another's endpoint; all three drift upward together, modeling conditionality creep and fiscal drag rather than a cyclical pattern. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the worker and caregiver seats the floor is background security that changed what a refusal means; from the employer seat the same arrangement is a cost structure that removed a management tool; from the taxpayer seat it is simultaneously a bill and an insurance policy, and which description dominates depends on transition risk the static declaration cannot express; from the excluded seat the labor market's old fallback persists intact beneath the floor that relieved everyone else. The administration seat experiences neither benefit nor burden but the compliance perimeter itself. These positions are computed per seat from the structural data; the authored rope claim reflects the reading's coordination-dominant structure and does not adjudicate the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: workers and caregivers sit near the beneficiary end (the floor subsidizes their refusal option; d near 0.1); unions mildly so (organizing leverage; d near 0.25); employers sit near the target end (they bear wage drift and lost disciplinary leverage; d near 0.8) — the reading counts their loss as correction of a prior advantage, but structurally they still bear the arrangement's costs; taxpayers are declared victims and the derivation will read them near full target, yet their true position is near-symmetric because the floor doubles as universal insurance — the derivation chain cannot see option value, and a power-atom override would misapply to the moderate-power worker seat as well, so the ambiguity is recorded as the taxpayer_net_incidence omega instead; the administration seat is near-neutral (runs the machine, collects no rents); excluded informal workers are governed by the labor market but not covered — spillover wage gains reach them thinly, leaving them near the middle with none of the protection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — destitution-backed wage discipline — is live wherever the floor is absent or eroded, and the arrangement's function is intact, so no mandatrophy declaration is authored. The risk this story watches is the inverse of a dead mandate kept alive theatrically: a live mandate quietly narrowed, where conditionality creep replaces the unconditional function with a conditional one while the statute's language and the scheme's name persist. The theater_ratio and suppression_requirement series are the tripwires: theater above 0.5 or a steepening suppression slope would indicate the freedom-floor function is being performed rather than delivered, at which point the arrangement warrants reclassification under whichever reading its actual operation has come to instantiate. Claiming rope while authoring real payer costs keeps that detection honest: if the engine computes tangled_rope or worse from the seats, the divergence flags the coordination/extraction boundary for review rather than settling it by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the freedom_floor_reading of the income_support_conditionality kernel; which reading a consumer adopts determines the entire victim set and computed type — what would change under the sibling readings?',
    'Cross-reading compilation: author and compile the dependency_trap_reading and wage_subsidy_reading stories, then diff beneficiary/victim sets, epsilon, and computed types against this story''s.',
    'Under dependency_trap_reading, recipients become victims and the arrangement computes toward snare; under wage_subsidy_reading, taxpayers become the primary victims and extraction concentrates on the funding side; this story''s rope claim holds only within the freedom-floor frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer frame: one of three readings of the income-support-conditionality kernel.').

omega_variable(
    employer_cost_valence,
    'Is the employer seat''s loss of destitution-backed workplace discipline a cost that belongs in the extraction ledger, or the removal of a prior advantage that the ledger should not count?',
    'Adopt an explicit accounting convention and price it: quantify employer-side losses (entry-wage drift, turnover, automation outlays) against the employer share of demand-stabilization and labor-supply-reliability benefits.',
    'Counting employer losses as extraction pushes the computed type toward tangled_rope; treating them as correction preserves the rope reading — the classification hinges on this bookkeeping choice, not on new data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_cost_valence, conceptual, 'Whether employer-side losses are extraction or de-extraction.').

omega_variable(
    conditionality_creep_risk,
    'Will the unconditional premise survive contact with fiscal pressure and administrative discretion, or does sanction machinery accrete until the floor is conditional in practice?',
    'Longitudinal tracking of sanction rates, exemption criteria, and appointment-compliance requirements against the statutory promise of unconditionality; the suppression_requirement series in this story is the leading indicator.',
    'Full convergence to practical conditionality dissolves this reading''s constraint into the workfare arrangement the kernel contests, collapsing the freedom-floor classification; partial creep leaves the rope claim intact with degraded margins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_creep_risk, empirical, 'Whether unconditionality survives administratively.').

omega_variable(
    taxpayer_net_incidence,
    'Is the median taxpayer a net funder of the floor or a net holder of its insurance, once option value, spillover wage effects, and stabilization benefits are counted?',
    'Lifetime-incidence microsimulation over the tax and benefit schedule, including transition probabilities onto the floor.',
    'Net-funder results sharpen the taxpayer seat toward full target and raise effective extraction; net-insurance results pull the seat toward symmetry and soften it — the derivation chain reads the victim declaration alone and cannot see option value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taxpayer_net_incidence, empirical, 'Taxpayer seat sits between funder and insuree.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(freedom_floor_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(freedom_floor_tr_t4, income_support_conditionality__freedom_floor_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(freedom_floor_tr_t8, income_support_conditionality__freedom_floor_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(freedom_floor_tr_t12, income_support_conditionality__freedom_floor_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(freedom_floor_tr_t16, income_support_conditionality__freedom_floor_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(freedom_floor_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(freedom_floor_tr_t24, income_support_conditionality__freedom_floor_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(freedom_floor_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(freedom_floor_be_t4, income_support_conditionality__freedom_floor_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(freedom_floor_be_t8, income_support_conditionality__freedom_floor_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(freedom_floor_be_t12, income_support_conditionality__freedom_floor_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(freedom_floor_be_t16, income_support_conditionality__freedom_floor_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(freedom_floor_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(freedom_floor_be_t24, income_support_conditionality__freedom_floor_reading, base_extractiveness, 24, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(freedom_floor_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(freedom_floor_su_t4, income_support_conditionality__freedom_floor_reading, suppression_requirement, 4, 0.17).
narrative_ontology:measurement(freedom_floor_su_t8, income_support_conditionality__freedom_floor_reading, suppression_requirement, 8, 0.19).
narrative_ontology:measurement(freedom_floor_su_t12, income_support_conditionality__freedom_floor_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement(freedom_floor_su_t16, income_support_conditionality__freedom_floor_reading, suppression_requirement, 16, 0.25).
narrative_ontology:measurement(freedom_floor_su_t20, income_support_conditionality__freedom_floor_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(freedom_floor_su_t24, income_support_conditionality__freedom_floor_reading, suppression_requirement, 24, 0.29).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial debate over unconditional income support decomposes into three structurally distinct constraints sharing one referent (the standing unconditional-support arrangement) with reading-indexed epsilon — freedom_floor_reading (this story), dependency_trap_reading, and wage_subsidy_reading. Each member links the other two via affects_constraints. The freedom-floor story sits downstream of the reservation-wage empirical literature all three readings cite; its classification is the pivot the sibling readings dispute, so its epsilon is authored for the standing arrangement as this reading assesses it, never for the arrangements the siblings would substitute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
