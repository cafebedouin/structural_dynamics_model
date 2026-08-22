% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__prohibition_reading, []).

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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Criminal Prohibition of Substance Use as Moral Transgression
 *   domain: criminal_justice/public_health/political_economy
 *
 * SUMMARY:
 *   This story instantiates the prohibition reading of the substance control
 *   kernel: substance use is treated as a moral transgression against social
 *   order, warranting criminal punishment of the user rather than treatment,
 *   regulation, or tolerance. Under this reading, users enter the criminal
 *   victim set directly (not merely the health-condition victim set of the
 *   harm reduction reading, nor the
 *   liberty-restricted-only-for-third-party-harm set of the legalization
 *   reading). The enforcement apparatus — police, prosecutors, private
 *   prisons, forfeiture units — becomes the primary structural beneficiary,
 *   collecting budget, occupancy revenue, and political capital in direct
 *   proportion to enforcement volume rather than to any measured reduction in
 *   harm. A substantial secondary externality is the violence of illicit
 *   markets that prohibition itself displaces into unregulated channels,
 *   since no legal dispute-resolution mechanism exists for an activity
 *   defined as criminal. This is a single, clean ε for the prohibition
 *   reading only; the harm_reduction_reading and legalization_reading are
 *   separate constraints with their own ε values and stakeholder sets, linked
 *   here only through the shared kernel and network edges — this file does
 *   not average over them or describe their contest internally.
 *
 * KEY AGENTS:
 *   - law_enforcement_agencies: primary agenda-setter and beneficiary (institutional/arbitrage) — administers punishment and collects budget/forfeiture revenue tied to volume
 *   - substance_users: primary target (powerless/trapped) — enters criminal victim set directly under this reading
 *   - private_prison_operators: secondary institutional beneficiary (organized/arbitrage) — occupancy-contingent revenue
 *   - illicit_market_organizations: excluded structural dependent (organized/arbitrage) — supplies the market prohibition displaces, including its enforcement violence
 *   - public_health_researchers: analytical observer (moderate/analytical) — documents comparative outcomes across kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.81).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Criminal Prohibition of Substance Use as Moral Transgression").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "criminal_justice/public_health/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, '3de72dbe-3f91-45cd-8d81-60a7c666c9b3').
narrative_ontology:cs_kernel_codification('3de72dbe-3f91-45cd-8d81-60a7c666c9b3', distributed).
narrative_ontology:cs_authority_grounding('3de72dbe-3f91-45cd-8d81-60a7c666c9b3', extraction).
narrative_ontology:cs_interpretation_layer_present('3de72dbe-3f91-45cd-8d81-60a7c666c9b3').
narrative_ontology:cs_reading_relation('3de72dbe-3f91-45cd-8d81-60a7c666c9b3', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3de72dbe-3f91-45cd-8d81-60a7c666c9b3', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('3de72dbe-3f91-45cd-8d81-60a7c666c9b3', foundational, use_itself_constitutes_moral_transgression).
narrative_ontology:cs_axiom_status(use_itself_constitutes_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('3de72dbe-3f91-45cd-8d81-60a7c666c9b3', use_itself_constitutes_moral_transgression, deontological).
narrative_ontology:cs_axiom('3de72dbe-3f91-45cd-8d81-60a7c666c9b3', foundational, state_punishment_is_necessary_to_preserve_social_order).
narrative_ontology:cs_axiom_status(state_punishment_is_necessary_to_preserve_social_order, holdable).
narrative_ontology:cs_axiom_grounding('3de72dbe-3f91-45cd-8d81-60a7c666c9b3', state_punishment_is_necessary_to_preserve_social_order, instrumental).
narrative_ontology:cs_reference_frame('3de72dbe-3f91-45cd-8d81-60a7c666c9b3', early_twentieth_century_moral_order_consensus).
narrative_ontology:cs_drift_state('3de72dbe-3f91-45cd-8d81-60a7c666c9b3', contemporary_public_health_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3de72dbe-3f91-45cd-8d81-60a7c666c9b3', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, private_prison_operators).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, asset_forfeiture_units).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, politicians_running_on_order_platforms).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, low_income_drug_defendants).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, communities_of_color_targeted_by_enforcement).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, families_of_incarcerated_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers drug enforcement, sets operational priorities, and receives budget allocations tied to arrest and seizure volume. Justifies the arrangement as protecting public order and deterring transgression. Collects federal grant funding, equipment transfers, and forfeiture revenue that scale with enforcement intensity rather than measured harm reduction.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, law_enforcement_agencies, beneficiary).

% Contracts with state and federal correctional systems guarantee per-bed revenue, often with minimum-occupancy clauses. Drug offense incarceration supplies a substantial, stable population. Lobbies to maintain sentencing structures that keep occupancy high; has no direct exposure to the moral or health claims underlying prohibition.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, private_prison_operators, beneficiary,
    organized, generational, arbitrage, national).

% Seizes cash, vehicles, and property from suspected drug offenders, often without conviction, and retains a share of proceeds for departmental use. Revenue is directly proportional to enforcement volume, creating an operational incentive to sustain and expand the criminalized category.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, asset_forfeiture_units, beneficiary,
    institutional, immediate, arbitrage, national).

% Campaigns on tough-on-crime and moral-order platforms, using visible enforcement statistics as evidence of effectiveness. Bears no direct cost of enforcement failures; can shift positions or retire from office if the framing becomes politically costly, unlike the populations subject to it.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, politicians_running_on_order_platforms, beneficiary,
    powerful, biographical, mobile, national).

% Faces arrest, prosecution, incarceration, and permanent criminal record for possession or use, regardless of dependency status or harm caused to others. Cannot exit the criminalized category by seeking treatment without risking self-incrimination in many jurisdictions; abstinence from all controlled substances is the only formally recognized exit.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, biographical, trapped, national).

% Cannot afford private counsel or bail, so pleads guilty at far higher rates than resourced defendants regardless of case merits, converting the moral-transgression framing into disproportionate carceral outcomes concentrated by income.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, low_income_drug_defendants, payer,
    powerless, biographical, trapped, regional).

% Experiences enforcement at rates disproportionate to underlying use rates, documented across multiple jurisdictions. Bears concentrated collateral consequences — housing exclusion, employment barriers, voting disenfranchisement — that compound across generations within the same neighborhoods.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, communities_of_color_targeted_by_enforcement, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, communities_of_color_targeted_by_enforcement, excluded).

% Loses income, caregiving capacity, and household stability when a member is incarcerated for use-related offenses. Bears costs (legal fees, lost wages, dependent care) with no formal role in the arrangement and no mechanism to seek redress.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, families_of_incarcerated_users, payer,
    powerless, generational, trapped, regional).

% Operates the supply chain that prohibition displaces from regulated to unregulated channels. Not a party to the formal arrangement but structurally dependent on it — prohibition creates the price premium and enforcement risk that constitutes their entire business model, including the violence used to enforce contracts outside legal recourse.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, illicit_market_organizations, excluded,
    organized, biographical, arbitrage, national).

% Studies incarceration outcomes, overdose trends, and enforcement disparities under prohibition regimes and compares them against alternative frameworks. Publishes findings that inform but do not control policy, and is frequently excluded from legislative drafting despite domain expertise.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_researchers, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, publicly legible signal that substance use is socially disapproved and subject to uniform state response, which some communities and political constituencies value as a marker of collective moral order regardless of measured health outcomes.
% TRANSFER_FUNCTION: Moves liberty, income, and family stability from substance users and their households to the enforcement, incarceration, and forfeiture apparatus, and moves political legitimacy to officeholders who campaign on visible enforcement activity.
% ABSENT_VOICES: Substance users themselves are rarely consulted in drafting the statutes that criminalize them; public health researchers documenting comparative outcomes are frequently excluded from legislative hearings in favor of law enforcement testimony; illicit market organizations that structurally depend on the arrangement have no formal voice but shape enforcement priorities through the violence prohibition displaces into unregulated channels.
% DISAPPEARANCE_RATIONALE: If criminal prohibition of use vanished overnight, arrest volumes, incarceration populations, forfeiture revenue streams, and the political rhetoric built on visible enforcement would collapse; law enforcement budget justifications, private prison occupancy contracts, and black market pricing structures all depend on the criminalized category continuing to exist.
% FOUNDING_PROBLEM: Early twentieth-century prohibition regimes were built to address genuine social harms associated with unregulated substance markets — public intoxication, unregulated potency, and perceived threats to labor discipline and family structure — by declaring use itself a moral transgression subject to state punishment.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement agencies and politicians running on order platforms attest the founding problem remains live, citing ongoing substance-related harms as evidence prohibition still functions as intended. Public health researchers, sentencing reform commissions, and comparative national drug-policy analyses from outside the enforcement and incarceration beneficiary set report that criminalization correlates with higher overdose mortality and no measurable reduction in use rates relative to regulatory alternatives, supporting a reading in which the original harm-reduction rationale has been substantially decoupled from the arrangement's persistence.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__prohibition_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.81) and rising over the interval because the punishment apparatus's budget and legitimacy justifications scale with enforcement volume rather than with any independently measured reduction in substance-related harm — a structural decoupling that produces steadily accumulating extraction even as public health evidence against the approach accumulates in parallel. Suppression is very high (0.88) because persistence depends on active, expanding criminalization machinery: arrest, prosecution, incarceration, forfeiture, and collateral civil disabilities, each of which forecloses exit for the target population. Theater ratio is moderate and rising (0.42) reflecting that a growing share of enforcement activity (highly visible raids, seizure announcements, sentencing enhancements) serves political-legitimacy signaling functions distinguishable from any measurable deterrent effect. Accessibility collapse is comparatively low (0.4) because, unlike a genuine mountain, workable alternatives (regulation, decriminalization, treatment-based frameworks) are visibly implemented in other jurisdictions and documented in the public health literature — the alternatives are suppressed by enforcement and political framing, not structurally foreclosed. Resistance is substantial (0.72), evidenced by sustained reform movements, ballot initiatives, and litigation challenging the framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement agencies, private prison operators, asset forfeiture units, and order-platform politicians sit near the full-beneficiary end of directionality: each collects a distinct revenue or legitimacy stream whose magnitude scales with enforcement intensity, and each retains institutional or political mobility independent of enforcement outcomes. Substance users, low-income defendants, targeted communities, and their families sit near the full-target end: trapped exit options, powerless structural position, and direct exposure to the criminal sanctions that constitute the constraint's operative mechanism. Illicit market organizations occupy an unusual excluded-but-structurally-dependent position — they benefit financially from the price premium prohibition creates but hold no formal role and bear the violence-enforcement costs that substitute for legal contract enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine harms from unregulated substance markets in the early twentieth century) is contested as live: enforcement-side beneficiaries attest it remains live, while public health researchers and comparative jurisdictional evidence attest the mechanism has decoupled from harm reduction and instead reproduces harm through incarceration, overdose risk, and multigenerational collateral consequences. This mismatch between founding_problem_status (contested, trending dead outside the beneficiary set) and disappearance_verdict (world_rearranges) is exactly the signature the R5 genealogy interview is designed to surface: an arrangement whose original justification has substantially eroded but whose institutional apparatus persists and continues to expand, which is the classic zombie-mandate pattern distinguishable from genuine ongoing coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prohibition_reading_identity,
    'Is the prohibition reading of the substance control kernel a defensible independent framework, or does it function primarily as legitimating cover for the enforcement apparatus''s revenue and legitimacy capture?',
    'Compare enforcement-intensity trends against independently measured substance-related harm trends across jurisdictions that maintain prohibition versus jurisdictions that shift to harm_reduction_reading or legalization_reading frameworks; if enforcement intensity rises while harm metrics do not improve relative to comparators, the coordination rationale weakens.',
    'If the coordination rationale is substantially hollowed out, the prohibition reading computes closer to snare than tangled_rope at the analytical seat, even though the moral-order coordination claim remains genuinely held by some constituencies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_reading_identity, conceptual, 'Whether prohibition''s moral-order coordination function is genuine or primarily a cover story for enforcement-apparatus extraction.').

omega_variable(
    sibling_reading_structural_delta,
    'What structurally changes if the harm_reduction_reading or legalization_reading were adopted in place of the prohibition_reading for the same underlying substance-use phenomenon?',
    'This is not resolved within this file — it is the subject of the sibling constraint files (harm_reduction_reading, legalization_reading), each with its own ε, beneficiary/victim structure, and stakeholder set. The delta is documented here for the committer record: under harm_reduction_reading, users exit the criminal victim set and enter a service-recipient position, and the primary beneficiary shifts from enforcement agencies to health-service providers; under legalization_reading, the state''s authority shifts from coercive to regulatory/tax-capture, and the criminalized victim set collapses almost entirely except for third-party-harm cases.',
    'Demonstrates the ε-invariance principle in practice: the three readings are not the same constraint measured three ways but three distinct constraints with different victim sets, different beneficiaries, and different ε values, linked structurally through the shared kernel rather than through a shared classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents where the three kernel readings diverge structurally, routed here per Rule 2 rather than folded into this file''s classification.').

omega_variable(
    black_market_violence_attribution,
    'Is the violence associated with illicit substance markets an externality of the prohibition reading specifically, or would comparable violence emerge under any regime that maintains price differentials between regulated and unregulated supply?',
    'Comparative analysis of violence rates in jurisdictions transitioning from prohibition to regulated/legalized frameworks (e.g., cannabis markets post-legalization) versus jurisdictions maintaining prohibition, controlling for baseline organized-crime presence.',
    'If violence substantially declines post-transition, it corroborates that the prohibition reading''s coercive-rather-than-service-provision authority structurally produces the externality rather than merely coinciding with it, strengthening the case that illicit_market_organizations'' excluded position is a direct structural product of this reading specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_violence_attribution, empirical, 'Whether black-market violence is a specific product of the prohibition reading''s coercive authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__prohibition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__prohibition_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__prohibition_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(subs_tr_t30, substance_control_kernel__prohibition_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(subs_tr_t40, substance_control_kernel__prohibition_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(subs_tr_t50, substance_control_kernel__prohibition_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__prohibition_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__prohibition_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__prohibition_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(subs_be_t30, substance_control_kernel__prohibition_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(subs_be_t40, substance_control_kernel__prohibition_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(subs_be_t50, substance_control_kernel__prohibition_reading, base_extractiveness, 50, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__prohibition_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__prohibition_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__prohibition_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(subs_su_t30, substance_control_kernel__prohibition_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(subs_su_t40, substance_control_kernel__prohibition_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(subs_su_t50, substance_control_kernel__prohibition_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of substance_control_kernel, each authored as a separate, ε-invariant story per the kernel/reading rules. prohibition_reading (this file) treats use as moral transgression warranting criminal punishment, producing high ε concentrated on a criminal victim set with enforcement apparatus as primary beneficiary. harm_reduction_reading treats use as a health condition, shifting the beneficiary set toward health-service providers and substantially lowering victim-set criminalization. legalization_reading treats use as an individual liberty matter with state authority limited to externality capture, collapsing the criminalized victim set to third-party-harm cases only. The three files share no metrics and are not averaged; they are linked here for contamination-propagation and family-tracing purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
