% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Autonomy-Limited Substance Regulation (Legalization Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the legalization reading of the contested
 *   substance_control_legitimacy kernel: competent adults hold autonomy over
 *   substance use, and state authority is legitimate only insofar as it
 *   prevents harm to non-consenting third parties (impaired driving,
 *   secondhand exposure), not self-regarding conduct. Under this reading
 *   users exit the victim set entirely — they are beneficiaries of
 *   decriminalization — and the constraint's extraction shifts onto two
 *   groups: third parties bearing spillover harm from a more available legal
 *   market, and the specific corporate/tax apparatus that captures surplus
 *   from the resulting licensed market. This is a single reading's structural
 *   picture; the prohibition and harm-reduction readings are separate
 *   constraints with their own victim sets and epsilon values, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - competent_adult_users: primary beneficiary of the autonomy claim (moderate/mobile) — exits criminal jeopardy, retains genuine exit from use itself
 *   - licensed_cannabis_alcohol_operators: agenda-setting beneficiary (organized/arbitrage) — captures market surplus and shapes licensing rules
 *   - bystanders_of_impaired_driving and secondhand_exposure_populations: primary targets under this reading (powerless/trapped-constrained) — bear the third-party harm the state authority is supposed to prevent
 *   - state_tax_authorities: institutional beneficiary with a revenue incentive to expand rather than minimize the taxed market
 *   - public_health_researchers: analytical observer tracking population-level outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.58).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.35).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Autonomy-Limited Substance Regulation (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, 'f0c3f04a-b94b-40a8-8573-07d6e8bbaaba').
narrative_ontology:cs_kernel_codification('f0c3f04a-b94b-40a8-8573-07d6e8bbaaba', distributed).
narrative_ontology:cs_authority_grounding('f0c3f04a-b94b-40a8-8573-07d6e8bbaaba', distributed).
narrative_ontology:cs_reading_relation('f0c3f04a-b94b-40a8-8573-07d6e8bbaaba', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('f0c3f04a-b94b-40a8-8573-07d6e8bbaaba', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('f0c3f04a-b94b-40a8-8573-07d6e8bbaaba', foundational, state_authority_bounded_by_third_party_harm).
narrative_ontology:cs_axiom_status(state_authority_bounded_by_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('f0c3f04a-b94b-40a8-8573-07d6e8bbaaba', state_authority_bounded_by_third_party_harm, deontological).
narrative_ontology:cs_axiom('f0c3f04a-b94b-40a8-8573-07d6e8bbaaba', foundational, competent_adult_self_regarding_conduct_is_not_states_business).
narrative_ontology:cs_axiom_status(competent_adult_self_regarding_conduct_is_not_states_business, holdable).
narrative_ontology:cs_axiom_grounding('f0c3f04a-b94b-40a8-8573-07d6e8bbaaba', competent_adult_self_regarding_conduct_is_not_states_business, deontological).
narrative_ontology:cs_reference_frame('f0c3f04a-b94b-40a8-8573-07d6e8bbaaba', harm_principle_liberal_settlement).
narrative_ontology:cs_drift_state('f0c3f04a-b94b-40a8-8573-07d6e8bbaaba', post_2012_commercial_legalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0c3f04a-b94b-40a8-8573-07d6e8bbaaba', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, licensed_cannabis_alcohol_operators).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, state_tax_authorities).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adult_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, bystanders_of_impaired_driving).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, secondhand_exposure_populations).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, low_income_heavy_users_facing_regressive_taxation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, low_income_heavy_users_facing_regressive_taxation).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, harm_principle_as_limit_on_state_coercion).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, adult_bodily_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchases substances through a regulated legal market instead of facing criminal penalty. Bears the market's price, tax, and quality-control terms but is no longer classified as a target of the state's coercive apparatus purely for personal use. Retains genuine exit — can abstain, self-regulate dose, or seek treatment without triggering criminal jeopardy.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adult_users, beneficiary,
    moderate, biographical, mobile, regional).

% Holds licenses to cultivate, manufacture, and sell regulated substances; lobbies for the specific rules (potency caps, retail density, tax rates) that shape the legal market's structure. Captures the surplus of the price gap between production cost and regulated retail price, and increasingly shapes enforcement priorities to favor incumbent license-holders over new entrants.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, licensed_cannabis_alcohol_operators, agenda_setter,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, licensed_cannabis_alcohol_operators, beneficiary).

% Collects excise revenue from the legal market, which funds enforcement of the third-party-harm boundary (DUI enforcement, secondhand-exposure regulation) and general revenue. Has an institutional incentive to expand the taxed market rather than minimize total substance use.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_tax_authorities, beneficiary,
    institutional, generational, analytical, regional).

% Shares roads and public space with impaired users; bears the risk of injury or death from impaired driving that a legal, more available market makes marginally more prevalent. Has no direct voice in licensing or potency decisions and cannot opt out of using shared infrastructure.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, bystanders_of_impaired_driving, payer,
    powerless, immediate, trapped, local).

% Neighbors, coworkers, and family members of users exposed to secondhand smoke or vapor in shared housing, workplaces, or public spaces where enforcement of use-location boundaries is inconsistent. Exit requires relocating housing or employment, which is often not practically available.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, secondhand_exposure_populations, payer,
    powerless, biographical, constrained, local).

% Benefits from decriminalization but bears a disproportionate share of excise taxes relative to income, since substance-use rates and tax burden as a share of income skew toward lower-income populations. Legal status without price relief leaves this group paying a de facto regressive tax to fund the regulatory apparatus.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, low_income_heavy_users_facing_regressive_taxation, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, low_income_heavy_users_facing_regressive_taxation, beneficiary).

% Former criminal-enforcement apparatus whose personal-possession caseload and budget justification shrink under this reading. Not consulted as a legitimacy-bearing party under the autonomy framework, though its institutional interests are directly affected by the reading's adoption.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibition_era_enforcement_agencies, excluded,
    institutional, biographical, constrained, regional).

% Studies population-level outcomes (DUI rates, youth initiation, treatment-seeking) under legalization regimes and reports findings that inform but do not control the regulatory settlement.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__legalization_reading, licensed_cannabis_alcohol_operators).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__legalization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a boundary between two legitimate state interests — respecting adult autonomy over self-regarding conduct, and preventing harm that spills onto non-consenting third parties — by licensing a regulated market instead of criminalizing possession, while still policing impaired driving and public exposure.
% TRANSFER_FUNCTION: Moves substance transactions from an illicit, untaxed market into a licensed, taxed market: revenue flows from users to licensed operators and tax authorities; the risk of externalized harm (impaired driving, secondhand exposure) is redistributed from the criminal-legal system onto bystanders who bear it without compensation.
% ABSENT_VOICES: Prohibition-era enforcement agencies and communities most harmed by historical criminalization both have structural stakes but limited voice in the current settlement: enforcement agencies because the framework treats their institutional preservation as irrelevant to legitimacy, and formerly-criminalized communities because expungement and reparative measures are frequently absent from the licensing-and-tax settlement even where the harm-principle logic would seem to require them.
% DISAPPEARANCE_RATIONALE: If the autonomy/third-party-harm boundary were abandoned overnight in favor of full criminalization, licensed operators would lose their legal basis, tax revenue would evaporate, users would face renewed criminal jeopardy, and enforcement resources would have to be rebuilt; if abandoned in favor of unrestricted use, third-party harm protections (DUI law, exposure limits) would need an entirely different legitimating basis. The boundary is load-bearing for the current regulatory architecture.
% FOUNDING_PROBLEM: Blanket criminalization of substance possession produced mass incarceration, racially disparate enforcement, and an untaxed illicit market, while doing little to address the genuine third-party harms (impaired driving, exposure) that justify state involvement at all.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers outside the licensed industry corroborate that legalization measurably reduces incarceration-related harm and generates tax revenue, but contest whether the current third-party-harm enforcement (DUI thresholds, exposure standards) is adequately funded or targeted; former enforcement agencies and some public-safety officials dispute that the founding problem (third-party harm) is being solved rather than merely relocated onto bystanders and low-income users.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is moderate-to-high because a legal, actively marketed substance industry generates real surplus captured disproportionately by licensed operators and tax authorities, and because the regressive tax structure imposes a real cost on low-income heavy users even as it decriminalizes them. Suppression (0.35, declining over the interval) is comparatively low and falling because the state's coercive posture toward personal possession recedes under this reading — enforcement increasingly targets only the narrower third-party-harm boundary (DUI, exposure) rather than possession itself. Theater ratio (0.28, rising) reflects growing gap between the harm-principle's stated justification (preventing third-party harm) and actual enforcement allocation, which skews toward licensing revenue protection and market administration rather than DUI/exposure enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the competent_adult_users seat, this reading looks like liberation — a rope removing an unjust criminal burden. From the bystanders_of_impaired_driving and secondhand_exposure_populations seats, the same structural settlement looks like an underenforced harm boundary that exposes them to spillover risk without recourse or compensation. From licensed_cannabis_alcohol_operators, it looks like a well-functioning regulated market they helped design. The engine computes these divergent per-seat classifications from the declared power/exit/scope data; the tangled_rope claim reflects that all three readings are structurally accurate simultaneously, not that any one seat is mistaken.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent adult users are structural beneficiaries under this reading — they were the prior victim class under prohibition and exit that set entirely, gaining mobile exit (can abstain, self-regulate, seek treatment without criminal jeopardy). Licensed operators and tax authorities are beneficiaries who additionally set the agenda (licensing terms, tax rates, potency caps). The third-party-harm bearers — bystanders of impaired driving and secondhand exposure populations — are the new primary targets: they did not choose to use, cannot exit shared public space or often their housing/employment, and bear the marginal increase in exposure that a larger, legal, marketed supply produces. Low-income heavy users occupy a dual position: beneficiaries of decriminalization, payers of a regressive tax that funds the very enforcement apparatus meant to protect third parties from harms they may also cause.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass incarceration and racially disparate enforcement from blanket criminalization, without adequately addressing genuine third-party harm) is only partially resolved: incarceration harms have measurably fallen, but the third-party-harm enforcement infrastructure this reading requires for its own legitimacy has not scaled with market growth, and the reading's tax and licensing architecture increasingly serves industry revenue interests rather than the harm-principle boundary it was built to police. Classifying this as tangled_rope (rather than a clean rope) captures that a genuine coordination function exists — the autonomy/harm boundary is a coherent legitimacy principle, not pure cover — while the licensed-market apparatus riding on it produces asymmetric extraction from bystanders and low-income users that requires active enforcement (licensing, taxation, DUI law) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    third_party_harm_enforcement_adequacy,
    'Does the licensed market''s tax revenue and regulatory apparatus actually fund and prioritize third-party-harm enforcement (DUI detection, exposure limits) at a level proportionate to the harm the market''s growth generates, or does revenue primarily fund market administration and licensing bureaucracy that entrenches incumbent operators?',
    'Comparative budget analysis across legalization jurisdictions tracking the share of excise revenue allocated to DUI enforcement and public-health harm mitigation versus licensing/regulatory administration and general revenue, over time.',
    'If enforcement is systematically underfunded relative to market growth, the tangled_rope classification is well-supported — a real coordination function (the harm boundary) is present but the extraction apparatus captures resources that should service that function. If enforcement scales with harm, the constraint moves closer to a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_enforcement_adequacy, empirical, 'Whether licensed-market revenue is proportionately redirected to the third-party-harm function that legitimates the reading.').

omega_variable(
    reading_kernel_disagreement_location,
    'Where exactly does the legalization reading''s core premise (harm principle bounding state authority to third-party harm) conflict with the prohibition reading''s core premise (substance use is inherently self-harmful in a way that grounds state authority independent of third-party effects)?',
    'This is not empirically resolvable — it is a disagreement about the proper scope of state authority over self-regarding conduct, i.e. a foundational disagreement in political philosophy (harm principle vs. legal moralism/paternalism) that different legal traditions resolve differently and that no single empirical finding settles.',
    'Because the two readings ground state authority on logically incompatible premises about WHOSE harm justifies coercion (third-party-only vs. self-harm-inclusive), this is the basis for the forecloses relation to the prohibition_reading: a jurisdiction cannot simultaneously hold that state authority is limited to third-party harm and that it extends to self-regarding harm as such — these are exhaustive and mutually exclusive positions on the same question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_disagreement_location, conceptual, 'The precise philosophical fault line between the legalization and prohibition readings'' foundational premises about the scope of legitimate state coercion.').

omega_variable(
    commercial_capture_vs_public_health_administration,
    'Is the shift toward corporate/commercial market administration (as opposed to a public-health-administered model under the harm_reduction_reading) an inherent feature of the autonomy/harm-principle logic, or a contingent policy choice this reading happens to have been implemented alongside?',
    'Comparative institutional analysis of jurisdictions that adopt the harm-principle/autonomy framework with non-commercial administration (e.g. state-monopoly retail models, non-profit cooperatives) versus commercial-licensing models, holding the underlying legitimacy premise constant.',
    'If commercial capture is contingent rather than inherent, the extractiveness attributed to this reading (0.58) is a property of one common implementation, not the reading itself, and a non-commercial implementation of the same autonomy premise would show substantially lower epsilon — suggesting this story''s high extractiveness measures implementation choice layered onto the kernel reading rather than the reading''s structural minimum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_capture_vs_public_health_administration, conceptual, 'Whether commercial market capture is intrinsic to the legalization reading or a separable implementation choice, bearing on whether epsilon here reflects the reading itself or one policy variant of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__legalization_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__legalization_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__legalization_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__legalization_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(subs_tr_t24, substance_control_legitimacy__legalization_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__legalization_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__legalization_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__legalization_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__legalization_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(subs_be_t24, substance_control_legitimacy__legalization_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__legalization_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__legalization_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__legalization_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__legalization_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(subs_su_t24, substance_control_legitimacy__legalization_reading, suppression_requirement, 24, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__legalization_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the substance_control_legitimacy kernel. prohibition_reading treats substance use itself as inherently harmful, grounding state authority in preventing self-harm through criminalization (users remain the primary victim class; epsilon reflects incarceration and enforcement costs). harm_reduction_reading treats substance use as a public health matter, grounding state duty in harm minimization without criminalization but typically without commercial market legalization (different beneficiary set — public health agencies rather than licensed commercial operators — and typically lower extractiveness since no corporate surplus capture is present). This legalization_reading grounds state authority in adult autonomy bounded by third-party harm, which removes users from the victim set but introduces a licensed commercial market whose surplus capture and regressive tax structure become the new extractive core, alongside genuinely under-addressed third-party harms (impaired driving, secondhand exposure). All three stories share ontological reference to the same underlying practice (state regulation of psychoactive substances) but instantiate structurally distinct constraints with distinct epsilon values, victim sets, and enforcement logics — consistent with the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
