% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:measurement_basis/2,
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
 *   This story instantiates the legalization reading of the
 *   substance_control_legitimacy kernel: competent adults hold a presumptive
 *   liberty to use substances, and the state's remaining authority is
 *   narrowed to preventing harm to third parties (impaired driving,
 *   secondhand exposure, harm to dependents). Under this reading users
 *   themselves exit the victim set that a prohibition reading would place
 *   them in — the constraint no longer treats their own use as the harm to be
 *   prevented. What remains extractive is (a) a licensed commercial market
 *   that has captured the regulatory space vacated by criminal law and now
 *   profits from consumption intensity in ways the original harm-principle
 *   justification did not anticipate, and (b) unresolved externalized harms
 *   to third parties and to communities carrying legacy costs from the
 *   prohibition era this reading superseded. The sibling readings —
 *   prohibition_reading (moral-duty criminalization) and
 *   harm_reduction_reading (public-health-derived state duty) — are NOT
 *   represented in this file; they are separate constraints with their own ε,
 *   victim sets, and stakeholder surfaces, linked only via
 *   network.affects_constraints and cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - competent_adult_users: primary beneficiary of the autonomy claim, exits the victim set under this reading
 *   - licensed_cannabis_and_alcohol_industry: agenda-setting beneficiary, captures the legal market
 *   - state_tax_and_licensing_authority: administers the narrowed regulatory boundary, collects revenue
 *   - third_parties_exposed_to_impaired_drivers: primary victim class this reading itself recognizes as legitimate
 *   - communities_with_disproportionate_enforcement_history: bear legacy cost not remedied by the reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.52).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.35).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Autonomy-Limited Substance Regulation (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '8d884667-bbfa-4d86-aff6-b3e04494ed0e').
narrative_ontology:cs_kernel_codification('8d884667-bbfa-4d86-aff6-b3e04494ed0e', distributed).
narrative_ontology:cs_authority_grounding('8d884667-bbfa-4d86-aff6-b3e04494ed0e', distributed).
narrative_ontology:cs_reading_relation('8d884667-bbfa-4d86-aff6-b3e04494ed0e', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('8d884667-bbfa-4d86-aff6-b3e04494ed0e', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('8d884667-bbfa-4d86-aff6-b3e04494ed0e', foundational, harm_principle_as_sole_legitimate_basis_for_coercion).
narrative_ontology:cs_axiom_status(harm_principle_as_sole_legitimate_basis_for_coercion, holdable).
narrative_ontology:cs_axiom_grounding('8d884667-bbfa-4d86-aff6-b3e04494ed0e', harm_principle_as_sole_legitimate_basis_for_coercion, deontological).
narrative_ontology:cs_axiom('8d884667-bbfa-4d86-aff6-b3e04494ed0e', foundational, adult_competence_presumption_absent_third_party_harm).
narrative_ontology:cs_axiom_status(adult_competence_presumption_absent_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('8d884667-bbfa-4d86-aff6-b3e04494ed0e', adult_competence_presumption_absent_third_party_harm, deontological).
narrative_ontology:cs_reference_frame('8d884667-bbfa-4d86-aff6-b3e04494ed0e', harm_principle_liberal_baseline).
narrative_ontology:cs_drift_state('8d884667-bbfa-4d86-aff6-b3e04494ed0e', post_state_level_legalization_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8d884667-bbfa-4d86-aff6-b3e04494ed0e', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adult_users).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, licensed_cannabis_and_alcohol_industry).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, state_tax_authorities).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, third_parties_exposed_to_impaired_drivers).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, secondhand_exposure_bystanders).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, low_income_users_in_regulated_markets).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, communities_with_disproportionate_enforcement_history).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, low_income_users_in_regulated_markets).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, harm_principle_as_limit_on_state_coercion).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, adult_competence_presumption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase and consume regulated substances (alcohol, cannabis, and in some jurisdictions other drugs) as a matter of personal liberty. Under this reading they exit the constraint's victim set entirely — the state's legitimate interest in their own use is minimal; they bear taxes and licensing costs but retain the core choice.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adult_users, beneficiary,
    moderate, biographical, mobile, national).

% Operates within a licensing regime it lobbies to shape, capturing the legal market created by decriminalization. Markets potency, convenience, and branding in ways that push consumption past the harm-principle's original justification; extracts profit from heavy users disproportionately, mirroring alcohol and tobacco industry patterns.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, licensed_cannabis_and_alcohol_industry, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, licensed_cannabis_and_alcohol_industry, agenda_setter).

% Sets licensing rules, tax rates, and the boundary of permitted third-party-harm regulation (impaired driving limits, secondhand exposure rules, marketing restrictions). Justifies its remaining authority narrowly, as harm-prevention rather than paternalism, but administers a revenue-generating apparatus with incentive to expand the regulated (and taxed) market.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_tax_and_licensing_authority, agenda_setter,
    institutional, generational, analytical, national).

% Bear the primary externalized risk this reading recognizes as legitimately regulable: injury or death from impaired driving. They have no relationship to the user's choice to consume and no exit from shared roads; their protection depends entirely on enforcement of the third-party-harm boundary, which is the one enforcement function this reading endorses.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, third_parties_exposed_to_impaired_drivers, payer,
    powerless, immediate, trapped, local).

% Children, coworkers, and cohabitants exposed to secondhand smoke or vapor, or affected by a household member's impaired caregiving. Their harm is real but harder to draw a bright regulatory line around than traffic injury, so enforcement of their protection is inconsistent.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, secondhand_exposure_bystanders, payer,
    powerless, immediate, constrained, local).

% Gain formal legal protection from criminalization but face regulated-market prices inflated by taxation and licensing overhead, sometimes pushing them toward a residual illicit market; also bear a disproportionate share of any remaining enforcement (public consumption citations, unlicensed sale prosecutions) despite the reading's formal decriminalization of personal use.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, low_income_users_in_regulated_markets, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, low_income_users_in_regulated_markets, beneficiary).

% Carry the accumulated harm of decades of prohibition-era enforcement (convictions, family disruption, wealth loss) that legalization does not automatically remedy; expungement and equity licensing provisions vary and are frequently underfunded or contested, leaving this population bearing legacy costs while new market entrants capture new profits.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, communities_with_disproportionate_enforcement_history, payer,
    powerless, generational, trapped, regional).

% Argue respectively that the harm principle understates aggregate social cost (prohibition reading) or that autonomy framing under-resources treatment and public health infrastructure (harm-reduction reading). Their objections are debated in policy fora but structurally outside the legalization regime's own operating logic once it is enacted.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibition_and_harm_reduction_advocates, excluded,
    organized, generational, analytical, national).

% Study population-level outcomes (traffic fatalities, youth use rates, treatment admissions, tax revenue allocation) across legalization jurisdictions and can attest whether the harm-principle boundary is being honored in practice or eroded by commercial pressure.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_researchers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__legalization_reading, licensed_cannabis_and_alcohol_industry).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__legalization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bright-line rule — state authority may regulate third-party harm from substance use but not the private choice of a competent adult to use — allowing courts, legislatures, and licensing bodies to coordinate around a single, administrable boundary instead of litigating paternalism case by case.
% TRANSFER_FUNCTION: Moves formal criminal liability and stigma off individual users and onto a licensed commercial sector; simultaneously moves tax revenue from untaxed illicit transactions to state coffers, and moves residual harm (impaired driving, secondhand exposure, legacy enforcement damage) onto third parties and historically over-policed communities who receive no corresponding share of the new legal market's profit.
% ABSENT_VOICES: Prohibitionists who believe the harm principle undercounts aggregate social cost, and harm-reduction advocates who believe autonomy framing starves public health infrastructure of funding priority, are both structurally outside this reading's operating logic once enacted into law — they participate in the ongoing political contest over the kernel but not in the day-to-day administration of the legalization regime.
% DISAPPEARANCE_RATIONALE: If the autonomy/harm-principle boundary were abandoned overnight, licensed markets would lose their legal basis, tax revenue streams would collapse, millions of adult users would revert to criminalized status, and enforcement resources would need to be redirected — the legal, commercial, and fiscal architecture built on this boundary is extensive and would visibly reorganize.
% FOUNDING_PROBLEM: Decades of criminalization of personal substance use produced mass incarceration, racially disparate enforcement, and a large untaxed illicit market, without demonstrably reducing use; the legalization reading was built to relocate state authority to the harm principle — regulate what harms others, leave personal choice alone.
% FOUNDING_PROBLEM_CORROBORATION: Users and the licensed industry attest the founding problem (over-criminalization) is substantially solved. Communities carrying legacy enforcement harm and public health researchers attest the underlying problem is only partially addressed — expungement lags licensing, and commercial market growth has introduced a distinct problem (aggressive marketing, potency escalation) the original reform did not anticipate; this corroboration comes from outside the beneficiary set (academic outcome studies, community advocacy groups excluded from licensing profits).
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate-and-rising 0.52 by interval end — not the low value one might expect from a liberty-maximizing reform, because the commercial capture of the licensed market (aggressive marketing, potency escalation, tax structures that regressively burden low-income users) has grown steadily since early legalization waves. Suppression is authored moderate-and-slightly-falling (0.40 down to 0.35): the coercive apparatus genuinely narrows as criminal enforcement of personal use recedes, but does not vanish, since impaired-driving and public-consumption enforcement persist and fall disproportionately on already-over-policed communities. Theater ratio is low but rising modestly (0.10 to 0.22) as equity-licensing and expungement provisions are increasingly cited rhetorically while underfunded in practice. Accessibility collapse is low (0.30) — real alternatives (continued illicit markets, interstate arbitrage, policy reversal) persist and are exercised. Resistance is moderate-high (0.55) reflecting active contestation from both prohibitionist and harm-reduction camps plus equity advocates within the legalization coalition itself.
 *
 * PERSPECTIVAL GAP:
 *   From the competent adult user's seat, this constraint is close to a rope: a coordination device that formalizes a liberty they already believed they had, at low personal cost. From the impaired-driving third party's seat, or from a community still absorbing legacy conviction harms, the same structure looks tangled — a real harm-prevention function (traffic safety) bundled with an enforcement apparatus and profit structure that continues to extract from parties who were never the intended targets of either the original prohibition or its liberalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (users, industry, tax authority) pull their directionality toward the low/subsidized end — the reform was built around and for their interests. Victim declarations (third parties harmed by impaired conduct, secondhand-exposure bystanders, low-income users facing regressive market pricing, legacy-enforcement communities) pull toward the high/target end, particularly because their exit options are trapped or constrained — a bystander cannot exit shared roads, and a community carrying conviction records cannot exit the historical fact of disparate enforcement by declining to participate in the new market.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass criminalization, racial disparity in enforcement, untaxed illicit markets) is genuinely partially solved — this is not a pure zombie mandate. But the reform's mandate to police only third-party harm has been quietly supplemented by a revenue-and-licensing apparatus whose growth is not required by the harm principle itself. Classifying this as tangled_rope rather than rope prevents mislabeling a structure that still does real coordination work (a workable, less carceral line between private choice and public harm) as if it were either purely benign or purely extractive; it also prevents treating the residual third-party and legacy-community harms as incidental noise rather than as the structure's actual victim set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_capture_vs_harm_principle_fidelity,
    'Is the licensed industry''s marketing and potency-escalation behavior a foreseeable and acceptable cost of the autonomy framing, or does it represent mission drift away from the harm principle that originally justified narrowing state authority?',
    'Compare consumption-intensity and youth-initiation trends in jurisdictions with strict marketing/potency caps versus those without, over a decade-plus horizon.',
    'If commercial behavior systematically increases harm beyond what informed autonomous choice would produce absent marketing pressure, the tangled_rope classification strengthens; if consumption patterns track pre-existing preference rather than manufactured demand, the extraction reading weakens toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_capture_vs_harm_principle_fidelity, empirical, 'Whether commercial market growth is within or beyond the harm-principle''s original scope.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (legalization_reading) of the substance_control_legitimacy kernel. The prohibition_reading and harm_reduction_reading readings locate the legitimate scope of state authority differently: prohibition_reading grounds authority in a moral duty extending to the user''s own conduct; harm_reduction_reading grounds it in a public-health duty to minimize harm without criminal sanction, potentially including affirmative treatment obligations the legalization_reading does not require. Where exactly does the disagreement live — in the definition of ''harm,'' in whether the state may act paternalistically at all, or in whether criminalization versus public-health intervention is the correct instrument?',
    'Structural analysis of each reading''s axioms (see cs_structure.axioms in each sibling file) shows the disagreement is not primarily empirical (all three readings can agree on drug-use epidemiology) but normative: it is located in whether personal risk to self counts as a harm the state may regulate, and if so, by what instrument.',
    'If the disagreement is purely instrumental (criminalization vs. public health as means to the same end), the readings could in principle converge on policy while retaining different justificatory language. If it is a genuine premise conflict (autonomy vs. paternalism), the readings are not reconcilable within one framework — this is the forecloses/coexists_with distinction captured in cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating where the three kernel readings actually diverge — instrument choice versus foundational premise.').

omega_variable(
    legacy_harm_remediation_adequacy,
    'Does the legalization reading, as actually implemented, adequately remedy the legacy harms of the prohibition era it supersedes, or does it primarily benefit new market entrants while leaving historically over-policed communities under-compensated?',
    'Audit expungement completion rates, equity-licensing program funding and uptake, and profit distribution in legal markets across jurisdictions, disaggregated by whether beneficiaries were previously criminalized under the prior regime.',
    'Low remediation adequacy would support treating communities_with_disproportionate_enforcement_history as an under-addressed victim class within an otherwise-liberalizing reform, reinforcing the tangled_rope classification even as personal-use criminalization recedes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legacy_harm_remediation_adequacy, empirical, 'Whether legalization remedies legacy prohibition-era harm or merely reallocates future profit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__legalization_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement_basis(subs_tr_t4, observed).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__legalization_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement_basis(subs_tr_t8, observed).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__legalization_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(subs_tr_t12, observed).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__legalization_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(subs_tr_t16, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(subs_tr_t20, projected).
narrative_ontology:measurement(subs_tr_t24, substance_control_legitimacy__legalization_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(subs_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__legalization_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement_basis(subs_be_t4, observed).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__legalization_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement_basis(subs_be_t8, observed).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__legalization_reading, base_extractiveness, 12, 0.43).
narrative_ontology:measurement_basis(subs_be_t12, observed).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__legalization_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement_basis(subs_be_t16, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(subs_be_t20, projected).
narrative_ontology:measurement(subs_be_t24, substance_control_legitimacy__legalization_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement_basis(subs_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__legalization_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement_basis(subs_su_t4, observed).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__legalization_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement_basis(subs_su_t8, observed).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__legalization_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement_basis(subs_su_t12, observed).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__legalization_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement_basis(subs_su_t16, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement_basis(subs_su_t20, projected).
narrative_ontology:measurement(subs_su_t24, substance_control_legitimacy__legalization_reading, suppression_requirement, 24, 0.35).
narrative_ontology:measurement_basis(subs_su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__legalization_reading, 0.1).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, impaired_driving_liability_standard).

% DUAL FORMULATION NOTE:
% This file is one of three constraints decomposed from the colloquial label 'substance control policy' / the substance_control_legitimacy kernel, per the ε-invariance principle. legalization_reading (this file), prohibition_reading, and harm_reduction_reading share the same underlying contest over the scope of state authority but instantiate structurally distinct constraints: different beneficiary/victim sets (users are beneficiaries here, victims under prohibition_reading), different ε (this reading's extraction stems primarily from commercial capture and residual third-party harm, not from criminalization overhead), and different required enforcement structures. Each reading is authored and classified independently; they are linked here as siblings in the same kernel contest, not merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
