% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Harm-Reduction Regime: Health-Administered Substance Control
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   In jurisdictions governed by this arrangement, personal possession and
 *   use of controlled substances are handled as administrative-health matters
 *   rather than crimes: detection routes a person to a dissuasion panel,
 *   civil sanctions escalate toward mandated treatment attendance, and
 *   criminal liability is reserved for production and supply. A publicly
 *   financed treatment and prevention apparatus — clinics, outreach, naloxone
 *   distribution, supervised consumption — constitutes the regime's working
 *   core, while the retained supply prohibition sustains an unregulated
 *   market whose products kill users and whose retail tier is staffed
 *   disproportionately by users themselves. This story is one member of a
 *   three-story family decomposing the colloquial label 'substance control
 *   legitimacy'; the sibling files carry the other two arrangements, and the
 *   epsilon values differ across the family because the arrangements differ
 *   structurally.
 *
 * KEY AGENTS:
 *   - - people_who_use_drugs: Primary target (powerless/trapped) — bears mandates, panel sanctions, and unregulated-product risk
 *   - - low_level_supply_sellers: Secondary target (powerless/trapped) — retains the criminal liability removed for users
 *   - - treatment_provider_network: Primary beneficiary (organized/constrained) — collects public funding and panel-referred clients
 *   - - public_health_authorities: Agenda setter (institutional/constrained) — administers panels, mandates, and budgets
 *   - - black_market_operators: Dual-positioned actor (organized/arbitrage) — profits from retained supply prohibition while absorbing enforcement losses
 *   - - general_residents: Diffuse beneficiary (moderate/mobile) — enjoys reduced disorder and disease at taxpayer cost
 *   - - civil_liberties_advocates: Excluded voice (moderate/mobile) — contests mandates from outside the panel system
 *   - - drug_policy_epidemiologists: Analytical observer (institutional/analytical) — supplies the outcome evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.45).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.4).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm-Reduction Regime: Health-Administered Substance Control").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, '67238f86-a736-47d3-b239-5a4b3ed0d718').
narrative_ontology:cs_kernel_codification('67238f86-a736-47d3-b239-5a4b3ed0d718', formalized).
narrative_ontology:cs_authority_grounding('67238f86-a736-47d3-b239-5a4b3ed0d718', expertise).
narrative_ontology:cs_interpretation_layer_present('67238f86-a736-47d3-b239-5a4b3ed0d718').
narrative_ontology:cs_reading_relation('67238f86-a736-47d3-b239-5a4b3ed0d718', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('67238f86-a736-47d3-b239-5a4b3ed0d718', substance_control_legitimacy__legalization_reading, influences).
narrative_ontology:cs_axiom('67238f86-a736-47d3-b239-5a4b3ed0d718', foundational, harm_minimization_grounds_state_authority).
narrative_ontology:cs_axiom_status(harm_minimization_grounds_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('67238f86-a736-47d3-b239-5a4b3ed0d718', harm_minimization_grounds_state_authority, instrumental).
narrative_ontology:cs_axiom('67238f86-a736-47d3-b239-5a4b3ed0d718', foundational, criminalizing_users_increases_net_harm).
narrative_ontology:cs_axiom_status(criminalizing_users_increases_net_harm, holdable).
narrative_ontology:cs_axiom_grounding('67238f86-a736-47d3-b239-5a4b3ed0d718', criminalizing_users_increases_net_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('67238f86-a736-47d3-b239-5a4b3ed0d718', public_health_stewardship_of_users).
narrative_ontology:cs_drift_state('67238f86-a736-47d3-b239-5a4b3ed0d718', contemporary_fentanyl_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('67238f86-a736-47d3-b239-5a4b3ed0d718', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_provider_network).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, general_residents).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, black_market_operators).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, low_level_supply_sellers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, black_market_operators).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, public_health_model_of_addiction).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, harm_minimization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and run the regime: convene dissuasion panels for detected users, set which substances and behaviors trigger referral, contract treatment capacity, distribute naloxone, and report outcome statistics to the legislature. Their discretion over mandate criteria determines who gets pulled into care. They cannot walk away from the population without abandoning the custodial duty their legitimacy rests on.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Clinics, outreach services, and prescribing programs financed largely from public budgets. Clients arrive through self-referral, street outreach, and panel-mandated attendance. Budget lines and staffing plans scale with the volume of referred and enrolled patients, and program reviews tie continued funding to enrollment and retention figures.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_provider_network, beneficiary,
    organized, biographical, constrained, national).

% Use substances obtained on an unregulated market. When detected, they face administrative panels, escalating civil sanctions, and mandated attendance at treatment services as the alternative to further sanction. They cannot decline the apparatus's jurisdiction over their use, and dependence, poverty, and housing instability limit relocation. They also consume product of unknown content and strength because the supply side remains illicit.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs, payer,
    powerless, biographical, trapped, national).

% Mostly sell small quantities to peers to finance their own use. Because the regime keeps production and sale criminal while sparing personal possession, they carry the imprisonment risk that the reforms removed for their customers, and arrest records follow them through housing and employment searches.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, low_level_supply_sellers, payer,
    powerless, biographical, trapped, regional).

% Import, adulterate, and retail substances through networks that exist because wholesale supply remains prohibited. Retained prohibition keeps margins high; enforcement operations periodically remove competitors and seize stock. They shift routes, formulations, and jurisdictions faster than regulators can respond.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_operators, beneficiary,
    organized, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, black_market_operators, payer).

% Experience fewer open drug scenes, lower discarded-syringe counts, and lower incarceration costs than under user criminalization, and fund the apparatus through taxation. Those who dislike proximity to drug markets can move to other neighborhoods or towns.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, general_residents, beneficiary,
    moderate, generational, mobile, regional).

% Litigate and publish against compelled treatment and against the continued criminalization of supply, arguing that competent adults should decide about their own consumption and that mandated care is care in name only. They hold no seat on the panels whose powers they contest.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, civil_liberties_advocates, excluded,
    moderate, generational, mobile, national).

% Track overdose mortality, infection incidence, treatment uptake, and market composition across jurisdictions; their cohort studies and evaluations are the evidentiary currency the regime's legitimacy claims draw on. They hold no financial stake in program budgets.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, drug_policy_epidemiologists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, treatment_provider_network).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuous, low-threshold contact between a stigmatized population and the health system: one infrastructure handles overdose response, infection prevention, treatment access, and the administrative processing of detected use, instead of each case being worked separately by police, emergency rooms, and courts.
% TRANSFER_FUNCTION: Moves public funds from general taxation into the treatment and prevention sector; moves coercive administrative attention (panel summons, mandated attendance, monitoring) onto detected users; relocates criminal liability from users to suppliers; and leaves the unregulated market's price and purity risks with consumers.
% ABSENT_VOICES: People who use drugs have no formal seat on the panels and commissions that govern them; policy is designed about them by clinicians and officials, with user unions consulting at the margins. Civil liberties advocates contest compelled treatment from outside the system. Both would object that the arrangement's consent structure is manufactured by the absence of the people it manages.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, detected users would revert to criminal processing or to no processing at all depending on jurisdiction, the treatment network would lose its referral spine and contracted funding, naloxone and supervised-consumption coverage would fragment, and the unregulated market would face either renewed user criminalization or unmanaged legality — the public-health, enforcement, and market arrangements around substance use would all reorganize.
% FOUNDING_PROBLEM: Late-1980s injection-driven HIV/AIDS transmission and rising overdose deaths, unfolding alongside mass incarceration that pushed users away from any contact with care.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: national mortality statistics and WHO/UNAIDS surveillance independently record continuing overdose and infectious-disease burdens, and peer-reviewed epidemiological evaluations of decriminalization cohorts come from academic groups with no program funding at stake. The treatment sector also attests the problem's liveness, but the external sources carry the corroboration.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).
:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.45 at interval end): the regime saves lives and connects people to care, but it compels treatment, monitors detected users, and preserves the supply-side criminalization that routes the unregulated market's harms back onto users. Suppression (0.40) is a raw structural property, unscaled by power or scope: user-side coercion has fallen from prohibition-era levels as decriminalization spread, while administrative compulsion and supply policing persist. Theater is low (0.25): most activity is functional clinical and preventive work, with a growing bureaucratic layer of panels, reporting, and metrics. Accessibility collapse is low (0.35) because rival arrangements — full legalization, renewed prohibition — remain live legislative options; resistance is moderate (0.45), mounted by user unions, civil liberties litigants, and market adaptation. The claimed type is tangled_rope on structure, not on metric tuning: the arrangement solves a real collective problem (population-scale contact with care) while extracting asymmetrically through mandates and retained supply crime, and it requires active enforcement (panels, supply policing) to hold. All three tracked series share one six-point grid (1988-2024) so temporal analysis samples every metric at every examined year; the extractiveness series rises as the treatment apparatus thickens and mandates formalize, the theater series rises slowly with bureaucratic layering, and the suppression series falls gently as user-side enforcement attrited while administrative coercion replaced it.
 *
 * PERSPECTIVAL GAP:
 *   From the panel administrator's seat the arrangement is custodial care: the same mandate the user experiences as compelled attendance reads as the mechanism that finally delivers treatment to someone who would otherwise die unseen. From the provider's seat, mandated referral is patient flow and program viability. From the user's seat, the identical structure is surveillance with a clinical interface. The engine computes these divergent per-seat classifications from the structural data; the divergence between the payer seats and the agenda-setting/beneficiary seats is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared structure maps cleanly onto directional positions: people_who_use_drugs and low_level_supply_sellers sit near the full-target end — they bear the mandates, sanctions, and market harms while collecting little. treatment_provider_network sits near the beneficiary end — public funds and referred clients flow in. general_residents are mild beneficiaries (safer streets, lower incarceration costs) who also pay the taxes, placing them near-symmetric. public_health_authorities collect authority and budget rather than direct rents. black_market_operators are genuinely dual-positioned: retained supply prohibition is their revenue condition (beneficiary-side) while enforcement operations are their cost (payer-side); the secondary_role records this, and their effective position should land mid-range rather than at either pole. No directionality overrides are needed — the beneficiary/victim declarations plus exit options produce the correct relationships for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — injection-driven HIV transmission and overdose death amid mass incarceration — is still live: overdose mortality reached record levels during the fentanyl era, so the arrangement has not outlived its function and is not mandatrophy-resolved. Localized atrophy is nonetheless visible: legacy dissuasion panels in mature jurisdictions process thin caseloads ceremonially, contributing the slow theater-ratio rise in the measurement series. Classification discipline cuts both ways: the genuine coordination function (population-scale contact with care, centralized overdose response) blocks a pure-extraction verdict, while the asymmetric burden (compelled treatment, retained supply crime, market harms routed back to users) blocks a pure-coordination verdict. The tangled_rope claim keeps both facts in view.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the substance_control_legitimacy kernel — how would the constraint''s structure change under the sibling readings?',
    'Author the prohibition_reading and legalization_reading as separate stories and compare computed types, epsilon, and victim sets across the family.',
    'Under the prohibition reading epsilon and suppression rise sharply (mass user criminalization); under the legalization reading treatment-mandate extraction collapses and the state surface shrinks to third-party-harm prevention. This story''s moderate profile is indexical to the harm-reduction reading alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: reading-indexed classification of a contested kernel.').

omega_variable(
    authority_locus_disagreement,
    'Where exactly do the readings locate legitimate authority over use — health-duty stewardship, adult autonomy, or moral duty — and which structural element (victim set, enforcement mode, mandate scope) does that locus determine?',
    'Comparative structural mapping of the three readings'' beneficiary/victim declarations and enforcement modes across the family files.',
    'The disagreement is located in the authority-grounding premise, not in empirical outcome data; resolving it reframes who counts as a victim (users-as-patients versus users-as-rights-holders versus users-as-offenders) and therefore recomputes every seat''s directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_locus_disagreement, conceptual, 'Location of the inter-reading disagreement in the kernel''s authority premise.').

omega_variable(
    mandate_evidence_vs_sector_interest,
    'Is the persistence and expansion of treatment mandates driven by outcome evidence or by the treatment sector''s institutional interest in mandated client flow?',
    'Natural experiments where mandates were relaxed or struck down: compare subsequent engagement and health outcomes against voluntarily accessed care in matched populations.',
    'If interest-driven, the mandate layer is extraction riding on the coordination function and the effective type trends toward snare at the mandate margin; if evidence-driven, mandates are coordination cost and the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_evidence_vs_sector_interest, empirical, 'Whether mandate persistence reflects evidence or sectoral capture.').

omega_variable(
    black_market_retention_cause,
    'Does the persistent black market survive because demand-side-only reform cannot eliminate it, or because retaining supply prohibition serves enforcement budgets and preserves the treatment sector''s captive referral inflow?',
    'Compare jurisdictions that paired decriminalization with regulated supply against decriminalization-only jurisdictions on market violence, product toxicity, and treatment inflow composition.',
    'If retention is chosen, part of the measured extraction is deliberately maintained and the regime''s harm-minimization claim weakens materially; if structural, the black market is a residual cost of the transition rather than a maintained feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(black_market_retention_cause, empirical, 'Cause of the persistent black market under the harm-reduction arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 1988, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substance_control_hr_tr_t1988, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 1988, 0.12).
narrative_ontology:measurement_basis(substance_control_hr_tr_t1988, observed).
narrative_ontology:measurement(substance_control_hr_tr_t1996, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 1996, 0.16).
narrative_ontology:measurement_basis(substance_control_hr_tr_t1996, observed).
narrative_ontology:measurement(substance_control_hr_tr_t2004, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2004, 0.2).
narrative_ontology:measurement_basis(substance_control_hr_tr_t2004, observed).
narrative_ontology:measurement(substance_control_hr_tr_t2012, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2012, 0.23).
narrative_ontology:measurement_basis(substance_control_hr_tr_t2012, observed).
narrative_ontology:measurement(substance_control_hr_tr_t2018, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2018, 0.24).
narrative_ontology:measurement_basis(substance_control_hr_tr_t2018, observed).
narrative_ontology:measurement(substance_control_hr_tr_t2024, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2024, 0.25).
narrative_ontology:measurement_basis(substance_control_hr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(substance_control_hr_be_t1988, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 1988, 0.3).
narrative_ontology:measurement_basis(substance_control_hr_be_t1988, observed).
narrative_ontology:measurement(substance_control_hr_be_t1996, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 1996, 0.34).
narrative_ontology:measurement_basis(substance_control_hr_be_t1996, observed).
narrative_ontology:measurement(substance_control_hr_be_t2004, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2004, 0.38).
narrative_ontology:measurement_basis(substance_control_hr_be_t2004, observed).
narrative_ontology:measurement(substance_control_hr_be_t2012, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2012, 0.41).
narrative_ontology:measurement_basis(substance_control_hr_be_t2012, observed).
narrative_ontology:measurement(substance_control_hr_be_t2018, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2018, 0.44).
narrative_ontology:measurement_basis(substance_control_hr_be_t2018, observed).
narrative_ontology:measurement(substance_control_hr_be_t2024, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2024, 0.45).
narrative_ontology:measurement_basis(substance_control_hr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(substance_control_hr_su_t1988, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 1988, 0.48).
narrative_ontology:measurement_basis(substance_control_hr_su_t1988, observed).
narrative_ontology:measurement(substance_control_hr_su_t1996, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 1996, 0.46).
narrative_ontology:measurement_basis(substance_control_hr_su_t1996, observed).
narrative_ontology:measurement(substance_control_hr_su_t2004, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2004, 0.44).
narrative_ontology:measurement_basis(substance_control_hr_su_t2004, observed).
narrative_ontology:measurement(substance_control_hr_su_t2012, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2012, 0.43).
narrative_ontology:measurement_basis(substance_control_hr_su_t2012, observed).
narrative_ontology:measurement(substance_control_hr_su_t2018, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement_basis(substance_control_hr_su_t2018, observed).
narrative_ontology:measurement(substance_control_hr_su_t2024, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2024, 0.4).
narrative_ontology:measurement_basis(substance_control_hr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'substance control legitimacy': the label conflates three structurally distinct arrangements — criminalizing prohibition, health-administered harm reduction, and autonomy-based legalization — with different victim sets, enforcement modes, and epsilon values. This file is the harm-reduction member. The prohibition arrangement is upstream (its enforcement machinery persists inside this arrangement's supply side, and its documented failures supplied this reading's founding problem); this reading is upstream of the legalization arrangement (its service infrastructure and evidence base condition legalization's operating environment). Each member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
