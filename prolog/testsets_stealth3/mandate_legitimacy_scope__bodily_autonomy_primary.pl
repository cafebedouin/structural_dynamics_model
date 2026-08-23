% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Compulsory Medical Intervention Without Informed Consent (Bodily-Autonomy-Primary Reading)
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   Standing arrangement under contest: compulsory-vaccination mandate
 *   regimes — school-entry statutes, healthcare-worker and military mandates,
 *   and the emergency-era expansion of employer and government mandates
 *   backed by termination, exclusion, and fine schedules. This file
 *   instantiates the bodily_autonomy_primary reading of the
 *   mandate_legitimacy_scope kernel: informed consent is a categorical
 *   precondition of legitimate medical intervention, and collective benefit
 *   carries no justificatory weight against its absence. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing
 *   mandate arrangement as this reading sees it — not for a
 *   consent-respecting counterfactual, which would drive epsilon to zero and
 *   erase the measurement. The claim (snare, from this reading's seat: the
 *   coordination story cannot legitimize what its axiom renders
 *   unjustifiable) and the metrics are independent authored facts; the engine
 *   computes per-seat classifications from the structural data. Sibling files
 *   author public_health_primary and proportionality_reading; the three form
 *   a constraint family linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - state_public_health_authorities: agenda-setting enforcer (institutional/arbitrage) — issues orders, designs exemptions, collects compliance
 *   - unvaccinated_coerced_persons: primary target (moderate/constrained) — bears the compelled intervention
 *   - religious_objector_communities: target with identity-fused refusal (organized/identity_locked)
 *   - medically_vulnerable_populations: indirect beneficiary (powerless/trapped) — receives protection it cannot self-provide
 *   - general_vaccinated_public: incidental beneficiary (moderate/mobile)
 *   - employers_and_institutions: implementing beneficiary (powerful/constrained)
 *   - patients_rights_advocates: excluded voice (organized/analytical)
 *   - constitutional_courts: analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.85).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.75).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Compulsory Medical Intervention Without Informed Consent (Bodily-Autonomy-Primary Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, 'b23b52c6-760d-462b-b4a4-b7b08652cd75').
narrative_ontology:cs_kernel_codification('b23b52c6-760d-462b-b4a4-b7b08652cd75', distributed).
narrative_ontology:cs_authority_grounding('b23b52c6-760d-462b-b4a4-b7b08652cd75', distributed).
narrative_ontology:cs_reading_relation('b23b52c6-760d-462b-b4a4-b7b08652cd75', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('b23b52c6-760d-462b-b4a4-b7b08652cd75', mandate_legitimacy_scope__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('b23b52c6-760d-462b-b4a4-b7b08652cd75', foundational, bodily_integrity_inviolable_absent_consent).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolable_absent_consent, holdable).
narrative_ontology:cs_axiom_grounding('b23b52c6-760d-462b-b4a4-b7b08652cd75', bodily_integrity_inviolable_absent_consent, deontological).
narrative_ontology:cs_axiom('b23b52c6-760d-462b-b4a4-b7b08652cd75', secondary, collective_benefit_never_justifies_nonconsensual_intervention).
narrative_ontology:cs_axiom_status(collective_benefit_never_justifies_nonconsensual_intervention, holdable).
narrative_ontology:cs_axiom_grounding('b23b52c6-760d-462b-b4a4-b7b08652cd75', collective_benefit_never_justifies_nonconsensual_intervention, deontological).
narrative_ontology:cs_reference_frame('b23b52c6-760d-462b-b4a4-b7b08652cd75', inviolable_bodily_integrity_baseline).
narrative_ontology:cs_drift_state('b23b52c6-760d-462b-b4a4-b7b08652cd75', contemporary_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b23b52c6-760d-462b-b4a4-b7b08652cd75', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, medically_vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, general_vaccinated_public).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, employers_and_institutions).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_persons).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, religious_objector_communities).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, police_power_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues compulsory-vaccination orders for school entry, healthcare employment, and — during declared emergencies — the general population; designs exemption categories, penalty schedules, and verification systems; reports compliance as coverage statistics. Wrote the arrangement and can amend or rescind it; its costs are administrative and political, not bodily.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold jobs, school places, professional licenses, or service access conditioned on accepting a medical procedure they decline. Compliance buys back access at the price of bodily self-determination; refusal keeps it at the price of livelihood, education, or licensure. Case-by-case legal challenge is slow, costly, and disfavored by controlling precedent.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_persons, payer,
    moderate, biographical, constrained, national).

% Decline on grounds fused with religious identity, where acceptance would breach obligations that constitute membership. Organized enough to litigate and lobby collectively; individual compliance would cost the community ties that give their refusal its meaning, so exit into compliance is not a live option for the devout core.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, religious_objector_communities, payer,
    organized, generational, identity_locked, national).

% Cannot safely take the vaccine (age, immunosuppression, contraindications) and depend on neighbors' coverage for indirect protection. They decide nothing in this arrangement; their safety rises or falls with strangers' compliance, and they have no substitute purchase for the good.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medically_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Accepted the intervention voluntarily and enjoy lower transmission risk as coverage rises. Largely indifferent to the enforcement controversy; their benefit accrues under either a mandate or a well-functioning voluntary campaign.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, general_vaccinated_public, beneficiary,
    moderate, biographical, mobile, national).

% Implement government mandates as workplace rules, admission conditions, and credentialing requirements; gain workforce continuity, inspection readiness, and reduced liability exposure. Bear tracking and exemption-processing costs, and pass penalty consequences through to employees and students.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, employers_and_institutions, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__bodily_autonomy_primary, employers_and_institutions, agenda_setter).

% Hold that consent is the load-bearing wall of medical ethics and that emergency framings eroded it. Sit mostly outside mandate-design rooms; heard in commentary, amicus briefs, and post-hoc hearings after rules are fixed.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, patients_rights_advocates, excluded,
    organized, biographical, analytical, national).

% Adjudicate challenges under a precedent line that upholds police-power vaccination while gesturing at bodily integrity. Review but rarely invalidate; their doctrines determine which arguments are even arguable, and they feel the arrangement's effects only as caseload.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level immunization coverage toward thresholds that interrupt transmission, solving the free-rider problem in communicable-disease control: each individual's privately best move (skip the injection, rely on others' coverage) diverges from the collective optimum.
% TRANSFER_FUNCTION: Moves decision-authority over a defined medical intervention from individuals (the unvaccinated-coerced) to state and administrative bodies, and moves disease risk off the medically vulnerable onto the bodies of those who refuse.
% ABSENT_VOICES: The coerced themselves rarely sit on mandate-design bodies; patients-rights and disability-rights advocates were largely outside emergency-era deliberation; conscientious objectors testify only after rules are drafted, in comment periods and courtroom amicus filings rather than at the drafting table.
% DISAPPEARANCE_RATIONALE: If the mandate apparatus vanished overnight, coverage would fall below thresholds in pockets of refusal, employers and universities would rewrite admission and employment conditions, exemption bureaucracies would dissolve, active litigation would collapse, and the medically vulnerable would lose indirect protection they cannot purchase elsewhere — the disease-control landscape would reorganize around whatever voluntary and incentive mechanisms replaced compulsion.
% FOUNDING_PROBLEM: Voluntary coverage proved insufficient to reach herd thresholds against smallpox and later polio and measles; states built compulsory-vaccination powers (codified in the Jacobson v. Massachusetts line, 1905) to solve free-riding in disease control.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological surveillance (WHO and national coverage-and-outbreak records) corroborates from outside the benefiting parties that coverage gaps produce outbreaks; bioethics literature corroborates that the free-rider problem is real while disputing the remedy's legitimacy. No source outside the enforcing parties attests that collective benefit dissolves the consent requirement — that step is attested only by the parties that enforce it.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.85: wherever a mandate binds, this reading holds that a fundamental interest — bodily self-determination — is taken without consent, and the emergency-era widening (employer termination, exclusion from services, fines) expanded the taken surface from school-entry niches into adult economic life. Suppression 0.75: refusal triggers structural penalties (job loss, enrollment denial, restricted access), and the enforcement machinery visibly thickened over the interval — hence the suppression_requirement series, which traces enforcement-capacity build-up rather than a mere shift in extraction. Theater 0.35: exemption hearings, committee reviews, and tiered penalty announcements increasingly ritualize a predetermined outcome as mandates tighten. Accessibility_collapse 0.6: once a mandate binds, the option set narrows to comply, refuse-and-absorb-penalty, or exit the institution — alternatives thin but do not vanish (jurisdictional variation, remote work, private schooling). Resistance 0.65: litigation waves, protest movements, and electoral backlash; individually weak refusers gain coalition leverage through organized litigation networks and religious communities. All three metric series run on one shared grid (T=0..24, approximately 2000-2024) so no row borrows an end-state value.
 *
 * PERSPECTIVAL GAP:
 *   Maximal seat divergence is this reading's signature prediction. The state seat experiences administration of a functioning coverage instrument; medically_vulnerable_populations experience a lifeline they cannot self-provide; employers_and_institutions experience a compliance overhead that buys continuity and liability shelter. The two payer seats experience the identical statute as bodily appropriation — unvaccinated_coerced_persons with constrained exit (comply or lose livelihood), religious_objector_communities with identity-locked exit (compliance would breach the community ties that constitute them). Constitutional_courts observe from a precedent line that presupposes the sibling reading's premise, which is why litigation repeatedly fails despite high resistance. Coalition check: the payer class is individually powerless-to-moderate; its leverage is organizational (litigation funds, congregations, professional associations), which is why resistance sits at 0.65 rather than near zero.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low d: medically_vulnerable_populations and general_vaccinated_public receive protection without bearing the intervention's coercive edge; employers_and_institutions collect continuity and liability shelter. Payer declarations map to high d: both victim groups bear the compelled intervention, and their exit atoms push them toward the full-target end — constrained for the general coerced, identity_locked for religious objectors. The state seat collects compliance (coverage statistics, enforcement authority) and sits near the beneficiary end despite administering rather than merely collecting. Suppression is authored as a raw structural property and is not scaled; extractiveness is scaled by the engine from these directionalities and the national scope, which amplifies effective extraction by making uniform verification and exit harder.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (free-rider insufficiency of voluntary coverage against smallpox, polio, measles) remains live — surveillance data corroborates it from outside the benefiting parties — so this is not a mandatrophy case, and the mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag. The classification work this reading performs is laundering-prevention: because its foundational axiom strips collective benefit of justificatory force, the arrangement cannot compute as a hybrid from this seat — the coordination half is empirically real but justificationally inert, which is exactly the structure the snare claim encodes. The engine's per-seat computation will nonetheless show beneficiary seats experiencing genuine coordination value; preserving that divergence, rather than reconciling it, is the corpus's measurement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the mandate_legitimacy_scope kernel: does the categorical consent reading instantiated here, or a sibling reading (public_health_primary, proportionality_reading), correctly specify when medical mandates are legitimate?',
    'Sustained constitutional adjudication plus explicit legislative justification: whichever reading''s premises survive as the operative legal test resolves the contest; comparative-jurisdiction analysis of exemption regimes supplies convergent evidence.',
    'If public_health_primary prevails, the victim set empties (compelled uptake stops counting as a violation) and effective extraction collapses toward the coordination-cost floor; if proportionality_reading prevails, extraction becomes factor-contingent (severity, safety, alternatives) rather than categorical; this file''s high-epsilon profile holds only under the categorical reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the mandate-legitimacy kernel governs; determines the victim set and epsilon.').

omega_variable(
    victim_set_boundary_ambiguity,
    'Does the victim set comprise all persons subjected to non-consensual intervention under mandate, or only those without a genuinely accessible exemption path?',
    'Audit of exemption grant rates, processing times, and burdens of proof across jurisdictions; interview data on whether nominal exemptions function in practice.',
    'If nominal exemptions are practically inaccessible, the victim set widens and measured extraction rises; if exemptions function, part of the coerced population is better described as bearing elevated transaction costs than as violated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_ambiguity, empirical, 'Where the boundary of the coerced victim set sits.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (termination, exclusion, fines) or internalized (stigma, self-blame, social ostracism of refusers)?',
    'Post-exit suppression trajectory: track refusers who relocate to non-mandating jurisdictions or employers; if social sanction and self-reproach persist after structural penalties vanish, part of the suppression is internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure and persists beyond any rescission of mandates, changing what removal of the arrangement would actually release.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized share of suppression on refusing agents.').

omega_variable(
    emergency_consent_validity,
    'Does consent obtained under emergency-use authorization, employer pressure, and access-conditioning retain the informational and voluntariness qualities the consent requirement presupposes, or does the violation extend to formally voluntary uptake?',
    'Bioethical analysis of consent validity under conditional-access pressure, cross-referenced with survey data on recipients'' understanding of authorization status and available alternatives.',
    'If pressured consent is invalid consent, the arrangement''s reach extends far beyond formal mandates and epsilon rises further; if valid, the measured arrangement stays bounded at the formally coerced margin.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emergency_consent_validity, conceptual, 'Whether conditioned consent counts as consent for the integrity violation.').

omega_variable(
    relaxation_trajectory_meaning,
    'Does post-peak relaxation of mandate enforcement reflect repudiation of the necessity carve-out or temporary retrenchment pending the next declared emergency?',
    'Track enforcement reactivation in the next public-health emergency; compare statutory sunset and revival clauses across jurisdictions.',
    'Retrenchment implies the arrangement persists latently and the temporal series understates steady-state extraction; genuine repudiation implies decay toward a narrower residual.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(relaxation_trajectory_meaning, empirical, 'Whether current relaxation is decay or dormancy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(mand_tr_t0, observed).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 4, 0.17).
narrative_ontology:measurement_basis(mand_tr_t4, observed).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(mand_tr_t8, observed).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(mand_tr_t12, observed).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 16, 0.3).
narrative_ontology:measurement_basis(mand_tr_t16, observed).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(mand_tr_t20, observed).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(mand_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(mand_be_t0, observed).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 4, 0.6).
narrative_ontology:measurement_basis(mand_be_t4, observed).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 8, 0.66).
narrative_ontology:measurement_basis(mand_be_t8, observed).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 12, 0.74).
narrative_ontology:measurement_basis(mand_be_t12, observed).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 16, 0.8).
narrative_ontology:measurement_basis(mand_be_t16, observed).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 20, 0.83).
narrative_ontology:measurement_basis(mand_be_t20, observed).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 24, 0.85).
narrative_ontology:measurement_basis(mand_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(mand_su_t0, observed).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 4, 0.48).
narrative_ontology:measurement_basis(mand_su_t4, observed).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(mand_su_t8, observed).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(mand_su_t12, observed).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(mand_su_t16, observed).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(mand_su_t20, observed).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 24, 0.75).
narrative_ontology:measurement_basis(mand_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, resource_allocation).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the mandate_legitimacy_scope kernel into three readings per the epsilon-invariance principle: the colloquial label 'vaccine mandate legitimacy' conflates three structurally distinct claims with different victim sets and epsilon profiles. This file authors the bodily_autonomy_primary reading (categorical consent; coerced enter the victim set; the state becomes a rights violator wherever mandates bind; high epsilon from mandate presence). Sibling files author public_health_primary (necessity legitimizes; victim set empty; epsilon near the coordination floor) and proportionality_reading (factor-contingent legitimacy; epsilon varies with severity, safety, and alternatives). Historical flow: public_health_primary is the upstream reading (Jacobson lineage) cited as settled ground by the other two; this reading exerts repudiation pressure on that lineage rather than receiving from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
