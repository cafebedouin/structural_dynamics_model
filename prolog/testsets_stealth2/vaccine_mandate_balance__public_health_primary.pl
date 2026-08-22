% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Public-Health-Primary Vaccine Mandate Authorization
 *   domain: public health ethics / constitutional law / political philosophy
 *
 * SUMMARY:
 *   This story instantiates the public_health_primary reading of the
 *   vaccine_mandate_balance kernel: collective protection supersedes
 *   individual consent when voluntary compliance fails to reach herd coverage
 *   and vulnerable populations face lethal exposure. The standing arrangement
 *   under assessment is the mandate machinery as it actually operates across
 *   the interval: school-entry and employment requirements, exemption
 *   channels, penalty regimes, and the recurring lapse-and-renewal cycle in
 *   which coverage erodes until an outbreak forces re-tightening. The
 *   reading's signature structural commitments are encoded in the
 *   declarations: immunocompromised herd-dependents hold BOTH beneficiary and
 *   victim status (protected when coverage holds, lethally exposed when it
 *   lapses), while conscientious refusers bear the enforcement machinery's
 *   heaviest penalties yet are deliberately withheld from the victim set,
 *   because this reading subordinates their consent claims to the necessity
 *   of third-party protection. KEY AGENTS (by structural relationship): -
 *   public_health_agencies: Agenda setter (institutional/constrained) —
 *   administers enforcement, collects fines and authority -
 *   immunocompromised_herd_dependents: Conditional beneficiary and lapse-time
 *   target (powerless/trapped) — protected when coverage holds, exposed when
 *   it slips - unvaccinated_conscientious_refusers: Enforcement target
 *   (moderate/identity_locked) — bears the penalties this reading declines to
 *   count as wrongful - infants_before_vaccination_age: Lapse-time target
 *   (powerless/trapped) - frontline_healthcare_workers: Dual-positioned
 *   (organized/constrained) — protected at work, employment-conditioned -
 *   hospital_administrators: Secondary administrator and beneficiary
 *   (powerful/mobile) - vaccinated_compliant_majority: Coordination
 *   beneficiary (organized/mobile) - courts_legislatures: Analytical observer
 *   (institutional/analytical) - future_pathogen_cohorts: Excluded voice
 *   (powerless/trapped)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.72).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.75).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public-Health-Primary Vaccine Mandate Authorization").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public health ethics / constitutional law / political philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, '6021d1f4-a51e-45cc-afa0-3a6d9b752768').
narrative_ontology:cs_kernel_codification('6021d1f4-a51e-45cc-afa0-3a6d9b752768', formalized).
narrative_ontology:cs_authority_grounding('6021d1f4-a51e-45cc-afa0-3a6d9b752768', lineage).
narrative_ontology:cs_interpretation_layer_present('6021d1f4-a51e-45cc-afa0-3a6d9b752768').
narrative_ontology:cs_reading_relation('6021d1f4-a51e-45cc-afa0-3a6d9b752768', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('6021d1f4-a51e-45cc-afa0-3a6d9b752768', vaccine_mandate_balance__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('6021d1f4-a51e-45cc-afa0-3a6d9b752768', foundational, necessity_supersedes_consent_against_lethal_third_party_risk).
narrative_ontology:cs_axiom_status(necessity_supersedes_consent_against_lethal_third_party_risk, holdable).
narrative_ontology:cs_axiom_grounding('6021d1f4-a51e-45cc-afa0-3a6d9b752768', necessity_supersedes_consent_against_lethal_third_party_risk, deontological).
narrative_ontology:cs_axiom('6021d1f4-a51e-45cc-afa0-3a6d9b752768', secondary, voluntary_compliance_failure_activates_compulsion_authority).
narrative_ontology:cs_axiom_status(voluntary_compliance_failure_activates_compulsion_authority, holdable).
narrative_ontology:cs_axiom_grounding('6021d1f4-a51e-45cc-afa0-3a6d9b752768', voluntary_compliance_failure_activates_compulsion_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('6021d1f4-a51e-45cc-afa0-3a6d9b752768', jacobson_police_power_lineage).
narrative_ontology:cs_drift_state('6021d1f4-a51e-45cc-afa0-3a6d9b752768', post_covid_mandate_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6021d1f4-a51e-45cc-afa0-3a6d9b752768', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_herd_dependents).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, frontline_healthcare_workers).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, hospital_administrators).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vaccinated_compliant_majority).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, immunocompromised_herd_dependents).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, infants_before_vaccination_age).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_conscientious_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, frontline_healthcare_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run immunization programs, set coverage targets, and operate the legal machinery that conditions school entry, employment, and institutional access on vaccination when voluntary uptake stalls below the herd threshold. Collect fine revenue and enforcement grants, and publish the coverage statistics that justify their budgets. Their exit is bounded by statute and political oversight; they cannot abandon the programs without legislative action.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Cannot be vaccinated safely or do not respond to vaccination, and depend on the immunity of the people around them to avoid lethal infection. When coverage holds, they move through schools, workplaces, and hospitals with ordinary freedom; when coverage slips through exemption growth, mandate repeal, or outbreak pockets, they retreat into isolation or absorb mortal risk. They cannot exit their condition; shielding is their only lever.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_herd_dependents, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, immunocompromised_herd_dependents, payer).

% Are too young for the vaccine schedule and catch whatever circulates. Outbreaks in under-covered communities reach them first; their parents choose daycare and travel under information they cannot verify. They bear disease risk directly and hold no procedural voice; representation runs entirely through parents and pediatricians.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, infants_before_vaccination_age, payer,
    powerless, immediate, trapped, national).

% Decline vaccination on religious, philosophical, or medical-skeptic grounds and carry the operative penalties: employment termination under facility rules, school and campus exclusion, fines where imposed, and social ostracism. Many treat refusal as an extension of conscience or identity, which makes the standard exits such as compliance, documented exemption, or home-based work feel like self-betrayal rather than options. Some pursue litigation or relocate across state lines; most absorb the penalties where they stand.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_conscientious_refusers, payer,
    moderate, biographical, identity_locked, national).

% Work where exposure risk concentrates. Facility rules tie employment to vaccination, which protects them on the ward but ended careers for colleagues who refused; union agreements and testing alternatives soften the edge in some systems. Exit means leaving patient care, a cost few can carry mid-career.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, frontline_healthcare_workers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, frontline_healthcare_workers, payer).

% Operate staff immunization policies to keep wards staffed and liability contained, and absorb the operational shock when refusals force terminations during staffing shortages. They can tighten policy during surges and relax it when pressure fades, moving faster than legislatures, and they answer to boards and insurers more than to either side of the mandate dispute.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, hospital_administrators, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, hospital_administrators, agenda_setter).

% Took the vaccine and expects the bargain honored: their compliance purchases collective protection, and they resent free-riding that erodes it. Their support supplies the political license the mandate machinery runs on; their patience thins when outbreaks trace to exemption clusters. Exit is trivial because they are already inside the arrangement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vaccinated_compliant_majority, beneficiary,
    organized, biographical, mobile, national).

% Adjudicate where compulsory medical power ends: courts test mandates against constitutional limits and emergency-powers doctrine, legislatures enact or preempt them state by state. Neither seat administers daily enforcement; both shape what the machinery may do next. Their review is periodic and retrospective rather than operational.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, courts_legislatures, observer,
    institutional, generational, analytical, national).

% Will face the next novel pathogen under rules written now: exemption carve-outs, preemption statutes, and court doctrines settled in this cycle bind responses they cannot yet vote on. No current table seats them; advocacy proxies speak in their name at best.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, future_pathogen_cohorts, excluded,
    powerless, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__public_health_primary, public_health_agencies).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in infectious-disease control: each individual's privately rational move is to let others accept vaccination risk while skipping it personally, which stalls voluntary coverage below the herd threshold needed to shield those who cannot be vaccinated. Compulsion closes the gap when persuasion stalls.
% TRANSFER_FUNCTION: Moves vaccination compliance, and its attendant risks and liberty costs, from the unvaccinated to the collective shield; moves fine revenue and enforcement authority to public health agencies; moves residual mortality risk off the immunocompromised and infants and onto the enforcement friction borne by refusers.
% ABSENT_VOICES: Future pathogen cohorts have no seat while exemption carve-outs, preemption statutes, and doctrinal precedents are set now. In philosophical-exemption jurisdictions, immunocompromised residents are spoken for by advocates rather than seated at the policy tables that decide exemption breadth.
% DISAPPEARANCE_RATIONALE: If the supersession principle vanished overnight, employer and school-entry requirements would lose their legal foundation, coverage would drift downward in low-trust regions, and the immunocompromised and pre-vaccination infants would reorganize life around shielding and isolation; outbreak response would shift to reactive quarantine rings.
% FOUNDING_PROBLEM: Lethal urban epidemics that voluntary compliance could not stop: the smallpox outbreaks behind Jacobson v. Massachusetts, in which city-wide compulsion was the only demonstrated route to interrupting transmission and protecting those who could not be inoculated.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: historical smallpox mortality records, the epidemiological findings recited in Jacobson-era litigation, and contemporary measles and pertussis outbreak investigations by surveillance bodies independent of mandate administrators, which repeatedly document infant deaths and immunocompromised fatalities tracing to coverage gaps.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because the standing arrangement imposes heavy operative burdens through its enforcement layer (termination, exclusion, fines) AND fails protectively during lapses, leaving the helpless lethally exposed; both facts are properties of the arrangement as it stands, and this reading counts the lapse-exposure as the arrangement's real wrong while treating the refuser burden as a justified price. Suppression (0.75) is a raw structural property, unscaled by power or scope: the machinery actively excludes, terminates, and fines, and its persistence depends on that enforcement rather than on participant preference. Theater_ratio (0.35) reflects paper mandates in easy-exemption states and symbolic renewals after emergencies pass, against a core of genuinely functional enforcement. Accessibility_collapse (0.60): once the regime is understood, exits narrow sharply but do not vanish, since medical exemptions, relocation, and home-based work survive. Resistance (0.70): sustained litigation, protest movements, and state preemption statutes. The claimed_type (tangled_rope) is stated from structure, independently of the metrics: a genuine coordination function (herd immunity against free-riding) fused with asymmetric cost-bearing through the same enforcement apparatus, requiring active enforcement to hold. The temporal series run on one shared grid (all three metrics at all seven points) so no metric row is silently backfilled; suppression_requirement is tracked because this story specifically traces enforcement-capacity change, the COVID-era ratchet to 0.85 followed by partial decay.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical structure. From the agency seat the arrangement is a functioning public good it built and must defend; from the refuser seat the same machinery operates as persecution of conscience, softened by nothing; from the immunocompromised seat the arrangement is a lifeline whose every exemption loophole reads as abandonment; from the hospital administrator seat it is a staffing and liability instrument to be tuned. The engine derives these per-seat classifications from the structural data; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (healthcare workers, administrators, compliant majority, and the immunocompromised while coverage holds) derive low directionality, near the subsidized end. The immunocompromised carry a dual declaration: beneficiary when the arrangement works, victim when it lapses, which nets them toward the beneficiary side of symmetric because they comply willingly and are coerced by nothing. Infants before vaccination age are pure lapse-time targets. Conscientious refusers receive NO victim declaration, by this reading's explicit commitment, yet their identity_locked exit and cost-bearing place them structurally near the full-target end regardless: the reading disputes the moral classification of their burden, not the fact that the machinery bears down on them. Agencies accrue the machinery's receipts (fine revenue, enforcement grants, expanded authority) without holding a victim declaration; the gain_flow field records that accrual.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview shows a live founding problem corroborated from outside the benefiting parties (outbreak surveillance, historical mortality records), paired with a world_rearranges disappearance verdict: no dead-mandate mismatch flag fires. The conditional trigger built into the reading itself, compulsion activating only when voluntary compliance fails, ties the arrangement to a recurrent empirical function rather than to a vanished original purpose, which is what distinguishes this tangled_rope from a piton drifting on inertia. The theater_ratio series is the watch-line: if paper mandates and symbolic renewals keep rising in low-incidence eras while enforcement decays, the arrangement slides toward theatrical maintenance, and the classification should be revisited. Conversely, mislabeling the arrangement as pure extraction would erase the genuine coordination function that justifies its coercive overhead; mislabeling it as pure rope would erase the asymmetric burden the refuser seat demonstrably carries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_kernel_reading_indexicality,
    'This constraint is the public_health_primary reading of kernel vaccine_mandate_balance; what structurally changes under the sibling readings bodily_autonomy_primary and proportionality_reading?',
    'Generate the sibling stories and compare computed classifications: bodily_autonomy_primary assigns the coerced refusers to the victim set and empties the lapse-exposure victims; proportionality_reading retains both sets but gates enforcement on severity and exemption-robustness thresholds.',
    'Under bodily_autonomy_primary the same enforcement machinery computes as pure extraction; under proportionality_reading it computes as conditionally permissible coordination. The disagreement is located in the priority ordering between collective outcome and individual consent at the moment voluntary compliance fails.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balance_kernel_reading_indexicality, conceptual, 'Reading-indexed classification of the shared mandate machinery across the kernel''s sibling readings.').

omega_variable(
    lapse_conditional_victimhood,
    'The immunocompromised hold both beneficiary and victim declarations, with victimhood activating when coverage lapses; is their harm a failure mode of the arrangement or intrinsic to it?',
    'Jurisdictional comparison of outbreak mortality among immunocompromised populations in mandate-restricting versus mandate-maintaining states, controlling for baseline coverage and pathogen circulation.',
    'If harm tracks lapses, the conditional victim declaration stands as authored; if outbreaks recur under intact mandates, the arrangement underprotects even while operating, and effective extraction on the dependent populations rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapse_conditional_victimhood, empirical, 'Whether lapse-time exposure of the herd-dependent is contingent failure or structural shortfall.').

omega_variable(
    refuser_burden_magnitude,
    'How heavy is the burden actually carried by conscientious refusers in income loss, exclusion duration, and penalty totals, and how concentrated by income and region?',
    'Administrative data on mandate-related terminations, unemployment durations, and penalty assessments, disaggregated by income decile and state policy regime.',
    'A concentrated, severe burden raises the enforcement seat''s effective extraction even under this reading''s refusal of victim status, lending force to the proportionality sibling''s threshold demands; a diffuse, shallow burden supports the justified-price framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refuser_burden_magnitude, empirical, 'Magnitude and distribution of the coercion costs borne by refusers.').

omega_variable(
    exemption_integrity_leakage,
    'Do reported coverage figures reflect genuine medical contraindication, or does philosophical-exemption leakage masquerade as medical exemption where philosophical routes have closed?',
    'Audit of exemption certificates against clinical records in states that eliminated philosophical exemptions.',
    'High leakage means the arrangement''s protective function is weaker than coverage statistics suggest, raising the effective exposure of the herd-dependent populations and pushing theater_ratio upward as enforcement chases paper compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_integrity_leakage, empirical, 'Integrity of the exemption channel beneath reported coverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 1963, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t1963, vaccine_mandate_balance__public_health_primary, theater_ratio, 1963, 0.15).
narrative_ontology:measurement(vacc_tr_t1975, vaccine_mandate_balance__public_health_primary, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(vacc_tr_t1990, vaccine_mandate_balance__public_health_primary, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(vacc_tr_t2005, vaccine_mandate_balance__public_health_primary, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(vacc_tr_t2020, vaccine_mandate_balance__public_health_primary, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(vacc_tr_t2023, vaccine_mandate_balance__public_health_primary, theater_ratio, 2023, 0.33).
narrative_ontology:measurement(vacc_tr_t2026, vaccine_mandate_balance__public_health_primary, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(vacc_be_t1963, vaccine_mandate_balance__public_health_primary, base_extractiveness, 1963, 0.45).
narrative_ontology:measurement(vacc_be_t1975, vaccine_mandate_balance__public_health_primary, base_extractiveness, 1975, 0.5).
narrative_ontology:measurement(vacc_be_t1990, vaccine_mandate_balance__public_health_primary, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(vacc_be_t2005, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(vacc_be_t2023, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2023, 0.74).
narrative_ontology:measurement(vacc_be_t2026, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2026, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t1963, vaccine_mandate_balance__public_health_primary, suppression_requirement, 1963, 0.4).
narrative_ontology:measurement(vacc_su_t1975, vaccine_mandate_balance__public_health_primary, suppression_requirement, 1975, 0.45).
narrative_ontology:measurement(vacc_su_t1990, vaccine_mandate_balance__public_health_primary, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(vacc_su_t2005, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(vacc_su_t2023, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2023, 0.78).
narrative_ontology:measurement(vacc_su_t2026, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2026, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'vaccine mandate debate' decomposes into three structurally distinct constraints sharing one kernel. The readings differ on victim-set membership and on the location of justification: this reading places immunocompromised-exposed in the victim set during coverage lapses and withholds victim status from coerced refusers; bodily_autonomy_primary inverts that assignment entirely; proportionality_reading splits the difference behind severity thresholds and exemption guarantees. Epsilon differs across the three because each reading assesses the same enforcement machinery under a different priority ordering; the shared label conflated them, and the decomposition separates them. Linked via network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
