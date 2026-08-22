% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Public-Health-Primary Reading of the Vaccine Mandate Balance
 *   domain: public health ethics/constitutional law/political philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading - public_health_primary - of the
 *   contested kernel vaccine_mandate_balance: the commitment that collective
 *   protection supersedes individual consent when voluntary compliance fails
 *   to reach herd coverage and vulnerable populations face lethal exposure.
 *   The standing arrangement under contest, and therefore the referent of
 *   epsilon, is the operative mandate regime as it actually runs: statutory
 *   school-entry laws, employer and licensure directives, exemption
 *   machinery, fine schedules, and exclusion orders - assessed by this
 *   reading's own lights as a justified necessity that nonetheless carries
 *   heavy coercive cost. The reading does NOT average over its siblings and
 *   does not hedge epsilon across them; bodily_autonomy_primary and
 *   proportionality_reading are separate constraints in separate files,
 *   linked through network.affects_constraints. Under this reading's
 *   structural delta, the unvaccinated-coerced are not members of the victim
 *   set (consent is subordinated to necessity, so their sanction costs are
 *   legitimate burden rather than wrong), while immunocompromised people
 *   living inside coverage gaps ARE victims - they bear lethal residual
 *   exposure that the arrangement's exemptions and enforcement shortfalls
 *   leave open. Epsilon is nonetheless authored high (0.70) because the
 *   enforcement machinery genuinely extracts: fines, termination, school and
 *   venue exclusion, and compelled purchase all fall on governed parties as
 *   real costs, whatever their justification. KEY AGENTS (by structural
 *   relationship): - public_health_authorities: Agenda setter
 *   (institutional/constrained) - sets thresholds, defines exemptions,
 *   operates enforcement - unvaccinated_conscientious_objectors: Payer seat
 *   (organized/identity_locked) - bears sanctions; not in the victim set
 *   under this reading - defaulting_non_vaccinators: Free-rider seat
 *   (moderate/mobile) - receives coverage while dodging contribution -
 *   immunocompromised_medically_unvaccinable: Primary beneficiary
 *   (powerless/trapped) - cannot vaccinate, depends on neighbors' coverage -
 *   infants_before_vaccination_age: Beneficiary (powerless/trapped) -
 *   pre-dosing window, no voice - elderly_chronic_condition_patients:
 *   Beneficiary (moderate/constrained) - weaker vaccine response, high
 *   breakthrough mortality - immunocompromised_in_coverage_gap_pockets:
 *   Victim seat (powerless/trapped) - sits downwind of exemption clusters and
 *   enforcement gaps - frontline_healthcare_workers: Dual payer/beneficiary
 *   (organized/constrained) - mandated as condition of practice, maximally
 *   exposed occupationally - employers_and_school_administrators:
 *   Implementing agenda setter (institutional/constrained) - runs card
 *   checks, exclusions, exemption processing - courts_and_legislatures:
 *   Observer (institutional/analytical) - redraws exemption breadth and
 *   enforcement powers - access_barred_would_comply_patients: Excluded voice
 *   (powerless/trapped) - willing but barred; sanctioned indiscriminately -
 *   vaccine_manufacturers: Beneficiary (institutional/arbitrage) - sells into
 *   legally guaranteed demand
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.7).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.72).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public-Health-Primary Reading of the Vaccine Mandate Balance").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public health ethics/constitutional law/political philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, 'cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9').
narrative_ontology:cs_kernel_codification('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9', formalized).
narrative_ontology:cs_authority_grounding('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9', expertise).
narrative_ontology:cs_interpretation_layer_present('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9').
narrative_ontology:cs_reading_relation('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9', vaccine_mandate_balance__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9', foundational, collective_protection_supersedes_individual_consent_at_herd_failure).
narrative_ontology:cs_axiom_status(collective_protection_supersedes_individual_consent_at_herd_failure, holdable).
narrative_ontology:cs_axiom_grounding('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9', collective_protection_supersedes_individual_consent_at_herd_failure, deontological).
narrative_ontology:cs_axiom('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9', secondary, nonvaccination_imposes_lethal_externalities_on_third_parties).
narrative_ontology:cs_axiom_status(nonvaccination_imposes_lethal_externalities_on_third_parties, holdable).
narrative_ontology:cs_axiom_grounding('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9', nonvaccination_imposes_lethal_externalities_on_third_parties, empirically_contingent).
narrative_ontology:cs_reference_frame('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9', collective_immunity_supremacy_frame).
narrative_ontology:cs_drift_state('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9', post_covid_mandate_polarization, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('cd6f57a3-7161-4de9-a7b0-73e4ac9aeee9', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_medically_unvaccinable).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, infants_before_vaccination_age).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, elderly_chronic_condition_patients).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, immunocompromised_in_coverage_gap_pockets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, defaulting_non_vaccinators).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_in_coverage_gap_pockets).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, frontline_healthcare_workers).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vaccine_manufacturers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_conscientious_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, defaulting_non_vaccinators).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, frontline_healthcare_workers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, employers_and_school_administrators).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, herd_immunity_threshold_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, state_police_power_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the coverage percentage at which they declare community protection achieved, define which exemptions qualify, and operate the enforcement chain: school-entry rules, workplace directives, fine schedules, and exclusion orders. They collect fine revenue and epidemiological credit when outbreaks are averted, and absorb blame when outbreaks break through. Exit looks like reassignment or resignation; the statutory duty follows the office.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Decline vaccination on religious, philosophical, or personal-integrity grounds and treat the injection decision as constitutive of who they are. They pay fines, complete exemption paperwork where available, change jobs, homeschool children, or accept exclusion from venues and workplaces. Leaving the stance would require abandoning a worldview, not filling out a form; they organize litigations, protests, and repeal campaigns.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_conscientious_objectors, payer,
    organized, biographical, identity_locked, national).

% Hold no principled objection; they miss doses because of scheduling, forgetfulness, or low perceived risk, and are content to be protected by everyone else's coverage. When enforcement reaches them they book the appointment quickly. Their costs are occasional hassle and the rare fine; their protection arrives whether or not they contribute.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, defaulting_non_vaccinators, beneficiary,
    moderate, immediate, mobile, local).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, defaulting_non_vaccinators, payer).

% Transplant recipients, chemotherapy patients, and others whose physicians advise against vaccination. They cannot acquire protection directly and depend entirely on the percentage of immune people around them. The only exit is isolation, purchased at the price of work, school, and social life; they rely on mandates they have no hand in enforcing.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_medically_unvaccinable, beneficiary,
    powerless, immediate, trapped, local).

% Sit below the age of first dosing for measles and other antigens and can be infected before their own series begins. They have no voice in any proceeding and no way to protect themselves; their safety is entirely a property of the coverage percentages in the rooms and neighborhoods they pass through.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, infants_before_vaccination_age, beneficiary,
    powerless, immediate, trapped, local).

% Mount weaker responses to some vaccines and die at higher rates from breakthrough infection. They advocate through associations and vote as a bloc, but individually can do little beyond limiting contact; their safety tracks the same community coverage figures, and isolating further costs them exactly the connections old age runs on.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, elderly_chronic_condition_patients, beneficiary,
    moderate, biographical, constrained, local).

% Live or work inside exemption clusters and low-coverage neighborhoods - by residence, family tie, or job - where reintroduced measles or pertussis spreads first. They carry the same medical fragility as other immunocompromised patients but sit downwind of the specific gaps that exemptions and uneven enforcement leave open; moving away from family, employment, or community is not a realistic option, so they absorb the residual exposure the arrangement fails to close.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_in_coverage_gap_pockets, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, immunocompromised_in_coverage_gap_pockets, beneficiary).

% Face mandates as a condition of licensure or employment while also facing the highest occupational exposure on the ward. Unions negotiate the terms; individual workers weigh patient protection against bodily integrity, and some leave the profession over the requirement. Their licensure ties them to the system that mandates them, so walking away means abandoning the career itself.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, frontline_healthcare_workers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, frontline_healthcare_workers, beneficiary).

% Operate the exclusion rules day to day: checking cards, processing exemption requests, sending unvaccinated staff home, managing resulting grievances and lawsuits. They gain predictable attendance and reduced liability when coverage holds, and pay administrative cost and workforce disruption when it does not. They did not write the rules but they are the machinery's hands.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, employers_and_school_administrators, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, employers_and_school_administrators, payer).

% Review mandate statutes and directives against constitutional objections, set exemption breadth by statute or ruling, and periodically redraw what the enforcement chain may do. They take testimony from every other seat in the arrangement, and their decisions reprice the entire structure - widening exemptions loosens enforcement, upholding directives hardens it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% Want the vaccinations and face cost, transport, documentation, or clinic-hour barriers that prevent them. Penalty schemes written against refusal reach them indiscriminately alongside deliberate objectors. They are rarely represented when exemption and sanction rules are drafted, and their situation exposes the difference between refusing and being unable to comply.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, access_barred_would_comply_patients, excluded,
    powerless, immediate, trapped, local).

% Sell doses into legally guaranteed demand, negotiating prices against governments committed to hitting coverage targets, with liability largely capped by statute. Their revenue scales with how far the mandate chain reaches; they press for broader schedules and against narrowing, and can shift production and pricing across jurisdictions worldwide.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vaccine_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__public_health_primary, vaccine_manufacturers).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in communicable-disease control: community immunity is a public good no individual can buy alone, and the mandate chain aligns private vaccination decisions with the coverage threshold that protects people who cannot be vaccinated at all.
% TRANSFER_FUNCTION: Moves decision authority over one medical intervention from individuals to collective bodies; moves sanction costs (fines, job loss, exclusion) from the collective ledger onto the unvaccinated; moves lethal-risk reduction to the immunocompromised, infants, and the elderly; and moves compelled-purchase revenue toward manufacturers and fine revenue toward public treasuries.
% ABSENT_VOICES: Access-barred would-comply patients (authored as the excluded seat) would object that sanction schemes punish inability as if it were refusal; infants and the severely immunocompromised cannot represent themselves anywhere in the process; and during emergency framings, conscientious-objector representatives were frequently kept out of exemption-design tables, entering the conversation only through litigation after the rules had hardened.
% DISAPPEARANCE_RATIONALE: If the mandate balance vanished overnight, coverage would decay toward voluntary uptake, historical resurgence patterns would resume in the gaps, and the immunocompromised, infants, and elderly would absorb the mortality difference; employers and schools would lose their exclusion authority, courts would lose their docket, and manufacturers would lose legally guaranteed demand. Every named seat's situation rearranges.
% FOUNDING_PROBLEM: Recurrent lethal epidemics in dense settlements - smallpox in the Jacobson era, measles and pertussis across the century - that voluntary individual action repeatedly failed to control, killing children and the vulnerable in waves no household could opt out of alone.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: WHO and CDC resurgence surveillance documents outbreaks tracking coverage gaps; pre-vaccine mortality tables and historical demography attest the founding toll; court findings in the Jacobson line independently articulate the state's interest; and hospital admission records during pertussis and influenza waves document the vulnerable-seat exposure. Conscientious-objector organizations dispute the modern thresholds and the proportionality of enforcement, but the existence of the founding problem itself is attested by sources no beneficiary controls.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.70 because the enforcement chain imposes concrete, concentrated costs - fines, job loss, educational exclusion, compelled purchase - that scale with how far the mandate chain reaches; the reading holds these costs justified, but justification does not shrink them, and epsilon measures magnitude, not wrongness. Suppression (0.72) is a raw structural property and is deliberately NOT scaled by power or scope in authorship: the engine owns any contextual scaling. Its mechanism is predominantly structural (statutes, exclusion orders, employment conditions), with a minority internalized share carried by social stigma - the split is routed to the stigma_statute_suppression_split omega rather than forced into the scalar. Theater ratio (0.30) reflects real function with a growing performative layer: card-check rituals, performatively defended broad exemption hearings, and documentation exercises that substitute for coverage work. Accessibility collapse is moderate (0.45): alternatives exist once the arrangement is understood - homeschooling, remote work, relocation, exemption claims - but each is costly, so alternatives narrow without vanishing. Resistance is substantial (0.62): organized litigation, protest movements, and legislative repeal campaigns meet the arrangement continuously. The temporal series runs on one shared seven-point grid (all three metrics authored at every point, t=0..120, mapping roughly 1905-2025) and traces a U-shape with a recent ratchet: high coercion in the smallpox era, a mid-century dip as school-entry laws normalized and compliance became habitual, then rising extraction and enforcement intensity from the exemption-politics era through the mass mandates of the pandemic period. The late-interval rise in base_extractiveness is accumulation, not noise: enforcement scope widened onto adult workplaces and the compelled-purchase channel matured. Identity-lock dynamics differentiate the payer seats: conscientious objectors are ideologically fused (exit means abandoning a worldview, so they compute as identity_locked), healthcare workers are professionally fused (licensure is the self), and public health authorities are institutionally fused (the agency has become its protective mission) - if the objectors' frame broke, their exit would read as mobile and their seat would compute as ordinary regulated parties rather than locked targets.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the agenda-setter seat the arrangement is a coordination achievement it built and maintains - the free-rider problem solved by aligning private decisions with the coverage threshold. From the identity_locked objector seat the same machinery is experienced as unconditional coercion with no honest exit, which computes toward the snare end despite this reading's refusal to register them as victims. From the immunocompromised seats the arrangement is a lifeline whose leaks they personally bleed through - beneficiary and victim in one body depending on which neighborhood they stand in. From the manufacturer seat it is guaranteed demand. The engine computes these per-seat classifications from the structural data; this reading's claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (immunocompromised_medically_unvaccinable, infants_before_vaccination_age, elderly_chronic_condition_patients) drive those seats toward the subsidized end: the arrangement delivers them protection they cannot purchase individually. The victim declaration (immunocompromised_in_coverage_gap_pockets) drives that seat toward the target end: they bear the arrangement's failure mode, lethal residual exposure, with trapped exit. The unvaccinated-coerced are deliberately NOT placed in the victims array - this reading's delta subordinates their consent to necessity - so their directionality derives from power and exit structure rather than victimhood; their identity_locked exit already pulls them toward the target end of the range, and no directionality override is authored because the override surface keys on power_atom alone and would misassign the dual-positioned and powerless-beneficiary seats that share those atoms. Defaulting non-vaccinators derive near the beneficiary pole (they receive coverage while dodging contribution) - they are the free-rider edge the enforcement exists to sweep in. Healthcare workers and employers occupy genuinely dual positions, carried by secondary_role rather than overridden. Access-barred would-comply patients hold the excluded seat: commentary-grade presence only, per the ruling that authored absences never drive classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - recurrent lethal epidemics that voluntary action never controlled - remains live: pathogens persist, coverage decays between crises, and resurgence follows every sustained gap, so no mandatrophy is declared and the founding-problem status x disappearance-verdict pair (live x world_rearranges) raises no zombie flag. The live risk is drift rather than obsolescence: theater_ratio has risen monotonically across the interval as documentation ritual grows alongside coverage work, and the fine_revenue_dependency_drift omega tracks whether enforcement entities are accumulating rent in the machinery. If enforcement decays into performance while the statutes persist - mandates maintained as symbols, exemptions widened past function - the arrangement would slide toward the piton boundary, and this reading would then face the uncomfortable result that its favored instrument survives mainly as theater. The classification discipline cuts both ways: it stops this reading from laundering the arrangement as pure coordination (the coercive extraction is real and measured), and it stops the sibling reading's characterization of the whole structure as pure extraction (the coordination function is real and load-bearing - the free-rider problem is genuine).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_counterfactual,
    'This constraint is one reading (public_health_primary) of the kernel vaccine_mandate_balance; what structurally changes if the sibling reading bodily_autonomy_primary were instantiated instead?',
    'Re-run the structural derivation under the sibling''s axiom set: the unvaccinated-coerced enter the victim set, the immunocompromised-exposed exit it, and epsilon''s assessment of the same enforcement machinery flips from justified-necessity-cost to wrongful-coercion.',
    'Under the sibling reading the identical arrangement computes as far more snare-like from nearly every seat; the victim set, directionality profile, and per-seat classifications all invert while the underlying statutes stay unchanged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_counterfactual, conceptual, 'Committer structure: which reading of the mandate-balance kernel is instantiated, and what the sibling reading would relocate.').

omega_variable(
    activation_threshold_calibration,
    'What coverage level, disease severity, and lethality-to-vulnerable-profile actually triggers the supersession condition under which collective protection legitimately overrides individual consent?',
    'Pathogen-specific serological and transmission modeling combined with severity-stratified hospitalization data, audited per antigen rather than asserted globally.',
    'If the trigger is set too low, the arrangement coerces where persuasion would suffice (extraction without necessity); if set too high, the victim set grows as gaps persist. The calibration determines whether the measured extraction is necessity-cost or overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(activation_threshold_calibration, empirical, 'Empirical location of the threshold at which the reading''s supremacy condition activates.').

omega_variable(
    fine_revenue_dependency_drift,
    'How much of the enforcement machinery''s measured extraction is necessary cost of compulsion, and how much is rent accumulated by entities that now depend on the enforcement stream (fine revenue, compliance contracting, compelled-purchase volume)?',
    'Audit of sanction-revenue flows, compliance-vendor contracts, and procurement margins against the marginal cost of operating the enforcement chain.',
    'A growing rent share would mark drift from coordination-with-cost toward extraction-for-capture, shifting the arrangement''s trajectory toward the snare boundary even under this reading''s own lights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fine_revenue_dependency_drift, empirical, 'Separating necessity-cost of enforcement from captured rent inside the same machinery.').

omega_variable(
    stigma_statute_suppression_split,
    'Is the measured suppression carried by statute and exclusion rules alone, or substantially by internalized social blame that persists wherever the legal machinery is relaxed?',
    'Post-repeal trajectory comparison across jurisdictions that dropped mandates: if exclusion and penalty behavior persist at prior levels after legal removal, the internalized share is large.',
    'If much of the suppression is internalized, repealing the statutes removes less coercive force than the legal text suggests, and the arrangement''s true suppressive footprint exceeds its structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_statute_suppression_split, empirical, 'Structural versus internalized components of the arrangement''s suppressive force.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel best framed as the constitutional-doctrinal balance (a stabilized legal commitment adjudicated by courts), or as the epidemiological fact-pattern (coverage externality plus vulnerable exposure) that the doctrine tracks?',
    'Test whether the two framings yield divergent cs_pattern classifications: the doctrinal framing centers courts and precedent; the fact-pattern framing centers surveillance data and would make the ''readings'' disagreements about threshold values rather than about authority.',
    'Under the fact-pattern framing, the sibling disputes become empirical calibration disputes rather than competing commitments, and the foreclosure relation to bodily_autonomy_primary weakens to a values disagreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'CS-framing under-determination: doctrinal-kernel versus fact-pattern-kernel framings of the same dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__public_health_primary, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(vacc_tr_t20, observed).
narrative_ontology:measurement(vacc_tr_t40, vaccine_mandate_balance__public_health_primary, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(vacc_tr_t40, observed).
narrative_ontology:measurement(vacc_tr_t60, vaccine_mandate_balance__public_health_primary, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(vacc_tr_t60, observed).
narrative_ontology:measurement(vacc_tr_t80, vaccine_mandate_balance__public_health_primary, theater_ratio, 80, 0.21).
narrative_ontology:measurement_basis(vacc_tr_t80, observed).
narrative_ontology:measurement(vacc_tr_t100, vaccine_mandate_balance__public_health_primary, theater_ratio, 100, 0.26).
narrative_ontology:measurement_basis(vacc_tr_t100, observed).
narrative_ontology:measurement(vacc_tr_t120, vaccine_mandate_balance__public_health_primary, theater_ratio, 120, 0.3).
narrative_ontology:measurement_basis(vacc_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__public_health_primary, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(vacc_be_t20, observed).
narrative_ontology:measurement(vacc_be_t40, vaccine_mandate_balance__public_health_primary, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(vacc_be_t40, observed).
narrative_ontology:measurement(vacc_be_t60, vaccine_mandate_balance__public_health_primary, base_extractiveness, 60, 0.44).
narrative_ontology:measurement_basis(vacc_be_t60, observed).
narrative_ontology:measurement(vacc_be_t80, vaccine_mandate_balance__public_health_primary, base_extractiveness, 80, 0.52).
narrative_ontology:measurement_basis(vacc_be_t80, observed).
narrative_ontology:measurement(vacc_be_t100, vaccine_mandate_balance__public_health_primary, base_extractiveness, 100, 0.61).
narrative_ontology:measurement_basis(vacc_be_t100, observed).
narrative_ontology:measurement(vacc_be_t120, vaccine_mandate_balance__public_health_primary, base_extractiveness, 120, 0.7).
narrative_ontology:measurement_basis(vacc_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__public_health_primary, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(vacc_su_t20, observed).
narrative_ontology:measurement(vacc_su_t40, vaccine_mandate_balance__public_health_primary, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(vacc_su_t40, observed).
narrative_ontology:measurement(vacc_su_t60, vaccine_mandate_balance__public_health_primary, suppression_requirement, 60, 0.47).
narrative_ontology:measurement_basis(vacc_su_t60, observed).
narrative_ontology:measurement(vacc_su_t80, vaccine_mandate_balance__public_health_primary, suppression_requirement, 80, 0.54).
narrative_ontology:measurement_basis(vacc_su_t80, observed).
narrative_ontology:measurement(vacc_su_t100, vaccine_mandate_balance__public_health_primary, suppression_requirement, 100, 0.63).
narrative_ontology:measurement_basis(vacc_su_t100, observed).
narrative_ontology:measurement(vacc_su_t120, vaccine_mandate_balance__public_health_primary, suppression_requirement, 120, 0.72).
narrative_ontology:measurement_basis(vacc_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the vaccine mandate debate' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing the kernel vaccine_mandate_balance. Each reading carries its own victim set, its own epsilon, and its own classification: this reading places immunocompromised-in-coverage-gaps in the victim set and the unvaccinated-coerced outside it; bodily_autonomy_primary inverts that placement; proportionality_reading conditions the whole apparatus on threshold satisfaction. The stories are linked through affects_constraints so contamination and foreclosure analysis can traverse the family; no single story averages across the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
