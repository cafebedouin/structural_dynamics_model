% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__risk_stratification_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Actuarial-Risk-Gated Vaccine Mandate Authority (Risk-Stratification Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   Between 2020 and 2024, vaccination requirements expanded from targeted
 *   facility rules to blanket application across employers, venues, schools,
 *   and federal programs, then contracted sharply as variant severity fell
 *   and review courts demanded risk-differentiated justification. This story
 *   authors that standing arrangement through the proportionality lens:
 *   mandate legitimacy is contingent on an actuarial risk threshold, so the
 *   blanket phase imposed compliance burdens on low-risk adults
 *   disproportionate to their situation while the targeted residue protects
 *   people who cannot protect themselves. Claim and metrics are authored
 *   independently: the arrangement coordinates protection around genuine risk
 *   concentration and simultaneously imposed unjustified burdens during its
 *   blanket phase; the engine computes per-seat classifications from the
 *   structural data, and any divergence between the tangled_rope claim and a
 *   computed seat type is the measurement, not an error. This file is one
 *   reading of the decomposed vaccine-mandate-legitimacy kernel; see
 *   kernel_context and the network note for the family structure.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Agenda setter (institutional/constrained) — designs, defends, and narrows the requirement regime
 *   - immunocompromised_patients: Protected beneficiary (powerless/trapped) — depends on others' coverage
 *   - long_term_care_residents: Protected beneficiary (powerless/trapped) — highest-consequence setting
 *   - frontline_healthcare_workers: Dual-positioned subject (organized/constrained) — bears the most durable requirements where the justification is strongest
 *   - low_actuarial_risk_adults: Primary payer (moderate/constrained) — bore blanket-phase burdens without proportional individual risk
 *   - unvaccinated_terminated_workers: Realized-cost payer (powerless/trapped) — lost employment; outside the conversation
 *   - healthy_schoolchildren: Payer (powerless/trapped) — school-entry rules in some jurisdictions
 *   - natural_immunity_claimants: Excluded voice (moderate/constrained) — equivalence claims never accommodated
 *   - constitutional_courts: Analytical observer (institutional/analytical) — proportionality review shapes the regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.47).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.4).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Actuarial-Risk-Gated Vaccine Mandate Authority (Risk-Stratification Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, 'e19c155d-b00a-4b9b-81fa-6edd3583bca2').
narrative_ontology:cs_kernel_codification('e19c155d-b00a-4b9b-81fa-6edd3583bca2', formalized).
narrative_ontology:cs_authority_grounding('e19c155d-b00a-4b9b-81fa-6edd3583bca2', lineage).
narrative_ontology:cs_interpretation_layer_present('e19c155d-b00a-4b9b-81fa-6edd3583bca2').
narrative_ontology:cs_reading_relation('e19c155d-b00a-4b9b-81fa-6edd3583bca2', vaccine_mandate_legitimacy__public_health_primacy_reading, influences).
narrative_ontology:cs_reading_relation('e19c155d-b00a-4b9b-81fa-6edd3583bca2', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_axiom('e19c155d-b00a-4b9b-81fa-6edd3583bca2', foundational, proportionality_gates_state_medical_coercion).
narrative_ontology:cs_axiom_status(proportionality_gates_state_medical_coercion, holdable).
narrative_ontology:cs_axiom_grounding('e19c155d-b00a-4b9b-81fa-6edd3583bca2', proportionality_gates_state_medical_coercion, deontological).
narrative_ontology:cs_axiom('e19c155d-b00a-4b9b-81fa-6edd3583bca2', secondary, actuarial_evidence_conditions_legitimacy).
narrative_ontology:cs_axiom_status(actuarial_evidence_conditions_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e19c155d-b00a-4b9b-81fa-6edd3583bca2', actuarial_evidence_conditions_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('e19c155d-b00a-4b9b-81fa-6edd3583bca2', proportionality_bounded_mandate_authority).
narrative_ontology:cs_drift_state('e19c155d-b00a-4b9b-81fa-6edd3583bca2', post_emergency_review_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e19c155d-b00a-4b9b-81fa-6edd3583bca2', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, long_term_care_residents).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, low_actuarial_risk_adults).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, unvaccinated_terminated_workers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, healthy_schoolchildren).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, frontline_healthcare_workers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, frontline_healthcare_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer vaccination requirement policy: they issue workplace and facility rules, define exemption categories, and defend each requirement's justification in review proceedings. During the emergency they applied requirements broadly; as severity data accumulated they narrowed application to high-exposure, high-consequence settings. Their authority now depends on producing actuarial justification for each remaining rule.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Cannot mount a protective response to vaccination themselves and depend on reduced transmission around them. Requirements on staff and visitors in the clinics and facilities they must regularly occupy are the main policy instrument lowering their exposure; they cannot opt out of shared air, and relocation does not remove the risk.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).

% Live in congregate settings with the highest severe-outcome rates recorded during the pandemic. Staff vaccination rules in these facilities are aimed directly at protecting them; they have no practical alternative residence and no way to self-protect against staff-borne exposure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, long_term_care_residents, beneficiary,
    powerless, biographical, trapped, regional).

% Work where patient vulnerability and occupational exposure intersect, and were the first and most durable subjects of facility entry requirements. Unions and professional bodies negotiated testing alternatives, hardship exemptions, and phased timelines; some members left acute care rather than comply. The same rules that condition their employment also reduce infection pressure inside their own workplaces.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, frontline_healthcare_workers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, frontline_healthcare_workers, beneficiary).

% Working-age adults without the conditions that predict severe outcomes faced the same blanket requirements as high-risk groups during 2021-2022: employer rules, venue passes, and in some sectors federal contractor mandates. Their individual severe-outcome risk was an order of magnitude below older cohorts, and their options were compliance, employer-specific exemptions where offered, job change within covered sectors, or refusal with termination risk.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_actuarial_risk_adults, payer,
    moderate, biographical, constrained, national).

% Lost jobs, contracts, or military careers after refusing requirements whose exemptions they did not fit. Litigation largely failed; rehiring after rescission was uneven. They bear the concentrated, realized cost of the enforcement episode and sit outside the current policy conversation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, unvaccinated_terminated_workers, payer,
    powerless, immediate, trapped, national).

% Subject to school-entry requirements in some jurisdictions despite severe-outcome rates far below adult cohorts. Parents may pursue exemptions of varying difficulty, but attendance itself is compulsory, so the bodily imposition is decided by rules the child has no part in making; developmental stakes in education continuity cut against both compliance and exclusion.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, healthy_schoolchildren, payer,
    powerless, generational, trapped, regional).

% People with documented prior infection argued their protection was equivalent to vaccination and sought recognition in exemption schemes; most mandate frameworks declined to certify infection history as meeting requirements, so this group remained outside the accommodation conversation while bearing the same compliance demands.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, natural_immunity_claimants, excluded,
    moderate, biographical, constrained, national).

% Adjudicate challenges to requirement rules under proportionality and major-questions doctrines: they upheld some occupational rules, blocked others (broad federal contractor and OSHA rules were stayed), and their reasoning increasingly demands risk-differentiated justification rather than categorical deference to health agencies.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates coercive public-health intervention where actuarial risk justifies it: protecting people who cannot respond to vaccination by requiring coverage in high-exposure, high-consequence settings, achieving protection thresholds at the lowest total coercion cost the evidence supports.
% TRANSFER_FUNCTION: Moves bodily autonomy and employment or school access from regulated individuals toward population-level risk reduction accruing to high-risk strata; moves enforcement burden onto employers, facilities, and schools; during the blanket phase it moved livelihood security from refusers to complying institutions.
% ABSENT_VOICES: Natural-immunity claimants were never seated in the accommodation conversation; terminated workers lost standing to object once separated; disability advocates raising accommodation failures had procedural but little substantive uptake. The unanimity of expert consensus on any given rule partly reflects who was in the room when it was designed.
% DISAPPEARANCE_RATIONALE: If the proportionality-gated requirement structure vanished overnight, long-term-care and clinical settings would lose their principal protection architecture, employers would face unstructured liability questions the next time a severe variant emerges, and the contest between blanket expansion and blanket abolition would have no mediating doctrine; the trapped beneficiary seats would absorb the difference first.
% FOUNDING_PROBLEM: Voluntary vaccination decisions create externalities, so individual choice undersupplies protection for people who cannot mount a vaccine response; emergency conditions in 2020-2021 produced blanket application before anyone differentiated requirements by actuarial risk.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: appellate proportionality rulings attest both that the externality problem is real and that its justification is setting-specific (occupational facility rules upheld, blanket federal rules stayed); published bioethics proportionality analyses and public-health surveillance data on risk heterogeneity independently document the founding problem's bounded persistence. No beneficiary-operated body solely attests it.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.47 at interval end) reflects this reading's assessment of the standing arrangement: the blanket-application phase imposed bodily and economic compliance costs on low-actuarial-risk adults far exceeding any proportionate demand their situation generated, while the targeted residue (facility staff rules) sits inside the reading's legitimate core. Suppression (0.40) is the residual enforcement posture after the 2023-2024 wind-down: termination-backed rules survive mainly in healthcare and long-term-care settings. Theater (0.31) captures passport and booster-rule maintenance that outlasted epidemiological relevance before recision. Accessibility collapse is low (0.38): understanding the proportionality analysis does not close off alternatives — voluntary uptake, testing regimes, and risk-targeted rules remain available, which is precisely this reading's claim. Resistance (0.68) was high throughout: litigation, legislative reversal, resignation waves, and continuing political salience. All three temporal series run on one shared seven-point grid (2020-2026); 2026 points are projections marked basis=projected. The trajectories are rise-and-decline, not cyclical: extraction and suppression peak together in 2022 and decay as the regime converges toward the risk-stratified form, while theater peaks in 2023 as enforcement outlives its justification before recision catches up.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is a defensible exercise of delegated health power now disciplined by review; from the terminated-worker seat it was an uncompensated taking of livelihood; from the healthcare-worker seat it is a condition of practice that also protects the worker's own patients and workplace. Same-level divergence: healthcare workers and office workers faced nominally identical requirements at similar bargaining power, but actuarial position — not power — differentiates their exits and their assessments. The organized seat's directionality override encodes that the requirement's protective incidence concentrates where healthcare workers themselves stand.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for the immunocompromised and long-term-care seats (trapped, powerless — the arrangement subsidizes their exposure reduction). Public health authorities derive low d as agenda-setter-beneficiaries, tempered by the proportionality-review costs they now bear. Victims derive high d: terminated workers (trapped, powerless) sit nearest the full-target end; schoolchildren (trapped, powerless) just below; low-risk adults (moderate, constrained) lower still because jurisdiction and employer shopping partially dilute the imposition. The single override corrects the organized seat: a role-plus-exit derivation would read healthcare workers as near-full targets, but the requirement's protective function concentrates on their own facilities, so d=0.45. Scope is national with functioning domestic review, so scope amplification of extraction is modest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — voluntary uptake undersupplying protection for people who cannot respond to vaccination — remains live in recurring form (novel pathogens, new variants), but the blanket-emergency form of the arrangement outlived its justification as severity fell. The classification keeps the two faces distinct: the targeted core is genuine coordination that mislabeling as pure imposition would dismantle to the detriment of the trapped beneficiary seats, while the blanket excess was real imposition that mislabeling as pure coordination would launder. The blanket form's mandate is resolved; the risk-stratified residue persists on recertifiable justification. The founding_problem_status=contested combined with disappearance_verdict=world_rearranges records that recurrence, not death, is the accurate genealogy — the arrangement rearranges the world whenever the next severe variant arrives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_definition_ambiguity,
    'Which actuarial quantity defines the threshold — individual severe-outcome risk, transmission contribution, or a combined index — and where is the cutoff set?',
    'Comparative doctrine review plus outcome back-testing of alternative thresholds against observed hospitalization and transmission data across jurisdictions that drew the line differently.',
    'A severe-outcome-weighted threshold shrinks the subject set toward the autonomy-protective pole; a transmission-weighted threshold expands it until nearly everyone is regulable, collapsing this reading''s distinction from unconditional mandate authority. Victim-set size and the constraint''s classification both swing on this definition — this is the declared collapse channel between the extreme readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_definition_ambiguity, conceptual, 'Threshold metric choice determines victim-set size and whether the reading stays intermediate or collapses into a sibling.').

omega_variable(
    actuarial_drift_under_variant_change,
    'Does the actuarial foundation of the remaining requirements track current variant severity and population immunity, or lag behind it?',
    'Mandate-by-mandate audit against current surveillance data, with sunset-and-recertify cycles tying each rule''s continuation to fresh risk estimates.',
    'Requirements legitimate at authorization may fail proportionality today without any doctrinal change; sustained lag converts targeted rules into inertial remnants and pushes the theater_ratio series upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actuarial_drift_under_variant_change, empirical, 'Whether the empirical foundation of remaining rules keeps pace with the risk landscape.').

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the risk_stratification_reading of the vaccine_mandate_legitimacy kernel; what changes structurally under the sibling readings?',
    'Generate the sibling stories (public_health_primacy_reading, bodily_autonomy_primacy_reading) and compare victim sets, coordination functions, and computed types across the family.',
    'Under public_health_primacy_reading the proportionality gate disappears and the subject set becomes all non-compliant persons; under bodily_autonomy_primacy_reading the coordination function disappears and every mandated person enters the harmed set. This reading''s intermediate position — coordination preserved, subject set bounded by threshold — is the structural delta; if the threshold omega resolves to either pole, this reading collapses into the corresponding sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a three-reading kernel, with specified collapse conditions.').

omega_variable(
    residual_enforcement_inertia,
    'Are the surviving targeted requirements maintained by current actuarial justification or by institutional habit and sunk administrative investment?',
    'Compare recertification records and enforcement expenditure against current risk data; examine whether agencies re-justify each rule or merely renew it.',
    'If inertia dominates, the targeted residue drifts toward theatrical maintenance and current theater exceeds the measured 0.31; if recertification is real, the residue remains functional coordination and the tangled_rope reading holds at the targeted core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_enforcement_inertia, empirical, 'Whether the targeted residue is living coordination or habitual renewal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 2020, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t2020, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2020, 0.14).
narrative_ontology:measurement(vacc_tr_t2021, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2021, 0.22).
narrative_ontology:measurement(vacc_tr_t2022, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2022, 0.36).
narrative_ontology:measurement(vacc_tr_t2023, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2023, 0.43).
narrative_ontology:measurement(vacc_tr_t2024, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2024, 0.39).
narrative_ontology:measurement(vacc_tr_t2025, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2025, 0.34).
narrative_ontology:measurement(vacc_tr_t2026, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2026, 0.31).

% Extraction over time
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2020, 0.28).
narrative_ontology:measurement(vacc_be_t2021, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2021, 0.47).
narrative_ontology:measurement(vacc_be_t2022, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2022, 0.61).
narrative_ontology:measurement(vacc_be_t2023, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2023, 0.57).
narrative_ontology:measurement(vacc_be_t2024, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2024, 0.52).
narrative_ontology:measurement(vacc_be_t2025, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2025, 0.49).
narrative_ontology:measurement(vacc_be_t2026, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2026, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(vacc_su_t2021, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2021, 0.56).
narrative_ontology:measurement(vacc_su_t2022, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2022, 0.71).
narrative_ontology:measurement(vacc_su_t2023, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2023, 0.6).
narrative_ontology:measurement(vacc_su_t2024, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2024, 0.5).
narrative_ontology:measurement(vacc_su_t2025, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2025, 0.43).
narrative_ontology:measurement(vacc_su_t2026, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2026, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, resource_allocation).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'vaccine mandate legitimacy' covers three structurally distinct claims that share mandate practices but fix different victim sets and different coordination functions. This file instantiates the risk-stratification reading (proportionality-gated, intermediate victim set). The public-health-primacy reading is the historically upstream claim — emergency justifications cited collective harm — and this reading exerts downstream pressure on it by conditioning legitimacy on actuarial evidence; the bodily-autonomy-primacy reading rejects the shared warrant premise outright. Epsilon differs across the family because each reading fixes a different referent victim set; measuring one reading with another's observable would violate epsilon invariance, hence separate files linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__risk_stratification_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
