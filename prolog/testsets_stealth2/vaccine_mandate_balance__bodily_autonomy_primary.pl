% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Compulsory Medical Intervention Regime — Bodily-Autonomy-Primary Reading
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The standing arrangement under contest is the
 *   compulsory-medical-intervention regime: statutory school-entry
 *   requirements, emergency-era workplace and healthcare mandates, and the
 *   exemption-adjudication and penalty machinery that enforces them. This
 *   story is the bodily_autonomy_primary reading of that arrangement: it
 *   assesses the regime by the light that individual consent over medical
 *   intervention is inviolable and that collective benefit, however real,
 *   never authorizes compulsion. On this reading the regime's burden lands on
 *   bodily integrity itself, the unvaccinated-coerced are its victims, and
 *   the immunocompromised — however genuinely protected — are beneficiaries
 *   whose risk acceptance is inherent to liberty, not a harm done to them.
 *   Epsilon's referent is this standing arrangement as this reading assesses
 *   it; the reading's endorsed alternative (a purely voluntary system) is a
 *   different arrangement and is not measured here. Claim and metrics are
 *   authored independently: the structural claim is tangled_rope because the
 *   regime does solve a real free-rider problem while extracting through the
 *   same structure under active enforcement; the reading's normative axiom,
 *   that the extraction is categorically impermissible, lives in the axioms
 *   and omegas, not in the metrics. Family note: the sibling readings author
 *   different epsilon values over the same arrangement —
 *   public_health_primary lowers epsilon and adds the
 *   exposed-immunocompromised to the victim set; proportionality_reading
 *   makes both victimhood and permissibility conditional on threshold
 *   variables. Those siblings are separate constraint files linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - unvaccinated_coerced_individuals: primary target (moderate/constrained) — bears compelled intervention under penalty of livelihood and schooling
 *   - public_health_agencies: agenda-setter and collector (institutional/arbitrage) — designs, imposes, adjudicates, and defends the requirements
 *   - immunocompromised_and_high_risk_patients: protected beneficiary (organized/trapped) — receives coverage-dependent protection, cannot exit exposure
 *   - mandating_institutions: implementing beneficiary (institutional/mobile) — hospitals, universities, employers, military; adopt and retract rules
 *   - constitutional_courts: boundary adjudicator (institutional/analytical) — sets where the consent line sits
 *   - minors_under_school_entry_rules: excluded bearer (powerless/trapped) — subjected without consent capacity or seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.7).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.63).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Compulsory Medical Intervention Regime — Bodily-Autonomy-Primary Reading").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, 'c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b').
narrative_ontology:cs_kernel_codification('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b', formalized).
narrative_ontology:cs_authority_grounding('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b', lineage).
narrative_ontology:cs_interpretation_layer_present('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b').
narrative_ontology:cs_reading_relation('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b', vaccine_mandate_balance__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b', foundational, medical_intervention_requires_affirmative_consent).
narrative_ontology:cs_axiom_status(medical_intervention_requires_affirmative_consent, holdable).
narrative_ontology:cs_axiom_grounding('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b', medical_intervention_requires_affirmative_consent, deontological).
narrative_ontology:cs_axiom('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b', foundational, collective_benefit_never_overrides_bodily_integrity).
narrative_ontology:cs_axiom_status(collective_benefit_never_overrides_bodily_integrity, holdable).
narrative_ontology:cs_axiom_grounding('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b', collective_benefit_never_overrides_bodily_integrity, deontological).
narrative_ontology:cs_reference_frame('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b', inviolable_consent_framework).
narrative_ontology:cs_drift_state('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b', post_covid_mandate_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c03f0fa9-6b91-4c98-bfa5-ef89c5057d6b', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_and_high_risk_patients).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, mandating_institutions).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, police_powers_public_health_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, herd_immunity_free_rider_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declined or delayed a mandated vaccination and faced consequences: termination from employment, exclusion from schools, universities, and healthcare workplaces, denial of entry to venues, or accumulating fines. Some obtained medical or religious exemptions where channels existed; others paid penalties, changed jobs, home-schooled, or relocated. Legal challenge and political organizing are open avenues that have already rolled back several mandates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    moderate, biographical, constrained, national).

% Design and impose vaccination requirements through emergency orders, workplace rules, and school-entry statutes; run the exemption-adjudication machinery; collect coverage data and defend the rules in court. Mandate programs carry budget lines, staffing, and expanded administrative authority. When political direction shifts, these agencies can retire mandates and pivot to incentive campaigns without leaving their seat.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies, beneficiary).

% Cannot be safely vaccinated or respond poorly to vaccines and rely on surrounding coverage for protection. Organized patient advocacy pushed for mandates during the pandemic. They bear exposure risk wherever coverage falls and cannot relocate away from airborne transmission; their protection arrives through other people's compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_and_high_risk_patients, beneficiary,
    organized, biographical, trapped, national).

% Hospitals, university systems, large employers, and the military adopted or administered requirement policies, gaining predictable staffing, reduced outbreak disruption, and liability cover. Most retained authority to drop the rules unilaterally and many did so once legal and political pressure mounted.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, mandating_institutions, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__bodily_autonomy_primary, mandating_institutions, agenda_setter).

% Adjudicate where the consent/police-power boundary sits: upholding, narrowing, or striking down specific mandate instruments. Their rulings determine which compulsions survive; they take testimony from every other seat and answer to precedent rather than to any party's program.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Are subjected to school-entry vaccination requirements without legal capacity to consent; parents or guardians nominally exercise the consent right on their behalf. They hold no seat in rule-setting and their dissent, where it exists, is voiced only through adults.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, minors_under_school_entry_rules, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__bodily_autonomy_primary, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Compels contribution to a public good: population-level disease protection exhibits a free-rider structure in which each person is safest when others vaccinate, so voluntary uptake alone has historically undersupplied coverage; requirements push coverage toward protective thresholds.
% TRANSFER_FUNCTION: Transfers decisional authority over one's own medical treatment from individuals (and parents) to state and institutional authorities, and redistributes infection risk from the immunocompromised and high-risk onto those who would have declined.
% ABSENT_VOICES: Minors subject to school-entry rules have no seat and no consent capacity of their own; denied exemption applicants had no channel into rule-design; and on this reading's own account the immunocompromised are heard but their claim is answered by majority calculus rather than treated as a veto — the people whose bodies are at stake in future outbreaks are not yet in the room at all.
% DISAPPEARANCE_RATIONALE: Coverage would fall below protective thresholds in pockets within months; institutions would rebuild around voluntary incentives, testing, and accommodation; the immunocompromised would face measurably higher exposure; agencies would lose the enforcement instrument and the litigation docket would empty — removal rearranges schooling, employment, and clinical operations.
% FOUNDING_PROBLEM: Recurring lethal epidemics in dense populations where voluntary uptake failed to protect the vulnerable: the police-power tradition (Jacobson v. Massachusetts, 1905) held that community survival justified compelling inoculation.
% FOUNDING_PROBLEM_CORROBORATION: Historical epidemiology outside the benefiting parties corroborates the founding problem: pre-vaccine mortality tables and outbreak records (smallpox, polio, measles) show voluntary uptake repeatedly undersupplying protection, and the Jacobson line of case law attests its recurrence. No source outside the benefiting parties attests that the problem is permanently solved or that voluntary means always suffice — that gap is exactly what this reading contests.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.70 at interval end) because the burden falls on bodily integrity itself — this reading holds no collective benefit can price consent, and even the post-rollback residual compels intervention under penalty of livelihood. Suppression (0.63) tracks the enforcement machinery actually applied: exclusion from employment, schooling, and venues, plus narrowed exemption channels in several jurisdictions; it peaked with the 2022-2023 mandate stack and eased as rollbacks landed. Theater ratio (0.38) reflects a mixed apparatus: vaccination delivery and safety monitoring are functional, while a growing share of peak-period activity was compliance documentation, attestation rituals, and penalty administration aimed at the holdout margin rather than at disease. Accessibility collapse (0.60): once a requirement binds, alternatives thin to exemption channels, job or jurisdiction change, or absorbing penalties — real but costly exits. Resistance (0.70) is unusually high for a health measure: litigation, legislative preemption, protest, and institutional reversals. The three series share one eight-point annual grid (t0=2019 through t7=2026); t7 is a projected year-end estimate. The trajectories trace one full crisis cycle — baseline routine mandates (2019-2020), emergency imposition (2021), peak stack (2022-2023), backlash-driven partial rollback (2024-2026). The cycle's driver is the interaction of emergency renewal with electoral and litigation pressure, not intermittent reinforcement by design, though each emergency renewal did re-price noncompliance upward before politics forced retreat. Scalars are measured at the post-rollback plateau: burden still elevated, enforcement easing.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the agency seat the arrangement is an instrument it deploys, retunes, and retires — low burden, high control. From the coerced seat the same instrument is a penalty attached to their body. From the immunocompromised seat it is a shield they cannot provide for themselves. From the court seat it is a boundary question with no stake in the outcome. The engine computes these per-seat classifications from power, exit, and declared position; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map cleanly: agencies and mandating institutions sit near the beneficiary end (they receive compliance, authority, staffing predictability, and liability cover); the coerced sit near the target end (they deliver the intervention's cost under penalty); the immunocompromised receive protection they cannot self-provide — strongly subsidized. One override is authored: the derivation reads trapped exit as target-position evidence, but the immunocompromised are trapped BECAUSE they depend on the arrangement's output; their d is authored at 0.12. Minors under school-entry rules carry no declared position and take the canonical fallback; their exclusion is recorded as a seat, not as a correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — lethal outbreaks outrunning voluntary protection — is recurrent rather than dead: dormant between emergencies, acute during them. That recurrence is why the arrangement resists simple obsolescence verdicts. This reading's contribution to mandatrophy discipline is to keep emergency-era coercion from being laundered into permanent consensus: each imposition must re-justify itself against consent rather than inherit legitimacy from the last emergency. Symmetrically, the live free-rider problem keeps the reading from dismissing the arrangement as pure parasitism — the coordination function is real even where the reading deems it non-authorizing. Classification stays contested at the family level; no resolved-mandatrophy flag is authored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates one reading (bodily_autonomy_primary) of the vaccine_mandate_balance kernel; how would the sibling readings restructure the constraint''s victim set and justification calculus?',
    'Not resolvable by data within this story: generate and compare the sibling stories (public_health_primary, proportionality_reading) as separate constraints; cross-seat classification divergence across the family locates the dispute.',
    'Under public_health_primary the immunocompromised-exposed join the victim set and epsilon falls for the same arrangement; under proportionality_reading victimhood becomes conditional on threshold violation. This story''s high epsilon and one-group victim structure hold only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings are separate constraints.').

omega_variable(
    victim_set_boundary_risk_acceptance,
    'Does infection risk accepted under a liberty-respecting regime count as harm inflicted on the immunocompromised, or is it the background risk of free association?',
    'Conceptual resolution within political philosophy: settle whether negative liberty entails responsibility for third-party exposure risk; empirically, compare morbidity-attribution studies across voluntary-versus-mandate jurisdictions.',
    'If accepted risk counts as harm, the immunocompromised migrate into the victim set and the arrangement gains a second extraction axis; if not, the victim set stays as authored and this reading''s structure is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_risk_acceptance, conceptual, 'Boundary of the victim set: risk acceptance inherent to liberty versus harm to the unprotected.').

omega_variable(
    voluntary_coverage_sufficiency,
    'Can voluntary uptake plus incentives reach protective coverage in a lethal outbreak, or does compulsion carry coverage the last mile?',
    'Jurisdictional natural experiments: states that banned mandates versus states that imposed them, matched on demographics; coverage and mortality deltas during the 2021-2022 surge.',
    'If voluntary means suffice, the enforcement machinery is removable overhead and the arrangement''s coordination claim collapses toward pure compulsion; if not, part of the measured burden is the irreducible price of the public good and the sibling readings gain force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_coverage_sufficiency, empirical, 'Whether the coercion mechanism is load-bearing for coverage or substitutable.').

omega_variable(
    residual_mandate_scope_trajectory,
    'Are the surviving mandates (healthcare employment, military, school-entry) the arrangement''s stable core or a receding overhang headed for full retirement?',
    'Track legislative repeals, court rulings, and institutional policy reversals through 2028; extend or flatten the measurement series'' post-2025 slope accordingly.',
    'Full retirement would date a transition toward inertial persistence of the exemption bureaucracy; a stable core would confirm the arrangement as a permanent feature with elevated steady-state burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_mandate_scope_trajectory, empirical, 'Whether post-rollback residuals are terminal decline or steady state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmb_bodily_autonomy_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_tr_t0, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_tr_t1, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 1, 0.26).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_tr_t1, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_tr_t2, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 2, 0.34).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_tr_t2, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_tr_t3, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 3, 0.43).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_tr_t3, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_tr_t4, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 4, 0.48).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_tr_t4, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_tr_t5, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 5, 0.44).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_tr_t5, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_tr_t6, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 6, 0.4).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_tr_t6, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_tr_t7, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 7, 0.38).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_tr_t7, projected).

% Extraction over time
narrative_ontology:measurement(vmb_bodily_autonomy_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_be_t0, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_be_t1, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 1, 0.46).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_be_t1, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_be_t2, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 2, 0.68).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_be_t2, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_be_t3, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 3, 0.8).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_be_t3, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_be_t4, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 4, 0.85).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_be_t4, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_be_t5, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 5, 0.79).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_be_t5, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_be_t6, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 6, 0.73).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_be_t6, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_be_t7, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 7, 0.7).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_be_t7, projected).

% Suppression requirement over time
narrative_ontology:measurement(vmb_bodily_autonomy_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_su_t0, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_su_t1, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 1, 0.37).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_su_t1, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_su_t2, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 2, 0.58).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_su_t2, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_su_t3, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 3, 0.74).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_su_t3, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_su_t4, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 4, 0.82).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_su_t4, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_su_t5, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 5, 0.75).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_su_t5, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_su_t6, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 6, 0.67).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_su_t6, observed).
narrative_ontology:measurement(vmb_bodily_autonomy_su_t7, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 7, 0.63).
narrative_ontology:measurement_basis(vmb_bodily_autonomy_su_t7, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: vaccine_mandate_balance decomposes into three readings with distinct epsilon values over the same standing arrangement. This member (bodily_autonomy_primary) authors high epsilon from coercion mechanisms and a one-group victim set; public_health_primary authors lower epsilon with a two-sided victim set (coerced plus exposed-immunocompromised); proportionality_reading makes both conditional on threshold variables. Each reading forecloses the others as a governing decision rule within a single framework, so the family models a live constitutional dispute rather than a measurement disagreement. Edges run between all members in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, organized, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
