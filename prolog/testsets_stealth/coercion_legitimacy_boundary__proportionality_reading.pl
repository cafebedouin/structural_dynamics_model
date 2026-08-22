% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Severity-Scaled Medical Coercion Boundary (Proportionality Reading)
 *   domain: public_health/legal_ethics
 *
 * SUMMARY:
 *   A century-old statutory and doctrinal arrangement ties the state's power
 *   to compel medical intervention - school-entry vaccination mandates,
 *   isolation and quarantine orders, employment-conditioned requirements - to
 *   measured features of the disease in question: how transmissible it is,
 *   how severe, whom it kills. Measles, with extreme transmissibility and
 *   real infant mortality, clears the bar and mandates stand; seasonal
 *   influenza does not, and compulsion there survives mainly as private
 *   employment practice. Interval units are years since 1905 (Jacobson v.
 *   Massachusetts); t=120 corresponds to 2025. This file instantiates the
 *   proportionality reading of the coercion-legitimacy kernel (see
 *   kernel_context); its epsilon is authored for the standing arrangement as
 *   that reading assesses it: mostly self-limiting coercion that tracks
 *   threat, leaking over-extraction at three identifiable seams -
 *   sub-threshold workplace mandates, emergency suspensions of adjudication,
 *   and the hearing-and-paperwork burden placed on objectors even for
 *   endorsed mandates. Sibling readings partition the same referent
 *   differently and are linked as a constraint family via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - public_health_authorities: Agenda-setting administrator (institutional/arbitrage) - sets pathogen thresholds, runs enforcement, collects compliance and administrative authority
 *   - - conscientious_objector_parents: Primary payer for endorsed mandates (organized/constrained) - bear coerced intervention on dependents, lose school access on refusal
 *   - - healthcare_workers_under_flu_requirements: Payer for sub-threshold mandates (organized/mobile) - bear employment-conditioned intervention the reading's own criterion scores below the coercion line
 *   - - immunocompromised_patients: Trapped beneficiary (powerless/trapped) - depend wholly on others' induced immunity
 *   - - pre_vaccination_infants: Trapped beneficiary (powerless/immediate horizon) - protection arrives only through community immunity
 *   - - hospital_employers: Secondary agenda-setter (institutional/arbitrage) - impose sub-threshold workplace requirements, accrue absenteeism and liability relief
 *   - - emergency_order_subjects: Excluded voice (moderate/trapped) - subjected to suspension-of-adjudication coercion without a seat in threshold-setting
 *   - - constitutional_courts: Analytical observer (institutional/analytical) - adjudicate the boundary's limits case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.45).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.52).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Severity-Scaled Medical Coercion Boundary (Proportionality Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health/legal_ethics").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, 'd9cc9090-6e8c-48f1-87af-a86a5ba6bce1').
narrative_ontology:cs_kernel_codification('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1', formalized).
narrative_ontology:cs_authority_grounding('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1', lineage).
narrative_ontology:cs_interpretation_layer_present('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1').
narrative_ontology:cs_reading_relation('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1', foundational, coercion_scales_with_epidemiological_threat).
narrative_ontology:cs_axiom_status(coercion_scales_with_epidemiological_threat, holdable).
narrative_ontology:cs_axiom_grounding('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1', coercion_scales_with_epidemiological_threat, instrumental).
narrative_ontology:cs_axiom('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1', secondary, pathogen_specific_adjudication_required).
narrative_ontology:cs_axiom_status(pathogen_specific_adjudication_required, holdable).
narrative_ontology:cs_axiom_grounding('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1', pathogen_specific_adjudication_required, empirically_contingent).
narrative_ontology:cs_reference_frame('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1', severity_scaled_coercion_baseline).
narrative_ontology:cs_drift_state('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1', post_covid_emergency_retrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d9cc9090-6e8c-48f1-87af-a86a5ba6bce1', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, pre_vaccination_infants).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, conscientious_objector_parents).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, healthcare_workers_under_flu_requirements).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, emergency_order_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, healthcare_workers_under_flu_requirements).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, hospital_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the rules deciding which diseases justify compelled vaccination, isolation, or screening, and run the enforcement machinery: school-entry exclusions, outbreak orders, disease reporting. Compliance, budget growth, and expanded emergency authority flow to them when threats rise. They can shift attention between pathogens and adjust thresholds; their own exposure to the rules they write is limited.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, beneficiary).

% Hold sincere objections to specific vaccines for their children. When a disease meets the threshold, refusal costs school and daycare access; exemption routes exist but require hearings, affidavits, and paperwork that some states are narrowing. Homeschooling, private school, or interstate moves are available at significant cost. They also share the low-transmission school environment the rules maintain.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, conscientious_objector_parents, payer,
    organized, biographical, constrained, national).

% Face annual influenza vaccination as a condition of hospital employment in many systems. Influenza's severity and transmission profile sits below the level at which this arrangement treats compulsion as justified, yet the requirement persists. Hospitals compete for their labor, so the pressure is real but escapable at career cost. They receive protection from workplace infection as a side effect.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, healthcare_workers_under_flu_requirements, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, healthcare_workers_under_flu_requirements, beneficiary).

% Cannot be vaccinated or respond poorly to vaccines; their safety depends on the immunity of everyone around them. They have no exit from exposure risk and no way to buy the protection individually. When community and school coverage stays high they move through ordinary life; when coverage drops, their world shrinks.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, regional).

% Are too young for the vaccine schedule while maximally vulnerable to the diseases it prevents. Protection reaches them only through the immunity of caregivers and surrounding communities. They cannot act, choose, or exit anything; their stake is carried by parents and clinicians.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, pre_vaccination_infants, beneficiary,
    powerless, immediate, trapped, regional).

% Impose influenza and other workplace vaccination requirements as employment conditions, citing patient safety, absenteeism, and liability. They set these terms unilaterally and can tighten or drop them seasonally. The requirements reduce staffing disruption and legal exposure.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, hospital_employers, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, hospital_employers, beneficiary).

% During declared outbreaks, ordinary threshold adjudication is suspended and orders issue directly: quarantine, closure, mass vaccination directives. Those subject to the orders had no seat in the deliberations that set the trigger conditions, and complying often means losing income or custody of daily routines for the order's duration. Exit is effectively unavailable while the declaration stands.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, emergency_order_subjects, excluded,
    moderate, immediate, trapped, national).

% Review exercises of the boundary after the fact: which compulsion survives scrutiny, which exemptions must be honored, when emergency powers overreach. Case-by-case decisions feed back into how agencies draft the next round of rules.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of infectious-disease control: reaching and holding herd-immunity thresholds and isolation compliance that voluntary participation cannot sustain, while rationing coercive capacity so it is spent only where transmission dynamics (transmissibility, severity, mortality) make voluntary provision fail.
% TRANSFER_FUNCTION: Moves decision authority over medical procedures from individuals to public-health authorities in proportion to measured threat; moves infection risk away from those who cannot be vaccinated (infants, the immunocompromised) onto mandated individuals; moves compliance costs onto objectors and, in workplace settings, onto employees.
% ABSENT_VOICES: Conscientious objectors and emergency-order subjects have no seat in threshold-setting: advisory committees are expert-dominated, legislatures act episodically, and affected individuals typically appear only as litigants after rules are fixed. Hospital employees encounter flu requirements as terms of employment, not as adjudicated policy.
% DISAPPEARANCE_RATIONALE: If the severity-scaled boundary vanished overnight, every jurisdiction would immediately face the question it answers: school-entry laws, employment requirements, and emergency powers would either collapse together, removing the only mechanism holding measles and similar outbreaks away from infants and the immunocompromised, or expand without limit, making any intervention compellable that an authority labels beneficial. School attendance law, hospital employment contracts, and emergency-power statutes all presuppose the boundary.
% FOUNDING_PROBLEM: The compulsory-smallpox era: cities suffered recurrent epidemics with heavy child mortality while voluntary vaccination plateaued below herd thresholds; Jacobson v. Massachusetts (1905) framed the standing question of when collective harm-prevention may override bodily integrity.
% FOUNDING_PROBLEM_CORROBORATION: Courts outside the benefiting parties continually re-litigate the boundary (the Jacobson lineage and its modern strict-scrutiny successors); bioethics scholarship from both autonomy-first and public-health-first camps attests the problem remains open; each novel pathogen, most recently SARS-CoV-2, forces fresh adjudication. No party to the dispute claims the founding problem is closed.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).
:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.45 at interval end) because most coercive activity targets pathogens the reading itself endorses as qualifying, counting there as coordination price rather than unjustified taking; the residual concentrates in flu-era workplace mandates, emergency suspensions, and exemption administration. Suppression (0.52) reflects real enforcement machinery - school exclusion, employment termination, fines, quarantine orders - bounded by exemption regimes and judicial review. Theater (0.26) captures partly performative exemption hearings and review boards plus mandates maintained symbolically where threat has receded. Accessibility collapse is moderate (0.55): exits exist (homeschooling, job change, jurisdiction shopping) but at real cost, unlike a natural limit where none exist. Resistance (0.6) is sustained litigation, repeal campaigns, and exemption movements. The measurement series run on ONE shared nine-point grid (every tracked metric authored at every point). The series are CYCLICAL, not monotonic: enforcement and extraction spike in epidemic crises (t=15 smallpox-campaign era, t=45 polio mobilization, t=120 COVID emergency), relax in calm interludes, and trough near t=90 when exemption regimes expanded. The oscillation is partly the extraction mechanism itself: crises suspend ordinary proportionality adjudication, producing intermittent over-coercion, followed by backlash-driven relaxation - intermittent reinforcement at institutional scale. The suppression_requirement series is authored deliberately because enforcement-capacity ratchet-and-relaxation is the dynamic this story traces; base_properties scalars reflect the interval endpoint (post-emergency retrenchment, still elevated above the t=90 trough). Suppression here is overwhelmingly structural (statutes, employment terms, orders) with a smaller internalized component (compliance-as-civic-duty norms); no interpersonal suppression-ambiguity omega is required, though the victim-status omega below carries the reading-indexed analogue.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical policy text. From the agenda-setter seat the arrangement is a calibrated instrument it built and can retune; from the trapped beneficiary seats (infants, immunocompromised) the same structure is a lifeline with no substitute; from the objector-parent seat an endorsed mandate is still a state-compelled procedure performed on one's child over objection - experienced as a rights injury even where the reading scores it as legitimate coordination price; from the mobile healthcare-worker seat the sub-threshold flu requirement is an annoyance with a real exit. Same nominal rule, four different lived constraints. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: trapped beneficiaries (immunocompromised patients, infants) sit nearest the full-beneficiary end - the constraint subsidizes them almost entirely. Public health authorities derive low-to-moderate d from their beneficiary secondary role, tempered by their exposure to judicial review. Payers derive high d: objector parents (organized, constrained exit) sit near the full-target end; healthcare workers' mobile exit moderates their d below the trapped payers'; emergency-order subjects combine victim status with trapped exit for the highest d in the story. Constitutional courts are analytical and direction-neutral. Scope is national for most seats, regional for the biologically local beneficiaries - verification of community-level coverage is easier than national-policy verification, a modest damping the engine applies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is LIVE - new pathogens recur and each forces fresh adjudication - so this is not an atrophied remnant kept alive by inertia; the mandatrophy_resolved flag stays unset and the R5 mismatch consumer finds status=live x verdict=world_rearranges, no zombie flag. Classification guards against two mislabels: calling the whole arrangement pure extraction ignores that its dominant activity (measles-class mandates) solves a real collective-action problem the reading endorses; calling it pure coordination ignores the identifiable payers (objectors, flu-mandated workers, emergency subjects) whose costs persist through the same structures. Hence tangled_rope with moderate epsilon. The theatrical elements (exemption hearings, symbolic review boards) are symptoms, not the test - the test is the cost asymmetry, and here the administrators demonstrably CAN retune thresholds (they do, pathogen by pathogen), which distinguishes this from an inertial piton. If adjudication were perfectly calibrated, epsilon would fall toward the coordination floor and the type would approach rope; the observable distance between 0.45 and that floor is exactly the leak the flu-mandate and emergency-bypass omegas name.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the proportionality_reading of kernel coercion_legitimacy_boundary; how would the classification move under the sibling readings?',
    'Generate the sibling stories (bodily_autonomy_primary, public_health_primary) against the same referent and compare victim sets, epsilon, and computed types.',
    'Under bodily_autonomy_primary every non-consensual intervention enters the victim column and epsilon approaches snare levels; under public_health_primary severity is one balancing input among many and the victim set loses its pathogen indexing. The disagreement is located in the victim-set rule, not in the coordination function, which all three readings accept.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a three-reading kernel; sibling readings would restructure the victim set.').

omega_variable(
    threshold_parameter_disagreement,
    'Which pathogen parameters (basic reproduction number, case fatality, age-stratified mortality, availability of a safe effective vaccine) enter the legitimacy threshold, and where do the cutoffs sit?',
    'Systematic comparison of mandate outcomes against pathogen parameter distributions; structured elicitation from public-health ethics panels independent of the administering agencies.',
    'Moving cutoffs redraws the victim set pathogen by pathogen - precisely the structural delta the kernel contest turns on; a stricter severity weighting shrinks the mandate set and lowers epsilon, a broader transmission weighting expands both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_parameter_disagreement, empirical, 'Where the severity/transmission cutoffs sit and which parameters count.').

omega_variable(
    emergency_bypass_membership,
    'Do emergency-order suspensions of ordinary adjudication belong to the standing arrangement this story measures, or are they a separate constraint?',
    'Trace whether emergency powers ride the same statutory authority as routine mandates and whether practice lapses back into proportionality adjudication when declarations end.',
    'If included, measured epsilon spikes during crises (visible as the extractiveness-series peaks at t=15, t=45, t=120) and the arrangement is less calibrated than its steady state suggests; if excluded, the standing arrangement is better calibrated than the series indicates and the spikes belong to a distinct emergency-powers story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_bypass_membership, conceptual, 'Whether the emergency bypass is inside or outside the measured arrangement.').

omega_variable(
    flu_mandate_anomaly,
    'Why do healthcare-worker influenza mandates persist when the reading''s own criterion assigns influenza below the coercion threshold?',
    'Institutional analysis of hospital liability exposure, absenteeism economics, and accreditation incentives; compare jurisdictions and systems that dropped the requirements against those that retained them.',
    'If inertia- and liability-driven, they are the extractive residue dragging epsilon above the reading''s calibrated floor and signal drift toward unconditional balancing; if defensible on institution-specific grounds, part of their cost is coordination price and epsilon is closer to calibrated than the anomaly suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flu_mandate_anomaly, empirical, 'Persistence of sub-threshold workplace mandates as the main epsilon leak.').

omega_variable(
    victim_status_indexicality,
    'Are measles-mandate objectors bearing a legitimate coordination price (this reading) or suffering rights violations (the autonomy reading)? Epsilon is indexed to the reading, so the same conduct yields different extraction under different commitment frameworks.',
    'Not resolvable by data alone; resolves by which commitment framework governs - track doctrinal movement in courts and legislatures toward or away from categorical autonomy protections.',
    'A doctrinal shift toward categorical autonomy would move the objector seat from coordination-price-bearer to victim and raise epsilon sharply; consolidation of the proportionality frame would confirm the current moderate value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_status_indexicality, preference, 'Reading-indexed victim status of mandate objectors; the epsilon value travels with the framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(coer_tr_t45, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement(coer_tr_t60, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(coer_tr_t75, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 75, 0.28).
narrative_ontology:measurement(coer_tr_t90, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 90, 0.33).
narrative_ontology:measurement(coer_tr_t105, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 105, 0.3).
narrative_ontology:measurement(coer_tr_t120, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 120, 0.26).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(coer_be_t45, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 45, 0.46).
narrative_ontology:measurement(coer_be_t60, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(coer_be_t75, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 75, 0.33).
narrative_ontology:measurement(coer_be_t90, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 90, 0.31).
narrative_ontology:measurement(coer_be_t105, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 105, 0.37).
narrative_ontology:measurement(coer_be_t120, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 120, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(coer_su_t45, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement(coer_su_t60, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(coer_su_t75, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement(coer_su_t90, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 90, 0.38).
narrative_ontology:measurement(coer_su_t105, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 105, 0.44).
narrative_ontology:measurement(coer_su_t120, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 120, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, public_health_primary).

% DUAL FORMULATION NOTE:
% The colloquial label 'vaccine mandate legitimacy' conflates three structurally distinct constraints sharing one kernel (coercion_legitimacy_boundary). Each reading partitions the same referent with a different victim-set rule and therefore a different epsilon: this proportionality reading indexes victims to pathogen parameters (moderate epsilon, case-by-case leakage); bodily_autonomy_primary makes every non-consensual intervention a victim event (high epsilon); public_health_primary dissolves the victim set into balancing outcomes (low-to-moderate, indeterminate). Linked as a constraint family; citation flow runs from court doctrine (upstream) into agency practice and workplace policy (downstream).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
