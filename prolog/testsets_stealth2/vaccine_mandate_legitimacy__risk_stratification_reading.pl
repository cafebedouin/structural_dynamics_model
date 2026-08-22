% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Risk-Stratified Vaccine Mandate Legitimacy (Proportionality-Gated Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the vaccine_mandate_legitimacy
 *   kernel: the risk_stratification_reading, under which mandate authority is
 *   legitimate only when gated by demonstrated actuarial risk differentials —
 *   blanket mandates fail proportionality, targeted mandates are permissible.
 *   The epsilon referent is the STANDING arrangement under contest: the
 *   blanket mandate regime as actually operated (2019-2024), assessed by this
 *   reading's own lights. Under those lights the regime carries a genuine
 *   coordination function (transmission reduction protecting the
 *   immuno-vulnerable) AND asymmetric extraction (low-risk individuals bore
 *   costs disproportionate to any benefit accruing to them), enforced
 *   actively through employment termination, venue exclusion, and credential
 *   revocation. The claim/metric gap is deliberate: claimed_type is authored
 *   from structure (both coordination and extraction present, active
 *   enforcement required), metrics from descriptive operation — the engine
 *   computes per-seat classifications and measures any divergence. Sibling
 *   readings (public_health_primacy_reading, bodily_autonomy_primacy_reading)
 *   are separate constraint files with their own epsilon, victim sets, and
 *   classifications; they are neither described inside this constraint nor
 *   averaged into it.
 *
 * KEY AGENTS:
 *   - public_health_bureaucracies: agenda-setter (institutional/identity_locked) — writes and enforces the rules, collects authority and appropriations
 *   - hospital_system_operators: dual-positioned beneficiary/payer (institutional/constrained) — receives surge relief, absorbed staffing losses
 *   - immunocompromised_high_risk_patients: primary beneficiary (powerless/trapped) — protected via reduced transmission, no exit possible
 *   - elderly_care_facility_residents: primary beneficiary (powerless/trapped) — highest per-exposure mortality, congregate setting
 *   - low_risk_working_age_adults: primary target (organized/constrained) — bears blanket-rule costs disproportionate to personal risk
 *   - natural_immunity_recovered_workers: target (moderate/constrained) — denied risk-class differentiation the reading's framework would supply
 *   - terminated_vaccine_refusers: concentrated-cost target (moderate/trapped) — consequence already borne, persists after rescission
 *   - actuarial_epidemiology_analysts: excluded expert seat (moderate/mobile) — held the threshold data, absent from policy design
 *   - constitutional_litigation_courts: analytical observer (institutional/analytical) — performs proportionality review retrospectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.55).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.55).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Risk-Stratified Vaccine Mandate Legitimacy (Proportionality-Gated Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, 'f8946395-66bd-4203-8675-0b76dcd30ed0').
narrative_ontology:cs_kernel_codification('f8946395-66bd-4203-8675-0b76dcd30ed0', formalized).
narrative_ontology:cs_authority_grounding('f8946395-66bd-4203-8675-0b76dcd30ed0', lineage).
narrative_ontology:cs_interpretation_layer_present('f8946395-66bd-4203-8675-0b76dcd30ed0').
narrative_ontology:cs_reading_relation('f8946395-66bd-4203-8675-0b76dcd30ed0', vaccine_mandate_legitimacy__public_health_primacy_reading, influences).
narrative_ontology:cs_reading_relation('f8946395-66bd-4203-8675-0b76dcd30ed0', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_axiom('f8946395-66bd-4203-8675-0b76dcd30ed0', foundational, coercion_requires_proportional_actuarial_justification).
narrative_ontology:cs_axiom_status(coercion_requires_proportional_actuarial_justification, holdable).
narrative_ontology:cs_axiom_grounding('f8946395-66bd-4203-8675-0b76dcd30ed0', coercion_requires_proportional_actuarial_justification, instrumental).
narrative_ontology:cs_axiom('f8946395-66bd-4203-8675-0b76dcd30ed0', foundational, blanket_application_fails_proportionality).
narrative_ontology:cs_axiom_status(blanket_application_fails_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('f8946395-66bd-4203-8675-0b76dcd30ed0', blanket_application_fails_proportionality, deontological).
narrative_ontology:cs_reference_frame('f8946395-66bd-4203-8675-0b76dcd30ed0', proportionality_gated_police_power).
narrative_ontology:cs_drift_state('f8946395-66bd-4203-8675-0b76dcd30ed0', contemporary_post_emergency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f8946395-66bd-4203-8675-0b76dcd30ed0', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_high_risk_patients).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, elderly_care_facility_residents).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, hospital_system_operators).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_bureaucracies).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_working_age_adults).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, natural_immunity_recovered_workers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, terminated_vaccine_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, hospital_system_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers vaccination requirements through emergency declarations, agency rulemaking, and guidance to employers and care facilities. Collects compliance data, enforcement authority, and appropriations tied to pandemic response. The organization's warrant and self-concept are fused with the mission of compelling protective behavior; abandoning the mandate instrument entirely would require repudiating the professional identity built around it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_bureaucracies, agenda_setter,
    institutional, generational, identity_locked, national).

% Lobbied for staff vaccination requirements to protect bed capacity, limit liability, and stabilize scheduling. Receives reduced surge risk and workforce predictability. Also absorbed real costs: experienced clinical staff resigned or were terminated rather than comply, and replacement hiring in tight labor markets was expensive. Cannot exit the regulatory environment they operate in.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, hospital_system_operators, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, hospital_system_operators, payer).

% Cannot achieve full protection from vaccination itself and depend on reduced transmission in the people around them. Benefit from any measure that lowers community viral load. Have no exit: their immune status travels with them, and isolation is its own harm.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_high_risk_patients, beneficiary,
    powerless, immediate, trapped, national).

% Live in congregate settings with the highest mortality risk per exposure. Benefit directly from staff vaccination requirements in their facilities. Cannot meaningfully relocate, and their families' visitation access is mediated by the same facility policies.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, elderly_care_facility_residents, beneficiary,
    powerless, immediate, trapped, local).

% Faced blanket requirements where their statistical risk of severe outcome was small. Bore compliance costs, recurring testing costs, documentation burdens, and termination risk. Individual exit meant changing industries, relocating, or refusing and accepting consequences; collective exit ran through litigation and electoral politics, which eventually narrowed the rules.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_working_age_adults, payer,
    organized, biographical, constrained, national).

% Documented prior infection with laboratory-confirmed antibodies and sought treatment as a distinct risk class. Blanket rules counted them identically to never-exposed individuals. Their requested accommodation — risk-stratified credit for recovery — is precisely the mechanism this reading's framework would supply, and it was refused during the blanket period.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, natural_immunity_recovered_workers, payer,
    moderate, biographical, constrained, national).

% Lost employment, military careers, or clinical privileges rather than comply. The consequence has already been borne: income loss, credential gaps, and career interruption persist after the rules themselves were rescinded. Re-entry into their former positions is largely unavailable.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, terminated_vaccine_refusers, payer,
    moderate, biographical, trapped, national).

% Held and published the age- and comorbidity-stratified risk curves that could have defined explicit thresholds. Were not seated in the policy processes that wrote threshold-free blanket rules. Would have objected that uniform application ignored a risk gradient spanning orders of magnitude; their analyses circulated in journals rather than in the drafting room.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, actuarial_epidemiology_analysts, excluded,
    moderate, biographical, mobile, continental).

% Adjudicate mandate legitimacy after implementation. Struck down some federal applications (the vaccinate-or-test rule for large employers) while upholding others (state healthcare worker mandates under the Jacobson lineage). Their docket determines which applications survive and effectively performs the proportionality review this reading's framework calls for — retrospectively.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, constitutional_litigation_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_bureaucracies).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: respiratory pathogens spread asymptomatically, individual vaccination decisions free-ride on others' uptake, and uncontrolled transmission kills third parties who cannot protect themselves. Requirements raise coverage above voluntary equilibrium, most consequentially in congregate and critical-care settings.
% TRANSFER_FUNCTION: Moves compliance costs — vaccination, recurring testing, documentation, termination risk — onto the mandated population, disproportionately the low-actuarial-risk working-age cohort under blanket application; moves reduced-transmission environments to high-risk members and care facilities; moves staffing predictability to hospital operators; moves enforcement authority and appropriations to public-health agencies.
% ABSENT_VOICES: Actuarial and epidemiological analysts holding the stratification data were structurally absent from blanket policy design — the exact expertise a proportionality gate requires never sat in the room where threshold-free rules were drafted. Low-risk young adults were represented chiefly through litigation filed after rules took effect. Courts entered only post-implementation, reviewing finished rules rather than shaping them.
% DISAPPEARANCE_RATIONALE: Overnight removal would reorganize workplace rules, school and university attendance conditions, healthcare staffing rosters, and long-term-care admission policies within weeks. High-risk settings would re-derive targeted protections on their own authority; agencies would lose an enforcement posture and the appropriations attached to it; the litigation docket that currently polices proportionality would empty.
% FOUNDING_PROBLEM: The acute pandemic phase: hospitals approaching capacity, elderly and immunocompromised people dying at extreme rates before population immunity existed, and voluntary uptake insufficient to bend transmission curves in congregate and critical settings.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: independent excess-mortality reconstructions, ICU capacity reporting from hospital trade associations, and contemporaneous obituary records corroborate that the founding problem was real and severe. Its present status is disputed from outside the beneficiary set as well: independent bioethicists split on whether residual risk in care settings keeps a targeted version live, while state legislatures and civil-liberties organizations attest the blanket version's problem is dead. No outside attester settles the question — hence contested.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (0.55 at interval end) is substantial but not maximal: the blanket component extracted heavily from the low-risk stratum while the targeted component delivered real protection to high-risk seats — the reading's own framework prices these differently, and the scalar blends them weighted toward the blanket failure mode that dominated practice. Suppression (0.55) is a raw structural property, unscaled by power or scope: termination, exclusion, and credential revocation were the operative mechanisms, softened by testing opt-outs and eventual rescission. Theater (0.48) rose steadily as mandates persisted past the emergency declarations and past the evidentiary shift in transmission-blocking efficacy — retention-of-policy replacing function. Resistance (0.65) was unusually high for a public-health measure: mass protest, state-level preemption bans, and successful coalition litigation (the large-employer rule was struck down), which is itself evidence against pure-snare dynamics — a pure snare suppresses coalition formation, whereas here coalition power visibly bent the constraint. Accessibility collapse (0.5) is moderate: testing-based alternatives, exemptions, remote work, and relocation existed but carried real costs. The measurement series run on ONE shared time grid (every tracked metric authored at every year 2019-2024) showing a coherent enforcement arc: build-up through 2022, peak, then partial decay as rescissions and litigation narrowed application — the suppression_requirement series is authored precisely because enforcement capacity changed dramatically over the interval, ratcheting up and then relaxing.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical rules. From the high-risk beneficiary seats the arrangement is life-saving coordination they cannot purchase individually. From the low-risk payer seats the same rules operate as disproportionate coercion — ordered to bear costs for a benefit stream they were statistically positioned not to need. From the agenda-setter seat the rules were a necessary tool whose abandonment would signal institutional failure. From the judicial seat the question is a proportionality test to be applied case by case. The engine computes these divergent per-seat classifications from the structural data (power, exit, role); this story does not adjudicate among them — it declares the structure and lets the computation run.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the protected seats: facility residents and immunocompromised patients sit nearest the full-beneficiary end, amplified toward subsidy by their trapped exit (nothing about the constraint costs them access to alternatives they never had). Payer declarations drive high directionality for the low-risk stratum, amplified by constrained-to-trapped exit: terminated refusers sit nearest the full-target end because the cost is sunk and unrecoverable. Hospital operators occupy a genuinely dual position — declared beneficiary with secondary payer role — so their derived directionality lands mid-range rather than at the subsidy end; the derivation reads both declarations. Public-health bureaucracies derive low d from their beneficiary status, but their identity_locked exit marks them as institutionally fused with the constraint rather than merely subsidized by it. Scope note: national spatial scope modestly amplifies effective extraction on the payer seats (verification harder, exit costlier at scale); suppression receives no such scaling by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — acute surge threatening to overwhelm hospitals before population immunity — was real, corroborated from outside the benefiting parties, and has attenuated: hybrid immunity transformed the risk landscape, and the blanket component's actuarial warrant expired with it. Hence mandatrophy_resolved is declared TRUE for the blanket component, while the targeted component retains a live warrant in care settings — the founding_problem_status is therefore contested rather than dead. The classification prevents mislabeling in both directions: a pure-snare reading would erase the documented coordination benefit flowing to trapped high-risk seats; a pure-rope reading would erase the disproportionate burden the low-risk stratum demonstrably bore. Tangled rope holds both truths. The rising theater_ratio traces the mandatrophy signature directly: as the acute warrant expired, enforcement activity increasingly defended the policy itself rather than the function, and the constraint survived on institutional inertia plus the residual legitimate core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is mandate legitimacy best read as proportionality-gated (this reading), unconditional under state duty (public_health_primacy_reading), or categorically impermissible (bodily_autonomy_primacy_reading) — and which reading''s framework survives contact with litigation outcomes and stratified risk data?',
    'Track the judicial docket and legislative preemption pattern: sustained proportionality review with upheld targeted mandates confirms this reading''s framework; wholesale deference to agency authority shifts toward the primacy reading; categorical rejection of all applications shifts toward the autonomy reading.',
    'The victim set is reading-relative: the primacy reading shrinks it toward zero (refusers recast as externality sources), the autonomy reading expands it to every mandated person, this reading bounds it to the disproportionate-burden stratum. Classification of the whole kernel family turns on which framework prevails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: this story is one reading of the vaccine_mandate_legitimacy kernel; sibling readings instantiate different constraints.').

omega_variable(
    actuarial_threshold_indeterminacy,
    'Where does the risk threshold sit — age cutoffs, comorbidity weighting, setting-specific exposure adjustment — and who has authority to define it?',
    'Settled actuarial methodology combined with democratic legitimation of the chosen cutoff; comparative analysis of jurisdictions that adopted explicit thresholds versus threshold-free blanket rules.',
    'This is the expected structural delta made precise: a narrow threshold (only the genuinely fragile) yields a small victim set and the reading collapses toward the public-health-primacy position; a broad threshold (most of the working-age population below it) yields a large victim set and the reading collapses toward the bodily-autonomy position. The reading is stable only for intermediate thresholds — it may collapse into either extreme.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_threshold_indeterminacy, conceptual, 'Threshold definition determines victim-set size and whether this reading remains distinct from its siblings.').

omega_variable(
    sterilizing_efficacy_durability,
    'Did vaccination reduce transmission durably enough to ground other-regarding coercion of the vaccinated-and-compliant, or did the other-regarding rationale decay with variant escape?',
    'Longitudinal household-transmission studies across variant eras comparing vaccinated and unvaccinated secondary attack rates.',
    'If transmission-blocking decayed substantially, the post-2022 mandate activity loses its coordination justification for the compliant majority and the rising theater_ratio is confirmed as functionless retention; if durable, part of the late-period enforcement was genuine coordination mispriced as theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sterilizing_efficacy_durability, empirical, 'Whether the other-regarding rationale for mandates survived variant-driven efficacy decay.').

omega_variable(
    infection_derived_immunity_equivalence,
    'Does documented prior infection confer protection equivalent to vaccination for mandate purposes?',
    'Head-to-head cohort studies of infection-derived versus vaccine-derived protection against severe outcome and transmission, adjusted for variant and time-since-exposure.',
    'Equivalence would confirm natural_immunity_recovered_workers as a wrongly flattened risk class — a core victim of blanket application and a vindication of this reading''s stratification logic; non-equivalence would shrink that victim set and weaken the reading''s strongest empirical exhibit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infection_derived_immunity_equivalence, empirical, 'Whether recovered-status individuals constitute a distinct risk class the blanket rules unjustifiably erased.').

omega_variable(
    threshold_capture_risk,
    'Does threshold-setting authority become a jurisdiction-maximizing instrument — agencies defining risk categories to preserve enforcement reach rather than to track the risk gradient?',
    'Compare agency-defined categories against independent actuarial curves; audit whether category boundaries track risk discontinuities or administrative convenience.',
    'If capture occurs, the targeted-mandate endorsement degrades into snare mechanics wearing stratification language, and this reading''s coordination claim fails from within; if boundaries track the gradient, the reading''s framework is robust to administration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_capture_risk, preference, 'Whether proportionality gating itself can be captured by the enforcing institution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 2019, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t2019, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2019, 0.05).
narrative_ontology:measurement(vacc_tr_t2020, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(vacc_tr_t2021, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2021, 0.2).
narrative_ontology:measurement(vacc_tr_t2022, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2022, 0.31).
narrative_ontology:measurement(vacc_tr_t2023, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2023, 0.41).
narrative_ontology:measurement(vacc_tr_t2024, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(vacc_be_t2019, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2019, 0.05).
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2020, 0.25).
narrative_ontology:measurement(vacc_be_t2021, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2021, 0.52).
narrative_ontology:measurement(vacc_be_t2022, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2022, 0.66).
narrative_ontology:measurement(vacc_be_t2023, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2023, 0.6).
narrative_ontology:measurement(vacc_be_t2024, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t2019, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2019, 0.05).
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2020, 0.22).
narrative_ontology:measurement(vacc_su_t2021, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement(vacc_su_t2022, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2022, 0.78).
narrative_ontology:measurement(vacc_su_t2023, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement(vacc_su_t2024, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'vaccine mandate legitimacy' decomposes into three structurally distinct readings of one kernel, each with its own epsilon, victim set, and classification. The public_health_primacy_reading is upstream (its framework produced the blanket rules first); this risk_stratification_reading is downstream (it emerged as the litigation-era limiting principle, constraining what blanket authority can sustain); the bodily_autonomy_primacy_reading is the parallel opposition (logically incompatible with any permissive reading within a single framework). Each story links to the others via affects_constraints; contamination propagates across the family — a judicial ruling that entrenches proportionality review strengthens this reading while eroding the primacy reading's operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
