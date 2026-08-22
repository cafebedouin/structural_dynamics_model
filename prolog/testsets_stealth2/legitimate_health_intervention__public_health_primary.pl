% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Population-Outcome Legitimacy Criterion for Coercive Health Intervention (Public-Health-Primary Reading)
 *   domain: public health policy/medical ethics/constitutional law
 *
 * SUMMARY:
 *   A population-level outcome criterion for the legitimacy of coercive
 *   health intervention: an intervention is legitimate when it measurably
 *   reduces population morbidity and mortality, and individual refusal is
 *   framed as externality imposition rather than as a protected exercise of
 *   bodily sovereignty. The standing arrangement under contest — and the
 *   epsilon referent for this story — is the mandate-and-enforcement regime
 *   this reading endorses: coverage mandates attached to employment and
 *   public access, enforced through termination and exclusion, with
 *   legitimacy continuously re-earned through surveillance measurement. This
 *   story is one reading of the kernel legitimate_health_intervention; the
 *   sibling readings (bodily_autonomy_primary, proportionality_reading) are
 *   separate constraints with their own epsilon, victim sets, and
 *   classifications, linked through the network. Claim and metrics are
 *   independent authored facts: the claimed type is tangled_rope because the
 *   free-rider structure of immunization is a genuine collective-action
 *   problem AND enforcement extracts concentrated costs from a trapped
 *   minority through actively maintained machinery; the metrics describe the
 *   regime's observed operation without being tuned to any predicted verdict.
 *
 * KEY AGENTS:
 *   - public_health_agencies: Agenda setter (institutional/constrained) — sets coverage targets, issues mandates, owns the surveillance apparatus that measures the outcomes that legitimate enforcement
 *   - unvaccinated_essential_workers: Primary target (powerless/trapped) — bears concentrated enforcement: termination, credential loss, insurance loss
 *   - unvaccinated_general_public: Secondary target (moderate/constrained) — bears diffuse access restrictions across venues, travel, and services
 *   - immunocompromised_patients: Primary protected beneficiary (powerless/trapped) — depends entirely on population coverage for protection
 *   - elderly_high_risk_populations: Protected beneficiary (moderate/constrained) — highest mortality exposure, politically organized, individually unable to exit the risk environment
 *   - vaccinated_compliant_majority: Coordination participant (moderate/constrained) — receives protection while bearing compliance costs and enforcement-manufactured consent
 *   - healthcare_institutions: Delegated enforcer and beneficiary (institutional/constrained) — administers mandates at the employment level, collects liability protection, bore real staffing costs
 *   - vaccine_manufacturers: Commercial beneficiary (institutional/arbitrage) — guaranteed demand behind a liability shield, globally reallocable production
 *   - anti_mandate_advocacy_networks: Organized coalition vehicle for the target population (organized/constrained) — litigation, legislation, protest
 *   - constitutional_courts: Analytical observer (institutional/analytical) — adjudicates the enforcement ceiling where the sibling readings currently compete
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.66).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.62).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.66).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Population-Outcome Legitimacy Criterion for Coercive Health Intervention (Public-Health-Primary Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public health policy/medical ethics/constitutional law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, elderly_high_risk_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, vaccinated_compliant_majority).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, healthcare_institutions).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, vaccine_manufacturers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_essential_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_general_public).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, anti_mandate_advocacy_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, vaccinated_compliant_majority).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, herd_immunity_threshold_theory).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, police_power_doctrine_jacobson).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set population coverage targets, issue vaccination mandates, and operate the disease surveillance systems that measure morbidity and mortality outcomes. Enforcement is delegated to employers and institutions. Their budget, statutory authority, and professional standing all ride on demonstrating measurable outcome reduction; abandoning the outcome-based legitimacy criterion would dissolve the basis of their mandate power, so exit from the arrangement is not a live option even when enforcement becomes politically costly.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Administer mandates at the employment level: condition hiring and continued employment on vaccination status, process exemption requests, and carry out terminations. They receive reduced workforce transmission risk, liability protection, and accreditation alignment. They also bore real costs during enforcement waves — staff losses, replacement hiring, litigation exposure — and are locked in by accreditation bodies, insurers, and public expectations once mandates attach to their employment structures.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, healthcare_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, healthcare_institutions, agenda_setter).

% Sell into a demand base that mandates guarantee: school-entry requirements, employment conditions, and access rules convert vaccination from a consumer choice into a compliance purchase. Liability for adverse events is channeled into no-fault compensation regimes in key jurisdictions rather than tort. Production is globally reallocable, so demand shocks in one market can be offset elsewhere.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, vaccine_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Cannot mount a protective response to vaccination or are medically contraindicated. Their protection comes entirely from the vaccination status of the people around them. They have no exit from exposure — infection risk is set by population coverage, not by their own choices — so they bear disease risk directly whenever coverage slips below threshold.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).

% Face the highest mortality from vaccine-preventable disease and gain the most from transmission reduction. They are a politically organized constituency that supports mandates, but individually they cannot exit the transmission environment; their protection, like the immunocompromised's, depends on other people's vaccination status.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, elderly_high_risk_populations, beneficiary,
    moderate, biographical, constrained, national).

% Comply with mandates and receive reduced transmission risk as the coordination good. They bear compliance costs: appointment and time burdens, adverse-event risk they individually absorb, documentation and proof-of-status requirements, and the tax share of publicly purchased doses. Part of their consent is manufactured by the enforcement threat — compliance under a termination or access penalty is not the same act as voluntary uptake, though it is recorded identically in coverage statistics.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, vaccinated_compliant_majority, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, vaccinated_compliant_majority, payer).

% Work in sectors where mandates attach to employment: healthcare, emergency services, transit, military. Refusal costs them their job, and with it frequently health insurance and professional credentials. Exit means abandoning a career field or complying against their conviction; relocation to non-mandate jurisdictions means leaving their profession's employment structure entirely. The costs land on them concentrated, immediate, and identity-laden.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_essential_workers, payer,
    powerless, biographical, trapped, national).

% Face access restrictions rather than termination: entry rules for venues, travel, and some services. The enforcement is softer and the substitution options broader than for essential workers, but the costs are real and diffuse through daily life — forgone travel, excluded venues, repeated testing burdens where testing substitutes for vaccination.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_general_public, payer,
    moderate, biographical, constrained, national).

% Organize the dispersed targets into litigation, legislation, and protest. They fund challenges to mandate authority, lobby for exemption expansion and state preemption laws, and absorb their members' job losses as organizing costs. Their constitutional claim is aimed at the legitimacy criterion itself, which is why the current framework cannot grant it without ceasing to be what it is.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, anti_mandate_advocacy_networks, payer,
    organized, biographical, constrained, national).

% Adjudicate the boundary between state police power and bodily integrity. Under the Jacobson lineage they deferred heavily to public health agencies' empirical judgments; a newer line of cases applies stricter review to some measures, and several state legislatures have preempted mandates entirely. Their rulings set the ceiling on enforcement and are the seat where the sibling readings currently compete.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__public_health_primary, public_health_agencies).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in population immunity: protection from communicable disease depends on aggregate coverage, each individual's incentive is to free-ride on others' vaccination, and voluntary uptake stalls below the threshold for the most transmissible pathogens. The mandate aligns individual incentive with the coverage threshold; surveillance measurement verifies that the threshold is held and that outcomes track it.
% TRANSFER_FUNCTION: Moves protection-relevant burden from the generally willing toward the unwilling by attaching employment, access, and standing to vaccination status; moves adverse-event risk onto the vaccinated individuals who absorb it individually; moves revenue to manufacturers in proportion to mandated coverage; and moves authority and budget justification to public health agencies in proportion to demonstrated enforcement and measured outcome reduction.
% ABSENT_VOICES: People whose adverse reactions are processed through no-fault compensation tables rather than weighed in the legitimacy criterion — their injuries are real but the outcome metric has no slot for them. Unvaccinated workers who comply under termination threat: their compliance is recorded as uptake, so their actual position never enters the record as either consent or refusal. Jurisdictions and populations outside the surveillance frame, where morbidity data is too poor to register in the outcome criterion that legitimates coercion. The anti-mandate litigants are present in court, but the claim they bring — that the outcome criterion itself is illegitimate — is one the current framework cannot grant without dissolving into a sibling reading.
% DISAPPEARANCE_RATIONALE: Coverage would drift down as free-riding resumed, losing herd-immunity thresholds for the most transmissible pathogens first; measles-type resurgences would follow within disease-generation timescales; immunocompromised and elderly mortality would rise directly; employers would lose the liability shield and agencies their primary enforcement instrument; and the legitimacy question would pass from contest to operation under whichever sibling reading the surviving institutions adopted.
% FOUNDING_PROBLEM: Communicable disease control before effective mandates: voluntary uptake insufficient to reach herd-immunity thresholds for highly transmissible, high-mortality pathogens, with epidemics recurring and the unprotected bearing catastrophic risk. The Jacobson-era formulation: may the state compel inoculation when individual refusal undermines population protection?
% FOUNDING_PROBLEM_CORROBORATION: Court records attest from outside the benefiting parties: Jacobson v. Massachusetts and its progeny document the founding problem, and post-2021 litigation contests the current scope of the same problem. Independent epidemiological surveillance — seroprevalence studies and outbreak reconstructions of post-coverage-decline resurgences — attests the coverage-outcome relationship without reference to agency authority. Pre-mandate-era mortality records corroborate the historical threat. What no outside source attests is that the current enforcement extension — termination and access exclusion for adult refusal against lower-severity threats — remains proportionate to the founding problem; on that question only the benefiting parties and their delegated enforcers attest.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: enforcement extracts concentrated, severe, hard-to-reverse costs — termination, credential loss, access exclusion — from a defined minority whose exit is structurally closed, even though this reading classifies those costs as justified correction. Suppression 0.62: persistence depends on actively maintained machinery — employment conditionality, verification systems, narrowing exemption regimes — rather than on voluntary preference; jurisdictional repeal and preemption cap the score below constraints with no exit at the system level. Theater 0.30: surveillance measurement is real and load-bearing, but post-crisis coverage metrics increasingly function as legitimacy maintenance in low-incidence periods. Accessibility collapse 0.55: the alternative (refuse and keep job and access) is largely collapsed inside mandate jurisdictions but persists across jurisdictions, through narrowing exemption channels, and via testing substitution. Resistance 0.60: mass litigation, organized preemption campaigns, resignation waves, and protest — real, organized, and partially successful. All three series share one time grid (t0-t24 at intervals of 4). The shape is gradual pre-crisis drift, a crisis spike at t20, and partial rollback settling above pre-crisis levels because enforcement infrastructure persists in healthcare employment and institutional policy after the repeal wave. The theater dip at t20 is structural, not noise: acute threat maximizes the functional share of measurement; the post-crisis rise is the mandatrophy signal to watch.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is the paradigm case of legitimate coordination: outcomes justify means, and refusal is a cost imposed on others. From the essential-worker seat the same structure is an uncompensated taking of livelihood for a protection they dispute needing, with exit priced at career abandonment. From the immunocompromised seat it is survival infrastructure. The engine computes these per-seat classifications from the structural data; the divergence between them is the measurement. Note also this reading's characteristic blind spot, visible in its own lights: costs imposed ON refusers are re-described as costs OF refusal — the enforcement burden is laundered into the refused party's own choice — which is exactly the move that keeps epsilon lower from this seat than it would be from the bodily_autonomy_primary sibling's seat assessing the identical regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the low-d end: public health agencies collect authority, budget, and legitimacy in proportion to enforcement (agenda-setter and beneficiary simultaneously); manufacturers collect guaranteed demand behind a liability shield with arbitrage-grade exit, placing them nearest the beneficiary bound; immunocompromised patients and the elderly receive protection they cannot self-provide (trapped beneficiaries — low d, high dependence); the compliant majority is genuinely dual-positioned, receiving protection while bearing compliance costs and consent manufactured by the enforcement threat, placing it near-symmetric and slightly beneficiary-side. Targets sit at the high-d end: essential workers (trapped, powerless) nearest the full-target bound; the general public (constrained) high but below them; the advocacy networks high-d as the organized vehicle of the same target population. Healthcare institutions are the one seat the role labels understate: role-beneficiary, but they bore real termination and staffing costs during enforcement waves — part-enforcer, part-bearer, which the derivation reads as more beneficiary-side than their full position. No directionality overrides are authored: the override mechanism is keyed to power atoms, and this story's three institutional seats (agencies, manufacturers, institutions) genuinely differ in directionality, so an atom-level correction would misfire across seats the derivation already distinguishes by role and exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — voluntary uptake stalling below herd-immunity thresholds for high-mortality pathogens — is live for the classic diseases and contested for the recent enforcement extension. The classification prevents two opposite errors. Reading the whole arrangement as pure extraction erases a real collective-action problem and the trapped beneficiaries whose survival depends on it (the free-rider structure is genuine, which blocks a snare verdict). Reading it as pure coordination erases concentrated, severe, actively enforced costs on a trapped minority (which blocks a rope verdict). Tangled rope holds both halves and keeps the enforcement declaration attached. The drift risk is mandatrophy by extension: mandates persisting into low-severity, low-incidence periods on coverage metrics alone — visible as the post-crisis theater rise in the measurement series. If theater keeps climbing while incidence stays low, the constraint is piton-drifting: enforcement maintained performatively after its function receded. founding_problem_status is authored contested rather than dead because the classic-disease problem is demonstrably live; the mismatch watch should key on the extension, not the founding core. Coalition check: the target population's coalition vehicle is organized and partially effective — litigation wins and state preemption — which is what keeps suppression at 0.62 rather than higher.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates only the public_health_primary reading of the legitimate_health_intervention kernel; what structurally changes if a sibling reading (bodily_autonomy_primary or proportionality_reading) is adopted instead?',
    'Compare against the sibling constraint stories (legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading): under bodily_autonomy_primary every non-consensually intervened-upon person enters the victim set and extraction is assessed near the full-coercion bound; under proportionality_reading the victim set and epsilon become threat-indexed, varying with disease severity rather than with refusal status.',
    'The victim set, epsilon, and classification are properties of THIS reading only; adopting a sibling re-partitions who counts as harmed (refusers-as-cost-imposers versus all-coerced-as-rights-violated) and can move the computed type from tangled_rope toward snare (autonomy sibling) or toward threat-contingent rope or scaffold (proportionality sibling).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three readings of the legitimate_health_intervention kernel; siblings change the victim set and epsilon over the same referent.').

omega_variable(
    refusal_status_disagreement_location,
    'Where exactly do the readings disagree: is individual refusal an externality imposition on others (a correctable cost) or a bodily-integrity exercise (an inviolable right), and can any single framework hold both?',
    'Not resolvable by additional data: the empirical substrate (transmission creates genuine interdependence) is shared by all three readings; the dispute is over the normative status of refusal. Resolution requires a framework-level choice among the sibling readings, not new evidence.',
    'If refusal is an externality, enforcement is correction and the coordination half of the reading holds; if refusal is a right, the enforcement burden is a rights violation and the constraint computes snare-side from every coerced seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(refusal_status_disagreement_location, preference, 'The kernel contest is located at the normative status of individual refusal, not at the empirical facts.').

omega_variable(
    mandate_outcome_attribution,
    'How much of the observed population-level morbidity/mortality reduction is causally attributable to mandates and their enforcement, as opposed to voluntary uptake, prior infection, therapeutics, and pathogen seasonality?',
    'Natural-experiment comparison across jurisdictions with and without mandates matched for baseline coverage and demographics; discontinuity analyses at mandate effective dates; seroprevalence-based reconstruction of transmission chains.',
    'This reading''s legitimacy criterion stands or falls on the attribution: large mandate-specific effects support the coordination reading; small effects leave enforcement without its legitimating function and push the constraint toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_outcome_attribution, empirical, 'Causal attribution of outcome reduction to mandates versus confounders.').

omega_variable(
    post_crisis_persistence_path,
    'Does the enforcement apparatus sunset as threat levels recede, or does it persist on institutional inertia with coverage metrics maintained as legitimacy ritual?',
    'Track mandate repeal versus maintenance in low-incidence periods; test whether theater_ratio continues rising after threat recedes; compare enforcement intensity against contemporaneous disease burden.',
    'Sunset behavior supports a transitional reading; persistence with rising theater indicates piton drift — enforcement maintained performatively after its function receded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_crisis_persistence_path, empirical, 'Whether the constraint transitions down as threat recedes or persists by inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(legi_tr_t4, legitimate_health_intervention__public_health_primary, theater_ratio, 4, 0.2).
narrative_ontology:measurement(legi_tr_t8, legitimate_health_intervention__public_health_primary, theater_ratio, 8, 0.22).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.24).
narrative_ontology:measurement(legi_tr_t16, legitimate_health_intervention__public_health_primary, theater_ratio, 16, 0.27).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__public_health_primary, theater_ratio, 20, 0.22).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(legi_be_t4, legitimate_health_intervention__public_health_primary, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__public_health_primary, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(legi_be_t16, legitimate_health_intervention__public_health_primary, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__public_health_primary, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(legi_su_t4, legitimate_health_intervention__public_health_primary, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__public_health_primary, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(legi_su_t16, legitimate_health_intervention__public_health_primary, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__public_health_primary, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial concept 'legitimate health intervention' decomposes into three structurally distinct constraints — one per reading of the kernel. They differ in victim set (refusers-as-vectors versus all-coerced versus threat-indexed), in epsilon over the shared referent, and in classification; they share the same empirical substrate (transmission interdependence is real under all three). This story is the public_health_primary member. The upstream empirical claim it depends on — mandate-attributable outcome reduction — is carried in the mandate_outcome_attribution omega and is the edge along which contamination propagates from measurement failures in the surveillance apparatus to the legitimacy of the enforcement it justifies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
