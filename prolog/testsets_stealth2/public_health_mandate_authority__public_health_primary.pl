% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public-Health-Primary Reading of Mandate Authority
 *   domain: public health law / constitutional rights / bioethics
 *
 * SUMMARY:
 *   This story instantiates the public_health_primary reading of the
 *   public_health_mandate_authority kernel: vaccination-and-coverage
 *   requirements understood as a standing obligation to protect the
 *   vulnerable commons — people who cannot be vaccinated and the hospital
 *   capacity everyone depends on — through collective action. The arrangement
 *   coordinates a genuine collective-action problem (coverage thresholds)
 *   while concentrating heavy costs on those who refuse: termination, unpaid
 *   leave, testing regimes, and service exclusion. Per the reading's
 *   signature move, the immunocompromised enter the victim set in the
 *   mandate's failure mode (protection promised, protection shortfall), while
 *   the unvaccinated are excluded from victim standing and framed as
 *   free-riders imposing an externality — a framing recorded here as an axiom
 *   and routed to omegas rather than treated as settled fact. Epsilon's
 *   referent is the standing mandate arrangement as this reading assesses it,
 *   never the rights-respecting arrangement the bodily_autonomy sibling would
 *   install. Sibling readings (bodily_autonomy_primary,
 *   proportionality_reading) are separate constraints in separate files; they
 *   are not averaged into this one.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda setter (institutional/constrained) — designs requirements, defines exemptions, delegates enforcement; authority and budget accrue to administering the regime
 *   - immunocompromised_patients: protected commons and failure-mode cost-bearers (powerless/trapped) — dual-positioned beneficiary/payer
 *   - hospital_systems: institutional beneficiary (institutional/constrained) — receives capacity relief and workforce retention
 *   - mandate_resistant_workers: primary cost-bearers (organized/identity_locked) — bear termination and exclusion; denied victim standing by this reading's framing
 *   - compliant_vaccinated_majority: mass beneficiaries (moderate/mobile) — small individual costs, collective protection received
 *   - bodily_autonomy_advocates: excluded voice (powerful/mobile) — rights claims pre-classified as externality denial inside this framing
 *   - federal_courts: analytical observer (institutional/analytical) — adjudicates the enforcement boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.58).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.6).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public-Health-Primary Reading of Mandate Authority").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public health law / constitutional rights / bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, 'f5c334f4-5986-49cf-aa29-a44ad7f988f7').
narrative_ontology:cs_kernel_codification('f5c334f4-5986-49cf-aa29-a44ad7f988f7', formalized).
narrative_ontology:cs_authority_grounding('f5c334f4-5986-49cf-aa29-a44ad7f988f7', expertise).
narrative_ontology:cs_interpretation_layer_present('f5c334f4-5986-49cf-aa29-a44ad7f988f7').
narrative_ontology:cs_reading_relation('f5c334f4-5986-49cf-aa29-a44ad7f988f7', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('f5c334f4-5986-49cf-aa29-a44ad7f988f7', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('f5c334f4-5986-49cf-aa29-a44ad7f988f7', foundational, vulnerable_commons_protection_obligation).
narrative_ontology:cs_axiom_status(vulnerable_commons_protection_obligation, holdable).
narrative_ontology:cs_axiom_grounding('f5c334f4-5986-49cf-aa29-a44ad7f988f7', vulnerable_commons_protection_obligation, deontological).
narrative_ontology:cs_axiom('f5c334f4-5986-49cf-aa29-a44ad7f988f7', foundational, refusal_imposes_uncompensated_externality).
narrative_ontology:cs_axiom_status(refusal_imposes_uncompensated_externality, holdable).
narrative_ontology:cs_axiom_grounding('f5c334f4-5986-49cf-aa29-a44ad7f988f7', refusal_imposes_uncompensated_externality, empirically_contingent).
narrative_ontology:cs_axiom('f5c334f4-5986-49cf-aa29-a44ad7f988f7', secondary, refuser_claims_classified_as_free_riding).
narrative_ontology:cs_axiom_status(refuser_claims_classified_as_free_riding, holdable).
narrative_ontology:cs_axiom_grounding('f5c334f4-5986-49cf-aa29-a44ad7f988f7', refuser_claims_classified_as_free_riding, conventional).
narrative_ontology:cs_reference_frame('f5c334f4-5986-49cf-aa29-a44ad7f988f7', commons_protection_standing_duty).
narrative_ontology:cs_drift_state('f5c334f4-5986-49cf-aa29-a44ad7f988f7', post_emergency_rescission_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f5c334f4-5986-49cf-aa29-a44ad7f988f7', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, hospital_systems).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, compliant_vaccinated_majority).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, immunocompromised_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the scope of vaccination requirements, define medical and religious exemption criteria, and delegate day-to-day enforcement to employers and service providers. Administering the regime brings expanded authority, staffing, and budget. Standing down the obligation framing entirely would concede the premise the agencies are constituted to uphold.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Cannot be vaccinated or mount adequate immune response, and so depend on the vaccination status of people around them. When community coverage holds, they receive protection they cannot purchase by any individual act. When coverage sags or variants evade immunity, they absorb infection exposure at mortal stakes; their only strategies are shielding and isolation, which carry their own severe costs.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, immunocompromised_patients, payer).

% Advocated for employee vaccination requirements to protect workforce availability and bed capacity. Receive reduced surge load and staff retention when coverage holds; administer employee compliance internally; absorb crisis-standards operations when waves overwhelm capacity regardless of policy. Cannot exit epidemiology.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, hospital_systems, beneficiary,
    institutional, generational, constrained, national).

% Decline required vaccination on religious, medical-skeptic, or liberty grounds and face termination, unpaid leave, recurring testing regimes, and exclusion from services and venues. Many bind refusal to political or religious identity such that compliance is experienced as betrayal rather than inconvenience. Organized through litigation networks, advocacy coalitions, and sympathetic legislatures. The governing public-health framing classifies their position as free-riding rather than counting them among the injured.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_workers, payer,
    organized, biographical, identity_locked, national).

% Accepted vaccination, frequently willingly, bearing small individual costs (appointments, transient side effects) in exchange for participation in collective protection. Low personal stakes; their compliance is the raw material of the coverage threshold. Mobility is high because the requirement asked little of them.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, compliant_vaccinated_majority, beneficiary,
    moderate, biographical, mobile, national).

% Litigate and legislate against mandate authority on bodily-sovereignty grounds. Inside the obligation framing's administrative conversation their claims arrive pre-classified as externality denial rather than as rights assertions, so they operate through courts, legislatures, and media outside the exemption-defining process.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, bodily_autonomy_advocates, excluded,
    powerful, biographical, mobile, national).

% Adjudicate challenges to mandate authority, including stays of broad employer rules and religious exemption claims. Weigh the state's interest in communal health against asserted individual liberties, and their rulings redraw the enforcement boundary the agencies administer.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__public_health_primary, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in immunization coverage: individual incentives to skip vaccination erode the coverage threshold that protects people who cannot be vaccinated and keeps hospital capacity functional. The requirement internalizes the transmission externality by making participation a condition of employment and service access.
% TRANSFER_FUNCTION: Moves the burden of immunity-building, with its attendant risks and inconveniences, onto holdout individuals via employment and access conditions; moves protection (reduced transmission exposure) to the immunocompromised, the medically fragile, and hospital systems; moves administrative authority and discretion to public health agencies and delegated employers.
% ABSENT_VOICES: Bodily-autonomy proponents and conscientious objectors were present as litigants but excluded from the framing conversation: inside the obligation reading their claims were pre-classified as free-riding rather than weighed as claims. Immunocompromised people harmed by breakthrough infections, for whom the delivered protection fell short, also lacked a seat once mandates passed, since the arrangement counted them as served rather than shortchanged.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen coverage gaps, re-expose hospital capacity to surge conditions, strip the immunocompromised of the protection layer they cannot self-provide, prompt reinstatement claims from terminated employees, and dissolve the exemption bureaucracies and delegated-enforcement relationships built around the requirements. Employment conditions, service-access rules, and institutional staffing policy would all reorganize around voluntary uptake.
% FOUNDING_PROBLEM: Catastrophic infectious-disease mortality met a coverage-dependent protective tool: voluntary uptake was insufficient to reach protective thresholds, leaving those who could not be vaccinated exposed and hospitals at risk of overwhelm.
% FOUNDING_PROBLEM_CORROBORATION: The historical founding problem is corroborated from outside the benefiting parties: hospital association capacity data and excess-mortality studies independently attest both the mortality stakes and the coverage dependence. Whether the problem remains live is attested mainly by the administering agencies themselves, citing variants and waning immunity; outside corroboration for continued liveness is thinner, with wastewater surveillance and severity data cited by both sides. No fully disinterested party attests ongoing liveness.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: substantial coercion on the resistant (employment and access loss) that this reading prices as the cost of a real obligation, not as illegitimate taking — hence well below snare range but far above rope's minimal-overhead profile. Suppression is authored at 0.60 as a raw structural property (unscaled by power or scope; the engine scales only extractiveness): termination authority, service denial, and delegated employer enforcement are the machinery that holds the arrangement. Theater_ratio 0.28 reflects real protective function with a performative residue (card-checking rituals outliving their evidentiary value). Accessibility_collapse 0.50: exemption channels and testing alternatives narrow the option space without closing it. Resistance 0.68: sustained litigation, protest, and legislative counteraction. The measurement series runs on one shared grid (points 0,4,8,12,16,20,24 for all three metrics) tracing a rise-hardening-erosion arc: extraction and suppression climb as enforcement hardens mid-interval, peak together, then erode under litigation and mass rescission, settling slightly above their starting points. The oscillation is driven by the threat-perception cycle (emergency solidarity, hardened enforcement, post-emergency relaxation) rather than intermittent reinforcement; base_properties reflect the interval-end state. Claim and metrics are independent: the reading claims an obligation structure; the metrics report what the arrangement actually cost its participants.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the mandate_resistant_worker seat — identity_locked, bearing concentrated termination and exclusion costs — the arrangement presents as enforced extraction with a coordination cover story, computing snare-ward. From the immunocompromised and hospital seats the same structure is a lifeline they cannot self-provide, computing rope-ward. From the agency seat it is an obligation faithfully administered. The engine derives these per-seat classifications from the structural data; this file authors the data and the claim, not the reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (immunocompromised_patients, hospital_systems, compliant_vaccinated_majority) derive low directionality — the arrangement subsidizes them. Immunocompromised_patients carry a secondary payer position encoding the failure-mode victimhood the kernel delta specifies: they are victims when the mandate fails, beneficiaries when it holds, and the engine blends this dual position. Mandate_resistant_workers hold the payer role with identity_locked exit: their refusal is fused with political and religious identity, so the exit that would end their costs (compliance) is experienced as self-betrayal — locked targets sit near the full-target end, amplifying their effective costs, which matches the expected structural delta of high extractiveness on the mandate-resistant. Note the deliberate asymmetry: resistant workers are structurally payers in stakeholders[] but are excluded from base_properties.victims[], enacting this reading's refusal of victim standing — the exclusion is the reading's signature, documented in the refuser_claims axiom and challenged by the externality_magnitude_dispute omega. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms produce the correct profile, and the excluded advocates' oppositional position is captured by their role and situation rather than by an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coverage-dependent protection against catastrophic mortality) is contested: externally corroborated as historical fact, but its continuing liveness is attested mainly by the administering agencies themselves. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no dead-mandate zombie flag fires, correctly, because the arrangements genuinely depend on the constraint. The theater_ratio series peaks mid-interval (0.34 at t=16) as card-checking rituals outlived their function before rescissions pruned them — a transient Goodhart signal, not terminal atrophy. Classification discipline: a pure-rope rendering (obligation talk alone) would erase the resistant's concentrated losses; a pure-snare rendering would erase the genuine, externally corroborated protection the arrangement delivered to people who cannot self-protect. Tangled_rope holds both facts in one structure: real coordination, real enforcement, real asymmetric costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (public_health_primary) of the public_health_mandate_authority kernel; would instantiating a sibling reading change the structural classification?',
    'Compare compiled sibling files: bodily_autonomy_primary centers the coerced in its victim set and authors epsilon against non-consensual intervention as such; proportionality_reading indexes legitimacy to threat severity, alternatives, coercion magnitude, and duration. The disagreement is located in the baseline: standing obligation versus categorical sovereignty versus sliding-scale calibration.',
    'Under bodily_autonomy_primary the same arrangement computes as pure imposition with the resistant as victims; under proportionality_reading portions of the arrangement certify as legitimate and others not, fragmenting the classification. This file''s tangled_rope verdict holds only within the obligation baseline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one kernel, three readings, classification is reading-relative.').

omega_variable(
    immunocompromised_dual_position,
    'Do immunocompromised patients sit structurally as beneficiaries of the standing arrangement, or as its victims whenever delivered protection falls short of the promised commons?',
    'Stratified infection and outcome data by immune status across coverage regimes: if severe outcomes among the immunocompromised track coverage failures systematically, the failure-mode victimhood is structural rather than incidental.',
    'Counting them as structural victims raises their effective-cost profile and pushes the arrangement toward a snare-leaning reading (protection sold, protection not delivered); counting them as beneficiaries supports the coordination reading. Authored here as dual-positioned pending that data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_dual_position, empirical, 'Whether the protected class is actually protected, or doubly exposed by the arrangement''s shortfalls.').

omega_variable(
    externality_magnitude_dispute,
    'Is the externality imposed by mandate-refusers as large as the free-rider frame asserts, given waning vaccine-derived immunity, differing contact patterns, and prior-infection immunity?',
    'Transmission attribution studies comparing refuser and complier cohorts under matched exposure conditions, audited for confounding by age, occupation, and prior infection.',
    'If the attributable externality is materially smaller than framed, the concentrated costs borne by resistant workers (employment loss, service exclusion) read as punitive rather than corrective, and the arrangement drifts toward snare classification; if large, the costs price a real imposition and the coordination reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_magnitude_dispute, empirical, 'Whether the free-rider framing tracks a measurable externality or discounts conscience claims.').

omega_variable(
    exemption_channel_integrity,
    'Do medical and religious exemption channels constitute genuine exit, or are they gauntlets whose approval rates and documentation burdens make nominal exit practically unavailable?',
    'Exemption application, approval, and appeal data across administering institutions, plus audit of documentation demands against stated criteria.',
    'Gauntlet-like channels mean effective suppression exceeds the authored scalar, raising effective costs on every non-compliant seat; genuinely open channels support the accessibility_collapse value authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_channel_integrity, empirical, 'Nominal versus real availability of the arrangement''s exit routes.').

omega_variable(
    post_emergency_persistence_question,
    'Do healthcare-sector and other surviving mandates serve the live protection of the vulnerable commons, or do they persist by institutional inertia and ritual compliance after their protective marginal value receded?',
    'Marginal-outbreak analysis: compare outbreak frequency and severity in covered versus uncovered institutions during late-interval, lower-severity circulation periods.',
    'If protective marginal value is negligible while enforcement continues, the theater_ratio trajectory understates inertial persistence and the arrangement drifts piton-ward in its residue; if protection remains material, the standing-duty reference frame stays intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_emergency_persistence_question, empirical, 'Live obligation versus inertial residue in the mandate''s surviving core.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t4, public_health_mandate_authority__public_health_primary, theater_ratio, 4, 0.14).
narrative_ontology:measurement(publ_tr_t8, public_health_mandate_authority__public_health_primary, theater_ratio, 8, 0.22).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__public_health_primary, theater_ratio, 12, 0.3).
narrative_ontology:measurement(publ_tr_t16, public_health_mandate_authority__public_health_primary, theater_ratio, 16, 0.34).
narrative_ontology:measurement(publ_tr_t20, public_health_mandate_authority__public_health_primary, theater_ratio, 20, 0.31).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(publ_be_t4, public_health_mandate_authority__public_health_primary, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(publ_be_t8, public_health_mandate_authority__public_health_primary, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__public_health_primary, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(publ_be_t16, public_health_mandate_authority__public_health_primary, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__public_health_primary, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(publ_su_t4, public_health_mandate_authority__public_health_primary, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(publ_su_t8, public_health_mandate_authority__public_health_primary, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__public_health_primary, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(publ_su_t16, public_health_mandate_authority__public_health_primary, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__public_health_primary, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'public health mandate' decomposes into three structurally distinct constraints instantiating one kernel (public_health_mandate_authority). This story is the public_health_primary reading: baseline obligation to protect the vulnerable commons, victim set centered on the immunocompromised (in the mandate's failure mode), refusers framed as externality-imposers rather than victims, high effective costs concentrated on the mandate-resistant. The sibling bodily_autonomy_primary reading emits a constraint with a categorical prohibition structure and a victim set centered on the coerced; the proportionality_reading emits a calibrated-legitimacy structure with context-indexed victim sets. Each sibling is a separate file with its own epsilon, beneficiaries, and victims; this file links them via reading_relations and network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
