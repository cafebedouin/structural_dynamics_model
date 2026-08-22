% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm-Reduction Substance Control Regime
 *   domain: public health policy/criminal justice/political economy
 *
 * SUMMARY:
 *   This story instantiates the harm-reduction reading of the
 *   substance-control kernel: substance use is governed as a health
 *   condition, intervened in pragmatically, and never conditioned on
 *   cessation. The standing arrangement under contest is the operating
 *   harm-reduction regime as this reading's own lights assess it: users have
 *   exited the criminal victim set for use itself, but remain subject to
 *   paternalistic health management (registration, program discipline,
 *   compulsory-referral pathways, benefit and custody conditionality), while
 *   the supply chain remains fully criminalized. The state has shifted to
 *   service-provider and steward roles, and overdose mortality and disease
 *   transmission have become the primary observables. CONSTRAINT FAMILY: the
 *   colloquial label 'drug policy' decomposes per the epsilon-invariance
 *   principle into three readings of one kernel, each a separate story with
 *   its own epsilon, victims, and observables. The prohibition reading
 *   authors high epsilon over a punishment arrangement whose victims are
 *   incarcerated and criminalized users; the legalization reading authors low
 *   epsilon over a liberty arrangement and identifies victims only where
 *   third-party harm goes uncompensated; THIS reading authors moderate
 *   epsilon (0.48) over the hybrid health/criminal arrangement described
 *   here, because its own lights concede the retained paternalism and
 *   supply-side criminalization as real costs of the regime it endorses. The
 *   prohibition-era statutes persist beneath the health-layer implementation,
 *   which is why the upstream prohibition story influences this one and why
 *   the hybrid's epsilon exceeds what a pure harm-reduction regime would
 *   carry.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda-setter (institutional/constrained) — administers the service network, sets eligibility and reporting rules, owns the outcome observables
 *   - active_drug_users: primary target and partial beneficiary (powerless/trapped) — bear paternalistic management and registration while receiving substitute medication and sterile supplies
 *   - drug_supply_participants: residual criminal target (powerless/constrained) — bear the retained supply-side enforcement the health frame leaves intact
 *   - treatment_service_providers: concentrated beneficiary (organized/mobile) — collect per-client contracted funding; program design doubles as revenue design
 *   - pharmaceutical_manufacturers: secondary beneficiary (institutional/arbitrage) — sell substitute medications and reversal agents into guaranteed public markets
 *   - taxpayers: diffuse payer (moderate/constrained) — fund the network through appropriations
 *   - general_public_residents: incidental beneficiary (moderate/mobile) — receive reduced public disorder and shifted emergency response
 *   - unregistered_drug_users: excluded (powerless/arbitrage) — outside the system and invisible to its observables until an overdose or arrest delivers them in
 *   - public_health_researchers: analytical observer (analytical/analytical) — produce the evidence base that sets funding fights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.48).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.45).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm-Reduction Substance Control Regime").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public health policy/criminal justice/political economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '0059d789-38ab-4d60-8dbc-0fe51d92a719').
narrative_ontology:cs_kernel_codification('0059d789-38ab-4d60-8dbc-0fe51d92a719', formalized).
narrative_ontology:cs_authority_grounding('0059d789-38ab-4d60-8dbc-0fe51d92a719', expertise).
narrative_ontology:cs_interpretation_layer_present('0059d789-38ab-4d60-8dbc-0fe51d92a719').
narrative_ontology:cs_reading_relation('0059d789-38ab-4d60-8dbc-0fe51d92a719', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('0059d789-38ab-4d60-8dbc-0fe51d92a719', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('0059d789-38ab-4d60-8dbc-0fe51d92a719', foundational, use_is_medical_condition_not_moral_failure).
narrative_ontology:cs_axiom_status(use_is_medical_condition_not_moral_failure, holdable).
narrative_ontology:cs_axiom_grounding('0059d789-38ab-4d60-8dbc-0fe51d92a719', use_is_medical_condition_not_moral_failure, empirically_contingent).
narrative_ontology:cs_axiom('0059d789-38ab-4d60-8dbc-0fe51d92a719', foundational, cessation_independent_harm_intervention).
narrative_ontology:cs_axiom_status(cessation_independent_harm_intervention, holdable).
narrative_ontology:cs_axiom_grounding('0059d789-38ab-4d60-8dbc-0fe51d92a719', cessation_independent_harm_intervention, instrumental).
narrative_ontology:cs_axiom('0059d789-38ab-4d60-8dbc-0fe51d92a719', secondary, state_health_stewardship_over_self_regarding_use).
narrative_ontology:cs_axiom_status(state_health_stewardship_over_self_regarding_use, holdable).
narrative_ontology:cs_axiom_grounding('0059d789-38ab-4d60-8dbc-0fe51d92a719', state_health_stewardship_over_self_regarding_use, deontological).
narrative_ontology:cs_reference_frame('0059d789-38ab-4d60-8dbc-0fe51d92a719', medicalized_public_health_stewardship).
narrative_ontology:cs_drift_state('0059d789-38ab-4d60-8dbc-0fe51d92a719', contemporary_overdose_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0059d789-38ab-4d60-8dbc-0fe51d92a719', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_systems).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, general_public_residents).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, treatment_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, active_drug_users).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, drug_supply_participants).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, active_drug_users).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, harm_minimization_consequentialism).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, disease_transmission_interruption_model).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, chronic_disease_model_of_addiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the service network: fund and license syringe-service programs, supervised consumption sites, and medication-assisted treatment clinics; set eligibility rules and reporting requirements; publish the overdose and infection statistics that define success. Their budgets and statutory mandates renew through legislatures, and their program designs must survive political review, which shapes what services they can offer and how outcomes are counted.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Attend clinics for substitute medication, exchange injecting equipment, and appear in outreach datasets. Enrollment brings sterile supplies, medications that prevent withdrawal, and connections to housing and primary care, but also registration in systems that share information with child-welfare and criminal-justice agencies, program rules carrying sanctions, and in some jurisdictions referral to compulsory treatment. Leaving the clinic means withdrawal returns within hours and any housing or benefits tied to enrollment lapse.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, active_drug_users, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, active_drug_users, beneficiary).

% Sell, share, or transport controlled substances. Using became a health matter, but selling did not: possession with intent, sharing at scale, and low-level dealing still carry arrest, prosecution, and imprisonment. Most people arrested sit on the lowest rung of the distribution chain; those higher up pass enforcement costs into street prices.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, drug_supply_participants, payer,
    powerless, biographical, constrained, national).

% Operate the clinics and outreach programs under contract. Revenue scales with enrolled clients and delivered services, so program-design choices such as intake rules, retention incentives, and outcome metrics are also business decisions. Provider associations advise agencies on standards and campaign for program expansion.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, treatment_service_providers, beneficiary,
    organized, biographical, mobile, national).

% Produce methadone, buprenorphine, naloxone, and related products. Public purchase guarantees and expanded prescribing guidelines enlarge their markets; they price globally and can shift production lines as policy environments change.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, pharmaceutical_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Fund the service network through appropriations. They see program budgets, overdose statistics, and occasional facility-siting disputes; their direct contact with the arrangement is the tax bill and neighborhood effects.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Experience the arrangement as fewer discarded needles, fewer public injections, ambulance calls answered by outreach teams instead of patrol cars, and treatment facilities as neighbors. They can move away from disliked facilities; most never interact with the system directly.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, general_public_residents, beneficiary,
    moderate, biographical, mobile, regional).

% Use without ever enrolling: they avoid clinics, dodge outreach workers, obtain supplies informally, and stay off the datasets that define the policy conversation. The avoidance strategy holds until an overdose, an arrest, or a hospital admission delivers them into the system involuntarily.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, unregistered_drug_users, excluded,
    powerless, immediate, arbitrage, national).

% Design and evaluate the studies agencies cite: cohort follow-ups, supervised-site evaluations, cost-effectiveness analyses. They hold no administrative power and collect nothing from the arrangement's operation, but their findings set what counts as evidence in funding fights.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__harm_reduction_reading, treatment_service_providers).
narrative_ontology:fixing_cost_class(substance_control_kernel__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of communicable disease and mass-casualty poisoning in illicit markets: centralized sterile-supply distribution, overdose surveillance, medication-assisted treatment infrastructure, and police-to-service diversion protocols that route users away from courts. Stated without evaluation.
% TRANSFER_FUNCTION: Moves public funds from taxpayers to treatment providers, pharmaceutical manufacturers, and public-health agencies; moves users from criminal-justice processing into health-system management; moves supply-market risk onto supply participants through retained criminalization of sale and distribution.
% ABSENT_VOICES: Unregistered users who avoid all contact with the system; active users who oppose program discipline and coerced referral; drug-user unions critical of compulsory treatment; legalization advocates excluded from health-agency proceedings; and supply participants, who bear the arrangement's enforcement but hold no seat in health-policy design.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, syringe programs and supervised sites would close, medication-assisted treatment patients would face abrupt discontinuation, police diversion protocols would revert to arrest-and-charge, and the overdose and disease surveillance apparatus would lose its organizing frame. Users would fall back into prohibition-era handling or unmanaged markets; provider contracts, pharmaceutical purchase guarantees, and agency mandates would all unwind.
% FOUNDING_PROBLEM: Under prohibition-era handling, injection-driven HIV and hepatitis C outbreaks, rising overdose mortality, and mass incarceration of users created public-health emergencies that criminal enforcement was visibly failing to control.
% FOUNDING_PROBLEM_CORROBORATION: Vital-statistics agencies, coroner systems, and infectious-disease surveillance networks attest the founding problem's persistence from outside the benefiting parties: overdose mortality and HCV incidence figures are produced by statistical bodies with no stake in program funding. Treatment providers and agencies also attest the problem is live, but they are insiders; the epidemiological record is the independent corroboration.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) rather than low because the arrangement's own lights count real costs: users trade registration, program discipline, and conditionality for services, and the supply chain bears unchanged criminal enforcement. Suppression (0.45) is authored as a raw structural property and is deliberately NOT scaled by power or scope — only extractiveness is scaled in the engine's computation; the value reflects the mixed enforcement picture: receded toward users, intact toward supply, with compulsory-treatment pathways available. Theater ratio (0.30) reflects a growing share of performative activity: harm-reduction branding over programs that retain abstinence-contingent housing and benefits, enrollment-count metrics substituting for health outcomes, and pilot sites maintained partly for political optics. Accessibility collapse is low (0.35) because the rival readings remain fully live political alternatives and unregistered use persists as a practical exit; resistance is moderate (0.52) — program attrition, supplier evasion, libertarian objection to paternalism, and community opposition to facility siting. TEMPORAL PATTERN: the suppression series shows a dip-then-ratchet rather than monotonic drift — enforcement recedes during early liberalization (diversion protocols mature, use decriminalized), then ratchets back up as fentanyl-era mortality spikes trigger supply-policing intensification and involuntary-commitment expansions. Each crisis phase justifies budget expansion that never fully unwinds during calm phases; the ratchet, not the oscillation itself, is the extraction mechanism. Base properties are measured at interval end, the post-ratchet phase of the most recent cycle. All three tracked series share one time grid so no metric row is sampled against another metric's end-state substitution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats should compute very differently from the payer seats. From the agency seat the arrangement is evidence-based stewardship it built and defends with surveillance data; from the provider seat it is care delivery that is also a revenue stream scaled to enrollment; from the user seat it is conditional surveillance in which help arrives pre-packaged with registration, sanction rules, and referral leverage; from the supply-participant seat it is barely distinguishable from the prohibition it replaced — the same arrest, the same courtroom, a different brochure. Same-power divergence: active users and supply participants are both largely powerless, yet their exits differ structurally (users are trapped by physical dependence on dispensed substitutes and conditionality-linked housing; suppliers are constrained but can shift territory, price, or role), so identical nominal standing produces different experienced constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (public health systems, residents, providers, manufacturers) derive low directionality for those seats; victim declarations (users, supply participants, taxpayers) derive high directionality. The hard case is active_drug_users, who are dual-positioned: listed among victims for the paternalistic-management and conditionality costs, yet substantial net recipients of subsidized medication, supplies, and housing pathways. The structural derivation will read their victim listing plus trapped exit and push their directionality toward the full-target end, overstating their net extraction position; the honest value sits nearer the middle. Per-agent correction of this is inexpressible through the override surface without colliding with the other powerless seats (supply participants, who ARE near-full targets, and unregistered users, who sit near the beneficiary end as anonymous informal consumers of the same services), so the residual is routed to the user_net_position_directionality omega rather than forced through a power-atom-keyed override. No directionality overrides are authored; the derivation is accepted as approximate and the known distortion is documented.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the arrangement as pure coordination (rope) would erase the asymmetric extraction its own lights concede: the per-client funding stream that rewards enrollment over outcome, the conditionality that converts care into leverage, and the supply-side criminalization that persists untouched beneath the health frame. Reading it as pure extraction (snare) would erase the genuine coordination function: disease-transmission interruption and overdose prevention demonstrably work, and no single victim class bears the whole cost. Tangled rope holds both halves. On the genealogy interview, the founding problem (prohibition-era epidemics and overdose mortality) is corroborated as live by independent vital-statistics and disease-surveillance bodies, and the disappearance verdict is world_rearranges, so the mismatch consumer finds no dead-mandate/zombie flag. The forward risk is abstinence creep: if program conditionality quietly reintroduces cessation requirements and outcome metrics drift to enrollment counts, the founding axiom erodes while the apparatus persists — the theater-ratio series (0.18 to 0.30) is the early-warning trace of that atrophy vector, and the abstinence_creep omega names the test.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    user_net_position_directionality,
    'Are active drug users net targets or net beneficiaries of the harm-reduction arrangement, once subsidized medication, sterile supplies, and housing pathways are weighed against registration, program discipline, conditionality, and compulsory-referral exposure?',
    'Longitudinal per-client accounting comparing the monetary and liberty value of services received against the impositions borne (sanctions, data sharing, mandated visits), across representative programs.',
    'If users are net beneficiaries, the user-seat effective extraction drops sharply and the arrangement reads closer to coordination at that seat; if net targets, the user seat computes snare-flavored and the tangled-rope reading survives only via the other seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_net_position_directionality, empirical, 'Dual-positioned users: net subsidy or net extraction.').

omega_variable(
    hybrid_layer_attribution,
    'How much of the measured extraction belongs to the harm-reduction layer proper versus the retained prohibition statutes and enforcement machinery operating beneath it?',
    'Cross-jurisdiction comparison of pure harm-reduction implementations against hybrid implementations that layer health services over intact prohibition codes, isolating the extraction attributable to each layer.',
    'If extraction concentrates in the retained-prohibition layer, this reading''s own constraint is cleaner than the measured 0.48 suggests and the corpus should decompose the layers into separate stories; if the health layer itself generates the extraction (conditionality, data integration), the reading is implicated directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_layer_attribution, conceptual, 'Attributing epsilon between the health layer and the retained criminal layer.').

omega_variable(
    abstinence_creep_operationalization,
    'Does cessation-independence survive operationally, or do abstinence-contingent housing, benefits, custody rules, and outcome metrics reintroduce cessation requirements in practice while the cessation-independent label persists?',
    'Program audits of conditionality clauses and outcome-measure definitions across funded sites, tracking the share of services gated on reduced or ceased use.',
    'If abstinence contingencies dominate, the arrangement betrays its founding axiom, theater_ratio is understated, and the regime drifts toward prohibition-instrumentalities administered by health agencies; if conditionality is marginal, the founding axiom holds and the current classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abstinence_creep_operationalization, empirical, 'Whether the cessation-independent premise is honored in program rules.').

omega_variable(
    supply_criminalization_separability,
    'Is the retained criminalization of supply a necessary guardrail of the harm-reduction arrangement (preventing commercial promotion and unregulated potency escalation) or extracted enforcement riding on the health frame''s legitimacy?',
    'Natural experiments from prescribed-supply and safe-supply models: if regulated supply channels reduce overdose mortality without promoting use, the criminalized-market guardrail is separable; if black-market replacement follows every relaxation, the guardrail is load-bearing.',
    'If separable, the supply-side suppression and extraction are removable without touching the coordination function; if load-bearing, part of the measured suppression is the price of the arrangement''s stability rather than extractive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_criminalization_separability, conceptual, 'Whether supply-side criminalization is guardrail or rent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harm_reduction_substance_control_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(harm_reduction_substance_control_tr_t0, observed).
narrative_ontology:measurement(harm_reduction_substance_control_tr_t4, substance_control_kernel__harm_reduction_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement_basis(harm_reduction_substance_control_tr_t4, observed).
narrative_ontology:measurement(harm_reduction_substance_control_tr_t8, substance_control_kernel__harm_reduction_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(harm_reduction_substance_control_tr_t8, observed).
narrative_ontology:measurement(harm_reduction_substance_control_tr_t12, substance_control_kernel__harm_reduction_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement_basis(harm_reduction_substance_control_tr_t12, observed).
narrative_ontology:measurement(harm_reduction_substance_control_tr_t16, substance_control_kernel__harm_reduction_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(harm_reduction_substance_control_tr_t16, observed).
narrative_ontology:measurement(harm_reduction_substance_control_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(harm_reduction_substance_control_tr_t20, observed).
narrative_ontology:measurement(harm_reduction_substance_control_tr_t24, substance_control_kernel__harm_reduction_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(harm_reduction_substance_control_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(harm_reduction_substance_control_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(harm_reduction_substance_control_be_t0, observed).
narrative_ontology:measurement(harm_reduction_substance_control_be_t4, substance_control_kernel__harm_reduction_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement_basis(harm_reduction_substance_control_be_t4, observed).
narrative_ontology:measurement(harm_reduction_substance_control_be_t8, substance_control_kernel__harm_reduction_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement_basis(harm_reduction_substance_control_be_t8, observed).
narrative_ontology:measurement(harm_reduction_substance_control_be_t12, substance_control_kernel__harm_reduction_reading, base_extractiveness, 12, 0.43).
narrative_ontology:measurement_basis(harm_reduction_substance_control_be_t12, observed).
narrative_ontology:measurement(harm_reduction_substance_control_be_t16, substance_control_kernel__harm_reduction_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement_basis(harm_reduction_substance_control_be_t16, observed).
narrative_ontology:measurement(harm_reduction_substance_control_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(harm_reduction_substance_control_be_t20, observed).
narrative_ontology:measurement(harm_reduction_substance_control_be_t24, substance_control_kernel__harm_reduction_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement_basis(harm_reduction_substance_control_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(harm_reduction_substance_control_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(harm_reduction_substance_control_su_t0, observed).
narrative_ontology:measurement(harm_reduction_substance_control_su_t4, substance_control_kernel__harm_reduction_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement_basis(harm_reduction_substance_control_su_t4, observed).
narrative_ontology:measurement(harm_reduction_substance_control_su_t8, substance_control_kernel__harm_reduction_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement_basis(harm_reduction_substance_control_su_t8, observed).
narrative_ontology:measurement(harm_reduction_substance_control_su_t12, substance_control_kernel__harm_reduction_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement_basis(harm_reduction_substance_control_su_t12, observed).
narrative_ontology:measurement(harm_reduction_substance_control_su_t16, substance_control_kernel__harm_reduction_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement_basis(harm_reduction_substance_control_su_t16, observed).
narrative_ontology:measurement(harm_reduction_substance_control_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(harm_reduction_substance_control_su_t20, observed).
narrative_ontology:measurement(harm_reduction_substance_control_su_t24, substance_control_kernel__harm_reduction_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement_basis(harm_reduction_substance_control_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: substance_control_kernel decomposes into three reading-stories per the epsilon-invariance principle. The colloquial label 'drug policy' conflates three structurally distinct arrangements with different epsilons, victim sets, and observables: prohibition (high epsilon; incarcerated users as victims; conviction rates as observable), harm reduction (this story; moderate epsilon; users bear paternalistic management while supply remains criminalized; overdose and disease transmission as observables), and legalization (low epsilon; victims only where third-party harm is uncompensated; externality pricing as observable). Edges run upstream-downstream: the prohibition reading is upstream (its statutes persist beneath this reading's implementation and supply the enforcement machinery this story counts as extraction), and this reading is upstream of the legalization reading (operating harm-reduction infrastructure — prescribed supply, supervised sites, regulated dispensation — builds the institutional substrate legalization debates inherit). Each story links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
