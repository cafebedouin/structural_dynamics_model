% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Regime under the Public Health Primacy Reading
 *   domain: public health policy / constitutional law / bioethics
 *
 * SUMMARY:
 *   Compulsory-vaccination authority rests on a doctrine lineage running from
 *   Jacobson v. Massachusetts (1905) through contemporary statutory mandate
 *   powers: the state's duty to prevent collective harm licenses overriding
 *   individual refusal when noncompliance externalizes risk onto others. This
 *   file instantiates the public_health_primacy_reading of the
 *   vaccine_mandate_legitimacy kernel: under this reading, unvaccinated
 *   status is treated as an externality and mandate enforcement as legitimate
 *   internalization. The standing arrangement assessed here is the operating
 *   mandate regime — occupational mandates, venue-access rules, school-entry
 *   requirements, exemption administration, and the enforcement machinery
 *   behind them — evaluated by this reading's own frame. Assumptions stated
 *   explicitly: the institutional lens is United States-centric (Jacobson
 *   lineage, federal/state enforcement split); the interval maps to
 *   approximately 2000-2025, with the acute-phase surge occupying roughly
 *   t=20-25; all metric values are reading-indexed judgments over the fixed
 *   referent of the standing mandate arrangement. KEY AGENTS (by structural
 *   relationship): public_health_authorities — agenda-setter
 *   (institutional/constrained), writes mandate rules and collects authority;
 *   mandate_refusers — primary target (moderate/identity_locked), bear
 *   termination, exclusion, and fines; denied_exemption_applicants —
 *   secondary target (moderate/constrained), denied exemptions and pushed
 *   into the full enforcement path; immunocompromised_individuals —
 *   beneficiary (powerless/trapped), protected only through others'
 *   compliance; general_vaccinated_public — beneficiary with payer costs
 *   (moderate/constrained); vaccine_manufacturers — beneficiary
 *   (institutional/arbitrage), guaranteed demand and capped liability;
 *   mandate_administering_employers — local enforcer bearing payer costs
 *   (institutional/constrained); constitutional_courts — analytical observer
 *   adjudicating the Jacobson lineage; disability_rights_advocates — excluded
 *   voice pressing accommodation-first design. CONSTRAINT FAMILY NOTE: this
 *   story is one of three readings of the shared kernel; the
 *   bodily_autonomy_primacy_reading and risk_stratification_reading files
 *   instantiate the sibling readings as separate constraints with their own
 *   epsilon values, victim sets, and classifications. Per the
 *   epsilon-invariance principle this file hedges nothing across readings —
 *   its epsilon refers solely to the mandate arrangement as the
 *   public-health-primacy frame assesses it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.58).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.72).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Regime under the Public Health Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public health policy / constitutional law / bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, 'ff4579ae-f7fb-49fd-a273-6a1642308611').
narrative_ontology:cs_kernel_codification('ff4579ae-f7fb-49fd-a273-6a1642308611', formalized).
narrative_ontology:cs_authority_grounding('ff4579ae-f7fb-49fd-a273-6a1642308611', lineage).
narrative_ontology:cs_interpretation_layer_present('ff4579ae-f7fb-49fd-a273-6a1642308611').
narrative_ontology:cs_reading_relation('ff4579ae-f7fb-49fd-a273-6a1642308611', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('ff4579ae-f7fb-49fd-a273-6a1642308611', vaccine_mandate_legitimacy__risk_stratification_reading, coexists_with).
narrative_ontology:cs_axiom('ff4579ae-f7fb-49fd-a273-6a1642308611', foundational, collective_duty_overrides_refusal).
narrative_ontology:cs_axiom_status(collective_duty_overrides_refusal, holdable).
narrative_ontology:cs_axiom_grounding('ff4579ae-f7fb-49fd-a273-6a1642308611', collective_duty_overrides_refusal, deontological).
narrative_ontology:cs_axiom('ff4579ae-f7fb-49fd-a273-6a1642308611', secondary, unvaccinated_status_is_externalized_cost).
narrative_ontology:cs_axiom_status(unvaccinated_status_is_externalized_cost, holdable).
narrative_ontology:cs_axiom_grounding('ff4579ae-f7fb-49fd-a273-6a1642308611', unvaccinated_status_is_externalized_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('ff4579ae-f7fb-49fd-a273-6a1642308611', collective_harm_prevention_supremacy).
narrative_ontology:cs_drift_state('ff4579ae-f7fb-49fd-a273-6a1642308611', post_acute_phase_endemic_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff4579ae-f7fb-49fd-a273-6a1642308611', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_manufacturers).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, general_vaccinated_public).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, mandate_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, denied_exemption_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, general_vaccinated_public).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, mandate_administering_employers).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, collective_harm_prevention_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, externality_internalization_principle).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, jacobson_compulsory_vaccination_lineage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft mandate orders, define qualifying exemptions, set verification standards, and direct enforcement through health departments. During acute outbreaks they gain expanded emergency powers, budgets, and staffing; between outbreaks they retain the authority infrastructure built during the last emergency. Leaving the system means leaving the career field entirely; senior careers are spent inside these agencies.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Sell vaccines into a market where mandates and procurement guarantees convert uncertain consumer demand into committed volume, and operate under indemnification arrangements that cap liability exposure. Product portfolios can pivot between candidates and indications; manufacturing capacity is redeployable across products.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Cannot mount a full response to vaccination or cannot be vaccinated at all, and depend on the surrounding population's immunity to reduce exposure probability. Their vulnerability cannot be relocated, deferred, or opted out of; their protection arrives only through other people's compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, national).

% Receive reduced infection probability and reduced severe-outcome risk from population coverage; pay for the programs through taxes and absorb the civic costs of community conflict over mandates. Most cannot leave the disease environment; moving regions changes exposure pools but not the underlying risk structure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, general_vaccinated_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, general_vaccinated_public, payer).

% Decline mandated vaccination for reasons ranging from medical distrust to religious conviction to political identity, and face the enforcement consequences: termination from mandated workplaces, denial of venue access, school-enrollment barriers, and in some jurisdictions fines or suspension. Getting vaccinated would end the costs immediately, but for a substantial cohort the refusal is fused with religious, political, or community identity such that compliance feels like self-abandonment; others hold a principled line they could in principle trade away but do not.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, mandate_refusers, payer,
    moderate, biographical, identity_locked, national).

% Submitted medical or religious exemption requests believing they qualified, were denied under agency criteria, and entered the full enforcement path — termination proceedings, access denial — while disputing the validity of the denial. Their exit is bounded by the appeal process and by institutional discretion; many exhaust appeals while carrying enforcement costs throughout.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, denied_exemption_applicants, payer,
    moderate, biographical, constrained, national).

% Hospitals, university systems, and large employers operate the mandates locally: run verification systems, process exemption requests, execute separations, and defend the decisions in court. They absorb staffing losses in tight labor markets, litigation exposure, and administrative cost, while gaining a uniformly immunized workforce and reduced outbreak disruption.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, mandate_administering_employers, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, mandate_administering_employers, payer).

% Review mandate orders, exemption practices, and enforcement methods against constitutional precedent running through Jacobson v. Massachusetts; uphold, narrow, or strike specific implementations. Their judgments reshape which mandate forms remain legally available without themselves administering anything.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Organize for accommodation-first pandemic policy — protective equipment, ventilation, remote options, targeted shielding — rather than blanket compliance requirements. They were largely outside the emergency-authority loops where mandate rules were written and press their case through comment periods and litigation after the fact.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, disability_rights_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the vaccination free-rider problem: individual decisions underweight the transmission risk each unvaccinated person imposes on others, so voluntary uptake undershoots the coverage threshold that protects the medically vulnerable. Centralized mandates align private choices with population-level risk targets and coordinate procurement, distribution, and verification at scale.
% TRANSFER_FUNCTION: Moves employment continuity, venue access, school enrollment, and bodily-decision autonomy from noncompliant individuals to the enforcing institutions; moves guaranteed procurement volume to manufacturers; moves authority, budget, and staffing permanence to public health agencies; moves reduced transmission risk to the whole covered population.
% ABSENT_VOICES: Refusers had no seat where exemption criteria were written — the rules defining who must comply were drafted without the people who would contest them. Disability-rights advocates were outside the emergency-authority loops and reached policy only through after-the-fact comment and litigation. Separated workers dispersed after enforcement, leaving no organized counterparty present where retention decisions were reviewed.
% DISAPPEARANCE_RATIONALE: Employer verification systems, school-entry rules, exemption-appeal machinery, and agency emergency-authority frameworks would lose their coordinating object overnight; staffing rules at hospitals and universities would be rewritten; manufacturers' demand guarantees would revert to market uncertainty; the medically vulnerable would lose the protection layer they cannot provide for themselves; and refuser communities would see enforcement costs vanish while litigation campaigns lost their object.
% FOUNDING_PROBLEM: Outbreaks of high-consequence transmissible disease outrunning voluntary uptake: smallpox-era compulsory vaccination laws (the Jacobson lineage) were built to reach coverage thresholds voluntary persuasion could not; the modern acute-phase problem was the same — achieving population protection faster than voluntary programs could during a novel-pathogen surge.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological coverage-threshold research, produced outside the benefiting parties, attests the founding problem is real and live during high-consequence surges; constitutional-court precedent along the Jacobson lineage attests the harm-prevention grounding independently of agency interest; and mandate-opposition legal scholarship, state-legislative findings, and post-acute-phase policy reviews — all outside the beneficiary set — attest that the acute-phase problem was substantially resolved while the arrangement persisted. No single seat's testimony is taken as the genealogy; the contested status rests on this split corroboration.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the arrangement transfers employment continuity, venue access, and bodily-decision autonomy from noncompliant individuals, and accretes authority, budget permanence, and guaranteed procurement volume to administering institutions — but the transfer purchases a real coordination output (coverage thresholds, protected vulnerable seats), which damps the net figure below snare territory. Suppression 0.72 is authored as a RAW STRUCTURAL property and is deliberately not reconciled with extractiveness: only extractiveness is scaled by directionality and scope in the engine's computation; suppression measures the enforcement machinery itself (verification systems, separation rights, access denial, fine regimes) that must stay actively maintained for the arrangement to hold. Theater ratio 0.30: attestation portals, badge rituals, and compliance paperwork carry a performative component, but the underlying screening and coverage functions are real. Accessibility collapse 0.45: alternatives persist (exemptions, testing-based pathways in some regimes, relocation, remote-work pockets) but collapse for trapped cohorts — the immunocompromised cannot exit vulnerability and identity-locked refusers cannot exit refusal. Resistance 0.60: litigation campaigns, state-level legislative bans on mandates, exemption movements, and workplace walkouts constitute sustained organized resistance. Claim/metric independence is preserved: claimed_type tangled_rope is asserted from this reading's seat because both a genuine coordination function and asymmetric extraction are structurally present; the metrics are authored descriptively, not tuned to any predicted engine verdict. Temporal design: all three tracked series run on ONE SHARED GRID (t = 0, 5, 10, 15, 20, 25) so every metric is authored at every examined point; the suppression_requirement series is included specifically because enforcement-capacity change is the dynamic being traced — the sharp t=20 jump models the acute-phase enforcement ratchet (emergency authorization, occupational mandates, access rules built in months), followed by partial institutional hardening rather than full demobilization through t=25. Coalition note: refuser resistance consolidated into litigation networks and legislative campaigns — class-level organization that a coalition-power analysis should weigh against the moderate individual power atom. End-state measurement values equal the base_properties scalars by construction of the shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural inputs. From the public_health_authorities seat the arrangement presents as coordination it designed, staffs, and defends — a low-directionality, subsidy-side experience. From the mandate_refusers seat the same structure operates as enforced extraction with identity-locked exit: the physical exit (compliance) exists but is ideologically fused for a substantial cohort, placing that seat near the full-target end. The denied_exemption_applicants seat experiences the enforcement edge at near-full intensity while disputing the legitimacy of their own classification — paying suppression costs under protest. The immunocompromised seat experiences genuine protection and would mourn the arrangement's loss — a rope-like experience from the most powerless seat. The general_vaccinated_public sits near symmetric: real risk reduction received, taxes and civic-division costs paid. Four computed types from one arrangement; the engine derives this divergence from the declared positions, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map cleanly: public_health_authorities collect authority and budget (damped d, further pulled toward the beneficiary end by their constrained exit — they are embedded in the system they run); vaccine_manufacturers collect guaranteed volume under capped liability, with arbitrage-grade portfolio exit pulling their derived d toward the beneficiary floor despite substantial rent collection; immunocompromised_individuals receive protection they cannot self-provide (near-zero d); general_vaccinated_public receives risk reduction but pays taxes and division costs (mildly damped d, near-symmetric). Victim declarations: mandate_refusers and denied_exemption_applicants derive high d, amplified by identity_locked and constrained exit respectively — trapped or identity-locked targets sit nearer the full-target end than mobile ones. National spatial scope modestly amplifies effective extraction for targets (harder verification at larger scope). No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options lands correctly for every seat because role separation is clean and no capture ambiguity requires correction — the closest candidate (authorities as both administrator and collector) is already handled by their beneficiary declaration plus constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reaching coverage thresholds against high-consequence transmissible pathogens faster than voluntary programs can — is live during acute phases and contested in steady state; the arrangement was never given a sunset clause, so its acute-phase justification was never converted into transitional design. The tangled_rope classification is what prevents mandatrophy mislabeling in both directions: it keeps the coordination half visible (blocking a pure-extraction misread during surges, when the arrangement demonstrably saves lives) while the victim and enforcement declarations keep the extraction half visible (blocking a pure-coordination misread in steady state, when enforcement persists past demonstrated need). The decay vectors are named: if transmission risk falls below the proportionality bar and enforcement persists anyway, the arrangement decays toward theatrical compliance maintenance with no concentrated fixer; if authority retention becomes the primary output, it decays toward pure extraction. The contested founding-problem status combined with a world_rearranges verdict avoids the dead-problem zombie flag, but the mismatch consumer should watch the steady-state branch: persistence-past-need is exactly the trajectory this arrangement's history (school-entry mandates persisting indefinitely, emergency frameworks retained between emergencies) makes plausible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the public_health_primacy_reading of the vaccine_mandate_legitimacy kernel; how would the classification change under the sibling readings over the same standing arrangement?',
    'Cross-reading compilation: compile the bodily_autonomy_primacy_reading and risk_stratification_reading files and compare computed per-seat types over the shared referent.',
    'Under bodily_autonomy_primacy the coerced-and-compliant become a victim seat and epsilon rises sharply; under risk_stratification the victim set narrows to blanket-mandate subjects and targeted implementations survive. The referent is fixed but epsilon is reading-indexed — these are different constraints, not different measurements of one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a shared kernel; sibling files instantiate the other readings.').

omega_variable(
    externality_attribution_validity,
    'Is unvaccinated status a genuine externality imposed on others, or is part of it a private risk choice that this reading''s frame attributes outward?',
    'Transmission-attribution studies quantifying onward-transmission differentials by vaccination status, waning-adjusted and variant-stratified, across the acute and endemic phases.',
    'If the attributed externality is materially overstated, the coordination justification thins and the measured extraction share rises toward pure extraction; if robust across variants, the hybrid coordination-plus-extraction reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_attribution_validity, empirical, 'Whether the externality premise survives variant-specific transmission evidence.').

omega_variable(
    authority_accretion_vs_disease_control,
    'Does the mandate arrangement primarily serve disease control, or has authority expansion and persistence become an independent objective of the administering bureaucracy?',
    'Post-acute-phase retention analysis comparing mandate persistence against measured transmission risk, plus budget and staffing trajectory audits of the administering agencies across inter-emergency periods.',
    'If authority accretion dominates, classification drifts toward pure extraction and the founding-problem mismatch flag fires; if persistence tracks disease-control need, the hybrid reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_accretion_vs_disease_control, empirical, 'Distinguishing coordination service from bureaucratic self-perpetuation.').

omega_variable(
    refuser_suppression_mechanism,
    'Is the suppression borne by refusers structural (separation, exclusion, fines) or internalized (identity fusion that makes the compliance exit unthinkable even where physically available)?',
    'Post-repeal trajectory: track refuser-cohort behavior in jurisdictions where mandates were lifted — if refusal persists unchanged absent enforcement, the internalized component is substantial.',
    'Internalized suppression raises effective suppression above the structural measure and strengthens the identity_locked exit classification; purely structural suppression predicts rapid compliance decay after repeal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refuser_suppression_mechanism, empirical, 'Structural versus internalized suppression among mandate refusers.').

omega_variable(
    proportionality_threshold_location,
    'At what collective-risk severity does coercion become proportionate under this reading''s own duty frame — and do blanket mandates clear that bar under endemic conditions?',
    'Deliberative-bioethics panel synthesis plus comparative-jurisdiction analysis of threshold-dependent mandate designs and their outcomes.',
    'If endemic-condition blanket mandates fall below the proportionality bar, this reading''s own frame condemns its steady-state operation and classification shifts toward pure extraction from the reading''s internal logic; if the bar is cleared, the coordination side dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_threshold_location, preference, 'Where the duty frame''s own proportionality line sits, and whether steady-state operation clears it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(vacc_tr_t5, observed).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t10, observed).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(vacc_tr_t15, observed).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(vacc_tr_t20, observed).
narrative_ontology:measurement(vacc_tr_t25, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement_basis(vacc_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement_basis(vacc_be_t5, observed).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(vacc_be_t10, observed).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(vacc_be_t15, observed).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(vacc_be_t20, observed).
narrative_ontology:measurement(vacc_be_t25, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(vacc_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(vacc_su_t5, observed).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(vacc_su_t10, observed).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(vacc_su_t15, observed).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(vacc_su_t20, observed).
narrative_ontology:measurement(vacc_su_t25, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(vacc_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'vaccine mandate legitimacy' decomposes into three structurally distinct normative commitments sharing one kernel. This file instantiates the public_health_primacy_reading — historically the operative reading, whose blanket implementations created the practical conditions under which the risk_stratification_reading's proportionality critique developed, and which stands in direct logical opposition to the bodily_autonomy_primacy_reading's categorical prohibition. Each reading authors its own epsilon over the same standing arrangement (the operating mandate regime): this file's epsilon is indexed to the public-health-primacy frame alone, and the sibling files carry their own victim sets, beneficiary structures, and classifications. Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
