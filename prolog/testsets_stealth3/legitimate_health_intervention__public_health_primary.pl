% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public-Health-Primary Legitimation Standard for Health Interventions
 *   domain: public health policy / medical ethics / constitutional law
 *
 * SUMMARY:
 *   The standing arrangement under contest is the outcome-based legitimation
 *   standard for imposed health interventions: an intervention counts as
 *   legitimate when it demonstrably reduces population-level morbidity or
 *   mortality, and individual refusal is administered as an externality - met
 *   with employment termination, exclusion from services, and credential
 *   checkpoints rather than persuasion alone. The line runs from Jacobson-era
 *   compulsory smallpox vaccination through school-entry statutes to the
 *   2020-2025 wave of employer mandates and digital access passes. This file
 *   instantiates ONE reading of the contested kernel
 *   legitimate_health_intervention - the public_health_primary reading - as a
 *   clean, epsilon-invariant constraint. The sibling readings are separate
 *   constraints in separate files:
 *   legitimate_health_intervention__bodily_autonomy_primary grounds
 *   legitimacy in informed consent and leaves refusers outside any victim set
 *   (no imposition is legitimate regardless of measured benefit), while
 *   legitimate_health_intervention__proportionality_reading indexes victim
 *   membership to threat characteristics. Under THIS reading the victim set
 *   is fixed by outcome arithmetic: the unvaccinated enter as vectors, the
 *   immunocompromised and elderly enter as protected beneficiaries, and
 *   enforcement machinery generates the measured costs. Claim and metrics are
 *   independent authored facts: claimed_type states the authoring seat's
 *   structural judgment (tangled_rope - genuine herd-immunity coordination
 *   fused with asymmetric enforcement costs), while the metrics describe
 *   observed operation including the COVID-era enforcement spike; the engine
 *   computes per-seat classifications and adjudicates any divergence.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda setter (institutional/constrained) - defines the outcome criteria deciding when imposition is legitimate; directs enforcement through employer directives and access rules; their budget and standing ride on demonstrating measurable reductions
 *   - employers_administering_mandates: dual-positioned collector (beneficiary/payer, institutional/arbitrage) - terminates or bars unvaccinated staff, gaining workforce uniformity and liability insulation while absorbing attrition, compliance cost, and litigation
 *   - unvaccinated_workers: primary target (moderate/trapped) - bears suspension, termination, and the externality label; mandates span major employers and licensure boards so changing jobs does not escape
 *   - unvaccinated_patrons: secondary target (moderate/constrained) - barred from venues, transit, and cross-border travel under credential regimes; alternatives exist only at real cost
 *   - unvaccinated_healthcare_workers: organized target (organized/trapped) - licensure binds them to regulated facilities; unions negotiated exemptions yet thousands were terminated in 2021
 *   - immunocompromised_patients: protected beneficiary (powerless/trapped) - cannot mount vaccine response; depends wholly on others' coverage for protection
 *   - elderly_high_risk_residents: beneficiary (moderate/constrained) - gains reduced exposure; isolation substitutes for community immunity only at severe life cost
 *   - philosophical_exemption_advocates: excluded voice (powerless/trapped) - consent-based objections have no seat in rule-setting forums; enter only through litigation after rules bind
 *   - constitutional_courts: analytical observer (institutional/analytical) - adjudicate mandate legality under police-powers doctrine; can reshape enforcement instruments but not the outcome-measurement standard itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.71).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.68).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.71).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public-Health-Primary Legitimation Standard for Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public health policy / medical ethics / constitutional law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '8dea246a-5c3a-4900-8bba-5207d5e04d50').
narrative_ontology:cs_kernel_codification('8dea246a-5c3a-4900-8bba-5207d5e04d50', formalized).
narrative_ontology:cs_authority_grounding('8dea246a-5c3a-4900-8bba-5207d5e04d50', expertise).
narrative_ontology:cs_interpretation_layer_present('8dea246a-5c3a-4900-8bba-5207d5e04d50').
narrative_ontology:cs_reading_relation('8dea246a-5c3a-4900-8bba-5207d5e04d50', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('8dea246a-5c3a-4900-8bba-5207d5e04d50', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('8dea246a-5c3a-4900-8bba-5207d5e04d50', foundational, population_benefit_grounds_legitimacy).
narrative_ontology:cs_axiom_status(population_benefit_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8dea246a-5c3a-4900-8bba-5207d5e04d50', population_benefit_grounds_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('8dea246a-5c3a-4900-8bba-5207d5e04d50', foundational, refusal_is_externality_imposition).
narrative_ontology:cs_axiom_status(refusal_is_externality_imposition, holdable).
narrative_ontology:cs_axiom_grounding('8dea246a-5c3a-4900-8bba-5207d5e04d50', refusal_is_externality_imposition, instrumental).
narrative_ontology:cs_reference_frame('8dea246a-5c3a-4900-8bba-5207d5e04d50', population_outcome_legitimacy_baseline).
narrative_ontology:cs_drift_state('8dea246a-5c3a-4900-8bba-5207d5e04d50', post_emergency_endemic_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8dea246a-5c3a-4900-8bba-5207d5e04d50', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, elderly_high_risk_residents).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, employers_administering_mandates).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_patrons).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_healthcare_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, employers_administering_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines the measurable-benefit criteria that decide when an imposed intervention is legitimate, certifies interventions against surveillance data, and during emergencies directs employer mandates and access rules. Agency budgets, statutory authority, and professional standing track demonstrated reductions in disease burden, giving the seat a continuous stake in the standard's maintenance. Leaving the arrangement would mean ceding the outcome-measurement franchise to rival bodies; the available exit is narrowed discretion, not departure.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Terminates or bars unvaccinated employees under regulatory directive or liability logic, collecting workforce uniformity, reduced outbreak disruption, and insurance-liability insulation. Pays compliance administration, attrition of experienced staff who refuse, grievance handling, and litigation exposure. Large firms lobby for carve-outs unavailable to smaller competitors and can shift policy templates across jurisdictions; their exit from the arrangement is partial repositioning, not withdrawal from labor markets.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, employers_administering_mandates, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, employers_administering_mandates, payer).

% Cannot mount an effective response to vaccination and depends entirely on the coverage of people around them for protection against circulating pathogens. Has no private substitute for population-level immunity - isolation is the only fallback and it consumes ordinary life. Bears acute risk whenever community coverage slips below protective thresholds.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_patients, beneficiary,
    powerless, immediate, trapped, national).

% Gains reduced exposure to vaccine-preventable disease through others' compliance with immunization requirements. Holds advocacy organizations but diffuse electoral weight; can retreat into isolation during outbreaks at severe cost to social life, caregiving access, and mental health, so exit exists only as self-impoverishment of daily living.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, elderly_high_risk_residents, beneficiary,
    moderate, biographical, constrained, national).

% Faces suspension, testing fees, and ultimately termination under employer mandates. Changing employers does not escape: parallel mandates span major chains, hospital systems, and government contractors, and some licensure boards condition practice on status. Carries the public label of externality source, which narrows sympathy and legal recourse. The realistic choices are compliance against conscience, exit from the formal workforce, or litigation after the fact.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_workers, payer,
    moderate, immediate, trapped, national).

% Barred from restaurants, venues, gyms, domestic transit segments, and cross-border travel under credential regimes. Substitutes exist - delivery, home entertainment, domestic road travel - but at real monetary and social cost, and the restriction can arrive overnight with an emergency order. Participation in civic and commercial life is conditional on a status they declined.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_patrons, payer,
    moderate, immediate, constrained, national).

% Work inside the institutions enforcing the standard. Licensure ties them to regulated facilities, so refusing means leaving the profession they trained for, not just one employer. Unions negotiated testing alternatives and exemption processes with partial success, yet thousands were terminated in the 2021 wave; their skills are non-portable outside licensed settings, making their trap deeper than the general workforce's despite superior organization.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_healthcare_workers, payer,
    organized, biographical, trapped, national).

% Hold consent-based commitments that the outcome criterion renders inadmissible in rule-setting forums: advisory committees, emergency orders, and employer policy processes take measured benefit as settled and never convene the objection. These advocates enter the conversation only through litigation after rules bind, through repealed-exemption campaigns, and through ballot initiatives years after the fact. No seat exists for them at the moment rules are written.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, philosophical_exemption_advocates, excluded,
    powerless, biographical, trapped, national).

% Adjudicate whether compulsory measures fall within police-powers doctrine, weighing the Jacobson lineage of deference to public-health determinations against liberty and equal-protection claims. Can invalidate particular enforcement instruments and force procedural protections for exemptions, but cannot displace the outcome-measurement standard itself, which enters their dockets as settled science rather than as the contestable legitimacy claim it is.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__public_health_primary, employers_administering_mandates).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns millions of individual immunization and exposure decisions with population-level protection no individual can purchase alone, solving the herd-immunity free-rider problem; secondarily fixes a single decision rule - demonstrated morbidity/mortality reduction - for when a health intervention may be imposed at all.
% TRANSFER_FUNCTION: Moves infection risk away from those who cannot be protected (immunocompromised, elderly) onto mandated individuals who bear vaccination, testing, and enforcement costs; moves decision authority over bodies from individuals to outcome-measuring authorities; the enforcement layer moves income (terminated wages, unpaid suspension) and access (venue entry, travel) from refusers to compliant participants.
% ABSENT_VOICES: Consent-holding refusers - philosophical and religious objectors - are absent from rule-setting and appear only as litigants after rules bind; medical-exemption applicants face criteria narrowed without their participation; populations subjected to historical compulsory campaigns under colonial administration were never consulted at all. Their absence is structural: the externality premise classifies their objection as the thing being regulated rather than as testimony.
% DISAPPEARANCE_RATIONALE: Overnight removal ends employer termination rules and access credentials immediately; coverage drifts downward as complacency and refusal rise; outbreaks concentrate in unvaccinated clusters within seasons; immunocompromised and elderly residents lose indirect protection with no substitute and bear the difference in admissions and deaths; agencies lose their principal imposition instrument while retaining recommendation authority; employers rebuild policies around liability self-protection. Arrangements across employment, commerce, and clinical practice reorganize around the standard's absence.
% FOUNDING_PROBLEM: Dense cities and airborne pathogens create free-rider dynamics in which voluntary immunization stalls below the threshold needed to protect those who cannot be vaccinated; compulsory vaccination under police powers was codified in the smallpox era to close that gap, generalized through school-entry statutes, and scaled to workplaces during pandemic emergencies.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: national vital-statistics offices and WHO surveillance record the mortality collapses following compulsory programs (smallpox, polio, measles); courts attested the founding problem from a non-agency seat beginning with Jacobson v. Massachusetts (1905); hospital-admission datasets document continuing burdens when coverage lapses. The genealogy is unusually well attested independent of the agencies that operate the arrangement today.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.71, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.71 at interval end) because enforcement reaches livelihoods and civic access: termination, exclusion from venues and transport, and credential gates are costs imposed on refusers far beyond the marginal cost of persuasion, though well short of carceral measures. Suppression (0.68) reflects the enforcement machinery itself - employer directives, licensure leverage, checkpoint administration - and is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled in the engine's computation. Theater ratio (0.36) is elevated but bounded: outcome measurement and dose delivery are real functions, while badge displays, attestation rituals, and testing theater layered onto already-vaccinated staff inflate performative activity, especially at the 2021 peak. Accessibility collapse (0.48) is moderate: medical exemptions persist, jurisdiction shopping and remote work softened workplace mandates late in the interval, but exemption criteria narrowed administratively and philosophical exemptions were abolished in much of the mapped space. Resistance (0.62) is substantial - litigation, union pushback, protest, and legislative repeal movements - because the constraint meets organized, articulate opposition rather than passive acquiescence. All three series run on one shared time grid (eight points, 1905-2025) so every metric is authored at every examined time point; the 2021 peak followed by partial 2025 decay models the emergency-wave shape, not a monotonic ratchet assumption.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the immunocompromised patient's position the arrangement is protective coordination she cannot buy for herself; from the terminated worker's position the identical structure is enforced dispossession justified by a metric he was never permitted to contest; from the agency's position it is the faithful application of an evidence standard; from the bench it is a police-powers doctrine question. Professional identity fuses the agenda-setter seat: public-health practitioners' self-concept is constituted through outcome measurement, so challenges to the standard register as attacks on epidemiology itself rather than as contestable legitimacy choices - an institutional identity lock the engine reads through the agency's constrained exit. Suppression here is overwhelmingly structural (legal penalties, employment condition, access denial) with a thinner internalized component (anticipatory self-exclusion by refusers who withdraw before enforcement lands); the omega battery handles residual ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionalities: immunocompromised patients (trapped, powerless) sit nearest the full-beneficiary pole - the constraint subsidizes them with protection they cannot purchase; elderly high-risk residents sit similarly low with slightly more mobility. Payer declarations drive high directionalities: unvaccinated workers (trapped) approach the full-target pole since termination removes income and the mandate web forecloses employer arbitrage; unvaccinated patrons run nearly as high but retain costly substitution; organized healthcare workers carry the highest-cost profile among payers because licensure traps them inside the enforcing institutions themselves. Employers are genuinely dual-positioned - they collect workforce uniformity and liability insulation yet pay attrition and compliance costs - keeping their effective directionality off the extreme beneficiary pole. Public health authorities are agenda setters who also benefit (authority and budget track demonstrated reductions); the derivation places them low but the measurement-self-certification omega flags the capture risk that would raise their d. No directionality overrides are declared: beneficiary/victim declarations plus exit options already produce the correct ordering, and overrides keyed by power atom would wrongly homogenize differentiated seats sharing an atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - free-rider stall below herd-protection thresholds leaving the unprotectable exposed - remains live: pathogens still circulate and thresholds still bind, so the arrangement is not a piton kept alive by inertia, and no sunset clause is authored. The classification guards against symmetric errors. Reading the arrangement as pure extraction (the bodily-autonomy error) erases the real coordination function: measured mortality collapses after compulsory programs are attested by vital-statistics bodies outside the benefiting parties, and the immunocompromised seat's dependence is genuine. Reading it as pure coordination (the agency's own framing) erases the asymmetric enforcement costs borne by a defined victim set through termination and exclusion. Tangled rope holds both truths in one structure: the same mandate that protects the trapped patient extracts the trapped worker's livelihood, and the engine's per-seat computation is what separates those experiences instead of averaging them into a single flattering or damning verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint instantiates only the public_health_primary reading of the legitimate_health_intervention kernel. Do the sibling readings instantiate structurally different constraints (different victim sets, different enforcement surfaces), such that any single-story treatment of the kernel would be mis-specified?',
    'Generate and classify the sibling stories legitimate_health_intervention__bodily_autonomy_primary and legitimate_health_intervention__proportionality_reading; compare victim sets, epsilon values, and computed types across the kernel family.',
    'If the siblings differ structurally as expected, the kernel must remain three linked stories: the bodily-autonomy reading empties the victim set entirely (no imposition is legitimate regardless of measured benefit), and the proportionality reading makes victim membership contingent on threat level. Merging them would average incompatible epsilon values over one label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Kernel-level decomposition across three competing readings of legitimate health intervention.').

omega_variable(
    outcome_measurement_self_certification,
    'Is the population-outcome evidence that confers legitimacy verified by bodies independent of the agencies imposing the interventions, or does the measuring authority effectively certify its own mandates?',
    'Trace the provenance of morbidity/mortality estimates used in mandate determinations; distinguish figures replicated by national statistics offices and academic consortia from agency-only reporting.',
    'Self-certification raises the agenda-setter seat''s effective directionality above the derived beneficiary-end value and pushes classification toward the extractive end; independently verified outcome data strengthens the genuine-coordination component of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outcome_measurement_self_certification, empirical, 'Independence of the measurement that legitimizes imposition.').

omega_variable(
    vector_label_vs_victim_status,
    'Does classifying unvaccinated individuals as disease vectors make them victims of this arrangement, or does their harm flow from the particular enforcement instruments chosen (termination, exclusion) rather than from the outcome-standard itself?',
    'Compare jurisdictions applying the same outcome standard with non-punitive instruments (education campaigns, insurance incentives, paid vaccination leave): if refuser welfare diverges sharply at equal coverage levels, the victim status is instrument-generated.',
    'If instrument-generated, the epsilon attributable to the legitimation standard falls and the enforcement layer carries the costs; if intrinsic to the externality framing, the classification of refusal itself is the mechanism imposing on refusers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vector_label_vs_victim_status, conceptual, 'Whether refuser harm attaches to the standard or to the chosen enforcement tools.').

omega_variable(
    emergency_enforcement_ratchet,
    'Was the 2020-2025 enforcement intensification (employment termination, access credentials) a temporary exercise of emergency authority that decays, or a ratchet establishing durable precedent for peacetime mandates?',
    'Track whether credential infrastructure and employer policy templates persist beyond emergency declarations; monitor legislative sunsets, repeals, and litigation outcomes through the interval end.',
    'A permanent ratchet shifts the trajectory toward sustained high suppression and warrants snare-drift monitoring; decay restores the pre-2020 profile in which compulsion was confined mainly to school-entry statutes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_enforcement_ratchet, empirical, 'Reversibility of the pandemic-era enforcement wave.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 1905, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1905, legitimate_health_intervention__public_health_primary, theater_ratio, 1905, 0.12).
narrative_ontology:measurement(legi_tr_t1940, legitimate_health_intervention__public_health_primary, theater_ratio, 1940, 0.15).
narrative_ontology:measurement(legi_tr_t1960, legitimate_health_intervention__public_health_primary, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(legi_tr_t1980, legitimate_health_intervention__public_health_primary, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(legi_tr_t2000, legitimate_health_intervention__public_health_primary, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(legi_tr_t2015, legitimate_health_intervention__public_health_primary, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(legi_tr_t2021, legitimate_health_intervention__public_health_primary, theater_ratio, 2021, 0.42).
narrative_ontology:measurement(legi_tr_t2025, legitimate_health_intervention__public_health_primary, theater_ratio, 2025, 0.36).

% Extraction over time
narrative_ontology:measurement(legi_be_t1905, legitimate_health_intervention__public_health_primary, base_extractiveness, 1905, 0.42).
narrative_ontology:measurement(legi_be_t1940, legitimate_health_intervention__public_health_primary, base_extractiveness, 1940, 0.5).
narrative_ontology:measurement(legi_be_t1960, legitimate_health_intervention__public_health_primary, base_extractiveness, 1960, 0.54).
narrative_ontology:measurement(legi_be_t1980, legitimate_health_intervention__public_health_primary, base_extractiveness, 1980, 0.56).
narrative_ontology:measurement(legi_be_t2000, legitimate_health_intervention__public_health_primary, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(legi_be_t2015, legitimate_health_intervention__public_health_primary, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(legi_be_t2021, legitimate_health_intervention__public_health_primary, base_extractiveness, 2021, 0.76).
narrative_ontology:measurement(legi_be_t2025, legitimate_health_intervention__public_health_primary, base_extractiveness, 2025, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1905, legitimate_health_intervention__public_health_primary, suppression_requirement, 1905, 0.3).
narrative_ontology:measurement(legi_su_t1940, legitimate_health_intervention__public_health_primary, suppression_requirement, 1940, 0.38).
narrative_ontology:measurement(legi_su_t1960, legitimate_health_intervention__public_health_primary, suppression_requirement, 1960, 0.44).
narrative_ontology:measurement(legi_su_t1980, legitimate_health_intervention__public_health_primary, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement(legi_su_t2000, legitimate_health_intervention__public_health_primary, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(legi_su_t2015, legitimate_health_intervention__public_health_primary, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(legi_su_t2021, legitimate_health_intervention__public_health_primary, suppression_requirement, 2021, 0.8).
narrative_ontology:measurement(legi_su_t2025, legitimate_health_intervention__public_health_primary, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'legitimate health intervention' covers three structurally distinct claims about what grounds legitimacy, and forcing them into one story would make epsilon observable-dependent. This file (public_health_primary) authors epsilon for the outcome-based legitimation regime with the unvaccinated in the victim set as vectors and enforcement machinery (termination, exclusion) generating high measured extraction. The sibling bodily_autonomy_primary authors epsilon for a consent-governed regime in which the refuser victim set is empty and coercion is illegitimate regardless of outcome; the sibling proportionality_reading authors epsilon for a threat-indexed regime in which victim membership varies with disease characteristics. Upstream/downstream structure: the outcome-measurement paradigm constituted by this reading defines the threat axis the proportionality reading weighs (declared as influences), while the consent premise of bodily_autonomy_primary contradicts this reading's externality premise so sharply that no single framework holds both (declared as forecloses).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
