% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: Public-Health-Primary Reading of Compelled Medical Intervention
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This file instantiates the public_health_primary reading of the
 *   coercion_legitimacy_boundary kernel: the standing arrangement under
 *   contest is the state's authority to compel medical intervention — in
 *   practice, compulsory immunization schedules backed by school-entry laws,
 *   employment conditions, exclusion penalties, and fines — evaluated by this
 *   reading's own lights, namely that compulsion is legitimate when
 *   collective harm-prevention outweighs individual autonomy. The epsilon
 *   referent is that standing compulsion arrangement, never the consent-based
 *   alternative this reading declines to adopt. The expected structural delta
 *   is realized in the authored data: unvaccinated refusers sit in the victim
 *   set as coerced payers, immunocompromised patients and below-age infants
 *   sit in the beneficiary set as protected parties, and the enforcement
 *   apparatus contributes substantially to measured extraction. The sibling
 *   readings (bodily_autonomy_primary, proportionality_reading) are separate
 *   constraint files with their own epsilon, victim sets, and
 *   classifications; they are linked, not folded in. KEY AGENTS (by
 *   structural relationship): - public_health_authorities: agenda-setter and
 *   administrative collector (institutional/constrained) — runs the apparatus
 *   and absorbs its backlash - state_legislatures: co-agenda-setter
 *   (institutional/constrained) — sets exemption scope under electoral
 *   pressure - immunocompromised_patients and vaccine_below_age_infants:
 *   protected beneficiaries (powerless/trapped) — receive what they cannot
 *   procure themselves - vaccinated_general_public: compliant beneficiary
 *   with incidental payer position (moderate/constrained) -
 *   unvaccinated_conscientious_objectors: primary payer
 *   (moderate/identity_locked) - healthcare_workers_under_mandates: payer
 *   with organized leverage (organized/constrained) -
 *   school_age_children_of_refusing_households: payer without voice
 *   (powerless/trapped) - constitutional_reviewers: analytical observer
 *   (institutional/analytical)
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda-setter (institutional/constrained) — administers requirements, collects compliance and budget, absorbs litigation and backlash
 *   - state_legislatures: co-agenda-setter (institutional/constrained) — sets and repeals exemption scope under cross-pressured electorate
 *   - immunocompromised_patients: protected beneficiary (powerless/trapped) — relies wholly on community coverage
 *   - vaccine_below_age_infants: protected beneficiary (powerless/trapped) — pre-eligibility shield depends on others
 *   - vaccinated_general_public: beneficiary with secondary payer position (moderate/constrained) — complies, funds, and generates the shared protection
 *   - unvaccinated_conscientious_objectors: primary payer (moderate/identity_locked) — bears exclusion and penalties; refusal fused with religious or worldview identity
 *   - healthcare_workers_under_mandates: payer (organized/constrained) — employment conditioned on immunization; exit costs a career
 *   - school_age_children_of_refusing_households: payer without voice (powerless/trapped) — bears classroom exclusion chosen by guardians
 *   - constitutional_reviewers: analytical observer (institutional/analytical) — polices the boundary of the police power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.63).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.7).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.63).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "Public-Health-Primary Reading of Compelled Medical Intervention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, 'f02e31c6-5161-4661-aec5-a2853bbc5887').
narrative_ontology:cs_kernel_codification('f02e31c6-5161-4661-aec5-a2853bbc5887', formalized).
narrative_ontology:cs_authority_grounding('f02e31c6-5161-4661-aec5-a2853bbc5887', lineage).
narrative_ontology:cs_interpretation_layer_present('f02e31c6-5161-4661-aec5-a2853bbc5887').
narrative_ontology:cs_reading_relation('f02e31c6-5161-4661-aec5-a2853bbc5887', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('f02e31c6-5161-4661-aec5-a2853bbc5887', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('f02e31c6-5161-4661-aec5-a2853bbc5887', foundational, collective_harm_prevention_outweighs_autonomy_when_balanced).
narrative_ontology:cs_axiom_status(collective_harm_prevention_outweighs_autonomy_when_balanced, holdable).
narrative_ontology:cs_axiom_grounding('f02e31c6-5161-4661-aec5-a2853bbc5887', collective_harm_prevention_outweighs_autonomy_when_balanced, instrumental).
narrative_ontology:cs_axiom('f02e31c6-5161-4661-aec5-a2853bbc5887', secondary, police_power_reaches_compulsory_medical_regulation).
narrative_ontology:cs_axiom_status(police_power_reaches_compulsory_medical_regulation, holdable).
narrative_ontology:cs_axiom_grounding('f02e31c6-5161-4661-aec5-a2853bbc5887', police_power_reaches_compulsory_medical_regulation, conventional).
narrative_ontology:cs_reference_frame('f02e31c6-5161-4661-aec5-a2853bbc5887', jacobson_police_power_deference).
narrative_ontology:cs_drift_state('f02e31c6-5161-4661-aec5-a2853bbc5887', post_covid_mandate_litigation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f02e31c6-5161-4661-aec5-a2853bbc5887', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, vaccine_below_age_infants).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, elderly_chronic_condition_adults).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, vaccinated_general_public).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_conscientious_objectors).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, healthcare_workers_under_mandates).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, school_age_children_of_refusing_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, vaccinated_general_public).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, jacobson_police_power_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, herd_immunity_threshold_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the recommended and required immunization schedule, set school-entry and workplace requirements, adjudicate exemption requests, and operate the disease-surveillance systems that document coverage and trigger outbreak response. Administering the requirement apparatus expands their remit, staffing, and budget; they also absorb litigation risk and political backlash whenever requirements tighten.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Enact, narrow, and repeal exemption statutes and the scope of required immunization. They answer simultaneously to constituents who want requirements strengthened and constituents who want exemptions restored, so their electoral exposure rises sharply with every tightening bill.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, state_legislatures, agenda_setter,
    institutional, biographical, constrained, regional).

% Cannot safely receive certain vaccines and depend on the immunity of the people around them for protection. They have no substitute for community coverage and no personal lever over anyone else's vaccination decision.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).

% Are too young for scheduled doses and are protected entirely by the coverage of everyone around them until they age into eligibility.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, vaccine_below_age_infants, beneficiary,
    powerless, immediate, trapped, national).

% Experience reduced exposure risk when community coverage is high. They can shield themselves further through isolation, but only at a severe cost to daily life.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, elderly_chronic_condition_adults, beneficiary,
    moderate, biographical, constrained, regional).

% Comply with schedules at modest personal cost and inconvenience, receive direct protection, and fund the enforcement apparatus through taxation. Their aggregate compliance is what produces the community protection every other seat rides on.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, vaccinated_general_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, vaccinated_general_public, payer).

% Decline scheduled vaccines on religious or philosophical grounds and face school exclusion for their children, employment barriers, and fines depending on jurisdiction. Their refusal is typically bound up with religious membership or a settled worldview about bodily integrity, so complying would carry costs well beyond the injection itself.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_conscientious_objectors, payer,
    moderate, biographical, identity_locked, national).

% Work under employer or state rules tying continued employment to up-to-date immunization. Refusal means termination or reassignment; leaving the sector sacrifices training, seniority, and income accumulated over a career.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, healthcare_workers_under_mandates, payer,
    organized, biographical, constrained, national).

% Bear classroom exclusion when their households decline required doses. They choose nothing in this process and appear in it only as subjects of their guardians' decisions.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, school_age_children_of_refusing_households, payer,
    powerless, immediate, trapped, local).

% Courts applying the century-old precedent upholding compulsory health measures review whether specific requirements remain within the police power, hear challenges from refused households and terminated workers, and can strike requirements they find to exceed necessity.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, constitutional_reviewers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in community immunity: each individual's protection depends on aggregate coverage crossing a threshold, so every person prefers that others participate while weighing their own abstention. Compulsory schedules align individual incentives with the threshold requirement and are administered once, centrally, rather than negotiated household by household.
% TRANSFER_FUNCTION: Moves decision authority over one's own body from individuals to state health bureaucracies; moves the burden of maintaining coverage onto those who would decline it, in the form of compelled injection or exclusion penalties; and moves residual disease risk away from the immunologically vulnerable onto whatever pockets of abstention remain.
% ABSENT_VOICES: Severely disabled adults in institutional settings who cannot self-advocate about their own exemption claims; residents of neighboring jurisdictions who bear cross-border outbreak risk from another state's coverage gaps; and future cohorts who will inherit whichever exemption settlement is enacted now. Categorical-autonomy objections are voiced loudly in the present conversation but are structurally outweighted rather than absent — the seats that never enter the room are the ones with no vote at all.
% DISAPPEARANCE_RATIONALE: If the compulsion apparatus vanished overnight, coverage would drift downward as abstention spread, documented outbreak patterns would resume in under-covered communities, the immunocompromised and below-age infants would lose the protection shield they cannot purchase any other way, and schools, employers, and health departments would rebuild admission and screening rules piecemeal. The arrangement's disappearance forces a large-scale reorganization of institutional admission and the risk distribution among its seats.
% FOUNDING_PROBLEM: Epidemic diseases such as smallpox, measles, and polio killed and maimed at scale, and voluntary uptake repeatedly fell short of the coverage needed to interrupt transmission, so early twentieth-century public health sought a legal instrument to compel participation.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiological surveillance outside the benefiting parties attests the founding problem's persistence: WHO European-region measles data and academic outbreak investigations document resurgent transmission wherever coverage slips below threshold (for example, after the UK MMR scare and in post-Soviet states). Courts in the Jacobson lineage accept the problem's reality while policing the scope of the response. Refuser communities dispute the magnitude and the remedy, not the existence of epidemic disease.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.63 because the burden concentrates on a minority seat that did not consent: refusers face compelled injection or exclusion penalties, and the burden is decoupled from anything they receive. Suppression is 0.70 because persistence depends on actively closing exits — non-medical exemptions have been repealed in whole jurisdictions, school exclusion is mechanically enforced, and employment conditioning removes the workplace alternative; suppression here is structural (legal penalties and closed channels), not internalized. Theater is 0.28: the preventive function is real and measurable in coverage and outbreak statistics, but a growing share of activity is documentation ritual — exemption paperwork, compliance audits, and enforcement ceremonies that defend the requirement itself rather than produce additional immunity. Accessibility_collapse is 0.45 because alternatives survive at real cost: medical exemptions (narrowly administered), homeschooling, private-school variation, relocation, and remote work all remain reachable, so understanding the arrangement does not eliminate exit the way a natural limit would. Resistance is 0.58: sustained litigation, legislative exemption fights, and organized refusal movements meet every tightening step. Claim/metric independence: claimed_type=tangled_rope is my structural belief — the arrangement solves a genuine collective-action problem (threshold coverage against free-riding) while imposing asymmetric burdens through active enforcement — and the metrics are authored independently as descriptive values; the engine computes per-seat types from the structural data and any divergence between my claim and a computed seat type is signal, not error. The three measurement series share one time grid (t=0..120, roughly 1905-2025, anchored at Jacobson, school-law diffusion, schedule institutionalization, mass campaigns, exemption-regime maturity, the 2015-16 exemption repeals, and the COVID-era employment mandates), so every metric is authored at every examined point. The suppression_requirement series is authored deliberately: the story traces enforcement-capacity change (school-law spread, exemption repeal, pandemic-era mandates), which is exactly the dynamic that scalar suppression alone cannot carry. The trajectories are monotonic ratchets with a visible step at exemption repeal — no oscillation, so no cyclical-measurement machinery is invoked.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data is built to make them do so. From the payer seats, the arrangement presents as imposed loss with suppressed exits: the identity-locked objector experiences compulsion as a direct claim on the body, the mandated worker as a condition of livelihood, the excluded child as a lost classroom — each sits near the full-target end of directionality, and their computed type should read far harsher than the story-level claim. From the protected beneficiary seats, the same structure presents as a subsidy they cannot buy anywhere else: the immunocompromised patient and the below-age infant receive protection produced entirely by other people's compliance, sitting near the beneficiary end where effective extraction inverts toward net gain. The agenda-setter seat experiences the arrangement as a functioning program it administers and defends. The engine derives these divergences from roles, power, and exit options; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the four protected seats — lowest for the trapped ones (immunocompromised patients, below-age infants), who have zero arbitrage and maximal dependence. Payer declarations drive high directionality for the three burden-bearing seats, amplified by exit posture: the identity-locked objector sits nearest the full-target end (refusal fused with religious and worldview identity makes compliance costlier than the penalty in subjective terms), the organized healthcare worker slightly lower (real but expensive exit through career change), the excluded child highest in helplessness though lowest in agency. The vaccinated public derives near-beneficiary directionality with a small upward pull from its secondary payer position (taxation and residual risk-bearing). Public health authorities and legislatures derive low-to-moderate directionality as agenda-setters who collect compliance and budget but also absorb backlash, litigation risk, and electoral exposure — they are not pure collectors. Constitutional reviewers sit at the analytical pole and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reaching coverage thresholds against free-riding — remains live, corroborated by outbreak resurgence wherever coverage slips, so this is not a mandate outliving its function and mandatrophy is not resolved. The classification matters in both directions. Reading the arrangement as pure extraction (snare) would erase the demonstrated coordination output: sustained high coverage produced real periods of endemic-disease elimination, and the protected seats are genuine net beneficiaries, not cover for a rent-collecting scheme alone. Reading it as pure coordination (rope) would erase the asymmetric structure: a identifiable minority bears compelled intervention or exclusion while the majority rides their compliance, exits are legislatively narrowed, and the enforcement apparatus itself accretes budget and remit. Tangled rope holds both halves — genuine coordination function, asymmetric extraction, active enforcement — and the temporal series shows the extraction half thickening over the interval (rising base_extractiveness and suppression_requirement) without the coordination half disappearing, which is the signature the lifecycle detectors should see.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the coercion_legitimacy_boundary kernel — what would change structurally if a sibling reading were instantiated instead?',
    'Compare the compiled sibling files directly: bodily_autonomy_primary removes the coerced-payer seats entirely (no compulsion, no victims of compulsion) and drops epsilon toward the coordination-cost floor; proportionality_reading indexes the victim set and epsilon to pathogen severity, shrinking the payer seat in low-severity seasons and restoring it in high-severity ones.',
    'Adopting bodily_autonomy_primary dissolves this reading''s victim set and its enforcement-driven extraction; adopting proportionality_reading makes epsilon pathogen-indexed rather than standing, changing the classification seasonally. The disagreement is located in the permissibility predicate itself: categorical prohibition versus conditional permission versus severity-scaled permission.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: which reading of the coercion-legitimacy kernel this file instantiates and what siblings would alter.').

omega_variable(
    balancing_test_indeterminacy,
    'Who performs the ''outweighs'' weighing, and with what metric — aggregated health outcomes, rights-side constraints, or some institutional judgment that is neither?',
    'Doctrinal analysis of how reviewing courts operationalize the balance (strict scrutiny availability, deference standards) combined with comparison of jurisdictions that weight the autonomy side differently at similar disease burdens.',
    'A QALY-style aggregation moves the balance point toward more compulsion and higher effective extraction on refusers; a side-constraint weighting moves it toward fewer compellable interventions and shrinks the victim set. The indeterminacy is the reading''s principal internal soft spot.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_indeterminacy, conceptual, 'Indeterminacy in the weighing procedure that licenses compulsion under this reading.').

omega_variable(
    exemption_administration_honesty,
    'Are medical-exemption denials driven by genuine clinical criteria or by enforcement-driven narrowing that converts a safety valve into a formality?',
    'Audit denial rates against published clinical contraindication criteria across jurisdictions with different exemption statutes; natural experiment from states that repealed non-medical exemptions versus neighbors that retained them.',
    'If denials track clinical criteria, the suppression measure reflects necessary enforcement of a real threshold; if denials systematically exceed clinical contraindication, part of the measured suppression is extraction creep dressed as rigor, and the payer seat is larger than the statute admits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_administration_honesty, empirical, 'Whether the exemption channel functions as a genuine valve or as suppressed exit.').

omega_variable(
    payer_seat_composition,
    'What fraction of the unvaccinated payer seat is convenience free-riding versus sincere conviction, and does the mix differ across jurisdictions and pathogens?',
    'Survey and behavioral data distinguishing access-barrier abstention from principled refusal; longitudinal tracking of households that vaccinate once friction (cost, scheduling) is removed.',
    'A high free-rider share supports the coordination framing — the payer seat is mostly defectors against a fair-share scheme, and compulsion is burden-reallocation. A high sincere-conviction share strengthens the minority-rights reading — the payer seat is a persecuted conscience group, and effective extraction on it is morally heavier even at identical arithmetic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payer_seat_composition, empirical, 'Composition of the coerced seat: defectors versus dissenters.').

omega_variable(
    deference_erosion_trajectory,
    'Will the post-COVID judicial narrowing of mandate deference consolidate into a durably smaller compulsion envelope, or reverse as outbreak memory fades?',
    'Track appellate rulings on mandate scope, exemption-repeal survival, and employment-conditioning cases over the next decade; compare the accepted scope of compulsion before and after the pandemic litigation wave.',
    'Consolidated erosion shrinks the enforcement apparatus, lowering suppression and effective extraction on payer seats; reversal restores the fuller envelope and confirms the rising suppression trajectory authored in the measurement series.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deference_erosion_trajectory, empirical, 'Durability of the drift away from the Jacobson-era deference reference frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clb_pubhealth_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(clb_pubhealth_tr_t0, observed).
narrative_ontology:measurement(clb_pubhealth_tr_t20, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(clb_pubhealth_tr_t20, observed).
narrative_ontology:measurement(clb_pubhealth_tr_t40, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 40, 0.17).
narrative_ontology:measurement_basis(clb_pubhealth_tr_t40, observed).
narrative_ontology:measurement(clb_pubhealth_tr_t60, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(clb_pubhealth_tr_t60, observed).
narrative_ontology:measurement(clb_pubhealth_tr_t80, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 80, 0.21).
narrative_ontology:measurement_basis(clb_pubhealth_tr_t80, observed).
narrative_ontology:measurement(clb_pubhealth_tr_t100, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 100, 0.23).
narrative_ontology:measurement_basis(clb_pubhealth_tr_t100, observed).
narrative_ontology:measurement(clb_pubhealth_tr_t110, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 110, 0.26).
narrative_ontology:measurement_basis(clb_pubhealth_tr_t110, observed).
narrative_ontology:measurement(clb_pubhealth_tr_t120, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 120, 0.28).
narrative_ontology:measurement_basis(clb_pubhealth_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(clb_pubhealth_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(clb_pubhealth_be_t0, observed).
narrative_ontology:measurement(clb_pubhealth_be_t20, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(clb_pubhealth_be_t20, observed).
narrative_ontology:measurement(clb_pubhealth_be_t40, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(clb_pubhealth_be_t40, observed).
narrative_ontology:measurement(clb_pubhealth_be_t60, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 60, 0.53).
narrative_ontology:measurement_basis(clb_pubhealth_be_t60, observed).
narrative_ontology:measurement(clb_pubhealth_be_t80, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 80, 0.56).
narrative_ontology:measurement_basis(clb_pubhealth_be_t80, observed).
narrative_ontology:measurement(clb_pubhealth_be_t100, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 100, 0.59).
narrative_ontology:measurement_basis(clb_pubhealth_be_t100, observed).
narrative_ontology:measurement(clb_pubhealth_be_t110, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 110, 0.61).
narrative_ontology:measurement_basis(clb_pubhealth_be_t110, observed).
narrative_ontology:measurement(clb_pubhealth_be_t120, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 120, 0.63).
narrative_ontology:measurement_basis(clb_pubhealth_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(clb_pubhealth_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(clb_pubhealth_su_t0, observed).
narrative_ontology:measurement(clb_pubhealth_su_t20, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 20, 0.47).
narrative_ontology:measurement_basis(clb_pubhealth_su_t20, observed).
narrative_ontology:measurement(clb_pubhealth_su_t40, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(clb_pubhealth_su_t40, observed).
narrative_ontology:measurement(clb_pubhealth_su_t60, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 60, 0.54).
narrative_ontology:measurement_basis(clb_pubhealth_su_t60, observed).
narrative_ontology:measurement(clb_pubhealth_su_t80, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 80, 0.58).
narrative_ontology:measurement_basis(clb_pubhealth_su_t80, observed).
narrative_ontology:measurement(clb_pubhealth_su_t100, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 100, 0.6).
narrative_ontology:measurement_basis(clb_pubhealth_su_t100, observed).
narrative_ontology:measurement(clb_pubhealth_su_t110, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 110, 0.66).
narrative_ontology:measurement_basis(clb_pubhealth_su_t110, observed).
narrative_ontology:measurement(clb_pubhealth_su_t120, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 120, 0.7).
narrative_ontology:measurement_basis(clb_pubhealth_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'state coercion of medical intervention' covers three structurally distinct claims and is decomposed into a three-file kernel family. This file (public_health_primary) authors the standing compulsion arrangement with a fixed victim set (refusers, mandated workers, excluded children) and enforcement-driven epsilon. bodily_autonomy_primary authors the categorical-prohibition arrangement, whose victim set under compulsion is empty by construction. proportionality_reading authors the severity-indexed arrangement, whose victim set expands and contracts with pathogen characteristics. The upstream reading (bodily_autonomy_primary) supplies the autonomy baseline that the other two weigh against; this reading influences the proportionality reading by supplying the balancing logic that severity-scaling refines. Each file carries its own epsilon, beneficiaries, and victims; no file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
