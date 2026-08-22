% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: State Vaccination Mandate Authority (Public Health Primary Reading)
 *   domain: public_health/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint instantiates the public-health-primary reading of the
 *   contested mandate-legitimacy-scope kernel. The reading asserts that state
 *   authority to compel vaccination is legitimate when necessary to protect
 *   vulnerable populations (immunocompromised, infants, severely allergic)
 *   who cannot protect themselves. Under this reading, the vulnerability of
 *   those dependent on herd immunity creates a duty in others to accept
 *   vaccination. The constraint operates by excluding unvaccinated
 *   individuals from environments where they pose transmission risk to
 *   vulnerable people. The reading authorizes state coercion justified by the
 *   protection of the powerless; sibling readings (bodily-autonomy-primary,
 *   proportionality-reading) prioritize individual consent and
 *   least-restrictive-means, respectively. This story models the constraint
 *   as its public-health-primary instantiation authors it: high extraction
 *   from the payers (vaccine-hesitant, those with side effects, religious
 *   objectors), genuine coordination benefit for the vulnerable, active state
 *   enforcement. The claim/metric relationship is intentional: the reading
 *   CLAIMS tangled-rope (genuine coordination function + asymmetric
 *   extraction + enforcement) while the authored metrics show the extraction
 *   is substantial and rising through early interval, then plateauing — that
 *   divergence is exactly what the engine measures.
 *
 * KEY AGENTS:
 *   - public_health_authority: institutional agenda-setter — sets mandate scope, defines 'necessary,' enforces exclusion
 *   - immunocompromised_populations: powerless beneficiaries — trapped, entirely dependent on herd immunity
 *   - vaccine_hesitant_individuals: moderate-power payers — constrained exit, forced compliance or occupational exclusion
 *   - individuals_with_vaccine_side_effects: powerless payers — identity-locked, bear physical harm cost
 *   - religious_objectors: organized payers — constrained exit, moral framework subordinated
 *   - courts_and_legislatures: institutional observers — review constitutional legitimacy and necessity claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.62).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.71).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "State Vaccination Mandate Authority (Public Health Primary Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, '75c65a20-83a9-4066-a75f-ef31fb208d88').
narrative_ontology:cs_kernel_codification('75c65a20-83a9-4066-a75f-ef31fb208d88', fixed_text).
narrative_ontology:cs_authority_grounding('75c65a20-83a9-4066-a75f-ef31fb208d88', lineage).
narrative_ontology:cs_interpretation_layer_present('75c65a20-83a9-4066-a75f-ef31fb208d88').
narrative_ontology:cs_reading_relation('75c65a20-83a9-4066-a75f-ef31fb208d88', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('75c65a20-83a9-4066-a75f-ef31fb208d88', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('75c65a20-83a9-4066-a75f-ef31fb208d88', foundational, vulnerable_protection_duty_overrides_consent).
narrative_ontology:cs_axiom_status(vulnerable_protection_duty_overrides_consent, holdable).
narrative_ontology:cs_axiom_grounding('75c65a20-83a9-4066-a75f-ef31fb208d88', vulnerable_protection_duty_overrides_consent, deontological).
narrative_ontology:cs_axiom('75c65a20-83a9-4066-a75f-ef31fb208d88', foundational, state_police_power_legitimate_for_public_health).
narrative_ontology:cs_axiom_status(state_police_power_legitimate_for_public_health, holdable).
narrative_ontology:cs_axiom_grounding('75c65a20-83a9-4066-a75f-ef31fb208d88', state_police_power_legitimate_for_public_health, conventional).
narrative_ontology:cs_reference_frame('75c65a20-83a9-4066-a75f-ef31fb208d88', constitutional_public_health_authority).
narrative_ontology:cs_drift_state('75c65a20-83a9-4066-a75f-ef31fb208d88', contemporary_high_income_low_disease_prevalence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('75c65a20-83a9-4066-a75f-ef31fb208d88', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, infants_ineligible_for_vaccine).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, severely_allergic_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, individuals_with_vaccine_side_effects).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, religious_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces vaccination mandates based on epidemiological assessment of disease severity and vaccine efficacy. Justifies the mandate as protecting vulnerable populations who cannot access vaccination or mount immune response. Exercises police power to exclude unvaccinated individuals from public spaces, schools, and employment where disease transmission risk to vulnerable populations is high.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Cannot mount effective immune response to vaccination and depend entirely on herd immunity for protection from serious disease. Without mandatory vaccination of the general population, they face severe isolation or life-threatening infection risk. Their protection is achieved through others' compliance, not their own choice.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Too young to receive vaccination themselves and depend on herd immunity from vaccinated caregivers and community. Serious infection during critical developmental window carries risk of permanent harm. Their protection requires near-universal vaccination among adults in their environment.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, infants_ineligible_for_vaccine, beneficiary,
    powerless, biographical, trapped, national).

% Cannot safely receive the vaccine due to documented anaphylactic reaction to vaccine components and depend on others' vaccination for protection. Have medical documentation of contraindication but still face disease exposure. Their safety requires others to absorb the vaccination duty.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, severely_allergic_individuals, beneficiary,
    moderate, biographical, constrained, national).

% Subject to mandate requirement despite philosophical or informational objections to the vaccine. Face exclusion from employment, education, and public accommodations if they refuse. Their options are submission to vaccination against their expressed preference, geographic relocation, or accepting occupational/social deprivation. The mandate compels action against their stated will.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_individuals, payer,
    moderate, biographical, constrained, national).

% Experience serious adverse reactions (myocarditis, thrombosis, neurological effects) from vaccination and bear the health costs directly. The mandate's justification (protecting vulnerable populations) does not weight their individual harm. Their professional identity, family participation, and social standing become conditional on accepting physical risk they have experienced.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, individuals_with_vaccine_side_effects, payer,
    powerless, biographical, identity_locked, national).

% Hold sincere religious or conscience-based objections to vaccination and organized to seek exemptions. The mandate treats their objection as less weighty than public health benefit and enforces compliance through occupational and educational exclusion. Their moral framework and community practices are subordinated to the state's assessment of collective benefit.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, religious_objectors, payer,
    organized, biographical, constrained, national).

% Assess vaccine safety and efficacy data independently and advise on individual risk-benefit calculations. They observe the mandate operating at both population level (epidemiology) and individual level (clinical contraindications), creating tension between two medical obligations. They document and report adverse reactions but their role is attestation, not authority over mandate policy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, medical_professionals, observer,
    institutional, generational, analytical, national).

% Review the mandate's constitutional legitimacy under public health exception doctrines (Jacobson framework in US context). They weigh collective benefit against individual bodily autonomy claims and can overturn or limit the mandate through injunction or statute. Their evidentiary threshold for 'necessary to protect vulnerable populations' becomes the operational constraint definition.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__public_health_primary, public_health_authority).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves herd immunity threshold that protects populations unable to mount immune response themselves (infants, severely immunocompromised) from preventable serious disease. Solves the coordination problem that individual vaccination choice yields sub-optimal collective protection for the most vulnerable — each person's decision to vaccinate or not carries externality to those dependent on herd immunity.
% TRANSFER_FUNCTION: Transfers bodily autonomy compliance burden from those who cannot be harmed by disease to those who can mount immune response, in exchange for protection of the vulnerable. The unvaccinated bear involuntary compliance cost (injection, infection risk shift if vaccine efficacy is incomplete) to subsidize the safety of the immunocompromised. Those with documented contraindication are exempted from the duty but others are not.
% ABSENT_VOICES: Individuals who would have serious adverse reactions to vaccination but do not yet know it (population-level adverse event discovery happens post-mandate); parents who would choose infection-acquired immunity for their children if free to decide; health care workers and others with prior natural immunity from infection who view re-vaccination as medically unnecessary. These voices are structurally excluded from the evidence base the mandate rests on — they would dispute the necessity claim but are not present in the authority structure that determines 'necessary.'
% DISAPPEARANCE_RATIONALE: If the mandate disappeared, vaccination rates would drop substantially (historical precedent: measles vaccination rates fall 10-30% when mandates are removed). Immunocompromised populations would face increased infection risk and some would require protective isolation. Disease incidence in vulnerable populations would rise. Occupational and educational access would reorganize around voluntary vaccination rates. The mandate's absence would directly alter the feasibility set for vulnerable-population protection.
% FOUNDING_PROBLEM: Serious communicable diseases (measles, polio, pertussis, diphtheria) in the pre-vaccine era killed or permanently disabled hundreds of thousands, with concentrated mortality among infants and vulnerable populations. The founding problem is: how to achieve population-level immunity to protect those biologically unable to protect themselves, in the absence of individual incentives to vaccinate when disease becomes rare.
% FOUNDING_PROBLEM_CORROBORATION: The public health authority attests the founding problem is still live, citing disease persistence in under-vaccinated communities (2019 measles outbreaks, ongoing pertussis circulation, polio in under-vaccinated regions). Independent epidemiologists and disease surveillance data corroborate that disease does not disappear and that immunocompromised individuals remain at risk. However, courts and bodily-autonomy advocates contest whether the severity in contemporary high-income settings justifies mandate-level coercion — the disease-threat severity has diminished compared to the founding era, whereas the mandate's coercive scope has remained constant or increased.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the burden on payers (vaccine-hesitant, side-effect-vulnerable, religious objectors) who absorb vaccination duty against their preference. At interval start (0.48) the metric is moderate — resistance is still strong and mandate enforcement is not yet universally applied. By t=24 it rises to 0.62 as enforcement hardens (employment mandates, school exclusions become systematic), then plateaus at t=32-40 as accommodation pathways (medical exemptions, religious exemptions in some jurisdictions) partially stabilize the burden. Suppression is high (0.71 at endpoint) because the mandate constrains exit severely: refusal means occupational deprivation, educational exclusion, or relocation. Theater rises modestly (0.18 to 0.28) because enforcement increasingly includes secondary performative elements (proof requirements, ongoing compliance documentation) alongside the core mandate function. Accessibility collapse is moderate (0.68) because alternatives exist (geographic mobility to lower-mandate jurisdictions, occupational pivots) but are costly and unavailable to many — the constraint does not foreclose alternatives entirely but renders them prohibitively expensive. Resistance is high (0.74) because substantial organized opposition persists throughout the interval from religious groups, bodily-autonomy advocates, and medical professionals concerned about one-size-fits-all policy. The tangled-rope claim holds because the constraint simultaneously coordinates a genuine public health problem (protection of the immunocompromised) AND extracts compliance from a large population whose preference is not coordinated with that problem. The two elements are inseparable: you cannot achieve the coordination without the extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the public-health-authority seat, the constraint is legitimate coordination: we are solving a real problem (protecting the vulnerable) through the minimal coercive means available (population-level immunity). From the vaccine-hesitant and side-effect-bearing seats, the constraint is coerced extraction: we are being forced to absorb a medical intervention against our preference to subsidize protection of others, with no reciprocal obligation on those others to do anything. From the immunocompromised seat, the constraint is just protection: we cannot protect ourselves and this is the only mechanism that works. These three positions are mutually incoherent without resolving the deeper question: does the vulnerability of the immunocompromised create an enforceable duty in others? The reading answers yes; the bodily-autonomy-primary reading answers no; the proportionality-reading answers 'only if disease severity, vaccine safety, and availability of less restrictive means all justify it.' The engine computes each seat's type independently from the structural data — this is how the three readings can instantiate three different constraints with three different seat-level classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Public-health-authority sits at d≈0.1 (beneficiary-side): it administers the mandate, maintains authority over exemption criteria, and collects legitimacy/political capital from disease prevention success. Immunocompromised populations sit at d≈0.05-0.10 (pure beneficiary): they receive protection without bearing compliance cost — the constraint subsidizes their safety. Vaccine-hesitant individuals sit at d≈0.85-0.95 (pure target): they bear the full compliance duty against their stated preference and have constrained exit. Individuals with side effects sit at d≈0.90 (target): they bear physical harm and identity-locked exit (cannot step out of their professional/family roles without cascading consequences). Religious objectors sit at d≈0.80 (target): they bear moral subordination and occupational exclusion. The engine derives these automatically from beneficiary/victim declarations + exit options; the marked divergence between beneficiary-seat directionality (low) and payer-seat directionality (high) is the perpspectival gap the mandate creates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (serious communicable disease threats to vulnerable populations) is live in the public-health-authority's attestation and supported by epidemiological data from under-vaccinated communities. However, courts and bodily-autonomy advocates dispute whether the threat level justifies mandate-scope coercion in contemporary high-income settings where disease prevalence is orders of magnitude lower than the founding era. The constraint shows modest mandatrophy pressure (theater rising from 0.18 to 0.28) as accommodation pathways (exemptions, exclusion-based rather than mandate-based protection) begin to substitute for direct enforcement. The tangled-rope classification prevents mislabeling: the coordination function is real (herd immunity does protect the vulnerable) but is inseparable from extraction (the duty is imposed on those who did not consent to it). A classification as 'rope' (pure coordination) would be false; a classification as 'snare' (pure extraction) would miss the genuine coordination element. Tangled rope captures both — the legitimacy question is whether the coordination is valuable enough to justify the extraction, not whether the extraction exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_severity_drift,
    'As disease prevalence falls due to successful vaccination, does the necessity justification for mandate-scope coercion remain stable, decline proportionally, or become subject to reinterpretation by the authority administering it?',
    'Longitudinal tracking of public health authority statements about mandate scope relative to disease severity metrics; court rulings on proportionality challenges; comparative analysis of mandate scope in different disease prevalence regimes.',
    'If necessity declines with disease prevalence but mandate scope is maintained or expanded, the constraint transitions toward snare (mandate persists by institutional inertia rather than live problem). If necessity-linked, the extraction metrics should track disease severity. Currently contested: are we in mandatrophy (founding problem solved, mandate persists), legitimate-adaptation (scope narrowing as necessity changes), or extraction-disguised-as-coordination?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_severity_drift, empirical, 'Whether mandate scope tracks the necessity condition or persists independently of it.').

omega_variable(
    duty_transfer_legitimacy,
    'Does the vulnerability of the immunocompromised create an enforceable duty in the general population to accept vaccination, or only a claim on resources for alternative protections (isolation support, treatment access)?',
    'This is a fundamental normative question answered differently by the three kernel readings. Resolution would require coherence test: can a single framework hold both bodily-autonomy-primary and public-health-primary as live positions, or does one foreclose the other? The bodily-autonomy reading says the vulnerability creates a claim on resources, not on bodies. The public-health reading says it creates a duty. These cannot coexist in a single framework — one must foreclose the other.',
    'If bodily-autonomy-primary forecloses public-health-primary: the entire mandate is delegitimized; constraint reclassifies as snare (coercion justified by false premise). If public-health-primary forecloses bodily-autonomy-primary: vulnerable-protection duty overrides consent; tangled-rope holds as legitimate. This is the structural core of the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(duty_transfer_legitimacy, conceptual, 'Whether the two primary readings are foreclosed to each other or merely coexistent.').

omega_variable(
    individual_harm_vs_population_benefit_asymmetry,
    'For individuals who experience serious vaccine side effects, does the population-level benefit to the vulnerable justify the individual harm, or does justice require compensation, accommodation, or exemption?',
    'Empirical: measurement of side-effect incidence rates and severity; determination of whether the burden falls randomly or concentrates on specific populations. Normative: court rulings and legislative policy on whether vaccine-injury compensation funds are sufficient, or whether exemption options should expand beyond documented contraindication.',
    'If side effects are rare and compensation is available, the constraint''s burden on this payer set is reduced and extraction narrative weakens. If side effects are non-rare and compensation absent, the constraint appears to require some people to absorb permanent health costs for others'' benefit — this is the strongest extraction claim. Identity-lock dynamics become acute here: professionals cannot step out of roles; caregivers cannot step out of family obligations; accepting the risk becomes identity-fused.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_harm_vs_population_benefit_asymmetry, empirical, 'Whether the individual harm from side effects is compensated or absorbed as an unacknowledged cost of the mandate.').

omega_variable(
    sibling_reading_foreclosure_status,
    'Do the public-health-primary and bodily-autonomy-primary readings foreclose each other (logically incompatible in a single framework) or merely coexist (held by different parties simultaneously)?',
    'Structural analysis: bodily-autonomy-primary asserts individual consent is inviolable; public-health-primary asserts duty to protect vulnerable overrides consent. If consent is inviolable, duty cannot override it. If duty to protect vulnerable is paramount, consent is not inviolable. These seem to logically foreclose each other — they cannot both be true within a single coherent framework.',
    'If foreclosure is structural: one reading must yield under sufficiently strong evidence or normative pressure; the contest will resolve rather than persist indefinitely. If coexistence is the stable state: different jurisdictions and parties adopt different readings permanently; the contest persists as an enduring political dispute rather than an empirical/logical one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'The logical relationship between the core axioms of sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(mand_tr_t0, observed).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__public_health_primary, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(mand_tr_t8, observed).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__public_health_primary, theater_ratio, 16, 0.25).
narrative_ontology:measurement_basis(mand_tr_t16, observed).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__public_health_primary, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(mand_tr_t24, observed).
narrative_ontology:measurement(mand_tr_t32, mandate_legitimacy_scope__public_health_primary, theater_ratio, 32, 0.29).
narrative_ontology:measurement_basis(mand_tr_t32, observed).
narrative_ontology:measurement(mand_tr_t40, mandate_legitimacy_scope__public_health_primary, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(mand_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(mand_be_t0, observed).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(mand_be_t8, observed).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(mand_be_t16, observed).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(mand_be_t24, observed).
narrative_ontology:measurement(mand_be_t32, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 32, 0.64).
narrative_ontology:measurement_basis(mand_be_t32, observed).
narrative_ontology:measurement(mand_be_t40, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(mand_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(mand_su_t0, observed).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(mand_su_t8, observed).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(mand_su_t16, observed).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(mand_su_t24, observed).
narrative_ontology:measurement(mand_su_t32, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(mand_su_t32, observed).
narrative_ontology:measurement(mand_su_t40, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(mand_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__public_health_primary, 0.12).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the mandate_legitimacy_scope kernel. The kernel is the contested claim that state authority to compel vaccination is legitimate. The public_health_primary reading (this file) privileges the protection of vulnerable populations and authorizes state coercion justified by their inability to protect themselves. The bodily_autonomy_primary reading privileges individual consent and bodily integrity, treating the mandate as illegitimate coercion regardless of collective benefit. The proportionality_reading treats legitimacy as contingent on disease severity, vaccine safety, and availability of less restrictive alternatives. Each reading instantiates a different constraint with a different ε value, different beneficiary/victim structure, and different per-seat classifications. The three constraints form a family linked by their common kernel: all three are readings of 'mandate legitimacy' but with irreconcilable premises about what makes it legitimate. The readings coexist across different parties and jurisdictions; none has (yet) achieved sufficient force to foreclose the others, though the structural analysis (omega_sibling_reading_foreclosure_status) identifies them as logically incompatible within a single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
