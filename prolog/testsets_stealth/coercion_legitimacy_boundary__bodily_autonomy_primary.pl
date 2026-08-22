% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Categorical Consent Boundary: Medical Intervention Without Consent Is Impermissible Regardless of Collective Benefit
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   The constraint under authorship is the categorical consent boundary as
 *   instantiated in contemporary law: competent adults cannot be subjected to
 *   medical intervention without their consent, and no quantum of collective
 *   benefit — outbreak control, herd immunity, research value — licenses
 *   overriding that refusal. The arrangement descends from the post-Nuremberg
 *   settlement and is maintained daily by courts striking down
 *   compelled-treatment schemes, by documentation regimes in every hospital,
 *   and by professional discipline. It solves a real mutual-security problem
 *   (no one wants to live where majorities can conscript bodies) while
 *   simultaneously reallocating disease risk onto people who never agreed to
 *   carry it: the immunocompromised and the not-yet-vaccinatable absorb the
 *   consequences of leaving uptake purely voluntary. The claim and the
 *   metrics are authored independently: the claimed type reflects the
 *   structure I believe true (a genuine coordination core with identified
 *   non-consenting cost-bearers and active judicial enforcement), while the
 *   metrics describe the arrangement's actual operation, including its
 *   externalized costs. KEY AGENTS (by structural relationship): -
 *   competent_adult_patients: Primary beneficiary (moderate/mobile) — holds
 *   an enforceable veto over interventions on their own bodies -
 *   conscientious_objectors_to_medical_intervention: Protected beneficiary
 *   (organized/identity_locked) — refusal converted into an unconditional
 *   protected position - civil_liberties_advocacy_organizations: Secondary
 *   beneficiary (organized/mobile) — collects organizational gains from
 *   keeping the boundary contested and intact -
 *   immunocompromised_individuals: Primary target (powerless/trapped) —
 *   absorbs the transmission risk the rule reallocates to them -
 *   newborns_before_vaccination_age: Target (powerless/trapped) — depends
 *   entirely on everyone else's voluntary uptake - public_health_authorities:
 *   Governed cost-bearer (institutional/constrained) — operates epidemics
 *   without its strongest instrument - constitutional_courts: Administrator
 *   (institutional/constrained) — draws and maintains the line case by case -
 *   hospital_administrators_and_clinicians: Dual-positioned
 *   (institutional/constrained) — bears compliance cost and collects a legal
 *   shield through the same structure - proportionality_bioethicists:
 *   Excluded voice (organized/mobile) — balancing inputs with no entry point
 *   in the categorical test - clinical_ethicists: Analytical observer
 *   (moderate/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.46).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.52).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.46).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Categorical Consent Boundary: Medical Intervention Without Consent Is Impermissible Regardless of Collective Benefit").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e59da679-dce1-4f6a-aa60-2c0ffbed79c8').
narrative_ontology:cs_kernel_codification('e59da679-dce1-4f6a-aa60-2c0ffbed79c8', fixed_text).
narrative_ontology:cs_authority_grounding('e59da679-dce1-4f6a-aa60-2c0ffbed79c8', lineage).
narrative_ontology:cs_interpretation_layer_present('e59da679-dce1-4f6a-aa60-2c0ffbed79c8').
narrative_ontology:cs_reading_relation('e59da679-dce1-4f6a-aa60-2c0ffbed79c8', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('e59da679-dce1-4f6a-aa60-2c0ffbed79c8', coercion_legitimacy_boundary__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('e59da679-dce1-4f6a-aa60-2c0ffbed79c8', foundational, nonconsensual_medical_intervention_categorically_impermissible).
narrative_ontology:cs_axiom_status(nonconsensual_medical_intervention_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('e59da679-dce1-4f6a-aa60-2c0ffbed79c8', nonconsensual_medical_intervention_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('e59da679-dce1-4f6a-aa60-2c0ffbed79c8', foundational, collective_benefit_never_outweighs_bodily_integrity).
narrative_ontology:cs_axiom_status(collective_benefit_never_outweighs_bodily_integrity, holdable).
narrative_ontology:cs_axiom_grounding('e59da679-dce1-4f6a-aa60-2c0ffbed79c8', collective_benefit_never_outweighs_bodily_integrity, deontological).
narrative_ontology:cs_reference_frame('e59da679-dce1-4f6a-aa60-2c0ffbed79c8', absolute_consent_requirement).
narrative_ontology:cs_drift_state('e59da679-dce1-4f6a-aa60-2c0ffbed79c8', pandemic_stress_test_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e59da679-dce1-4f6a-aa60-2c0ffbed79c8', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, competent_adult_patients).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, conscientious_objectors_to_medical_intervention).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_advocacy_organizations).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, newborns_before_vaccination_age).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, hospital_administrators_and_clinicians).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, hospital_administrators_and_clinicians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every adult with decision-making capacity holds a legally enforceable veto over surgery, medication, and examination performed on their own body. Treatment proceeds only on expressed consent, and a refusal binds clinicians even when they judge the refusal harmful. The protection travels across borders that share the norm and can be waived at any moment by consenting.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, competent_adult_patients, beneficiary,
    moderate, biographical, mobile, global).

% Religious and philosophical communities whose doctrines forbid receiving certain medical products. The categorical rule converts their refusal from a negotiable preference into a protected position that no outbreak severity can override. Experiencing the protection as optional would require abandoning the belief community that constitutes who they are, so they do not treat exit as a choice.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, conscientious_objectors_to_medical_intervention, beneficiary,
    organized, generational, identity_locked, national).

% Litigating organizations that challenge compulsory-treatment statutes, collect membership dues and donations keyed to defending the boundary, and accumulate precedent wins that expand their standing and fundraising base. Their organizational growth is tied to the boundary remaining both contested and intact.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% People whose immune systems cannot mount protection from vaccination and who therefore depend on the immunity of those around them. When compulsion is off the table and voluntary uptake sags, they absorb the resulting transmission risk in full. They cannot opt out of exposure, cannot relocate away from airborne pathogens, and never agreed to carry the risk the rule leaves with them.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, biographical, trapped, global).

% Infants too young for routine vaccine schedules. They have no voice, no vote, and no ability to avoid exposure; their only shield is the vaccination rate of everyone around them, which the categorical rule leaves entirely to persuasion.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, newborns_before_vaccination_age, payer,
    powerless, immediate, trapped, global).

% Agencies charged with controlling epidemics that may inform, incentivize, exclude, and sometimes quarantine, but may not compel the injection or treatment itself. During severe outbreaks they absorb the political blame for spread they were barred from preventing by the surest available means, and they cannot abandon the mission that exposes them to that blame.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, payer,
    institutional, generational, constrained, national).

% Courts that drew the line and maintain it: they hear challenges to compelled treatment, apply the categorical standard, strike down mandate statutes that fail it, and occasionally recognize narrow exceptions such as incapacity emergencies and substituted judgment. They cannot resign from the role; every new outbreak hands them the same question again.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% They operate under the rule daily: obtaining and documenting consent before touching patients, facing liability and discipline for violations, and in exchange receiving a bright-line legal shield that converts consent disputes into documented-process questions. They bear the compliance cost and collect the legal protection through the same structure, and they cannot decline either half.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, hospital_administrators_and_clinicians, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, hospital_administrators_and_clinicians, beneficiary).

% Scholars and government advisors who argue that severity and transmissibility should scale the permission to coerce. Inside the categorical framework their balancing inputs have no entry point — the applicable test contains no weighing step — so their influence runs through political channels outside the doctrine rather than through it.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, proportionality_bioethicists, excluded,
    organized, biographical, mobile, national).

% Hospital ethics consultants and academic bioethicists who watch how the line performs case by case, publish analyses of its edge cases, and advise institutional committees. They collect no direct gain from the arrangement and bear none of its imposed risks.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, clinical_ethicists, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the mutual-security problem of bodily inviolability: each person's assurance that neither the state, nor a majority, nor an institution can conscript their body for others' ends, with each person's protection underwriting everyone else's. It also stabilizes the trust settlement between patients and medicine by making consent the sole legitimacy condition for touching a body.
% TRANSFER_FUNCTION: Moves decision authority over individual bodies from collective institutions (state, public health agencies, hospitals) back to each individual; symmetrically, it moves disease-risk management from the coercible population onto voluntary cooperation and onto third parties who depend on herd immunity, without compensation.
% ABSENT_VOICES: Proportionality-minded public health ethicists would object that the test admits no weighing step, and they are structurally outside the doctrinal conversation — their inputs enter through legislation and politics, never through the applicable standard. Immunocompromised patients speak in public discourse but hold no formal seat inside the adjudicative test, which weighs only the coerced individual's claim; the people bearing the rule's largest externalized cost are not parties whose burden the framework counts.
% DISAPPEARANCE_RATIONALE: If the categorical bar vanished overnight, dormant compulsory-treatment and mandatory-vaccination statutes would reactivate during the next serious outbreak, emergency-use powers would expand, and the consent-documentation settlement between medicine and patients would renegotiate within a few epidemic cycles. The litigation and advocacy economy built on defending the boundary would lose its object, and the risk currently carried by the immunocompromised and newborns would be partially shifted back onto the objector population.
% FOUNDING_PROBLEM: The arrangement was built to end the historical practice of non-consensual medical experimentation and treatment: the Nazi physician trials, the Tuskegee study, forced sterilization programs, and involuntary psychiatric confinement. Its founding instrument is the Nuremberg Code's demand that the voluntary consent of the human subject be absolutely essential.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Nuremberg Code's text and successor instruments (the Declaration of Helsinki, CIOMS guidelines) attest the founding problem independently of any current beneficiary, and historical scholarship on Tuskegee, sterilization abuse, and psychiatric detention documents it without reliance on advocacy organizations. The corroboration supports the consent requirement itself while disputing its categorical reach — epidemiologists and public-health scholars attesting from outside the beneficiary set argue the founding history justifies consent as a default, not an absolute bar — which is why the status is recorded as live rather than settled.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).
:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.46) because the arrangement's yield is not a wealth transfer but a risk reallocation: the option-value of compulsion is taken from public health authorities, and the resulting transmission burden lands on immunocompromised individuals and newborns who did not consent to carry it — an irony internal to the rule's own logic, since it forbids imposing non-consented burdens on objectors while imposing a non-consented burden on the herd-immunity-dependent. Suppression (0.52) reflects active foreclosure of an entire policy category by judicial enforcement against elected branches, not street-level coercion. Theater is low (0.16): the rule performs real work continuously — consent documentation, struck-down mandates, disciplined violators — with only a late-rising ceremonial component (periodic reaffirmation declarations issued while carve-outs quietly widen). Accessibility_collapse (0.45) is moderate: within the framework, alternatives to consent collapse once the categorical premise is accepted, but voluntary-program alternatives (incentives, education, exclusion policies, quarantine) persist in the world. Resistance (0.62) is high and recurrent: every serious outbreak reactivates professional and political contestation. The measurement series run on one shared grid (points 0/15/30/45/60/75, spanning the post-Nuremberg era to the pandemic stress test) so every tracked metric is authored at every examined time point; the suppression_requirement series is included because the story genuinely traces enforcement-capacity change — declaratory code, then ethics-board machinery, then constitutionalization, then stress.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the conscientious objector's position the arrangement is an inviolable guarantee — near-zero burden, total protection, exit unthinkable because leaving means leaving the belief community. From the immunocompromised patient's position the same arrangement is an unchosen hazard: they bear the cost of everyone else's liberty without having agreed to it, and they cannot exit exposure. From the court's position it is a recurring line-drawing exercise; from the public health authority's position it is a handcuffed toolkit and a blame magnet during outbreaks. The engine computes these per-seat classifications from the power, exit, and directional data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for competent_adult_patients (pure protection, waivable at will), conscientious_objectors (protection fused with identity), and civil_liberties_advocacy_organizations (organizational gains, mobile exit). Victim declarations drive high directionality for immunocompromised_individuals and newborns_before_vaccination_age (trapped — no exit from exposure exists) and for public_health_authorities (constrained — the restricted option is their core mission instrument). Hospital administrators and clinicians sit dual-positioned: they pay compliance costs and collect a liability shield through the same structure, landing near symmetric. Constitutional_courts administer without materially collecting or paying, sitting near the middle. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already order the seats correctly, and the override mechanism keys on power atoms, which would misfire here because institutional-level agents (courts, authorities, hospital systems) occupy genuinely different positions despite sharing a power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — non-consensual experimentation and treatment as practiced at Nuremberg, Tuskegee, in forced sterilization programs and psychiatric detention — is live, not dead: capacity disputes, pediatric research, and pandemic pressure keep it current, so no mandatrophy resolution is declared. The classification work this story performs is preventing mislabeling in both directions. Reading the arrangement as pure coordination (rope) erases the identified non-consenting cost-bearers whose burden grows each time voluntary uptake sags; reading it as pure extraction (snare) erases the genuine mutual-disarmament function that every potential conscript — which is everyone — benefits from. The tangled_rope claim holds both halves. There is also a standing temptation to present this boundary as a mountain — a self-evident moral truth requiring no defense — which is why emerges_naturally is authored false: the boundary is constructed, historically dated, actively enforced, and contested, and presenting it as natural law would be exactly the false-summit move the framework exists to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is one reading (bodily_autonomy_primary) of the coercion_legitimacy_boundary kernel; the sibling readings (public_health_primary, proportionality_reading) locate the disagreement in whether the boundary admits any balancing input. Would adopting either sibling reading change the victim set, the beneficiary set, or the enforcement profile of the boundary?',
    'Comparative classification of the sibling stories: author public_health_primary and proportionality_reading as separate files and diff their computed victim sets, beneficiary sets, and effective-extraction profiles against this one.',
    'If the siblings shift immunocompromised individuals from victim to beneficiary and move mandate administrators into the beneficiary set, the kernel''s readings differ structurally rather than rhetorically, and cross-reading contamination analysis becomes meaningful; if the structures converge, the contest is nominal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    emergency_carveout_boundary,
    'Where does the categorical rule actually break under extremity — lifesaving treatment of the unconscious, psychiatric emergency detention, a pathogen with case fatality far above any historical vaccine-preventable disease?',
    'Systematic survey of emergency-doctrine case law and statutory emergency-power activations across jurisdictions, mapped against the categorical formulation''s own exceptions (implied consent, substituted judgment, incapacity).',
    'If carve-outs already swallow most extreme cases, the measured extraction of the categorical form is lower than authored and the rule functions as a strong presumption rather than an absolute; if carve-outs stay narrow, the categorical claim is load-bearing and the third-party risk transfer is real at the margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_carveout_boundary, empirical, 'Whether the categorical boundary survives genuine impossibility cases or already contains the exceptions its critics demand.').

omega_variable(
    herd_immunity_dependent_burden,
    'How much attributable morbidity and mortality does the categorical bar actually impose on those who cannot be protected by vaccination — what is the measured size of the risk transferred onto non-consenting third parties?',
    'Epidemiological attribution studies comparing outbreak burden in jurisdictions with and without compulsory-authority statutes, controlling for voluntary uptake, healthcare access, and pathogen circulation.',
    'A large attributable burden raises the effective extraction experienced by the trapped victim seats and strengthens the asymmetric-extraction half of the hybrid structure; a negligible burden collapses the victim declaration toward symbolic and pushes the arrangement toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herd_immunity_dependent_burden, empirical, 'Magnitude of the disease-risk externality borne by herd-immunity-dependent populations.').

omega_variable(
    persistence_conviction_or_inertia,
    'Does the boundary persist because populations genuinely hold bodily inviolability as a prepolitical right, or because post-Nuremberg instruments, professional socialization, and litigation infrastructure reproduce it mechanically?',
    'Cross-generational attitude surveys combined with natural experiments where enforcement capacity lapsed (jurisdictions that decriminalized compulsion or lapsed review) — did voluntary compliance with the norm hold without the machinery?',
    'If conviction-driven, the constraint is robust to enforcement decay and its suppression requirement is overstated; if inertia-driven, it is a candidate for degradation into theatrical maintenance once the founding generation exits, and the theater ratio trajectory matters more than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_conviction_or_inertia, conceptual, 'Source of the boundary''s persistence: lived moral commitment versus institutional reproduction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 15, 0.09).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 30, 0.1).
narrative_ontology:measurement(coer_tr_t45, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 45, 0.11).
narrative_ontology:measurement(coer_tr_t60, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 60, 0.13).
narrative_ontology:measurement(coer_tr_t75, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 75, 0.16).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 15, 0.26).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 30, 0.31).
narrative_ontology:measurement(coer_be_t45, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 45, 0.36).
narrative_ontology:measurement(coer_be_t60, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(coer_be_t75, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 75, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(coer_su_t45, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 45, 0.44).
narrative_ontology:measurement(coer_su_t60, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(coer_su_t75, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 75, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the limits of medical coercion' decomposes into three structurally distinct readings of one kernel (coercion_legitimacy_boundary). This story is the categorical reading; the sibling stories instantiate the outweighing reading and the severity-scaling reading. Each has its own epsilon, victim set, and enforcement profile; they are linked here because each reading is cited as the refutation of the others in live constitutional and bioethics dispute, so contamination propagates across the family — a doctrinal drift in one reading (e.g., widening emergency carve-outs) changes the operating environment of the others. Upstream/downstream: the categorical reading is the doctrinal baseline the balancing readings define themselves against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
