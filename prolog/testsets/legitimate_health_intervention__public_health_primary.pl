% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Legitimacy (Population Morbidity Primary)
 *   domain: public_health/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This constraint embodies one reading of the contested kernel
 *   'legitimate_health_intervention': legitimacy derives from measurable
 *   reduction in population-level morbidity/mortality; individual refusal is
 *   reframed as externality imposition. Under this reading, vaccine refusers
 *   become victims (from a different reading's perspective, they are targets
 *   of rights violation) and immunocompromised populations become
 *   beneficiaries. The constraint operates through enforcement mechanisms
 *   (employment termination, access restriction, school exclusion) that carry
 *   high extractiveness (0.68 by interval end) sustained by active
 *   suppression. The measurement series shows rising extractiveness and
 *   suppression in the interval 0–24 (early enforcement intensification)
 *   followed by plateau at 24–36 (enforcement infrastructure matured and
 *   stabilized). Theater ratio remains low-to-moderate, indicating the
 *   public-health coordination function is real but growing share of
 *   enforcement activity defends mandate boundaries rather than disease
 *   prevention. The claim/metric gap is deliberate: the constraint is CLAIMED
 *   as tangled_rope under this reading (genuine coordination function +
 *   asymmetric enforcement) while the temporal trajectory tracks enforcement
 *   infrastructure hardening and rising suppression cost.
 *
 * KEY AGENTS:
 *   - public_health_authority: Institutional agenda-setter; frames legitimacy in population-outcome terms; owns enforcement machinery.
 *   - immunocompromised_populations: Powerless beneficiaries; trapped exit; entire public participation depends on mandate's immunological protection.
 *   - vaccine_refusers: Moderate-power payers; constrained exit; bear employment termination and access restriction from enforcement.
 *   - healthcare_workers: Organized payers facing strictest enforcement; simultaneously benefit from workplace disease control.
 *   - bodily_autonomy_advocates: Excluded structural position; their core claim (consent-centered legitimacy) is foreclosed by this reading's framework.
 *   - epidemiologists_measuring_benefit: Institutional agenda-setters determining what 'population benefit' means; measurement definitions gate legitimacy.
 *   - courts_and_constitutional_review: Analytical observers testing whether population-benefit framework satisfies proportionality doctrine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.68).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.71).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Legitimacy (Population Morbidity Primary)").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '7c6356f1-d099-4678-86cf-7c25c8c9eb18').
narrative_ontology:cs_kernel_codification('7c6356f1-d099-4678-86cf-7c25c8c9eb18', fixed_text).
narrative_ontology:cs_authority_grounding('7c6356f1-d099-4678-86cf-7c25c8c9eb18', expertise).
narrative_ontology:cs_interpretation_layer_present('7c6356f1-d099-4678-86cf-7c25c8c9eb18').
narrative_ontology:cs_reading_relation('7c6356f1-d099-4678-86cf-7c25c8c9eb18', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('7c6356f1-d099-4678-86cf-7c25c8c9eb18', legitimate_health_intervention__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('7c6356f1-d099-4678-86cf-7c25c8c9eb18', foundational, population_outcome_measurement_legitimates_intervention).
narrative_ontology:cs_axiom_status(population_outcome_measurement_legitimates_intervention, holdable).
narrative_ontology:cs_axiom_grounding('7c6356f1-d099-4678-86cf-7c25c8c9eb18', population_outcome_measurement_legitimates_intervention, empirically_contingent).
narrative_ontology:cs_axiom('7c6356f1-d099-4678-86cf-7c25c8c9eb18', foundational, individual_refusal_constitutes_externality_imposition).
narrative_ontology:cs_axiom_status(individual_refusal_constitutes_externality_imposition, holdable).
narrative_ontology:cs_axiom_grounding('7c6356f1-d099-4678-86cf-7c25c8c9eb18', individual_refusal_constitutes_externality_imposition, instrumental).
narrative_ontology:cs_reference_frame('7c6356f1-d099-4678-86cf-7c25c8c9eb18', population_disease_prevention_primary).
narrative_ontology:cs_drift_state('7c6356f1-d099-4678-86cf-7c25c8c9eb18', contemporary_endemic_transition_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7c6356f1-d099-4678-86cf-7c25c8c9eb18', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, healthcare_system_capacity).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, vaccine_refusers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, workers_facing_employment_termination).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, healthcare_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, healthcare_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets vaccination mandates, enforcement mechanisms (employment termination, access restrictions), and determines the measurement scope for 'population benefit.' Frames mandate as coordination solution to a collective-action problem (under-vaccination relative to herd-immunity thresholds). Owns the epidemiological data that legitimates the constraint under this reading. Authority derives from scientific credibility and institutional position; exit from this role is non-applicable (analytical seat).
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Cannot be vaccinated due to medical contraindication (e.g., immunosuppressive therapy, severe prior reaction). Depend entirely on population immunity (herd effect) to access public space, education, healthcare, employment without extreme isolation or extreme risk. Unvaccinated populations create a disease vector that directly collapses their exit options: public participation without isolation is contingent on high population immunity. The constraint exists primarily to protect this population.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Refuse vaccination on grounds of bodily autonomy, prior adverse reaction, or low personal risk perception. Face employment termination in healthcare, education, and public service; exclusion from public schools; restriction from healthcare facilities; social stigma. From this reading's seat, refusal is reframed as harming immunocompromised populations through disease transmission. Exit options include geographic relocation (high cost), informal employment (lower wages, less security), or compliance. Constrained exit means moderate power anchors at moderate vulnerability under enforcement.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, vaccine_refusers, payer,
    moderate, biographical, constrained, national).

% Face the strictest vaccine mandates: employment termination for refusal, with limited exemption pathways. Simultaneously benefit from reduced disease transmission in healthcare facilities, which protects their ability to work. The constraint is structurally asymmetric for this seat: their occupational power (organized, skilled, in-demand) would normally afford mobile exit, but the mandate's scope eliminates most occupational alternatives within healthcare. They must either comply or leave the profession entirely.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, healthcare_workers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, healthcare_workers, beneficiary).

% Lose employment for vaccine refusal even in sectors where disease transmission risk is low (e.g., outdoor work, home-based work, remote work). The enforcement mechanism (employment termination) is applied categorically, not calibrated to occupational risk. They bear the full extraction cost (lost income, loss of benefits, career disruption) while immunocompromised populations benefit from population-level protection that may be over-broad for their occupational context.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, workers_facing_employment_termination, payer,
    moderate, biographical, constrained, national).

% Would argue that mandate severity should scale with disease threat level, occupational exposure risk, age, prior infection status, and vulnerable-population density. From this reading's framework, they are structurally excluded: legitimacy derives from population-level outcome, not from individual risk calibration. Their core claim — that proportionality matters — is foreclosed by this reading's legitimacy basis.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, proportionality_advocates, excluded,
    moderate, biographical, constrained, national).

% Provide epidemiological measurements that quantify 'population benefit': prevented deaths, prevented hospitalizations, prevented disease spread. They define the measurement scope that operationalizes this reading's legitimacy claim. Their methodological choices (what counts as benefit, what populations are included, what outcomes matter) are structural gates through which the constraint's legitimacy operates. Authority derives from scientific credibility; exit is non-applicable (analytical seat).
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, epidemiologists_measuring_benefit, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, epidemiologists_measuring_benefit, observer).

% Hold that informed consent is a foundational legitimacy condition for medical intervention, superseding population benefit. This reading structurally forecloses their position: if legitimacy is population-outcome-derived, consent becomes a secondary procedural matter, not foundational. They are excluded from the decision framework itself — their framing of the problem is replaced by this reading's framing (externality imposition rather than rights violation).
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, bodily_autonomy_advocates, excluded,
    powerful, generational, constrained, national).

% Evaluate whether this reading's legitimacy framework satisfies constitutional doctrine (right to bodily integrity, due process, equal protection). Test whether enforcement mechanisms (employment termination, access restriction) are proportional to the state's interest in population health. Measure whether less-restrictive alternatives exist and whether exemptions are applied consistently. Courts either ratify or constrain this reading's scope.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, courts_and_constitutional_review, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__public_health_primary, public_health_authority).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: individual vaccination choice, left uncoordinated, produces under-vaccination relative to herd-immunity threshold (typically 70–95% depending on disease transmissibility). Without coordination, individuals choosing rationally for their own risk profile would under-vaccinate relative to population-level risk, leaving vulnerable subsets (immunocompromised who cannot be vaccinated) exposed to disease and healthcare infrastructure exposed to surge capacity loss. The mandate coordinates individual choice toward the population-level equilibrium that protects the immunocompromised.
% TRANSFER_FUNCTION: Moves disease-transmission burden from vaccine refusers to the mandate structure. Refusers lose employment security, public access, educational access, and bodily autonomy of choice. Immunocompromised populations gain public-space access through population immunity protection. Healthcare system infrastructure gains capacity protection from reduced surge demand. The constraint transfers the compliance cost (employment termination, social restriction) from immunocompromised to refusers.
% ABSENT_VOICES: Vaccine-hesitant and low-health-literacy populations, cultural and religious vaccine objectors, individuals with prior severe adverse reactions (a small but real population), and proportionality advocates are structurally excluded. Their claims — that compliance should be voluntary, that risk should be individually assessed, that mandates should scale with threat level — are reframed as externality imposition rather than valid concerns about autonomy or proportionality. Their exclusion from the decision framework means the constraint operates without negotiation over its scope or intensity.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared overnight (mandates lifted, employment protections restored, access restrictions removed), vaccine uptake would decline in refuser populations, population immunity would fall below herd-immunity threshold, disease transmission would accelerate in vulnerable populations, and healthcare system surge capacity would contract. Immunocompromised populations would face elevated disease risk requiring renewed isolation or intensive vaccination-focused public health campaigns. The constitutional and public health orders would reorganize around a weaker collective-action mechanism, likely shifting toward voluntary incentives or proportionality-calibrated enforcement.
% FOUNDING_PROBLEM: Preventable infectious disease produces population-level morbidity and mortality; individual vaccination choice, if left uncoordinated via market or voluntary preference, produces under-vaccination relative to the herd-immunity threshold necessary to protect vulnerable populations who cannot be vaccinated. Population-level disease burden is the founding problem, not individual autonomy or proportionality concern.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists and immunologists attest that preventable disease burden remains substantial and that unvaccinated populations create disease vectors. Constitutional law scholars and bodily autonomy advocates contest whether the founding problem justifies the enforcement mechanisms employed, or whether it was over-solved (disease severity declining, variant escape increasing, vaccination uptake higher via voluntary means). No corroboration from outside the public-health-authority benefiting set; this reading's core claim (that population benefit legitimates mandate) is itself the contested terrain.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).

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
 *   Extractiveness rises from 0.45 to 0.68 over 30 time units, driven by expanding enforcement scope (employment mandates, school access restrictions, healthcare facility exclusions) and employment-termination consequences for refusers. By t=24 the enforcement infrastructure has stabilized — suppression plateaus at 0.71 — indicating the constraint has hardened into institutional routine. Theater ratio rises from 0.12 to 0.28 but remains below the piton threshold (0.5), indicating the coordination function (herd immunity protection for immunocompromised) remains real but enforcement activity increasingly defends mandate boundaries rather than direct disease prevention. Suppression is high (0.71) because the constraint's persistence depends on actively excluding exit alternatives: employment compliance, geographic relocation, and informal work are the bounded options. Accessibility_collapse at 0.62 reflects constrained but not completely foreclosed alternatives — refusers can still find employment outside mandate scope, move jurisdictions, or comply; the constraint does not eliminate all options, but it eliminates most convenient ones. Resistance at 0.73 is high because vaccine hesitancy remains persistent and substantial despite enforcement; the constraint must be continuously defended, suggesting it lacks the 'natural law' character of uncontested coordination.
 *
 * PERSPECTIVAL GAP:
 *   The public_health_authority and the immunocompromised_populations should compute as net beneficiaries with low directionality (d near 0.0–0.2), experiencing the constraint as protective coordination. The vaccine_refusers and workers_facing_employment_termination should compute as high-d targets (0.7–0.95) experiencing the constraint as coercive extraction. Courts and constitutional reviewers should compute near-symmetric (d ~ 0.5): they see both the coordination benefit and the autonomy cost, weighted by doctrine. The engine's per-seat computation should diverge sharply between beneficiary and payer seats, reflecting the structural asymmetry this reading instantiates: what is coordination for immunocompromised becomes extraction for refusers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: immunocompromised populations hold role=beneficiary, power=powerless, exit=trapped — the constraint subsidizes their public participation (d approaches 0.0, maximum beneficiary). Payer directionality: vaccine_refusers hold role=payer, power=moderate, exit=constrained (employment termination is the enforcement gate; geographic relocation is costly; informal work is available but lower-wage) — they target d around 0.75–0.85. Healthcare workers hold role=payer with high occupational power but face the strictest mandate enforcement, so their d is higher (0.65–0.75) than their raw power would suggest; the constraint overrides their occupational leverage. The public_health_authority is the agenda_setter, power=institutional — they derive directionality from their role as enforcer, not from bearing costs, so d is low (0.1–0.2). Courts are analytical observers with d=0.5. No directionality overrides are needed; the structural derivation captures the intended per-seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as pure extraction by declaring the immunocompromised beneficiary and coordination-function structures. Without those declarations it would be a snare (high extraction, high suppression, no beneficiary). The tangled_rope classification requires both a coordination function (herd immunity protection — genuine) and asymmetric enforcement (employment termination, access restriction — clearly asymmetric). The measurement trajectory supports tangled_rope: extractiveness rises to 0.68 (high but not snare-maximal) and plateaus rather than accelerating, suppression plateaus at 0.71 (high and sustained but not escalating), theater ratio stays below 0.3 (indicating function is real, not pure performance). If extractiveness were rising toward 0.85+ and theater ratio toward 0.6+, the reclassification pressure toward snare would strengthen; the observed plateau suggests the constraint has found an enforcement equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_scope_definition,
    'What populations and disease outcomes count as ''measurable population benefit''? Does benefit calculation include only prevented deaths, or also prevented illness, prevented healthcare surge, prevented lost work days, prevented long-term sequelae?',
    'Epidemiological methodological transparency: declaring the measurement scope upfront (e.g., ''all-cause mortality in high-risk cohorts'' vs. ''infection-prevention across all populations''). Comparison of jurisdictions using different measurement scopes and observing whether mandate scope/intensity tracks measurement scope.',
    'Narrow scope (deaths only) tends to reduce perceived population benefit, weakening this reading''s legitimacy claim; broad scope (all morbidity) tends to amplify it. Measurement scope is the gate through which this reading operates — who defines it has structural power over what counts as legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_scope_definition, conceptual, 'Measurement scope shapes what qualifies as population benefit under this reading.').

omega_variable(
    externality_imposition_threshold,
    'At what population-immunity level does individual refusal transition from ''exercising autonomy'' to ''imposing externality''? Is there a threshold (e.g., 85% population immunity) below which refusal is externality, above which it is autonomous choice?',
    'Explicit threshold declaration by public health authority; empirical measurement of population immunity level and disease transmission at which mandate enforcement begins and ends (or is reweighted). Comparison with proportionality reading''s threshold approach.',
    'A clear threshold would support this reading''s coherence; the absence of a threshold leaves the externality claim categorical (refusal is always externality) rather than contingent, which strengthens the snare-classification pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_imposition_threshold, empirical, 'Whether externality imposition is threshold-contingent or categorical.').

omega_variable(
    autonomy_foreclosure_vs_coexistence,
    'Does this reading logically foreclose bodily autonomy as a legitimacy condition, or can both legitimacy bases coexist (one reading dominant in high-threat scenarios, the other in low-threat scenarios)?',
    'Examining court rulings and policy documents: do they describe autonomy as categorically overridden by population benefit, or do they reserve autonomy as a fallback when population benefit is marginal? Natural experiments from jurisdictions using proportionality thresholds (autonomy recovered below threat level).',
    'Pure foreclosure means this reading strictly rules out autonomy-centered framing — the two readings cannot coexist in one framework. Coexistence means proportionality advocates have structural room to argue for threshold calibration. Foreclosure strengthens this reading''s logical coherence but increases vulnerability to bodily_autonomy_primary counter-claims in constitutional review.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_foreclosure_vs_coexistence, conceptual, 'Whether this reading forecloses bodily autonomy as a legitimacy condition or allows coexistence via proportionality thresholds.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (employment termination gates compliance), internalized (refusers have accepted the legitimacy frame), or both? Does suppression persist after the enforcement mechanism is removed?',
    'Post-mandate lift observation in jurisdictions that ended enforcement: if suppression persists (continued low vaccination in refuser populations despite no legal requirement), the suppression is partially internalized. If vaccination uptake recovers, suppression was primarily structural.',
    'High structural suppression suggests the constraint is coercive and dependent on active enforcement; high internalized suppression suggests refusers have accepted the legitimacy frame or are risk-averse enough to comply regardless. Mixed suppression indicates the constraint has created path-dependent compliance habits that persist even after enforcement softens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural enforcement or internalized acceptance.').

omega_variable(
    exempt_populations_and_proportionality,
    'Why are some populations exempt (medical contraindication, prior severe infection, religious objection in some jurisdictions) while others are not? What principle determines exemption scope, and does it cohere with the population-benefit legitimacy claim?',
    'Examining exemption policy: if exemptions are medically determined (contraindication, prior infection), this is proportionality framing leaking in. If exemptions are political/religious, this suggests the reading''s universality is compromised. Measuring whether exemption scope tracks with threat-level heterogeneity or with political pressure.',
    'Principled exemptions (medically determined) suggest this reading tolerates proportionality calibration and is closer to tangled_rope. Arbitrary or politically-driven exemptions suggest the constraint is inconsistent with its own legitimacy claim and is closer to snare. The existence of exemptions challenges the claim that refusal is categorically externality imposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exempt_populations_and_proportionality, empirical, 'Whether exemptions undermine the population-benefit legitimacy claim by revealing hidden proportionality logic.').

omega_variable(
    kernel_reading_contest_risk,
    'This reading forecloses bodily_autonomy_primary and competes with proportionality_reading. What mechanism in constitutional law or democratic process adjudicates between readings? Is foreclosure by this reading stable under challenge, or does it generate durable opposition that eventually shifts institutional framing toward autonomy or proportionality?',
    'Historical observation: how stable is this reading''s dominance over 10-year intervals? Do courts enforce it consistently, or do they migrate toward autonomy or proportionality framing? Do public opinion and electoral pressure support or erode this reading''s legitimacy?',
    'A durable, court-enforced reading is a credible constraint with stable classification. A fragile reading that undergoes court reversal or legislative override becomes a piton (theater-heavy enforcement of an unsupported legitimacy claim) or transient constraint (sunset by legal change). Long-term institutional instability would pressure reclassification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_risk, conceptual, 'Whether this reading remains institutionally dominant or is subject to reversal under competing readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__public_health_primary, theater_ratio, 6, 0.16).
narrative_ontology:measurement_basis(legi_tr_t6, observed).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(legi_tr_t12, observed).
narrative_ontology:measurement(legi_tr_t18, legitimate_health_intervention__public_health_primary, theater_ratio, 18, 0.26).
narrative_ontology:measurement_basis(legi_tr_t18, observed).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(legi_tr_t24, observed).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__public_health_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(legi_tr_t30, observed).
narrative_ontology:measurement(legi_tr_t36, legitimate_health_intervention__public_health_primary, theater_ratio, 36, 0.28).
narrative_ontology:measurement_basis(legi_tr_t36, projected).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__public_health_primary, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(legi_be_t6, observed).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(legi_be_t12, observed).
narrative_ontology:measurement(legi_be_t18, legitimate_health_intervention__public_health_primary, base_extractiveness, 18, 0.64).
narrative_ontology:measurement_basis(legi_be_t18, observed).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.67).
narrative_ontology:measurement_basis(legi_be_t24, observed).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__public_health_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(legi_be_t30, observed).
narrative_ontology:measurement(legi_be_t36, legitimate_health_intervention__public_health_primary, base_extractiveness, 36, 0.68).
narrative_ontology:measurement_basis(legi_be_t36, projected).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__public_health_primary, suppression_requirement, 6, 0.56).
narrative_ontology:measurement_basis(legi_su_t6, observed).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.63).
narrative_ontology:measurement_basis(legi_su_t12, observed).
narrative_ontology:measurement(legi_su_t18, legitimate_health_intervention__public_health_primary, suppression_requirement, 18, 0.69).
narrative_ontology:measurement_basis(legi_su_t18, observed).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(legi_su_t24, observed).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__public_health_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(legi_su_t30, observed).
narrative_ontology:measurement(legi_su_t36, legitimate_health_intervention__public_health_primary, suppression_requirement, 36, 0.71).
narrative_ontology:measurement_basis(legi_su_t36, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=36
narrative_ontology:measurement(legi_grid_01, legitimate_health_intervention__public_health_primary, accessibility_collapse(class), 0, 0.41).
narrative_ontology:measurement(legi_grid_02, legitimate_health_intervention__public_health_primary, accessibility_collapse(class), 36, 0.64).
narrative_ontology:measurement(legi_grid_03, legitimate_health_intervention__public_health_primary, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(legi_grid_04, legitimate_health_intervention__public_health_primary, accessibility_collapse(individual), 36, 0.62).
narrative_ontology:measurement(legi_grid_05, legitimate_health_intervention__public_health_primary, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(legi_grid_06, legitimate_health_intervention__public_health_primary, accessibility_collapse(organizational), 36, 0.71).
narrative_ontology:measurement(legi_grid_07, legitimate_health_intervention__public_health_primary, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(legi_grid_08, legitimate_health_intervention__public_health_primary, accessibility_collapse(structural), 36, 0.72).
narrative_ontology:measurement(legi_grid_09, legitimate_health_intervention__public_health_primary, resistance(class), 0, 0.65).
narrative_ontology:measurement(legi_grid_10, legitimate_health_intervention__public_health_primary, resistance(class), 36, 0.71).
narrative_ontology:measurement(legi_grid_11, legitimate_health_intervention__public_health_primary, resistance(individual), 0, 0.68).
narrative_ontology:measurement(legi_grid_12, legitimate_health_intervention__public_health_primary, resistance(individual), 36, 0.73).
narrative_ontology:measurement(legi_grid_13, legitimate_health_intervention__public_health_primary, resistance(organizational), 0, 0.42).
narrative_ontology:measurement(legi_grid_14, legitimate_health_intervention__public_health_primary, resistance(organizational), 36, 0.38).
narrative_ontology:measurement(legi_grid_15, legitimate_health_intervention__public_health_primary, resistance(structural), 0, 0.38).
narrative_ontology:measurement(legi_grid_16, legitimate_health_intervention__public_health_primary, resistance(structural), 36, 0.34).
narrative_ontology:measurement(legi_grid_17, legitimate_health_intervention__public_health_primary, stakes_inflation(class), 0, 0.44).
narrative_ontology:measurement(legi_grid_18, legitimate_health_intervention__public_health_primary, stakes_inflation(class), 36, 0.67).
narrative_ontology:measurement(legi_grid_19, legitimate_health_intervention__public_health_primary, stakes_inflation(individual), 0, 0.38).
narrative_ontology:measurement(legi_grid_20, legitimate_health_intervention__public_health_primary, stakes_inflation(individual), 36, 0.69).
narrative_ontology:measurement(legi_grid_21, legitimate_health_intervention__public_health_primary, stakes_inflation(organizational), 0, 0.51).
narrative_ontology:measurement(legi_grid_22, legitimate_health_intervention__public_health_primary, stakes_inflation(organizational), 36, 0.73).
narrative_ontology:measurement(legi_grid_23, legitimate_health_intervention__public_health_primary, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(legi_grid_24, legitimate_health_intervention__public_health_primary, stakes_inflation(structural), 36, 0.71).
narrative_ontology:measurement(legi_grid_25, legitimate_health_intervention__public_health_primary, suppression(class), 0, 0.45).
narrative_ontology:measurement(legi_grid_26, legitimate_health_intervention__public_health_primary, suppression(class), 36, 0.71).
narrative_ontology:measurement(legi_grid_27, legitimate_health_intervention__public_health_primary, suppression(individual), 0, 0.42).
narrative_ontology:measurement(legi_grid_28, legitimate_health_intervention__public_health_primary, suppression(individual), 36, 0.68).
narrative_ontology:measurement(legi_grid_29, legitimate_health_intervention__public_health_primary, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(legi_grid_30, legitimate_health_intervention__public_health_primary, suppression(organizational), 36, 0.74).
narrative_ontology:measurement(legi_grid_31, legitimate_health_intervention__public_health_primary, suppression(structural), 0, 0.52).
narrative_ontology:measurement(legi_grid_32, legitimate_health_intervention__public_health_primary, suppression(structural), 36, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__public_health_primary, 0.12).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'legitimate_health_intervention'. All three readings share the same epidemiological facts but differ in what legitimates intervention. public_health_primary: legitimacy from population-outcome measurement. bodily_autonomy_primary: legitimacy from consent-centered framework (forecloses population-primary when applied to same population in same jurisdiction). proportionality_reading: legitimacy from proportionality balancing (coexists with public_health_primary in pluralist jurisdictions but competes in high-enforcement scenarios). Each reading has distinct ε, distinct beneficiary/victim structure, and distinct enforcement implications. Not decomposition per ε-invariance, but rather an irreducible reading contest over what ground legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
