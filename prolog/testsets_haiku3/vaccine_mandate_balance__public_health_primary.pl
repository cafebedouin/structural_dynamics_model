% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Vaccine Mandate Enforcement (Public Health Priority Reading)
 *   domain: public_health/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the public_health_primary reading of
 *   the contested vaccine_mandate_balance kernel. The reading asserts that
 *   when voluntary vaccination compliance fails to achieve herd immunity and
 *   vulnerable populations face lethal exposure without mandate-enforced
 *   coverage, collective protection justifiably supersedes individual consent
 *   to vaccination. Immunocompromised and too-young-to-vaccinate individuals
 *   enter the victim set only if mandates are absent (they bear lethal risk
 *   from low coverage); vaccine-hesitant unvaccinated adults enter the victim
 *   set when mandates are present (they bear coercion). The constraint's
 *   persistence depends on maintaining that unvaccinated individuals are not
 *   rights-violated by coercion to protect the immunocompromised—a contested
 *   normative claim that differs from the bodily_autonomy_primary reading
 *   (which would reverse the victim identification) and the
 *   proportionality_reading (which would permit mandates conditionally, with
 *   robust exemptions). The claim/metric gap is deliberate and structural to
 *   the kernel contest: this reading claims tangled_rope (genuine
 *   coordination problem + asymmetric extraction, both defendable under
 *   public-health-primary logic); a sibling reading would claim snare (pure
 *   extraction masked as coordination). The engine computes divergence across
 *   seats; this story provides the structural data for one reading's
 *   typology.
 *
 * KEY AGENTS:
 *   - immunocompromised_exposed_populations (powerless, trapped exit) — face lethal risk when vaccine coverage is low; beneficiaries of mandate
 *   - vaccine_hesitant_unvaccinated_adults (moderate power, constrained exit) — bear mandate coercion directly; targets of extraction under this reading's framing
 *   - public_health_authority (institutional power, analytical exit) — sets, administers, and enforces mandates; agenda-setter
 *   - courts_and_legislatures (institutional power, analytical exit) — potential constraint revisers; observers within this story
 *   - alternate_risk_reduction_advocates (moderate power, constrained exit) — excluded from decision-space once public-health-primary reading governs; would argue for proportionality and robust exemption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.68).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.72).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Vaccine Mandate Enforcement (Public Health Priority Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, 'fa851a3a-d637-47a5-9996-07df30f0f612').
narrative_ontology:cs_kernel_codification('fa851a3a-d637-47a5-9996-07df30f0f612', formalized).
narrative_ontology:cs_authority_grounding('fa851a3a-d637-47a5-9996-07df30f0f612', extraction).
narrative_ontology:cs_interpretation_layer_present('fa851a3a-d637-47a5-9996-07df30f0f612').
narrative_ontology:cs_reading_relation('fa851a3a-d637-47a5-9996-07df30f0f612', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('fa851a3a-d637-47a5-9996-07df30f0f612', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('fa851a3a-d637-47a5-9996-07df30f0f612', foundational, collective_protection_overrides_consent_when_lethal_vulnerable_risk).
narrative_ontology:cs_axiom_status(collective_protection_overrides_consent_when_lethal_vulnerable_risk, holdable).
narrative_ontology:cs_axiom_grounding('fa851a3a-d637-47a5-9996-07df30f0f612', collective_protection_overrides_consent_when_lethal_vulnerable_risk, deontological).
narrative_ontology:cs_axiom('fa851a3a-d637-47a5-9996-07df30f0f612', secondary, herd_immunity_requires_mandate_enforcement).
narrative_ontology:cs_axiom_status(herd_immunity_requires_mandate_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('fa851a3a-d637-47a5-9996-07df30f0f612', herd_immunity_requires_mandate_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('fa851a3a-d637-47a5-9996-07df30f0f612', herd_immunity_achievable_by_mandate).
narrative_ontology:cs_drift_state('fa851a3a-d637-47a5-9996-07df30f0f612', post_endemic_transition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fa851a3a-d637-47a5-9996-07df30f0f612', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_exposed_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, pediatric_unvaccinated_dependents).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, healthcare_system_capacity).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, vaccine_hesitant_unvaccinated_adults).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, medical_exemption_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals with compromised immune systems (cancer patients, transplant recipients, HIV+ persons below viral suppression) face lethal risk from vaccine-preventable diseases in low-coverage environments. They cannot receive certain vaccines and depend entirely on herd immunity for protection. They benefit from mandate-enforced coverage thresholds that keep disease circulation below their exposure vulnerability. They have no exit: relocation, isolation, or self-protection are the only alternatives, all extremely costly.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_exposed_populations, beneficiary,
    powerless, immediate, trapped, national).

% Adults who decline vaccination due to risk perception, distrust, religious belief, or philosophical objection to government mandate are subject to employment penalties, school/childcare exclusion, or (in some jurisdictions) direct legal liability for disease transmission. They bear the constraint's coercive force directly: lose livelihood, access to public services, or face fines. Their exit options are constrained: geographic relocation to lower-mandate jurisdictions, homeschooling/unschooling, employment in exemption-tolerant sectors, or medical exemption pursuit. The vaccine-hesitancy belief fuses with identity; persistence of hesitancy after mandate removal signals identity-lock rather than pure structural coercion response.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vaccine_hesitant_unvaccinated_adults, payer,
    moderate, biographical, identity_locked, national).

% Children too young for vaccination or with legitimate contraindications depend on environmental immunity from peers and adults. Mandate-enforced herd immunity in schools and community settings protects them from diseases that are genuinely lethal or severely disabling at ages where vaccination eligibility is restricted. They have no autonomous choice; parents exercise proxy consent but cannot opt out of disease exposure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, pediatric_unvaccinated_dependents, beneficiary,
    powerless, immediate, trapped, national).

% Individuals with genuine medical contraindications (severe allergy history, myocarditis risk) must navigate exemption systems that vary by jurisdiction and are under increasing scrutiny for fraud. They bear the administrative burden and risk of exemption denial; they are subject to the same school/employment consequences if their exemption is invalidated. Their exit option is the exemption process itself, which is structurally constrained by the mandate's enforcement criteria.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, medical_exemption_applicants, payer,
    moderate, biographical, constrained, national).

% Sets vaccination coverage thresholds, defines exemption criteria, implements school/workplace enforcement, and monitors herd immunity indicators. Justifies mandates as protecting vulnerable populations below vaccination eligibility and preserving healthcare system capacity during surge events. Administers exemption review, outbreak response, and enforcement against non-compliant institutions. Their constraint is the standing arrangement of mandate + enforcement machinery; they are the constraint's architects and maintainers.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Researchers, clinicians, and public health practitioners who argue for risk-stratified approaches (targeted mandates for high-risk settings only, robust medical exemption, voluntary compliance with incentives) are structurally excluded from the decision space once the public-health-primary reading takes institutional form. Their alternative framing—that proportionality and exemption robustness are compatible with herd immunity achievement—is not represented in the constraint's enforcement logic.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, alternate_risk_reduction_advocates, excluded,
    moderate, biographical, constrained, national).

% Judicial and legislative bodies review mandate legality, constitutionality, and proportionality. They have authority to modify the constraint structure, define exemption standards, or mandate sunset clauses. Their role is observational within the constraint story itself, but structural to its ultimate persistence or revision.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__public_health_primary, public_health_authority).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of herd immunity: individual incentives to free-ride (accept protection from others' vaccination without bearing vaccination's risks) prevent voluntary achievement of the coverage threshold needed to protect those who cannot be vaccinated. The mandate coordinates behavior toward a collective threshold.
% TRANSFER_FUNCTION: Moves the risk and inconvenience burden of vaccination from the population broadly to the unvaccinated subset; moves the benefit of disease protection from voluntary adopters to entire population including those with legitimate contraindications. Enforces a transfer from individual autonomy (the unvaccinated) to collective security (herd immunity).
% ABSENT_VOICES: Individuals with rare vaccine adverse events whose cases inform risk-benefit analysis but do not aggregate to policy visibility; communities with historical medical trauma and legitimate vaccine distrust whose voice in exemption design is overridden by the enforcement mandate; parents of vaccine-injured children advocating for stricter safety review before mandate expansion. These constituencies would argue for narrower mandate scope, more robust exemption processes, and risk stratification; they are not excluded by formal rule but by the constraint's implementation logic, which treats herd immunity as non-negotiable.
% DISAPPEARANCE_RATIONALE: If vaccine mandates and their enforcement machinery disappeared overnight, voluntary vaccination rates would fall in most jurisdictions below herd immunity threshold for highly contagious pathogens (measles, pertussis). Disease circulation would rise among unvaccinated populations, creating lethal exposure risk for immunocompromised and too-young-to-vaccinate individuals. Healthcare system surge capacity would be tested. The arrangement's disappearance would restructure who faces disease risk and whose vulnerability is protected.
% FOUNDING_PROBLEM: Historically, vaccine-preventable diseases (smallpox, polio, measles) were lethal or disabling at scale, especially to children. When vaccination became available, achieving sufficient population coverage to eliminate disease required solving the free-rider problem: individuals who benefit from others' vaccination without risking vaccination themselves. Voluntary compliance fell short of elimination thresholds in most pre-mandate settings.
% FOUNDING_PROBLEM_CORROBORATION: The public health authority attests the founding problem is live for newly emergent or resurging pathogens with high asymptomatic transmission (COVID-19, pertussis resurgence). Medical anthropologists and vaccine-hesitant communities attest the founding problem has been substantially solved for historically targeted diseases (measles, polio) via infrastructure (routine pediatric vaccination, high baseline coverage) not requiring active mandates in many settings; they argue mandate persistence for 'solved' problems indicates functional drift toward control rather than protection. Epidemiological data from low-mandate vs. high-mandate jurisdictions during the COVID-19 pandemic supports both readings depending on which outcomes (mortality, hospitalization, breakthrough infection) are weighted.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the mandate subordinates individual consent to collective need without requiring consent of the coerced (the unvaccinated). This is structural extraction, not a bug: the reading's normative claim is precisely that consent can be overridden when lethal risk to the vulnerable requires it. Suppression is also high (0.72) because compliance is enforced through employment penalties, school exclusion, and (in some cases) legal liability—coercive mechanisms that do not dissolve in the absence of mandate (exit remains constrained). Theater ratio is moderate-low (0.28): the vaccine itself performs a real biological function (antibody production, immunization), and herd immunity threshold is a real epidemiological concept, not a proxy. The theatrical component rises over time as disease risk falls (early intervals: theater reflects genuine disease threat; later intervals: theater reflects institutional performance of the mandate when threat is attenuated). Accessibility collapse is high (0.79) because once vaccine requirements enter school/work rules, alternatives collapse: homeschooling is expensive, job market discrimination against unvaccinated is systematic, geographic exit to low-mandate jurisdictions requires resources most hesitant individuals lack. Resistance is high (0.71) because substantial populations reject the mandate's normative frame and mount active resistance (anti-mandate organizing, legal challenges, exemption fraud). The trajectory shows extractiveness and suppression rising toward a plateau as the mandate hardens into institutional infrastructure (t0–t15), then stabilizing (t15–t35) once enforcement machinery is routinized. Theater rises early as institutional justification is built, then stabilizes as the mandate becomes normalized.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute this constraint as tangled_rope from the public_health_authority seat (genuine coordination problem + asymmetric enforcement, both justified by emergency; beneficiaries exist + victims exist + active enforcement required). From the vaccine_hesitant_unvaccinated seat, the computation may diverge toward snare: the same metrics, interpreted through bodily autonomy, produce 'pure extraction masked as coordination'—the authority has no real coordination function, only dominance. From the immunocompromised seat, the computation may diverge toward rope: genuine coordination with no asymmetric extraction (the constraint protects without imposing coercive transfer on beneficiaries themselves). The perspectival gap is the point of the reading: what one seat experiences as necessary collective action, another experiences as unjust coercion, and the third experiences as lifesaving protection. The metrics are authored honestly from the public_health_primary reading's structural position; the engine's per-seat computation reveals the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Identity-lock is a significant mechanism here. The unvaccinated-hesitant adult's exit option is listed as constrained, but a closer mechanism is at play: the hesitancy itself persists as a fused belief even when barriers are removed. If employment penalties vanished tomorrow, most hesitant individuals would not suddenly vaccinate—their distrust of the vaccine or government mandate has become part of their identity. This identity-lock is partially structural (the mandate creates incentives for organizing around vaccine rejection, which deepens group identity) and partially pre-existing (prior distrust of medical institutions, government, or expertise). The suppression ambiguity omega addresses this: we cannot cleanly separate how much of the observed hesitancy is rational response to coercion vs. internalized belief that would persist post-exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status/disappearance_verdict mismatch flags a potential mandatrophy trajectory. Founding problem (free-rider problem in herd immunity achievement) is contested: the public-health-authority attests it is live for novel pathogens; the alternate_risk_reduction_advocates attest it is solved for historically targeted diseases via baseline infrastructure. If the status shifts from live to dead (disease is eliminated or becomes endemic at low-lethality levels), but the mandate persists, mandatrophy is indicated: the constraint outlives its founding justification. The theater_ratio trajectory supports this reading: as threat falls over time, the ratio rises, suggesting the mandate's function shifts from disease prevention toward institutional maintenance. A future state where founding_problem_status = dead but disappearance_verdict = world_rearranges (mandates still shape behavior despite low threat) would trigger mandatrophy classification. Currently, the founding problem is contested and the theater ratio is rising—signals of potential mandatrophy in formation, not yet resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    herd_immunity_threshold_empirical,
    'What is the actual herd immunity threshold for the target pathogen in this population, and does voluntary compliance reliably achieve it without mandate?',
    'Epidemiological modeling with regional data; comparison of mandate-present vs. mandate-absent jurisdictions for the same pathogen; natural experiments from mandate suspension or lift.',
    'If voluntary compliance historically achieves threshold (or if threshold is demonstrably lower than mandate-advocates claim), the free-rider problem is overstated and the mandate''s coordination function is weaker than claimed. If voluntary compliance consistently falls short, the coordination function is genuine. This directly affects whether the constraint should compute as tangled_rope (genuine + asymmetric) vs. snare (false coordination claim).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(herd_immunity_threshold_empirical, empirical, 'Whether free-rider problem and herd immunity shortfall are real or overstated.').

omega_variable(
    consent_override_legitimacy_reading,
    'Can individual consent be justifiably overridden to protect third parties from lethal risk, or is bodily autonomy inviolable regardless of consequences?',
    'This is a normative question without empirical resolution. The answer depends on which foundational ethical principle (collective-harm-prevention vs. inviolable-autonomy) is taken as prior. Both readings are internally coherent; the divergence is conceptual/philosophical.',
    'If collective-harm-prevention is accepted as foundational (public_health_primary reading), mandates compute as justified tangled_rope. If inviolable-autonomy is taken as foundational (bodily_autonomy_primary reading), mandates compute as unjust snare. This omega identifies the reading-indexing: there is no observer-independent resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_override_legitimacy_reading, conceptual, 'Whether collective protection can override consent; reading-dependent normative framing.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is vaccine hesitancy primarily a response to mandate coercion (structural suppression) or primarily a pre-existing internalized belief that the mandate did not create?',
    'Post-mandate-removal observation: if hesitancy drops sharply, suppression was structural; if hesitancy persists or increases (movement consolidation, identity crystallization), suppression is partly internalized. Historical data from pre-mandate hesitancy rates also inform baseline.',
    'If suppression is internalized, the constraint''s effective suppression is higher than authored (target population carries the hesitancy with them beyond mandate removal), supporting snare classification from target seats. If suppression is primarily structural, the constraint is more revocable and the tangled_rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether vaccine hesitancy is mandate-induced (structural) or mandate-amplified (internalized prior).').

omega_variable(
    exemption_robustness_and_capture,
    'Are exemption processes genuinely available as an exit option for those with legitimate contraindications, or have they been captured by enforcement logic and rendered purely performative?',
    'Audit: measure grant rates for medical exemptions, compare against epidemiological estimates of legitimate contraindications; interview medical exemption applicants on process transparency and outcomes; compare exemption policies across jurisdictions.',
    'If exemptions are robust and grant rates track medical need, the constraint''s victim set is narrower (only truly opposed, not the legitimately contraindicated), and directionality for medical_exemption_applicants shifts downward (less target-like). If exemptions are captured and performative, the victim set expands and the constraint tilts more toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_and_capture, empirical, 'Whether exemption processes are genuine exit or institutional theater.').

omega_variable(
    reading_instantiation_alternate_framing,
    'If the bodily_autonomy_primary reading were instantiated instead of public_health_primary, what would change in the constraint''s structural classification?',
    'Generate the sibling reading as a separate constraint story. In that story: victims would be unvaccinated-coerced (same metrics, different normative frame); beneficiaries would shift from vulnerable-protected to authority-empowered; ε for the mandate would remain high but the type would shift from tangled_rope to snare (extraction without genuine coordination defense). This is not an omega—it is a constraint family resolution path.',
    'This omega documents the kernel-reading multiplicity: the same constraint (vaccine mandate enforcement machinery) instantiates different types depending on which normative reading governs the victim identification. The public_health_primary reading sees coordination + justified asymmetry (tangled_rope); bodily_autonomy_primary sees pure extraction (snare). Neither is wrong within its reading; the divergence reflects the kernel''s irreducible contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_instantiation_alternate_framing, conceptual, 'Kernel-reading indexing: same constraint, different type across readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__public_health_primary, theater_ratio, 5, 0.16).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__public_health_primary, theater_ratio, 10, 0.2).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_balance__public_health_primary, theater_ratio, 15, 0.24).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__public_health_primary, theater_ratio, 20, 0.27).
narrative_ontology:measurement(vacc_tr_t25, vaccine_mandate_balance__public_health_primary, theater_ratio, 25, 0.28).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_balance__public_health_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement(vacc_tr_t35, vaccine_mandate_balance__public_health_primary, theater_ratio, 35, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__public_health_primary, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__public_health_primary, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__public_health_primary, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__public_health_primary, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(vacc_be_t25, vaccine_mandate_balance__public_health_primary, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_balance__public_health_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(vacc_be_t35, vaccine_mandate_balance__public_health_primary, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__public_health_primary, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__public_health_primary, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_balance__public_health_primary, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__public_health_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(vacc_su_t25, vaccine_mandate_balance__public_health_primary, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_balance__public_health_primary, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(vacc_su_t35, vaccine_mandate_balance__public_health_primary, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__public_health_primary, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vaccine_mandate_balance kernel. The bodily_autonomy_primary reading instantiates the same enforcement machinery but with inverted victim/beneficiary roles and snare classification. The proportionality_reading instantiates a conditional variant with robust exemptions and risk-stratified application. All three stories share the same referent (vaccine mandate enforcement in a given jurisdiction) but produce different ε and type classifications based on their normative framing of consent override legitimacy. They are structurally linked: the public_health_primary reading's institutional dominance creates pressure on the bodily_autonomy_primary reading (influences relation); the proportionality_reading coexists with public_health_primary in legislative and judicial deliberation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, powerless, 0.02).
constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, moderate, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
