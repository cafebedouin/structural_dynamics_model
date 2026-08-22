% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: State Vaccine Mandate Authority (Public Health Primacy Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint embodies one reading of contested vaccine mandate
 *   legitimacy: the public health primacy reading, which treats state duty to
 *   prevent collective harm as a supreme justification for mandating medical
 *   intervention. Under this reading, unvaccinated status is reframed as a
 *   negative externality—a harm imposed on others through disease
 *   transmission risk—which shifts the burden of justification from the state
 *   to the refuser. The reading legitimizes mandate authority by dissolving
 *   the distinction between preventing harm (a legitimate state function) and
 *   requiring medical procedures (a boundary on individual bodily autonomy).
 *   This is one of three structurally distinct readings of the same contested
 *   kernel; the other readings (bodily autonomy primacy, risk stratification)
 *   would classify the same constraint very differently.
 *
 * KEY AGENTS:
 *   - Public health authority: institutional agenda-setter, expands regulatory reach and gains epistemic monopoly over medical legitimacy
 *   - Vaccinated population: organized beneficiary, protected by the mandate through collective compliance
 *   - Vaccine refusers: moderate-power payers, bear employment/social costs; constrained exit (compliance or withdrawal from regulated spaces)
 *   - Medical exemption seekers: powerless payers, identity-locked to medical contraindication but treated as refusers
 *   - Legislative oversight: institutional observer, reviews whether collective-harm duty justifies mandate scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.68).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.71).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "State Vaccine Mandate Authority (Public Health Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, '6568b393-dc5a-41fb-a5c4-2c3d0814819f').
narrative_ontology:cs_kernel_codification('6568b393-dc5a-41fb-a5c4-2c3d0814819f', formalized).
narrative_ontology:cs_authority_grounding('6568b393-dc5a-41fb-a5c4-2c3d0814819f', extraction).
narrative_ontology:cs_interpretation_layer_present('6568b393-dc5a-41fb-a5c4-2c3d0814819f').
narrative_ontology:cs_reading_relation('6568b393-dc5a-41fb-a5c4-2c3d0814819f', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('6568b393-dc5a-41fb-a5c4-2c3d0814819f', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('6568b393-dc5a-41fb-a5c4-2c3d0814819f', foundational, state_duty_collective_harm_prevention_supreme).
narrative_ontology:cs_axiom_status(state_duty_collective_harm_prevention_supreme, holdable).
narrative_ontology:cs_axiom_grounding('6568b393-dc5a-41fb-a5c4-2c3d0814819f', state_duty_collective_harm_prevention_supreme, deontological).
narrative_ontology:cs_axiom('6568b393-dc5a-41fb-a5c4-2c3d0814819f', foundational, unvaccinated_status_is_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_is_externality, holdable).
narrative_ontology:cs_axiom_grounding('6568b393-dc5a-41fb-a5c4-2c3d0814819f', unvaccinated_status_is_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('6568b393-dc5a-41fb-a5c4-2c3d0814819f', collective_welfare_protection_framework).
narrative_ontology:cs_drift_state('6568b393-dc5a-41fb-a5c4-2c3d0814819f', contemporary_post_acute_emergency, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6568b393-dc5a-41fb-a5c4-2c3d0814819f', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authority).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, medical_exemption_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces vaccine mandates under framing that unvaccinated status is a negative externality impairing collective welfare. Gains expanded regulatory authority, budget allocation, and legitimacy to intervene in medical decision-making. Justifies the mandate by citing disease transmission dynamics and population immunity thresholds. Enforcement occurs through employment barriers, school exclusions, travel restrictions, and social pressure campaigns.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Receives the purported collective-action coordination benefit: reduced transmission risk, herd immunity thresholds approached, lower disease prevalence. Under this reading, they are the beneficiaries of the mandate's enforcement because it protects them through others' compliance. The reading frames their protection as a positive externality created by the mandate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population, beneficiary,
    organized, biographical, mobile, national).

% Bear the suppression costs of the mandate: employment termination, school exclusion, restricted travel, social stigma, loss of access to public services. Under this reading, their refusal is reframed as imposing externalities on others, justifying state coercion. Their exit options include relocating to jurisdictions without mandates, accepting vaccination against their stated preferences, or withdrawing from regulated spaces (homeschooling, leaving employment). The constraint operates on the premise that their preference not to vaccinate is structurally illegitimate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers, payer,
    moderate, biographical, constrained, national).

% Individuals with genuine medical contraindications face a secondary victimization: the mandate's universality means they are treated as refusers despite medical inability to comply. Their identity as someone with a legitimate medical condition that prohibits vaccination becomes inert under the mandate's enforcement. Exit requires proving medical legitimacy to authorities, which is expensive and epistemically dependent on the same public health establishment administering the mandate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, medical_exemption_seekers, payer,
    powerless, biographical, identity_locked, national).

% Practitioners of alternative medicine, naturopathy, and other non-pharmaceutical frameworks are structurally excluded from the conversation. They would contest the reading's premise that vaccination is the only legitimate public health intervention and that unvaccinated status is inherently externalized harm. Their exclusion is maintained by defining them as outside the bounds of recognized expertise.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, competing_medical_frameworks, excluded,
    moderate, biographical, trapped, national).

% Courts and legislatures examine whether the state's duty to prevent collective harm actually justifies the scope and method of mandate enforcement. They take testimony from public health authorities, constitutional scholars, refusers, and affected persons. They can revise the mandate's scope, impose sunset clauses, or strike it as exceeding enumerated authority.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, legislative_oversight_body, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authority).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of free-riding on herd immunity: individuals have incentive to avoid vaccination's costs while benefiting from others' compliance. The mandate internalizes this externality by enforcing universal participation, achieving population immunity thresholds that individual choice would not reach.
% TRANSFER_FUNCTION: Moves the autonomy cost (accepting unwanted medical intervention) from the vaccinated many to the unvaccinated few. Simultaneously transfers expanded regulatory authority from legislatures to public health agencies, and transfers legitimacy from individual consent to bureaucratic risk assessment. The reading treats these transfers as justified by the magnitude of the negative externality.
% ABSENT_VOICES: Medical practitioners outside the state-authorized expertise framework are excluded. Vaccine-injured persons whose harm is attributed to other causes are not in the conversation. Persons whose medical contraindications are unrecognized by the diagnostic apparatus are treated as refusers. Their absence from the mandate-setting process reflects the reading's closure of the legitimate-expertise boundary.
% DISAPPEARANCE_RATIONALE: If the mandate and its enforcement vanished overnight, vaccination rates would drop to below-herd-immunity thresholds in many regions, disease prevalence would rise, and population immunity would decline. The vaccinated population would experience higher transmission risk. Public health agencies would lose the regulatory authority they accumulated. The reading frames this as a public health catastrophe; the bodily_autonomy reading frames it as liberation.
% FOUNDING_PROBLEM: COVID-19 pandemic produced high transmissibility in a largely unvaccinated population. Public health authorities determined that voluntary vaccination would not achieve immunity thresholds necessary to suppress disease. The mandate emerged as a tool to bridge the gap between achievable voluntary rates and the threshold needed for collective protection.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities attest the founding problem remains live: new variants evade immunity, vaccination rates remain below thresholds in some jurisdictions. Epidemiologists cite modeling studies showing mandate necessity for achieving targets. Constitutional scholars and refusers attest the problem has been substantially mitigated by therapeutics, prior infection, and voluntary vaccination uptake; legislative testimony from affected persons contests whether the original emergency justifies the mandate's persistence and scope. The contested status reflects genuine disagreement about whether the founding coordination problem still drives the constraint or whether it now operates as pure authority capture.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.41 to 0.68 over the interval as the mandate persists beyond the emergency phase and becomes institutionalized. Early extractiveness is lower because emergency framing legitimizes rapid state action; later extractiveness rises as the founding problem (acute pandemic) recedes but enforcement infrastructure remains and expands. Suppression requirement stays high (0.71 endpoint) because refusers actively contest the mandate's legitimacy and must be continuously coerced rather than coordinated. Theater ratio remains moderate (0.28) because public health justification is real—disease transmission is a material phenomenon—but a growing fraction of enforcement effort goes to defending the mandate's scope and duration rather than addressing emergent variants. The shared time grid across all three metrics anchors the measurement series: every metric is authored at every time point, preventing temporal distortion from misaligned grids.
 *
 * PERSPECTIVAL GAP:
 *   The public health authority seat and the vaccine-refuser seat compute entirely different constraint types from the same structural data. From the authority's perspective, the mandate is rope-like coordination: individuals have incentive to free-ride on herd immunity, so the mandate internalizes this negative externality and achieves a collectively better outcome. From the refuser's perspective, the same structure is snare-like extraction: a bureaucracy claims expertise monopoly over medical legitimacy, redefines refusal as harm, and uses state coercion to suppress an alternative medical framework. The engine computes both seats' types independently from power, exit options, and beneficiary/victim declarations. The perspectival gap is the measurement the corpus takes: where the two seats' computed types diverge most sharply is where the constraint's classification is most contestable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is asymmetric by construction. The public health authority sits at d~0.1 (near-full beneficiary: it gains authority, budget, and epistemic legitimacy; it can exit the mandate framework without personal cost). Vaccine refusers sit at d~0.85 (near-full target: employment loss, travel restriction, social exclusion; their exits are costly—they must either comply or withdraw from regulated life). Vaccinated individuals sit at d~0.45 (moderate beneficiary: genuine protection from disease, but indirect cost borne through others' suppression; they remain in the constraint by choice but with asymmetric information about its operation). Medical exemption seekers sit at d~0.92 (full target: they cannot exit by choosing vaccination because a medical condition forecloses that option; identity-locked). The beneficiary/victim declarations feed this derivation: naming public health authority and vaccinated population as beneficiaries, and vaccine refusers and medical exemption seekers as victims, establishes the directional asymmetry the engine uses to compute effective extraction per seat.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint sits at the border between tangled rope and snare classification. Tangled rope requires genuine coordination (solved: individuals free-ride on herd immunity) plus asymmetric extraction (present: refusers bear suppression costs). The constraint's persistence depends on active enforcement (employment checks, school audits, travel restrictions, social stigma maintenance). The founding problem (pandemic, need for collective vaccination) was live but is now contested: disease prevalence has declined, therapeutics have emerged, and voluntary vaccination has captured a large fraction of the population. The mandatrophy question is: does the constraint still solve the founding coordination problem, or does it now operate as pure authority consolidation? The measurement series shows extractiveness rising as the founding problem recedes, and suppression requirement staying high despite falling disease pressure—a signature of a constraint whose original justification has weakened but whose enforcement infrastructure has hardened. The reading itself (public health primacy) assumes the founding problem is still live ('protecting collective welfare justifies mandate authority'); a contending reading (risk stratification) would argue the problem is solved and a narrower mandate is proportional; the bodily autonomy reading would argue the problem never justified this constraint in the first place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_reification,
    'Is unvaccinated status a reified externality (a harm-property of the person), or is disease transmission risk a statistical probability distributed across a population such that no individual''s status determines their causal contribution to collective harm?',
    'Epidemiological analysis distinguishing individual-level causal contribution (vaccination status + disease exposure + transmission chain specificity) from population-level statistical association (vaccination status correlates with population transmission rates). Post-intervention comparison of transmission risk across vaccinated and unvaccinated individuals in equal-exposure conditions.',
    'If unvaccinated status is a reified externality, the mandate''s framing is descriptively accurate and its suppression is justified harm-prevention. If transmission is probabilistic and individual-status-dependent (some unvaccinated persons never transmit; some vaccinated persons do), the reification is a conceptual error that treats correlation as causation, and the mandate''s suppression is applied to non-causers. This shifts the constraint from coordination-plus-extraction toward pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(externality_reification, empirical, 'Whether unvaccinated status is a reified externality or a statistical population property.').

omega_variable(
    founding_problem_persistence,
    'Does the founding coordination problem—free-riding on herd immunity in an acute emergency—persist as the constraint''s structural justification, or has the problem been substantially solved by disease evolution, therapeutic availability, and voluntary vaccination uptake such that the mandate now operates to defend bureaucratic authority rather than to solve the founding problem?',
    'Comparison of disease prevalence, mortality, and morbidity trajectories with and without mandate enforcement in matched jurisdictions. Assessment of whether mandate relaxation correlates with health outcomes degradation or with negligible impact. Review of public health authority statements and budget allocations to determine whether enforcement effort tracks emerging disease threat or tracks institutional consolidation.',
    'If the founding problem persists, the constraint is tangled rope: coordination benefit (herd immunity, disease suppression) justifies asymmetric extraction (refuser suppression). If the problem is solved, the constraint is snare: no genuine coordination benefit, only authority consolidation. This is the mandatrophy question: is this a mechanism that solved a real problem and now persists in inertia, or is it a mechanism whose original problem has receded and which now operates as pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding collective-action problem persists or has been substantially solved.').

omega_variable(
    medical_autonomy_boundary,
    'Is the boundary between legitimate state health authority and protected bodily autonomy located at (a) the prevention of narrowly defined demonstrable harms to specific others (bodily autonomy primacy reading), (b) the prevention of any collective welfare impairment even where individual causal responsibility is distributed and probabilistic (public health primacy reading), or (c) the prevention of high-threshold actuarial risk where individual risk stratification shows the refuser''s status constitutes above-baseline contribution to collective harm (risk stratification reading)?',
    'Constitutional jurisprudence evolution: how courts resolve mandate challenges reveals which reading the judiciary treats as binding. Comparative law analysis: jurisdictions that have rejected mandates vs. those that enforce them, and the justification language each uses. Consensus development in bioethics literature on where autonomy boundaries lie relative to collective welfare duties.',
    'This omega documents the kernel''s irreducible ambiguity: no mathematical fact about disease transmission or immunity determines which reading is correct. The boundary is a normative choice—a constitutional commitment—that three readings interpret differently. The reading authored here (public health primacy) places the boundary at collective welfare; the bodily autonomy reading places it at individual medical self-sovereignty; the risk stratification reading places it at demonstrated individual actuarial contribution above baseline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medical_autonomy_boundary, conceptual, 'Where the boundary between state health authority and bodily autonomy lies—a constitutional question, not an empirical one.').

omega_variable(
    suppression_vs_internalization,
    'How much of the measured suppression (0.71) is structural (external barriers: employment policy, school rules, travel restrictions) versus internalized (the person has come to believe their refusal is illegitimate, that unvaccinated status is shameful, that compliance is the moral choice)? Does the suppression persist after the external barriers are removed?',
    'Post-mandate-relaxation trajectories: when external enforcement stops (employment barriers lifted, school rules changed, travel restrictions removed), do refusers'' stated preferences about vaccination change? Do they seek vaccination voluntarily? Do they maintain their refusal but accept the legitimacy of state authority to demand it? Longitudinal surveys of persons in mandate-relaxed jurisdictions.',
    'If suppression is primarily structural, it decays when enforcement stops—the constraint''s power depended on coercive infrastructure. If suppression is significantly internalized, persons continue to experience obligation even after barriers are removed—the constraint has colonized preference formation, making it more durable but also more extractive (the targets are suppressed even in the absence of external force).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_internalization, empirical, 'Whether suppression is structural or internalized, determining the constraint''s durability and the target''s actual freedom.').

omega_variable(
    expertise_monopoly_legitimacy,
    'Is the public health authority''s claimed monopoly on medical legitimacy justified by superior epistemic access to disease dynamics, or is it a jurisdictional grab that excludes legitimate competing frameworks (alternative medicine, patient-reported experience, lived expertise from prior infection)?',
    'Comparative accuracy of public health authority predictions versus competing frameworks: whose models of disease progression, variant emergence, vaccine effectiveness, and harms have proven more accurate over time? Inclusion diversity analysis: whose expertise is admitted to mandate-setting conversations versus excluded? Post-hoc assessment of excluded perspectives'' predictive and strategic value.',
    'If the monopoly is justified, the excluded-voice stake seat''s exclusion reflects genuine lack of relevant expertise, and the mandate''s framing as state welfare-protection is sound. If the monopoly is unjustified, the excluded voices represent alternative readings of the kernel whose absence from the conversation makes the public health primacy reading appear consensual when it is actually contested. This bears on whether the mandate captures genuine collective wisdom or represents bureaucratic authority capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_monopoly_legitimacy, conceptual, 'Whether public health authority expertise monopoly is justified or a jurisdictional grab disguised as expertise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 36, 0.27).
narrative_ontology:measurement(vacc_tr_t48, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 48, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.41).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 36, 0.67).
narrative_ontology:measurement(vacc_be_t48, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 48, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 36, 0.7).
narrative_ontology:measurement(vacc_su_t48, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 48, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.18).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_legitimacy kernel decomposes into three structurally distinct constraint stories: public_health_primacy_reading (this story), bodily_autonomy_primacy_reading, and risk_stratification_reading. Each reading produces different ε values, different beneficiary/victim structures, and different computed types because each reading holds different normative premises about where the state's authority boundary lies. The three stories are linked via network.affects_constraints to show the kernel contest. The public_health_primacy reading treats unvaccinated status as an externality and justifies mandate scope; the bodily_autonomy reading treats medical self-sovereignty as inviolable and forecloses mandates; the risk_stratification reading conditions mandate legitimacy on actuarial thresholds. These are not disagreements about empirical facts—they are disagreements about the meaning of the contested kernel. Readers should examine all three stories together to understand the structure of the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
