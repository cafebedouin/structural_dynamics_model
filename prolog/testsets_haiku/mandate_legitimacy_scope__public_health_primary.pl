% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: State Vaccination Authority for Vulnerable Population Protection (Public Health Reading)
 *   domain: public_health/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint instantiates the PUBLIC_HEALTH_PRIMARY reading of the
 *   mandate_legitimacy_scope kernel. Under this reading, state authority to
 *   mandate vaccination is legitimate when necessary to protect vulnerable
 *   populations (immunocompromised, infants, elderly) from serious harm. The
 *   reading treats the vulnerable population's protection need as the primary
 *   legitimating frame: if mandatory vaccination is necessary to achieve herd
 *   immunity, and herd immunity is necessary to protect those who cannot
 *   vaccinate, then the mandate is justified. Unvaccinated individuals bear a
 *   duty to protect vulnerable others through vaccination, and their refusal
 *   constitutes an externality-imposing choice that justifies state
 *   intervention. This reading coexists with bodily_autonomy_primary (which
 *   denies state authority to mandate medical intervention regardless of
 *   benefit) and proportionality_reading (which requires balancing disease
 *   severity, vaccine safety, and less restrictive alternatives). The three
 *   readings are structurally distinct claims about what makes a vaccination
 *   mandate legitimate.
 *
 * KEY AGENTS:
 *   - State public health authority (institutional, agenda-setter) — sets and enforces mandate policy
 *   - Immunocompromised individuals (powerless, trapped) — primary beneficiaries, protection-dependent
 *   - Vaccine hesitant/unvaccinated (moderate, constrained) — primary targets, duty-bearing, enforcement-subject
 *   - Individuals with medical contraindications (moderate, constrained) — collateral victims of blanket enforcement
 *   - Public health epidemiologists (institutional, observer/beneficiary) — provide necessity data, benefit from validation
 *   - Bodily autonomy advocates (excluded) — would reject the reading's core premise
 *   - Proportionality advocates (excluded) — would require balancing factors this reading treats as settled
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.68).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.72).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "State Vaccination Authority for Vulnerable Population Protection (Public Health Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, '568587e7-7c51-4ecc-9674-608293ffa557').
narrative_ontology:cs_kernel_codification('568587e7-7c51-4ecc-9674-608293ffa557', formalized).
narrative_ontology:cs_authority_grounding('568587e7-7c51-4ecc-9674-608293ffa557', expertise).
narrative_ontology:cs_interpretation_layer_present('568587e7-7c51-4ecc-9674-608293ffa557').
narrative_ontology:cs_reading_relation('568587e7-7c51-4ecc-9674-608293ffa557', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('568587e7-7c51-4ecc-9674-608293ffa557', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('568587e7-7c51-4ecc-9674-608293ffa557', foundational, collective_harm_prevention_justifies_mandate).
narrative_ontology:cs_axiom_status(collective_harm_prevention_justifies_mandate, holdable).
narrative_ontology:cs_axiom_grounding('568587e7-7c51-4ecc-9674-608293ffa557', collective_harm_prevention_justifies_mandate, deontological).
narrative_ontology:cs_axiom('568587e7-7c51-4ecc-9674-608293ffa557', foundational, unvaccinated_bear_duty_to_vulnerable).
narrative_ontology:cs_axiom_status(unvaccinated_bear_duty_to_vulnerable, holdable).
narrative_ontology:cs_axiom_grounding('568587e7-7c51-4ecc-9674-608293ffa557', unvaccinated_bear_duty_to_vulnerable, deontological).
narrative_ontology:cs_reference_frame('568587e7-7c51-4ecc-9674-608293ffa557', vulnerable_protection_primary).
narrative_ontology:cs_drift_state('568587e7-7c51-4ecc-9674-608293ffa557', contemporary_low_prevalence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('568587e7-7c51-4ecc-9674-608293ffa557', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, infants_unable_to_vaccinate).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, elderly_with_waning_immunity).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_unvaccinated).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, individuals_with_medical_contraindications).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, individuals_with_medical_contraindications).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, public_health_epidemiologists).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, parents_and_guardians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces vaccination requirements, deciding which diseases and which populations trigger mandates. Justifies authority as protecting those who cannot protect themselves. Administers health surveillance, licensing penalties for non-compliance, and school/workplace exclusions. Frames the mandate as collective obligation to prevent harm to vulnerable others.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, state_public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Cannot receive live vaccines or some attenuated vaccines due to their condition. Depend structurally on high community vaccination rates (herd immunity threshold) to avoid serious infection. Without mandate enforcement, they face isolation or exposure. Their protection is THE stated justification for the mandate, yet they exercise no control over the enforcement mechanism.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, national).

% Face employment restrictions, school exclusions, and social stigma if they refuse vaccination. Some cite religious or philosophical objections; others report fear of side effects or distrust of rapid development/approval timelines. Their choice set is narrowed by the enforcement infrastructure: comply, leave jurisdiction/profession, or accept exclusion. The mandate treats their refusal as a threat to others regardless of their own risk calculus.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_unvaccinated, payer,
    moderate, biographical, constrained, national).

% Have documented medical reasons they cannot receive certain vaccines (prior anaphylaxis, active disease, immunosuppression from legitimate treatment). They are structurally victimized by blanket mandates that do not account for individual contraindications, yet they are also beneficiaries when high vaccination coverage protects them indirectly. The mandate's enforcement often fails to distinguish their case from voluntary refusal.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, individuals_with_medical_contraindications, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, individuals_with_medical_contraindications, beneficiary).

% Make vaccination decisions for dependent children but face mandate enforcement that overrides their judgment when the state deems vaccination necessary. They bear the responsibility for adverse events (real or perceived) even when the decision was mandated. School enrollment and childcare access are conditioned on compliance.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, parents_and_guardians, payer,
    moderate, biographical, constrained, national).

% Deliver vaccines, assess contraindications, and document consent (where required). They navigate tension between patient autonomy and public health mandate. Their professional judgment is constrained by mandate enforcement when they identify legitimate contraindications that the mandate does not recognize. Some jurisdictions restrict their ability to provide medical exemptions.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, medical_professionals, observer,
    organized, generational, mobile, national).

% Provide epidemiological data and thresholds that inform mandate policy (R0 estimates, herd immunity percentages, disease burden). They advise on necessity and scope but do not directly enforce. Their models and recommendations become the legitimating frame for coercive policy. They benefit from mandate compliance as validation of their predictions, creating incentive structure toward higher necessity thresholds.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_epidemiologists, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, public_health_epidemiologists, beneficiary).

% Would argue that state medical mandate violates fundamental bodily integrity rights regardless of collective benefit, and that informed consent cannot be compelled. They are structurally excluded from the decision-making process under the public_health_primary reading because that reading's core premise subordinates individual bodily integrity to collective harm prevention. Representation of this voice would reframe the constraint entirely.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, bodily_autonomy_advocates, excluded,
    moderate, biographical, constrained, national).

% Would argue that mandate legitimacy depends on disease severity, vaccine safety/efficacy ratios, and availability of less restrictive alternatives (testing, isolation, voluntary high-risk-group vaccination). They are structurally excluded under this reading because the public_health_primary frame does not require balancing these factors; vulnerable protection is presumed sufficient justification once necessary.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, proportionality_standards_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__public_health_primary, state_public_health_authority).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves collective-action coordination problem: unvaccinated individuals impose externality (disease transmission) on those who cannot vaccinate; the mandate internalizes this externality by making vaccination a duty to others, not just personal risk calculation. Without coordination, individually rational choices (non-vaccination when disease seems distant) produce collectively irrational outcome (vulnerable populations unprotected).
% TRANSFER_FUNCTION: Transfers bodily integrity rights (freedom from medical intervention without consent) from unvaccinated individuals to the state authority and vulnerable populations. The state uses enforcement power (employment restrictions, school exclusion, licensing penalties) to compel compliance. Vulnerable populations receive protection (via herd immunity threshold achievement). The mandate-enforcer gains authority legitimacy and visible compliance metrics.
% ABSENT_VOICES: Bodily autonomy advocates are structurally excluded — their core claim (medical autonomy is non-negotiable) contradicts the public_health_primary reading's foundational premise (collective harm prevention justifies medical mandate). Proportionality advocates are excluded — their claim requires balancing legitimacy factors that this reading treats as already settled (disease severity + vulnerability = mandate justified). Medical professionals providing contraindication-based exemptions are silenced when enforcement prohibits exemptions. The excluded voices would reframe the constraint entirely; they represent competing readings of the same kernel.
% DISAPPEARANCE_RATIONALE: If state vaccination authority to compel vaccination disappeared overnight under this reading's framework, vulnerable populations lose their primary protection mechanism and face isolation or serious infection risk; herd immunity thresholds collapse; public health capacity to respond to future epidemics is degraded. The state loses a tool for population-level disease control. Some unvaccinated individuals would gain bodily autonomy; some would face guilt/moral pressure if vulnerable people they knew fell ill. The medical/epidemiological apparatus would reorganize around voluntary vaccination campaigns and individual risk stratification.
% FOUNDING_PROBLEM: Serious communicable diseases (smallpox, polio, measles) pose high mortality/morbidity risk, especially for immunocompromised individuals, infants, and elderly. Individuals making private vaccination choices do not account for the protection benefit they provide to those who cannot vaccinate. High disease rates in the unvaccinated population force vulnerable individuals into isolation or unacceptable risk. State authority to compel vaccination solves this coordination failure and population-level harm.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and epidemiologists attesting to ongoing disease burden and herd immunity necessity. Immunocompromised patient advocates attesting to real protection dependency. Contested by bodily autonomy advocates (who argue the founding problem is a coordination frame imposed on individuals' medical autonomy) and proportionality advocates (who argue that modern vaccines, disease rarity in developed countries, and less restrictive alternatives have substantially altered the founding problem's relevance). Legislative testimony and academic literature document the contest; no external corroboration reaches consensus on whether the founding problem remains live under contemporary disease epidemiology.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers bodily integrity rights from unvaccinated individuals to state authority without consent-based legitimacy; the transfer is defended as necessary for vulnerable protection, not as benefit-neutral coordination. Suppression is substantial (0.72) because enforcement relies on exclusion (employment, school, public health licensing restrictions) and social stigma; alternatives (voluntary vaccination, testing, occupational sorting) are systematically closed off or stigmatized as insufficient. Theater moderates (0.28) because the protective function for vulnerable populations is genuine and epidemiologically real — enforcement activity does accomplish herd immunity thresholds — but a growing share of enforcement extends beyond disease control into broader public conformity (e.g., healthy young adults without vulnerable contacts mandated alongside those at genuine transmission risk). Accessibility collapse is high (0.71): once the reading becomes policy, exit from the unvaccinated group is extremely costly (job loss, school exclusion, geographic relocation). Resistance is moderate (0.58): substantial contestation from bodily autonomy advocates and proportionality advocates, but weaker organization than agenda-setter institutions. The measurement series show extractiveness and suppression rising over the interval as mandates extended from high-risk occupations (healthcare) to broader populations, then plateauing as disease prevalence declined — a pattern consistent with function expansion beyond vulnerability protection toward population-control conformity. The shared time grid (every metric measured at every time point) prevents misaligned projections that would date type transitions artificially.
 *
 * PERSPECTIVAL GAP:
 *   The state agenda-setter and vulnerable populations perceive this constraint as legitimate coordination (protecting those who cannot protect themselves); they compute it as rope-like. Unvaccinated and hesitant individuals perceive it as coercive mandate lacking consent basis; they compute it as snare. Medical professionals with exemption authority perceive it as overriding their clinical judgment; they sit between rope (genuine health protection) and snare (compliance-enforcing exclusion). The engine computes per-seat classification from structural data: state institutional power + vulnerable protection beneficiary + unvaccinated payer with constrained exit = tangled_rope from the agenda-setter perspective (coordinating for a real collective good, enforcing asymmetrically), snare from the hesitant payer perspective (extraction without negotiated consent, constrained exit). The 'claimed_type: tangled_rope' reflects the predominant institutional framing; the metrics reflect substantive operation showing extractive character.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority benefits directly (governance legitimacy, population-level control metrics, administrative power); derives d ≈ 0.1 (beneficiary seat, institutional power, maximum exit via analytical distance). Immunocompromised individuals benefit from protection but exercise no enforcement control; d ≈ 0.2 (beneficiary seat, powerless, trapped, no voice in mandate design). Unvaccinated hesitant individuals bear the cost (employment/school/social exclusion), have constrained alternatives (comply, leave jurisdiction, accept isolation), no say in necessity determination; d ≈ 0.85 (target seat, moderate power, constrained exit). Medical contraindication cases are collaterally victimized by blanket enforcement but structurally intended as beneficiaries; d ≈ 0.65 (payer seat, moderate power, constrained exit, secondary beneficiary role inadequate to offset enforcement burden). Bodily autonomy advocates would challenge the entire d derivation by rejecting the legitimacy frame; proportionality advocates would modulate d based on disease severity and alternatives availability. Under public_health_primary reading, their alternative d values are not computed — they are excluded from the decision process.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy risk patterns. The founding problem (serious disease risk to immunocompromised) was acute and real in the pre-vaccine era; it remains live in ongoing disease control, but disease prevalence in developed countries has declined substantially, making the necessity claim for broad-population mandates increasingly contestable. The theater_ratio rise (0.12 → 0.28) indicates enforcement activity extending beyond disease-control function into compliance-conformity performance. The divergence between 'claimed_type: tangled_rope' (genuine coordination framing) and the measured extractiveness (0.68, rising) suggests the constraint is functioning partly as legitimacy performance: the vulnerable protection narrative justifies enforcement that extends to populations without vulnerable contacts or measurable transmission risk. The measurement plateau after t=20 (extractiveness and suppression stabilizing as disease prevalence declined further) suggests enforcement infrastructure persisted despite reduced founding-problem acuity — classic mandatrophy signature. The six_questions battery captures this: founding_problem_status is contested because epidemiologists and public health authorities treat the founding problem as live (ongoing disease risk), while bodily autonomy and proportionality advocates treat it as substantially dissolved (disease rare enough that vulnerability protection no longer requires population-wide coercion). The mismatch between claimed_type and computed seat-divergence (state reads rope, hesitant read snare) is the mandatrophy detector.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_boundary_ambiguity,
    'What level of disease prevalence, mortality risk, and vulnerable-population protection dependency justifies mandatory vaccination versus voluntary high-risk-group campaigns?',
    'Comparative epidemiology across jurisdictions with different mandate stringency; analysis of herd immunity thresholds and vulnerable-population infection rates under voluntary vs. mandatory regimes; cost-benefit analysis of mandate enforcement overhead vs. incremental protection gain.',
    'If strict necessity threshold is applied (mandate justified only when disease control cannot be achieved voluntarily), this reading would be reclassified as extractive beyond vulnerable-protection justification — approaching snare. If loose necessity threshold (mandate justified whenever disease is present and vulnerable populations benefit), the reading remains rope-like.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_boundary_ambiguity, empirical, 'Where the boundary between necessary and unnecessary mandate lies given disease epidemiology.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (legal penalties, employment restrictions, public health licensing barriers) or internalized (individuals have adopted the mandate as legitimate moral duty, persisting even after enforcement threats removed)?',
    'Post-enforcement removal trajectories: if suppression persists after legal penalties are lifted, the mechanism is partly internalized (has become moral obligation via norm adoption); if suppression collapses, the mechanism is purely structural.',
    'If suppression is primarily structural, the constraint''s effective coercive force depends on continuous enforcement and would weaken under political pressure. If internalized, the constraint has colonized self-governance and would persist through norm persistence even without active enforcement — deepening the extractive character because the target population no longer recognizes their own interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether mandate suppression is externally enforced or has become internalized moral obligation.').

omega_variable(
    reading_foreclosure_question,
    'Does the public_health_primary reading logically foreclose the bodily_autonomy_primary reading within a single legal framework, or do they represent genuinely coexistent positions?',
    'Constitutional and legal philosophy analysis: do the core premises of each reading (collective harm prevention vs. bodily integrity as non-negotiable right) necessarily contradict such that no single legal framework can hold both, or can the same legal system recognize both while different institutional seats prioritize differently?',
    'If foreclosed: the readings are incompatible; adoption of public_health_primary as policy necessarily rejects bodily_autonomy_primary as illegitimate. If coexistent: both readings remain live options; different jurisdictions can adopt different readings without logical contradiction, though the social dispute remains unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_question, conceptual, 'Whether the public_health_primary and bodily_autonomy_primary readings logically foreclose each other.').

omega_variable(
    vulnerable_population_dependency_empirical,
    'What is the actual vulnerability of immunocompromised individuals to disease, and what vaccination coverage threshold is necessary to achieve meaningful herd immunity for them under contemporary epidemiology?',
    'Immunological and epidemiological research documenting breakthrough infection rates in immunocompromised individuals under various herd immunity levels; surveillance data on disease transmission in vaccinated vs. unvaccinated populations.',
    'If vulnerability is high and herd immunity threshold is steep (mandate necessity claim is well-grounded), the reading remains as rope-like; if vulnerability is lower or herd immunity is achievable through voluntary vaccination of healthcare workers and high-risk occupations, the mandate''s extension to broad populations becomes harder to defend as vulnerable-protection necessary — suggesting snare character.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vulnerable_population_dependency_empirical, empirical, 'Empirical facts about vulnerable population protection dependency under contemporary disease prevalence.').

omega_variable(
    kernel_reading_coexistence,
    'This constraint is one reading of the mandate_legitimacy_scope kernel; three readings are fielded (public_health_primary, bodily_autonomy_primary, proportionality_reading). Are these readings genuinely coexistent (different parties hold them without logical contradiction) or does one reading logically foreclose another?',
    'Structural analysis of reading premises: if public_health_primary''s core claim (collective harm prevention justifies medical mandate) logically contradicts bodily_autonomy_primary''s core claim (bodily integrity is non-negotiable), they foreclose each other. If proportionality_reading''s requirement (balancing factors must be considered) is compatible with both other readings'' core premises (just adds additional constraints), it coexists with both.',
    'Foreclosure would indicate one reading must be rejected for logical consistency; coexistence would indicate the three readings are genuinely live positions in unresolved social dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Structural relationships between the three mandate-legitimacy readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mand_tr_t5, mandate_legitimacy_scope__public_health_primary, theater_ratio, 5, 0.16).
narrative_ontology:measurement(mand_tr_t10, mandate_legitimacy_scope__public_health_primary, theater_ratio, 10, 0.21).
narrative_ontology:measurement(mand_tr_t15, mandate_legitimacy_scope__public_health_primary, theater_ratio, 15, 0.25).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__public_health_primary, theater_ratio, 20, 0.27).
narrative_ontology:measurement(mand_tr_t25, mandate_legitimacy_scope__public_health_primary, theater_ratio, 25, 0.28).
narrative_ontology:measurement(mand_tr_t30, mandate_legitimacy_scope__public_health_primary, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(mand_be_t5, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(mand_be_t15, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(mand_be_t25, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(mand_be_t30, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(mand_su_t5, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(mand_su_t15, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(mand_su_t25, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(mand_su_t30, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__public_health_primary, 0.12).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% mandate_legitimacy_scope is a contested kernel with three distinct readings, each instantiating a different constraint with different ε, different beneficiary/victim structures, and different classifications. The three stories (public_health_primary, bodily_autonomy_primary, proportionality_reading) are linked via this network field. They are NOT different views of one constraint — they are three different constraints grounded in three different readings of the same kernel. The ε-invariance principle requires decomposition: each reading answers 'what makes a vaccination mandate legitimate?' differently, producing structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__public_health_primary, powerless, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
