% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: State Duty to Prevent Collective Harm Justifies Mandate Authority (Public Health Primacy Reading)
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel: the
 *   legitimacy of vaccine mandate authority. The kernel is the claim that
 *   'state duty to prevent collective harm justifies mandate authority,' and
 *   this story instantiates the PUBLIC HEALTH PRIMACY reading — the view that
 *   unvaccinated status constitutes an externality (transmission risk to
 *   others), that this externality is unavoidable and harmful, and that state
 *   authority to mandate vaccination flows necessarily from the duty to
 *   prevent collective harm. This reading is held by public health
 *   institutions, epidemiological analysis communities, and much state
 *   administrative authority during pandemics. It is contested by two sibling
 *   readings: the bodily autonomy primacy reading (which holds that
 *   individual bodily autonomy is inviolable and that mandate authority
 *   cannot override it regardless of externality) and the risk stratification
 *   reading (which holds that externalities should be addressed through
 *   precision targeting by risk profile rather than categorical mandates).
 *   The three readings are not empirical alternatives (testable as
 *   true/false) but normative alternatives (grounded in different
 *   foundational commitments about what duties the state bears). This
 *   constraint models how one of these three normative commitments generates
 *   a distinct classification profile.
 *
 * KEY AGENTS:
 *   - Public Health Bureaucracy (institutional/arbitrage): Primary beneficiary — gains legitimate authority to mandate medical intervention, expand surveillance capacity, allocate healthcare resources, and direct population behavior. Experiences constraint as pure coordination mechanism.
 *   - Vaccine Refusers (powerless/trapped): Primary victim — face employment termination, institutional access denial, professional licensing suspension, school enrollment barriers. No meaningful exit; maximum experienced extraction.
 *   - Hesitant Population (moderate/constrained): Secondary victim — benefit from disease prevention coordination but bear autonomy costs and trust degradation. Constrained but not trapped; partial exit through relocation, exemption seeking, or employment change.
 *   - Immunocompromised Population (moderate/mobile): Secondary beneficiary — gain protection from unvaccinated transmission risk; experience mandate as protection mechanism. Mobile exit (can comply or seek alternative risk environments).
 *   - Healthcare System Capacity (institutional/arbitrage): Institutional beneficiary — mandate preserves system capacity by reducing transmission-driven hospitalization surges. Experiences constraint as functional coordination.
 *   - Bodily Autonomy Principle (analytical/analytical): Abstract collective — the normative principle that bodily self-determination is inviolable. Bears structural cost if mandate authority overrides it; becomes victim in this reading's framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.58).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.72).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "State Duty to Prevent Collective Harm Justifies Mandate Authority (Public Health Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, '8ba1f32d-77e1-4fa9-8f54-36e247dce414').
narrative_ontology:cs_kernel_codification('8ba1f32d-77e1-4fa9-8f54-36e247dce414', formalized).
narrative_ontology:cs_authority_grounding('8ba1f32d-77e1-4fa9-8f54-36e247dce414', extraction).
narrative_ontology:cs_interpretation_layer_present('8ba1f32d-77e1-4fa9-8f54-36e247dce414').
narrative_ontology:cs_reading_relation('8ba1f32d-77e1-4fa9-8f54-36e247dce414', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('8ba1f32d-77e1-4fa9-8f54-36e247dce414', vaccine_mandate_legitimacy__risk_stratification_reading, coexists_with).
narrative_ontology:cs_axiom('8ba1f32d-77e1-4fa9-8f54-36e247dce414', foundational, unvaccinated_status_is_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_is_externality, holdable).
narrative_ontology:cs_axiom_grounding('8ba1f32d-77e1-4fa9-8f54-36e247dce414', unvaccinated_status_is_externality, empirically_contingent).
narrative_ontology:cs_axiom('8ba1f32d-77e1-4fa9-8f54-36e247dce414', foundational, state_duty_collective_harm_primacy).
narrative_ontology:cs_axiom_status(state_duty_collective_harm_primacy, holdable).
narrative_ontology:cs_axiom_grounding('8ba1f32d-77e1-4fa9-8f54-36e247dce414', state_duty_collective_harm_primacy, deontological).
narrative_ontology:cs_reference_frame('8ba1f32d-77e1-4fa9-8f54-36e247dce414', public_health_emergency_authority).
narrative_ontology:cs_drift_state('8ba1f32d-77e1-4fa9-8f54-36e247dce414', post_acute_phase_decline, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8ba1f32d-77e1-4fa9-8f54-36e247dce414', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_population).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_system_capacity).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, bodily_autonomy_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VACCINE REFUSER (SNARE) — Trapped by mandate enforcement: employment termination, institutional access denial, professional licensing suspension, school enrollment barriers. No meaningful exit exists within jurisdiction — either comply (violates stated bodily autonomy) or face cumulative coercive penalties. Extraction is maximum: coercion is applied without coordination benefit to this agent. No genuine choice mechanism; suppression is structural and total within the constraint's scope.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__public_health_primacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HESITANT POPULATION — MODERATE POWER (TANGLED ROPE) — Face genuine coordination benefits (disease prevention in their social networks, healthcare system preservation) alongside significant extraction (career risk, medical autonomy constraint, trust degradation). Not fully trapped — some agents can and do relocate, change employment, or find medical exemptions. The constraint solves a real coordination problem (herd immunity, system capacity) while asymmetrically extracting compliance through coercive mechanisms. This is the canonical tangled rope: mixed coordination + extraction requiring active enforcement.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH BUREAUCRACY (ROPE) — Experiences mandate as pure coordination: efficient disease control, system capacity preservation, population health metrics. No extraction from this perspective — the mandate mechanism itself becomes the instrument of institutional authority and resource allocation. Exit is possible (arbitrage): the bureaucracy can choose enforcement intensity, exemption criteria, and mandate timing. Gains authority without bearing costs of enforcement (costs are borne by refusers and constrained populations). Derives legitimacy from public health necessity.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__public_health_primacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH ETHICS COALITION (SCAFFOLD) — Organized analysis recognizes mandate as temporary public health tool with sunset. From this perspective, the constraint is a justified extraordinary measure during epidemic crisis (high mortality/severity), but temporally bounded: as transmission declines, disease severity decreases, or better risk-stratified alternatives emerge, mandate authority should sunset to baseline regulatory frameworks. Theater is low (functional disease prevention), exit is mobile (coalition can advocate for sunset), and coordination benefit is transparent. This reading sees the mandate as legitimate DURING crisis but delegitimized if extended beyond epidemiological justification.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__public_health_primacy_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the externality logic appears immutable: unvaccinated individuals create disease transmission risk that others cannot avoid (absent isolation). This is framed as a natural law of epidemiology — infectious disease creates unavoidable externalities, and state authority to control externalities is a foundational principle of law (Pigouvian correction of market failure). From this view, mandate authority is not extractive but a necessary response to irreducible physical reality. However, this naturalizes contingent policy choices: risk stratification, exemption regimes, and enforcement mechanisms are all policy decisions, not laws of nature. FALSE SUMMIT CANDIDATE.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__public_health_primacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: HISTORICAL INSTITUTIONAL AUTHORITY (PITON) — Observes mandate authority as institutionalized power maintained through theatrical epidemiological necessity rather than functional disease control. As pandemic acuity declines, mandate persistence persists through institutional inertia, emergency-declaration extensions, and bureaucratic self-perpetuation rather than epidemiological justification. Theater increases (performative booster mandates with minimal marginal benefit), while extraction continues. This reading sees the mandate system itself as a degraded institutional form: once-justified extraordinary power that has atrophied functionally but persists through authority maintenance rituals.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__public_health_primacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__public_health_primacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__public_health_primacy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, TR),
    TR >= 0.70.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): This reading produces moderately high extractiveness because coercive enforcement is applied asymmetrically — refusers bear maximum costs while beneficiaries (public health authority, protected populations) bear minimal costs. However, extractiveness is not at snare levels (≥0.66) because genuine coordination benefits exist: disease prevention is not purely extractive but serves a legitimate collective good. The constraint solves a real coordination problem (herd immunity threshold for disease control) while imposing asymmetric suppression costs. The rising trajectory (0.48 → 0.58) reflects that as disease severity declines, the ratio of extraction to coordination benefit increases — the same enforcement mechanism persists (path dependency, institutional inertia) while the epidemiological justification weakens. Suppression (0.72): High suppression reflects the coercive character of enforcement — employment termination, institutional access denial, licensing suspension, school enrollment barriers. These are not incentives or nudges but categorical penalties for non-compliance. Suppression is applied to a powerless agent (vaccine refusers with no arbitrage options) and rises over time as administrative mechanisms for enforcement mature (workplace vaccination tracking, school mandates, healthcare worker requirements). Theater ratio (0.38): Moderate-low theater indicates that the mandate mechanism has genuine functional content (disease prevention works) and not primarily performative function. However, theater increases over time (0.15 → 0.38) as epidemiological justification decreases — as disease severity and transmission decline, continued mandate enforcement becomes increasingly theatrical (performative disease prevention rather than functionally necessary coercion). The low initial theater reflects emergency crisis conditions; the rising trajectory reflects post-crisis institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The public health primacy reading produces a maximally wide perspectival gap. Public health bureaucracy sees the constraint as legitimate coordination (Rope) — solving a real collective action problem with no extraction from their structural position. Vaccine refusers see pure extraction (Snare) — coercion applied without benefit. Hesitant populations see mixed coordination and extraction (Tangled Rope) — both benefits (disease prevention) and costs (autonomy). The analytical observer at civilizational scale risks seeing an immutable natural law (Mountain) — externalities are unavoidable, state duty to control them is foundational — but the identifiable beneficiaries (public health bureaucracy authority expansion) trigger false summit detection: the 'natural law' is actually a contingent institutional arrangement that benefits specific agents. This perspectival gap is diagnostic evidence that the constraint's legitimacy depends on which reading's normative framework you adopt: if collective harm prevention is the foundational duty, the constraint is legitimate coordination; if bodily autonomy is foundational, the constraint is unjustified extraction. The readings are not empirically resolvable but require normative choice.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading derives directionality from the structural position of each agent. Public health bureaucracy: beneficiary status + arbitrage exit = low d (benefits from constraint, can adjust enforcement intensity). Vaccine refusers: victim status + trapped exit = high d (bear costs, no meaningful exit). Hesitant population: victim status + constrained exit = moderately high d (bear costs, but some exit capacity through relocation, exemption, employment change). Immunocompromised: beneficiary status + mobile exit = low d (benefit from protection, can choose vaccination or isolation). The engine computes f(d) from these d values; public health perspectives see low chi (coordination, no extraction from their position), while refuser perspectives see high chi (extraction, coercion). Scope modifier σ(S) = 1.0 (national scale); no amplification or dampening from scope. The perspectival gap reflects that the same constraint produces radically different classifications depending on the observer's structural position: beneficiaries see rope, victims see snare, analytical observers risk naturalizing as mountain (false summit due to identified beneficiaries).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve mandatrophy (it is not above the 0.70 threshold), but it contains the raw materials for potential future mandatrophy if the constraint drifts toward pure snare territory. The current classification (Tangled Rope) holds together because the constraint contains both coordination function (disease prevention) and extraction mechanism (coercive enforcement). The rising extractiveness trajectory (0.48 → 0.58) and theater trajectory (0.15 → 0.38) show drift toward snare territory. If disease severity and transmission decline further (plausible over medium term), the constraint could cross into snare classification if extractiveness exceeds 0.66 and suppression remains ≥0.60. At that point, mandatrophy would require resolution: either the reading must shift to acknowledge that extraction exceeds coordination function (moving toward the bodily autonomy or risk stratification readings), or the sunset clause must trigger (transitioning to Scaffold). The measurements track this potential drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_boundary_definition,
    'Does unvaccinated status constitute an externality equivalent to pollution or infectious disease transmission, or is it a difference in risk profile that should be addressed through risk stratification rather than categorical mandate?',
    'Epidemiological analysis: quantify transmission risk differential between vaccinated and unvaccinated; compare marginal harm to coercion cost; evaluate risk stratification (age, comorbidity, vaccination status) as alternative allocation mechanism.',
    'If true externality: mandate is justified coordination mechanism (Rope from public health view). If risk-profile difference: risk stratification reading becomes dominant, and coercive mandate becomes illegitimate extraction (Snare from refuser view confirmed). Classification shifts to Tangled Rope or Snare depending on externality magnitude.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(externality_boundary_definition, empirical, 'Whether unvaccinated status constitutes a true externality or a risk-profile difference').

omega_variable(
    proportionality_threshold_ambiguity,
    'At what disease severity / transmission rate / healthcare system utilization threshold is mandate authority proportional to externality magnitude? When does mandate exceed the minimum necessary coercion?',
    'Comparative analysis across jurisdictions and time periods: mandate intensity vs disease metrics; threshold analysis (at what hospitalization rate does mandate become disproportionate?); counterfactual: would voluntary incentives (paid vaccination, priority access, social recognition) have achieved equivalent coordination at lower suppression cost?',
    'If threshold analysis shows mandate persists above medical justification: extraction exceeds coordination function, constraint reclassifies to Snare. If threshold is clear and respected: Tangled Rope sustained. If threshold is absent or shifting: suppression is arbitrary (omegas unresolved), constraint degrades toward Piton (institutional inertia without functional justification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, empirical, 'Proportionality threshold between externality magnitude and mandate coercion').

omega_variable(
    natural_law_vs_policy_construction,
    'Is the mandate authority grounded in immutable epidemiological law, or is it a contingent policy construction that naturalizes specific enforcement choices (threshold vaccination rates, exemption regimes, duration of enforcement)?',
    'Textual and historical analysis: compare this reading''s framing of ''duty to prevent collective harm'' against alternative readings (bodily autonomy, risk stratification). Identify which elements are empirically constrained (disease transmission) and which are policy-decided (mandate threshold, exemption criteria, enforcement mechanisms). FSM trigger: if beneficiaries are identifiable (public health bureaucracy authority expansion), the mountain classification is a false summit.',
    'If natural law: Mountain classification sustained. If policy construction: false summit triggered, reclassifies to Tangled Rope or Snare depending on whether extraction exceeds coordination function. This omega is critical for distinguishing legitimate collective harm prevention from extractive authority expansion disguised as natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_policy_construction, conceptual, 'Whether mandate authority is rooted in natural law or contingent policy construction').

omega_variable(
    autonomy_loss_quantification,
    'What is the magnitude of bodily autonomy loss imposed by mandate? Is it equivalent to standard public health interventions (quarantine, isolation, treatment mandate for active TB), or does it exceed precedent in scope or coercion intensity?',
    'Comparative legal and bioethical analysis: autonomy cost of vaccine mandate vs quarantine, contact tracing, communicable disease isolation orders, mandatory TB treatment; precedent from other medical requirements (licensing, professional standards); exit cost analysis (employment termination vs medical non-compliance penalties). Suppression magnitude calibration.',
    'If autonomy loss is consistent with precedent: suppression (0.72) may be justified. If autonomy loss exceeds precedent: suppression is illegitimate (omegas unresolved), constraint degrades or reclassifies. If autonomy loss is presented as zero (''voluntary'' with employment termination consequences): theater increases, institutional inertia risk (Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_loss_quantification, empirical, 'Magnitude of bodily autonomy loss relative to precedent and coercion intensity').

omega_variable(
    sibling_reading_empirical_contingency,
    'How much of the divergence between this reading (public health primacy) and the bodily autonomy primacy reading is rooted in empirical disagreement (transmission risk, autonomy cost) vs normative disagreement (which values take priority)?',
    'Structured empirical assessment: map specific factual claims each reading depends on (transmission risk per vaccine status, autonomy harm magnitude, disease severity, alternative coordination mechanisms). Identify which empirical claims are contested and which are settled. Separate empirical uncertainty from value disagreement.',
    'If empirical, measurable disagreements drive divergence: both readings are empirically contingent and should coexist until data resolves. If value disagreement is primary: readings foreclose each other only within specific value frameworks; multiple frameworks can hold both readings. This informs whether relation is ''forecloses'' vs ''coexists_with''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_contingency, conceptual, 'Extent to which sibling reading divergence is empirical vs normative').

omega_variable(
    sunset_clause_institutional_enforcement,
    'Does this reading contain an implicit or explicit sunset clause (mandate authority expires when disease severity/transmission drops below threshold), or does it authorize permanent state capacity expansion?',
    'Textual and institutional analysis: examine public health authority enabling legislation, emergency declaration language, and administrative practice. Does mandate terminate automatically when conditions change, or does it require active legislative repeal? Are conditions for termination specified (disease severity metrics, hospitalization thresholds)? Historical precedent from prior public health emergencies (post-polio, post-smalleradication).',
    'If sunset is clear and enforced: Scaffold classification gains empirical support (genuine temporary authority). If sunset is absent or routinely extended: Piton trajectory confirmed (institutional authority persisting beyond functional justification). If sunset exists but institutional barriers prevent execution: institutional inertia decouples from epidemiological necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_institutional_enforcement, empirical, 'Whether mandate authority includes enforceable sunset clause').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vax_mandate_theater_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vax_mandate_theater_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(vax_mandate_theater_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 4, 0.38).

% Extraction over time
narrative_ontology:measurement(vax_mandate_extract_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(vax_mandate_extract_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(vax_mandate_extract_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 4, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vax_mandate_suppress_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(vax_mandate_suppress_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(vax_mandate_suppress_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 4, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, pandemic_emergency_authority_expansion).

% DUAL FORMULATION NOTE:
% The vaccine mandate legitimacy kernel decomposes into three structurally distinct constraints corresponding to three sibling readings. Each reading instantiates a different normative commitment about what duties the state bears: (1) public health primacy (this constraint) grounds state authority in externality prevention; (2) bodily autonomy primacy grounds refusal in inviolable self-determination; (3) risk stratification grounds authority in precision targeting rather than categorical mandates. The three constraints have different ε values, different beneficiary/victim sets, and different institutional dynamics. They are not empirical alternatives (not testable as true/false) but normative alternatives (not jointly satisfiable within a single framework). The network structure connects all three; changes in one constraint's institutional realization affect the others' legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__public_health_primacy_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
