% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: State-Coerced Medical Intervention (Bodily Autonomy Primary Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel:
 *   legitimate_health_intervention. The kernel is the competing authority
 *   claim about what makes medical policy legitimate — bodily autonomy as
 *   inviolable, or proportionality between intervention and threat, or public
 *   health outcomes as primary. This reading (bodily_autonomy_primary)
 *   declares that legitimacy requires informed consent and cannot be
 *   overridden by state authority even for demonstrable public benefit. From
 *   this reading's structural perspective, mandate-coerced medical
 *   interventions constitute extraction: the state uses employment,
 *   education, and service-access contingencies to force compliance from
 *   unwilling individuals. The constraint exhibits the classic snare
 *   signature: high suppression (coercive enforcement machinery),
 *   moderate-to-high extractiveness (asymmetric benefit to the state and
 *   vaccination-compliant population; asymmetric cost to the coerced), and
 *   low theater (the coercion is explicit, not disguised as voluntary
 *   coordination). The critical analytic task is to document how this reading
 *   relates to its siblings: the public_health_primary reading (which treats
 *   population-level morbidity reduction as legitimacy source) and the
 *   proportionality_reading (which weights autonomy against threat but
 *   neither makes autonomy primary). This reading forecloses the
 *   public_health_primary reading's core premise (that individual refusal is
 *   merely externality imposition) while coexisting with the
 *   proportionality_reading in the same legal framework (jurisdictions can
 *   adopt proportionality thresholds that sometimes align with
 *   bodily_autonomy_primary outcomes).
 *
 * KEY AGENTS:
 *   - Mandate-Coerced Individuals: Primary victim (powerless/trapped) — face binary choice: medical intervention or employment/service loss. No genuine exit options; maximum experienced extraction.
 *   - State Public Health Apparatus: Primary beneficiary (institutional/arbitrage) — gains enforcement authority and administrative control. Perceives the constraint as pure coordination, not extraction.
 *   - Vaccinated Population: Secondary beneficiary (moderate/constrained) — benefits from disease protection and herd immunity but also constrained by mandate regime.
 *   - Medical Refusal Coalition: Organized resistance (organized/mobile) — religious bodies, civil liberties groups, philosophical libertarians. Have litigation and political leverage; experience mixed extraction and contestation.
 *   - Legitimacy Doctrine System: Institutional actor (institutional/arbitrage) — maintains the consent fiction (piton perspective). The informed-consent ritual persists despite coercion context.
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing the reading-dependence as natural law rather than doctrinal choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.58).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "State-Coerced Medical Intervention (Bodily Autonomy Primary Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '848236a0-9ce2-4967-9570-087c9ee7be96').
narrative_ontology:cs_kernel_codification('848236a0-9ce2-4967-9570-087c9ee7be96', formalized).
narrative_ontology:cs_authority_grounding('848236a0-9ce2-4967-9570-087c9ee7be96', lineage).
narrative_ontology:cs_interpretation_layer_present('848236a0-9ce2-4967-9570-087c9ee7be96').
narrative_ontology:cs_reading_relation('848236a0-9ce2-4967-9570-087c9ee7be96', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('848236a0-9ce2-4967-9570-087c9ee7be96', legitimate_health_intervention__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('848236a0-9ce2-4967-9570-087c9ee7be96', foundational, bodily_integrity_inalienable).
narrative_ontology:cs_axiom_status(bodily_integrity_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('848236a0-9ce2-4967-9570-087c9ee7be96', bodily_integrity_inalienable, deontological).
narrative_ontology:cs_axiom('848236a0-9ce2-4967-9570-087c9ee7be96', secondary, state_coercion_delegitimizes_consent).
narrative_ontology:cs_axiom_status(state_coercion_delegitimizes_consent, holdable).
narrative_ontology:cs_axiom_grounding('848236a0-9ce2-4967-9570-087c9ee7be96', state_coercion_delegitimizes_consent, deontological).
narrative_ontology:cs_reference_frame('848236a0-9ce2-4967-9570-087c9ee7be96', informed_consent_doctrine_supremacy).
narrative_ontology:cs_drift_state('848236a0-9ce2-4967-9570-087c9ee7be96', contemporary_mandate_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('848236a0-9ce2-4967-9570-087c9ee7be96', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_apparatus).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, vaccinated_population).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, employment_contingent_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, bodily_integrity_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MANDATE-COERCED INDIVIDUAL (SNARE) — Faces binary choice: submit to medical intervention or lose employment, education access, or public services. Exit options are illusory — relocation to non-mandating jurisdictions is economically prohibitive for most. The constraint extracts compliance through coercion, not coordination. No genuine benefit accrues to the coerced agent from the intervention itself (they would refuse if free); the 'benefit' is avoiding punishment. Maximum experienced extraction.
constraint_indexing:constraint_classification(legitimate_health_intervention__bodily_autonomy_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VACCINATED POPULATION / DISEASE-PROTECTED (TANGLED ROPE) — Benefits from reduced disease transmission and herd immunity threshold. Also constrained by the mandate regime (cannot claim full autonomy if accepting mandatory vaccination). Experiences mixed extraction and genuine coordination: the constraint both protects them (coordination function) and restricts their freedom to choose (extraction via enforcement). Moderate extraction with real coordination benefit.
constraint_indexing:constraint_classification(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH APPARATUS / STATE ACTOR (ROPE) — Experiences the constraint as pure coordination: mandates are tools for solving collective action problems (free-rider vaccination avoidance, disease spread). The state apparatus benefits from enforcement capacity and implementation authority. No extraction experienced by the state — the coercion is the mechanism, not the cost. The state perceives its own action as legitimate coordination.
constraint_indexing:constraint_classification(legitimate_health_intervention__bodily_autonomy_primary, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MEDICAL LEGITIMACY RITUAL (PITON) — The informed-consent doctrine persists as performative: consent forms are signed but meaningful choice is absent when non-compliance triggers employment termination. The ritual of 'consent' provides institutional cover for coercion. Theater ratio high (consent theater) while the actual enforcement is non-negotiable. The legitimacy frame has degraded from genuine autonomous decision-making to compliance theater.
constraint_indexing:constraint_classification(legitimate_health_intervention__bodily_autonomy_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a deontological/civilizational perspective, bodily integrity is treated as an inalienable right that cannot be overridden by utilitarian calculation or state authority. This perspective frames the constraint as an immutable principle: no legitimate government can coerce medical intervention on unwilling individuals, period. The state attempting to do so moves into illegitimacy by definition. However, this classification risks false-summit status — the 'immutability' of bodily integrity is contestable (the public_health_primary reading does not treat it as immutable). The analytical observer must document the reading-dependence.
constraint_indexing:constraint_classification(legitimate_health_intervention__bodily_autonomy_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: MEDICAL REFUSAL COALITION / ORGANIZED RESISTANCE (TANGLED ROPE) — Organized agents (religious bodies, civil liberties groups, philosophical libertarians) perceive both extraction (coercion mechanism) and coordination (collective defense of bodily autonomy principle). They have exit options (litigation, political mobilization, jurisdictional arbitrage) and leverage. The constraint extracts compliance from individual targets but faces resistance from organized counter-coalitions. Moderate-to-high extraction with active contestation.
constraint_indexing:constraint_classification(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimate_health_intervention__bodily_autonomy_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimate_health_intervention__bodily_autonomy_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, TR),
    TR >= 0.70.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading's core claim is that coerced medical intervention extracts compliance from unwilling targets. The extraction is not total (agents remain alive and functional) but substantial (they lose employment, education access, or public services if they refuse). The measurement reflects enforcement severity — employment contingency is coercive but not physical confinement. A reading that emphasized physical quarantine or imprisonment would yield higher ε. Suppression (0.72): High. The mechanism of suppression is explicit: employment termination, education denial, service exclusion. These are structured barriers to refusal. Suppression is not about hidden constraints but about clearly stated consequences for non-compliance. Theater ratio (0.35): Moderate-low. This reading emphasizes that the coercion is transparent rather than disguised. The state does not claim the mandate is voluntary; it explicitly enforces compliance. The consent forms are theater (the theater field), but the coercive mechanism itself is functionally direct, not performative. The low theater distinguishes this from the piton perspective, which emphasizes the consent-ritual degradation.
 *
 * PERSPECTIVAL GAP:
 *   The bodily_autonomy_primary reading generates maximum perspectival gap. The state sees rope or tangled rope (coordination for public benefit). The coerced individual sees snare (pure extraction). The vaccinated population sees tangled rope (they benefit but are also constrained). The coalition sees tangled rope with contestation (they can mobilize). The consent ritual sees piton (degraded institution). The analytical observer risks seeing mountain (naturalizing reading-dependence as natural law). The core gap: whether the state's extraction is 'legitimate coordination' (state view) or 'illegitimate coercion' (reading-dependent view).
 *
 * DIRECTIONALITY LOGIC:
 *   The bodily_autonomy_primary reading positions mandate-coerced individuals as victims: they are the targets of extraction. The state and vaccinated population are beneficiaries: they receive the benefit of enforcement compliance and disease reduction. From the coerced individual's perspective (powerless/trapped), directionality d is high (near 1.0) — they are the target of asymmetric extraction with no exit. From the state's perspective (institutional/arbitrage), directionality d is low (near 0.0) — the state is the beneficiary extracting from the coerced. This structural relationship — beneficiary extracting from trapped victim — produces the snare classification. The derivation chain: victim status + trapped exit → high d → high f(d) → high χ. The organized coalition perspective differs: they are organized agents with mobile exit options (litigation, mobilization), so their derived d is lower, producing tangled rope instead of snare. The piton perspective's institutional directionality diverges from the organized coalition because the legitimacy ritual itself (not the coalition's capacity) is the actor.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through explicit reading-framing. It is a snare under bodily_autonomy_primary because the reading treats individual refusal as a foundational right that cannot be overridden. Under public_health_primary, the same constraint would be tangled rope or rope (state coordination for measurable benefit). Under proportionality_reading, it would be tangled rope for high-risk interventions on low-threat diseases and rope for high-risk interventions on severe-threat diseases. The mandatrophy is not resolved by finding 'the true type' but by documenting how type varies with reading. The analytical observer cannot avoid the reading-dependence through sufficiently refined measurement. The constraint's type is determined by normative framework, not by empirical fact alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_dependence_of_legitimacy,
    'Is bodily integrity an inalienable right that cannot be overridden (bodily_autonomy_primary reading) or is legitimacy determined by proportionality between intervention severity and public health threat (proportionality_reading)?',
    'This is a foundational reading choice, not an empirical question. Resolution requires explicit commitment to a normative framework (deontological autonomy-first vs. consequentialist proportionality). Different legal traditions and constitutional regimes make different choices.',
    'If bodily_autonomy_primary is adopted: mandates on severe interventions (surgical procedures, irreversible modifications) are categorically illegitimate regardless of public benefit; mandates on low-risk interventions (vaccines) may be proportionate but remain problematic. If proportionality_reading is adopted: mandates on highly effective interventions for severe threats (smallpox) are legitimate; mandates on moderate-risk interventions for low-threat diseases are not. Omegas shift: what counts as ''extraction'' depends on which reading is operative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_dependence_of_legitimacy, conceptual, 'Whether bodily autonomy is inalienable or subject to proportionality weighing').

omega_variable(
    consent_degradation_empirical,
    'When medical mandates are enforced via employment contingency or service access barriers, does the signed ''informed consent'' form constitute genuine consent or theater?',
    'Empirical behavioral analysis: measure proportion of mandate-coerced individuals who would choose the intervention absent employment/access consequences. Track consent comprehension and autonomous choice capacity in high-coercion vs. low-coercion settings. Compare real-world consent quality metrics in employment-mandatory vs. voluntary-access contexts.',
    'If consent is substantially degraded by coercion context: piton classification confirmed (consent ritual is performative). If consent quality is maintained across settings: tangled rope classification for moderate-coercion regimes becomes more defensible. Theater_ratio measurement depends on this empirical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_degradation_empirical, empirical, 'Whether employment-contingent ''consent'' is genuine or performative').

omega_variable(
    herd_immunity_threshold_necessity,
    'For a specific disease and intervention, is the state-enforced compliance rate strictly necessary to achieve documented public health outcomes, or could lower participation rates (with voluntary uptake only) achieve substantially similar outcomes?',
    'Comparative epidemiological analysis: model disease spread under observed voluntary uptake vs. mandate-induced uptake, controlling for baseline risk factors. Identify diseases where herd immunity threshold is unattainable through voluntary means alone vs. those where voluntary rates approximate mandate outcomes.',
    'If herd immunity threshold is necessity-driven: extractiveness may be justified as coordination overhead (tangled rope). If voluntary rates are sufficient or near-sufficient: mandate extraction is not coordinationally necessary — ε rises (snare territory). If voluntary rates would achieve similar outcomes: the state''s extraction has no public health justification — pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herd_immunity_threshold_necessity, empirical, 'Whether mandate-level compliance is epidemiologically necessary or merely sufficient').

omega_variable(
    alternative_exit_routes_availability,
    'What proportion of mandate-targeted individuals can realistically exercise exit options (relocation, occupational switch, home schooling, alternative services) without catastrophic cost?',
    'Labor market analysis: document income penalty, housing accessibility, geographic feasibility, and skill transferability for exit pathways. Measure actual exit rates and their socioeconomic profile. Identify which exit routes are genuinely available vs. theoretically available only to wealthy individuals.',
    'If exit routes are genuinely available to substantial population: exit_options upgrade from trapped to constrained. If exit routes are accessible only to wealthy: the constraint has a wealth-dependent trap structure (poor agents trapped, wealthy agents constrained) — requires differentiated perspectives per socioeconomic group. If exit routes are negligible: trapped classification confirmed; suppression measure is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_exit_routes_availability, empirical, 'Realistic availability of exit options for mandate-coerced individuals').

omega_variable(
    bodily_autonomy_principle_status,
    'Is the bodily-autonomy-primary reading''s foundational axiom (bodily_integrity_inalienable) holdable in contemporary discourse or has it been formally overridden by public health prioritization in constitutional law?',
    'Jurisprudential analysis: examine precedent in constitutional law jurisdictions (US, EU, common law) for how courts balance bodily autonomy against public health. Identify whether the autonomy principle remains live or has been formally subordinated in doctrine.',
    'If axiom is holdable: the bodily_autonomy_primary reading is a live legitimate position. If axiom is overridden in case law: the reading persists as normative claim but lacks institutional authority grounding; it is an oppositional reading against established doctrine. Status field must reflect actual jurisprudential standing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_principle_status, conceptual, 'Current jurisprudential status of bodily autonomy as inalienable vs. overridden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lhi_ba_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lhi_ba_tr_t3, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 3, 0.3).
narrative_ontology:measurement(lhi_ba_tr_t6, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(lhi_ba_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lhi_ba_be_t3, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(lhi_ba_be_t6, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lhi_ba_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lhi_ba_su_t3, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 3, 0.68).
narrative_ontology:measurement(lhi_ba_su_t6, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, vaccine_mandate_labor_market_dynamics).

% DUAL FORMULATION NOTE:
% The legitimate_health_intervention kernel has three constraint stories corresponding to three readings of legitimacy. This file (bodily_autonomy_primary) treats bodily integrity as inalienable. The proportionality_reading story weighs autonomy against public health threat. The public_health_primary story treats population-level outcomes as primary legitimacy source. Each story has its own ε, its own perspectives, and its own classification. The readings are linked via network.affects_constraints to show the kernel contest. The ε values differ because each reading's structural assumptions change what counts as extraction: what looks like 'state coordination for collective benefit' (ε ≈ 0.40, rope) to the public_health_primary reading looks like 'coercive extraction from unwilling targets' (ε = 0.58, snare) to the bodily_autonomy_primary reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__bodily_autonomy_primary, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
