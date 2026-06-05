% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__risk_stratification_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Vaccine Mandate Legitimacy via Risk Stratification (Risk Stratification Reading)
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint represents ONE reading of a contested kernel: vaccine
 *   mandate legitimacy. The kernel itself is a stabilized commitment that
 *   different parties read differently. The risk-stratification reading holds
 *   that mandate legitimacy is contingent on actuarial risk thresholds —
 *   blanket mandates violate proportionality, but targeted mandates
 *   (high-risk populations, high-transmission occupations) are defensible.
 *   This reading sits between two extremes: the public_health_primacy reading
 *   (public health authority can mandate vaccination broadly to protect
 *   collective population health) and the bodily_autonomy_primacy reading
 *   (individuals have an inviolable right to refuse medical intervention
 *   regardless of public health benefit). The risk-stratification reading
 *   accepts mandates but constrains their scope through proportionality:
 *   mandate coverage should match evidence-based risk profiles. As the
 *   pandemic has evolved, the constraint has shifted from low extractiveness
 *   (early pandemic, clear high-risk groups) toward higher extractiveness
 *   (later variants with flatter risk profiles across age groups, mandates
 *   persisting despite evidence updates, suppression mechanisms hardening).
 *   The theater ratio has risen as the constraint's functional justification
 *   (evidence-based high-risk protection) decouples from its enforcement
 *   machinery (universal or broad-category mandates).
 *
 * KEY AGENTS:
 *   - High-Risk Population: Primary beneficiary (powerful/mobile) — experiences the constraint as coordination; benefits from herd immunity and targeted protection
 *   - Low-Risk Unvaccinated Individual: Primary victim (powerless/trapped) — perceives mandate as illegitimate extraction; trapped by nationwide enforcement
 *   - Borderline-Risk Population: Secondary victim (moderate/constrained) — mixed experience; mandates apply to them despite marginal risk; exit constrained
 *   - Public Health Authority: Enforcer (institutional/constrained) — administers the constraint; experiences mixed coordination (solving herd immunity problem) and extraction (authority consolidation, administrative convenience)
 *   - Evidence-Based Policy Coalition: Organized actors (organized/mobile) — epidemiologists, medical ethics boards, transparency advocates; perceive scaffold with sunset as evidence accumulates
 *   - Emergency Authority Framework: Institutional apparatus (institutional/arbitrage) — maintains constraint through inertia as original evidence justification degrades; sees constraint as increasingly performative
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent policy choice as inherent to public health science
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.38).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.48).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Vaccine Mandate Legitimacy via Risk Stratification (Risk Stratification Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '6932ef53-cee0-406e-ad0d-83e0d58a0dc8').
narrative_ontology:cs_kernel_codification('6932ef53-cee0-406e-ad0d-83e0d58a0dc8', formalized).
narrative_ontology:cs_authority_grounding('6932ef53-cee0-406e-ad0d-83e0d58a0dc8', extraction).
narrative_ontology:cs_interpretation_layer_present('6932ef53-cee0-406e-ad0d-83e0d58a0dc8').
narrative_ontology:cs_reading_relation('6932ef53-cee0-406e-ad0d-83e0d58a0dc8', vaccine_mandate_legitimacy__public_health_primacy_reading, influences).
narrative_ontology:cs_reading_relation('6932ef53-cee0-406e-ad0d-83e0d58a0dc8', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('6932ef53-cee0-406e-ad0d-83e0d58a0dc8', foundational, mandate_scope_proportional_to_actuarial_risk).
narrative_ontology:cs_axiom_status(mandate_scope_proportional_to_actuarial_risk, holdable).
narrative_ontology:cs_axiom_grounding('6932ef53-cee0-406e-ad0d-83e0d58a0dc8', mandate_scope_proportional_to_actuarial_risk, empirically_contingent).
narrative_ontology:cs_axiom('6932ef53-cee0-406e-ad0d-83e0d58a0dc8', secondary, threshold_updates_track_evidence).
narrative_ontology:cs_axiom_status(threshold_updates_track_evidence, overridden).
narrative_ontology:cs_axiom_grounding('6932ef53-cee0-406e-ad0d-83e0d58a0dc8', threshold_updates_track_evidence, empirically_contingent).
narrative_ontology:cs_reference_frame('6932ef53-cee0-406e-ad0d-83e0d58a0dc8', proportionality_constrained_mandate_scope).
narrative_ontology:cs_drift_state('6932ef53-cee0-406e-ad0d-83e0d58a0dc8', contemporary_omicron_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6932ef53-cee0-406e-ad0d-83e0d58a0dc8', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_population_protected).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_system_capacity).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_unvaccinated_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, medical_autonomy_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-RISK POPULATION (ROPE) — Perceives the mandate as pure coordination. The constraint solves a genuine collective action problem: absent mandate, vaccination rates for immunocompromised and elderly populations would remain suboptimal because individual choice reflects private cost-benefit, not herd immunity threshold. The beneficiary experiences legitimate coordination benefit — the mandate coordinates vaccination to levels that protect them. Exit option is mobile (can relocate to jurisdictions with similar protections, or accept vaccination as condition of certain settings). No meaningful extraction perceived.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__risk_stratification_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-RISK UNVACCINATED INDIVIDUAL (SNARE) — For an agent whose actuarial risk is below the threshold defined by the reading, the mandate appears as pure extraction: the constraint is enforced against them despite the evidence-based case for their risk category not exceeding the legitimacy threshold. The victim experiences suppression through employment requirements, educational access barriers, and travel restrictions. Exit is trapped — they cannot reasonably relocate to escape nationwide or regional mandates. They perceive the constraint as an illegitimate use of state power that violates proportionality: it imposes compulsory medical intervention on an agent whose actual risk does not justify it.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__risk_stratification_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: BORDERLINE-RISK POPULATION (TANGLED ROPE) — For individuals whose actuarial risk sits at or near the threshold, the constraint mixes genuine coordination (they do benefit from broad vaccination) with extraction (the mandate applies to them despite their risk being marginal). Exit is constrained: they can accept vaccination to avoid the suppression mechanisms, but the cost is real (bodily autonomy, medical decision-making autonomy). The constraint coordinates herd immunity while extracting a rights concession from a population whose risk does not clearly justify it.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH AUTHORITY (TANGLED ROPE) — The enforcement institution experiences the constraint as mixed coordination and extraction. Genuine coordination: the authority is solving the legitimate herd immunity problem by establishing mandates tied to evidence-based risk thresholds. Extraction: the authority derives legitimacy and power consolidation from the emergency declaration and enforcement machinery. The authority's exit is constrained: it could rescind mandates below the threshold, but doing so requires abandoning the uniform approach and investing in individualized risk assessment, which reduces administrative simplicity and loses some enforcement power. The authority experiences the constraint as legitimate coordination (from the high-risk protection goal) but also knows it functions partly as extraction (administrative convenience, authority consolidation).
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EVIDENCE-BASED POLICY COALITION (SCAFFOLD) — Organized actors (epidemiological societies, medical ethics boards, transparency advocates) perceive the risk-stratification approach as a temporary scaffold with a clear sunset: as vaccine safety data mature, variant risk profiles stabilize, and population immunity accumulates, the justification for broad mandates erodes. The scaffold transitions to targeted protection (e.g., mandates for healthcare workers and high-risk settings only). This perspective sees the constraint as legitimate during the acute phase but unsustainable long-term without evidence-driven threshold updates. Low effective extraction because the coalition has agency and sees an exit path (threshold reductions, mandate rescissions).
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__risk_stratification_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: EMERGENCY AUTHORITY FRAMEWORK (PITON) — The institutional apparatus sees the mandate legitimacy regime as increasingly performative. Early in the pandemic, the risk-stratification reading provided genuine functional guidance: narrow mandates to high-risk populations and high-transmission settings. Over time, the constraint becomes theater — mandates persist beyond evidence justification (theater_ratio rising), enforcement mechanisms harden (suppression increases), and the authority maintains the constraint partly through institutional inertia. Exit is arbitrage (the institution can rescind mandates, transition to voluntary programs), but doing so requires acknowledging that the emergency justification has degraded. The piton classification reflects that the constraint's original function (evidence-based risk coordination) atrophies while the enforcement machinery persists.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__risk_stratification_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, this reading risks naturalizing the constraint as an immutable principle: 'Public health requires risk-based mandatory vaccination; proportionality demands threshold-based mandates; these are structural features of legitimate epidemiology.' However, the base properties reveal this as a false summit: the constraint benefits identifiable actors (healthcare system, high-risk populations), requires active enforcement, and exhibits rising theater. The mountain classification is a perspectival trap — it naturalizes a contingent policy choice as inherent to public health science.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__risk_stratification_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__risk_stratification_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__risk_stratification_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, TR),
    TR >= 0.70.

:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The risk-stratification reading permits mandates only where actuarial evidence supports them. In the early pandemic, with clear high-risk populations and unknown natural immunity, extractiveness was low (~0.15) — mandates targeted genuinely high-risk groups, and the constraint functioned as coordination. Over time, extractiveness has risen as: (1) risk profiles have flattened across age groups (Omicron variants lower hospitalization risk at all ages), (2) natural immunity and vaccination rollout have reduced absolute risk for many populations, (3) thresholds have not updated to match new evidence, creating mandate scope creep. By t12, extractiveness reaches 0.38 — mandates persist for populations whose risk no longer clearly justifies them. Suppression (0.48): Moderate-high. The constraint operates through employment requirements (healthcare workers, public employees), educational access barriers (school attendance), and travel restrictions. These are significant but not absolute barriers — individuals can choose to accept vaccination, relocate to jurisdictions with lower mandates, or engage in political advocacy to change thresholds. Suppression is higher at t12 than t0 because enforcement mechanisms have hardened and alternative compliance paths have narrowed. Theater ratio (0.52): Moderate. Early in the pandemic, risk-stratification mandates were functional — targeting high-risk populations reflected genuine evidence and coordinated vaccination. Over time, theater has increased as mandates have become decoupled from current evidence: variant risk profiles have changed, but mandate categories persist; suppression mechanisms harden despite reduced risk justification; the constraint becomes increasingly about authority maintenance and less about evidence-based protection. Claimed type (tangled_rope): This reading is structurally a tangled_rope because it accepts mandates (coordination function) but applies them in a way that imposes costs on low-risk populations (extraction). The constraint is not pure coordination (which would permit only mandates clearly justified by high risk) nor pure extraction (which would maximize mandate scope for administrative convenience).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across its index space. The high-risk population perceives rope (pure coordination), while the low-risk individual perceives snare (pure extraction). The public health authority perceives mixed tangled_rope (genuine coordination need + enforcement convenience), while the evidence-based coalition perceives scaffold (temporary measure with sunset as evidence updates). The emergency framework perceives piton (performative maintenance of authority), while the analytical observer risks perceiving mountain (naturalizing proportionality as inherent to epidemiology). The gap reveals that the constraint's classification depends entirely on the observer's structural position: who benefits, who bears costs, what exit options they have. There is no single 'true' classification — the presheaf over the observation site IS the full answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from their structural position relative to the constraint. High-risk populations are beneficiaries with mobile exit (they can relocate to jurisdictions with similar mandates) — canonical d ≈ 0.15, f(d) ≈ -0.01, producing rope classification. Low-risk unvaccinated are victims with trapped exit (nationwide mandates, no reasonable relocation option) — derived d ≈ 0.95, f(d) ≈ 1.42, producing snare classification. Borderline-risk populations are partially victims (marginal risk) with constrained exit (can accept vaccination at cost) — derived d ≈ 0.55, f(d) ≈ 0.75, producing tangled_rope. Public health authority is both beneficiary (gains enforcement power) and victim (constrained by evidence requirements if it accepts the reading) — derived d ≈ 0.45, f(d) ≈ 0.45, producing tangled_rope. Evidence-based coalition is organized with mobile exit (can advocate for threshold updates and exit/enter jurisdictions) — derived d ≈ 0.40, f(d) ≈ 0.40, producing scaffold. Emergency framework is institutional with arbitrage exit (can rescind mandates) — derived d ≈ 0.15, f(d) ≈ -0.01, but theater gate produces piton. Analytical observer is d ≈ 0.72, f(d) ≈ 1.15, producing mountain (false summit).
 *
 * MANDATROPHY ANALYSIS:
 *   The risk-stratification reading resolves mandatrophy by accepting that proportionality constraints on mandate scope are foundational, not deferential. This distinguishes it from the public_health_primacy reading, which treats proportionality as secondary to public health authority judgment. The reading avoids the contradiction between 'mandates are legitimate coordination' and 'blanket mandates are over-extraction' by restricting mandate scope to populations where risk evidence supports them. The mandatrophy is not fully resolved at the empirical level (omega variables remain about threshold definition and variant fluidity), but it is resolved at the structural level: the constraint is legitimately tangled_rope (mixed coordination and extraction) when thresholds match current evidence, and it drifts toward snare (pure extraction) when thresholds persist despite evidence updates. The measurement trajectory shows this drift: extractiveness rises from 0.15 to 0.38, reflecting that the constraint's original justification (high-risk protection) is decoupling from its application (persistent mandates despite flattened risk profiles). Theater ratio rising from 0.30 to 0.52 indicates the constraint is transitioning from functional coordination toward performative authority maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_definition_under_determination,
    'What actuarial risk threshold distinguishes legitimate mandate scope from overreach? How is it defined and by whom?',
    'Comparative analysis across jurisdictions; epidemiological consensus documents; legal threshold definitions (e.g., CFR, state regulations); assessment of whether threshold is evidence-derived or politically negotiated',
    'If threshold is high (e.g., >5% hospitalization risk): victim set is small, constraint appears as rope from more perspectives. If threshold is low (e.g., >0.1% risk): victim set is large, constraint appears as snare from more perspectives. If threshold is undefined or politically floating: constraint collapses into the public_health_primacy_reading (universal mandate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_definition_under_determination, empirical, 'Actuarial risk threshold definition and derivation').

omega_variable(
    risk_stratification_feasibility,
    'Can public health authorities actually implement individualized risk assessment and targeted mandates, or does administrative capacity force a choice between blanket mandates and no mandates?',
    'Audit of jurisdictions attempting risk-stratified mandates; cost analysis of individualized vs. blanket approaches; assessment of compliance mechanisms for targeted mandates',
    'If feasible: risk-stratification reading is structurally coherent; mandates remain tangled_rope for borderline cases. If infeasible: risk-stratification reading collapses into blanket mandates (public_health_primacy) or into no mandates (bodily_autonomy_primacy), depending on political pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_stratification_feasibility, empirical, 'Feasibility of implementing individualized risk assessment for mandate scope').

omega_variable(
    variant_risk_profile_fluidity,
    'As variant severity profiles change, how stable is the risk threshold? Does the constraint update to match new evidence, or does the threshold persist through bureaucratic inertia?',
    'Timeline analysis: comparison of mandate updates to variant emergence and risk reassessment; audit of whether thresholds were revised when new variant data arrived',
    'If thresholds update fluidly: constraint remains tangled_rope. If thresholds persist despite changed evidence: constraint drifts toward piton (performative enforcement) or snare (extraction masked by outdated evidence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(variant_risk_profile_fluidity, empirical, 'Responsiveness of risk thresholds to new variant and epidemiological data').

omega_variable(
    proportionality_axiom_contestability,
    'Is proportionality itself a foundational principle that constrains mandate scope, or is it one policy consideration among others that can be overridden by public health authority judgment?',
    'Constitutional law analysis; case law on proportionality standards; assessment of whether courts enforce proportionality constraints on emergency measures, or defer to public health authority discretion',
    'If proportionality is foundational: this reading''s axiom (mandate_scope_proportional_to_risk) is holdable. If proportionality is deferential: the axiom is weaker, and this reading coexists_with but is subordinate to public_health_primacy_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_axiom_contestability, conceptual, 'Whether proportionality is foundational to mandate legitimacy or deferential to authority discretion').

omega_variable(
    bodily_autonomy_foreclosure_scope,
    'Does the risk-stratification reading''s acceptance of mandates for high-risk populations logically foreclose the bodily_autonomy_primacy reading, or can both coexist as competing principles within a single legal framework?',
    'Constitutional analysis: whether courts recognize a hierarchy between bodily autonomy and public health authority, or treat them as competing principles that must be balanced contextually',
    'If foreclosed: this reading forecloses bodily_autonomy_primacy. If coexistent: they coexist_with each other, and the constraint remains contested within a single framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bodily_autonomy_foreclosure_scope, conceptual, 'Whether risk-stratification logically forecloses bodily autonomy primacy or permits coexistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmlegit_rs_theater_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(vmlegit_rs_theater_t6, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(vmlegit_rs_theater_t12, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 12, 0.52).

% Extraction over time
narrative_ontology:measurement(vmlegit_rs_extract_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(vmlegit_rs_extract_t6, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(vmlegit_rs_extract_t12, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 12, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(vmlegit_rs_suppress_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(vmlegit_rs_suppress_t6, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(vmlegit_rs_suppress_t12, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 12, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family modeling the contested kernel 'vaccine mandate legitimacy'. Each sibling reading (public_health_primacy and bodily_autonomy_primacy) instantiates a distinct constraint with different beneficiary/victim structures and ε values. The risk-stratification reading sits structurally between them, accepting mandates in high-risk cases but rejecting them in low-risk cases. Network links establish the reading relationships: this reading coexists_with its siblings, and proportionality constraints create influences (downstream pressure on public_health_primacy to justify mandate scope by evidence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
